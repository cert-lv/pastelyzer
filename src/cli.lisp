(in-package #:pastelyzer)

(defun resolve-sgr-code (code)
  (or (typecase code
        (integer
         code)
        (keyword
         (case code
           (:clear 0)
           (:bright 1)
           (:dim 2)
           (:reverse 7)
           (:normal 22)
           (:black 30)
           (:red 31)
           (:green 32)
           (:yellow 33)
           (:blue 34)
           (:magenta 35)
           (:cyan 36)
           (:white 37))))
      (error "Invalid SGR code: ~S" code)))

(defun sgr (stream &rest codes)
  (format stream "~C[~{~A~^;~}m"
          (code-char 27)
          (mapcar #'resolve-sgr-code codes)))

(define-compiler-macro sgr (&whole form stream &rest codes &environment env)
  (cond ((every (lambda (code) (constantp code env))
                codes)
         `(format ,stream ,(apply #'sgr nil codes)))
        (t
         form)))

(defvar *color-output* nil)

(defclass cli-job (job)
  ())

(defmethod resolve-domains-p ((job cli-job))
  nil)

(defmethod analysable-parts ((path pathname) (job cli-job))
  (list (make-instance 'binary-fragment
                       :body (read-file-into-byte-vector path))))

(defmethod analysable-parts ((input (eql :stdin)) (job cli-job))
  #-ccl
  (let ((body (read-stream-content-into-byte-vector *standard-input*)))
    (list (make-instance 'binary-fragment :body body)))
  #+ccl
  (let ((stdin (two-way-stream-input-stream *terminal-io*)))
    (setf (ccl::stream-external-format stdin)
          (ccl:make-external-format :line-termination :unix
                                    :character-encoding nil))
    (let ((body (read-stream-content-into-string stdin)))
      (list (make-instance 'binary-fragment
                           :body (map '(vector (unsigned-byte 8))
                                      #'char-code
                                      body))))))

(defmethod noteworthy-artefact-p ((target base64-blob) (ctx cli-job))
  t)

(defmethod noteworthy-artefact-p ((target hex-blob) (ctx cli-job))
  t)

(defun walk (job &aux (root (job-subject job)))
  (labels ((process-artefact (node)
             (sort (reverse (extract-artefacts node job))
                   #'< :key #'artefact-source-seq-start))
           (color (&rest attrs)
             (when *color-output*
               (apply #'sgr t attrs)))
           (walk-node (node prefix lastp
                       &aux (imp (noteworthy-artefact-p node job)))
             (multiple-value-bind (start end)
                 (artefact-source-seq-bounds node)
               (format t "~{~A~}~:[├~;└~]─ ~A..~A "
                       prefix lastp start (1- end))
               (color (if imp :green :white))
               (format t "~A" (type-of node))
               (color :dim :white)
               (write-string ": ")
               (cond (imp
                      (write-string
                       (artefact-context-before node :limit 16 :bol t))
                      (color :clear :bright :green)
                      (write-string (one-line (artefact-source node)
                                              :limit 48
                                              :continuation "…"
                                              :mode :squeeze))
                      (color :clear :dim :white)
                      (write-string
                       (artefact-context-after node :limit 16 :eol t)))
                     (t
                      (write-string (one-line (artefact-description node)
                                              :limit 48
                                              :continuation "…"
                                              :mode :squeeze))))
               (color :clear)
               (terpri))
             (walk-children (process-artefact node)
                            (append prefix (list (if lastp "   " "│  ")))))
           (walk-children (list prefix)
             (loop for (child . morep) on list
                   do (walk-node child prefix (not morep)))))
    (let ((*print-length* 16))
      (format t "~&~A~%" root)
      (walk-children (process-artefact root) nil))
    job))

(defun process-item (item)
  (handler-case
      (walk (make-instance 'cli-job :subject item))
    (error (condition)
      (format *error-output* "~&~A~%" condition))))

(defun run-cli (&key paths (colour (isatty *standard-output*))
                &allow-other-keys)
  (let ((*color-output* colour))
    (loop for (item . morep) on paths
          collect (if (string= "-" item)
                      (process-item :stdin)
                      (process-item (parse-namestring item)))
          when morep do (terpri))))
