(in-package #:pastelyzer)

(defconstant ESC (code-char 27))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
   (format stream "~C[~{~A~^;~}m" ESC (mapcar #'resolve-sgr-code codes))))

(define-compiler-macro sgr (&whole form stream &rest codes &environment env)
  (cond ((every (lambda (code) (constantp code env))
                codes)
         `(format ,stream ,(apply #'sgr nil codes)))
        (t
         form)))

(defvar *color-output* nil)

(defclass cli-job (job)
  ())

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

(defmethod noteworthy-artefact-p ((target t) (ctx cli-job))
  nil)

(defmethod noteworthy-artefact-p ((target base64-blob) (ctx cli-job))
  t)

(defmethod noteworthy-artefact-p ((target hex-blob) (ctx cli-job))
  t)

(defmethod render-node ((view (eql :mono-term)) (node artefact) (job cli-job)
                        &optional (stream *standard-output*))
  (let ((imp (noteworthy-artefact-p node job)))
    (multiple-value-bind (start end)
        (artefact-source-seq-bounds node)
      (format stream "~A..~A ~A: ~A"
              start end (type-of node)
              (one-line (if imp
                            (artefact-source node)
                            (artefact-description node))
                        :limit 72
                        :continuation "…"
                        :mode :squeeze)))))

(defmethod render-node ((view (eql :color-term)) (node artefact) (job cli-job)
                        &optional (stream *standard-output*))
  (let ((imp (noteworthy-artefact-p node job)))
    (multiple-value-bind (start end)
        (artefact-source-seq-bounds node)
      (format stream "~A..~A " start end)
      (sgr stream (if imp :green :white))
      (format stream "~A" (type-of node))
      (sgr stream :dim :white)
      (write-string ": ")
      (cond (imp
             (write-string (artefact-context-before node :limit 16 :bol t)
                           stream)
             (sgr stream :clear :bright :green)
             (write-string (one-line (artefact-source node)
                                     :limit 48
                                     :continuation "…"
                                     :mode :squeeze)
                           stream)
             (sgr stream :clear :dim :white)
             (write-string (artefact-context-after node :limit 16 :eol t)
                           stream))
            (t
             (write-string (one-line (artefact-description node)
                                     :limit 48
                                     :continuation "…"
                                     :mode :squeeze)
                           stream)))
      (sgr stream :clear))))

(defun render-tree (roots &key (stream *standard-output*)
                               (children-fn (constantly '()))
                               (print-fn #'princ))
  (labels ((draw-node (node prefix lastp)
             (format stream "~&~{~A~}~:[├~;└~]─ " prefix lastp)
             (funcall print-fn node stream)
             (fresh-line stream)
             (draw-children (funcall children-fn node)
                            (append prefix (list (if lastp "   " "│  ")))))
           (draw-children (list prefix)
             (loop for (child . morep) on list
                   do (draw-node child prefix (not morep)))))
    (let ((*print-length* 16)
          (*print-pretty* nil))
      (dolist (root roots)
        (format stream "~A~%" root)
        (draw-children (funcall children-fn root) nil)))))

(defun walk (job view)
  (render-tree (list (job-subject job))
               :children-fn (lambda (node)
                              (sort (extract-artefacts node job)
                                    #'< :key #'artefact-source-seq-start))
               :print-fn (lambda (node stream)
                           (render-node view node job stream))))

(defun process-item (item view)
  (handler-case
      (walk (make-instance 'cli-job :subject item) view)
    (error (condition)
      (format *error-output* "~&~A~%" condition))))

(defun run-cli (&key paths (colour (isatty *standard-output*))
                &allow-other-keys)
  (let ((*color-output* colour))
    (loop for (item . morep) on paths
          collect (process-item (if (string= "-" item)
                                    :stdin
                                    (parse-namestring item))
                                (if colour :color-term :mono-term))
          when morep do (terpri))))

(defun update-status (stream)
  (write-string #.(format nil "~C[K~C" ESC #\return) stream)
  (finish-output stream))

(defun process-unprocessed (&key (batch-size 1000000))
  "Re-process contents that have not been processed by current version."
  (check-type batch-size (integer 0 *))
  (flet ((process-content (id size)
           (format *standard-output* "PROC ~D (~/fmt:nbytes/)" id size)
           (update-status *standard-output*)
           #+sbcl
           (when (< *huge-fragment-bytes* size)
             ;; Reduce the chance of running out of memory when
             ;; processing big documents.
             (sb-ext:gc :full t))
           (handler-case
               (let ((content (fetch-content id)))
                 (analyze content))
             (error (condition)
               (msg :error "Failed to process content ~D: ~A"
                    id condition)))))
    (db:with-connection ()
      (loop
        (format *standard-output* "WAIT Fetching next batch")
        (update-status *standard-output*)
        (when (zerop (db:map-unprocessed-content-ids #'process-content
                                                     :limit batch-size))
          (return))))))
