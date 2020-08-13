(defpackage #:sys
  (:use :common-lisp)
  #+sbcl
  (:import-from #:sb-posix
                #:mkdtemp
                #:mkstemp)
  #+ccl
  (:import-from #:ccl
                #:delete-directory)
  (:export #:isatty
           #:mkdtemp
           #:mkstemp
           #:delete-directory
           #:open-tmpfile
           #:run-program
           #:with-temporary-directory
           #:with-temporary-file

           #:*tmpdir*
           #:*home*
           #:initialize-directories))

(in-package #:sys)

(defvar *tmpdir* #p"/tmp/"
  "Where temporary files and directories will be created.")

(defvar *home* #p""
  "Pastelyzer home directory.")

(defun initialize-directories (&key home tmp)
  (setq *home* (probe-file (or home "./")))
  (setq *tmpdir* (or tmp
                     (let ((path (uiop:getenv "TMPDIR")))
                       (when path
                         (pathname path)))
                     (probe-file "/tmp/")
                     *home*)))

#+sbcl
(defgeneric isatty (stream)
  (:method ((other t))
    nil)
  (:method ((fd integer))
    (not (zerop (sb-unix:unix-isatty fd))))
  (:method ((stream synonym-stream))
    (isatty (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream sb-sys:fd-stream))
    (sb-sys:fd-stream-fd stream)))

#+ccl
(defgeneric isatty (stream)
  (:method ((other t))
    nil)
  (:method ((fd integer))
    (ccl::isatty fd))
  (:method ((stream synonym-stream))
    (isatty (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream stream))
    (isatty (ccl::stream-device stream nil)))
  (:method ((stream ccl::two-way-stream))
    (and (isatty (ccl::stream-device stream :input))
         (isatty (ccl::stream-device stream :output)))))

#-(or sbcl ccl)
(defun isatty (stream)
  (declare (ignore stream))
  nil)

#+ccl
(defun mkstemp (template)
  (ccl:with-filename-cstrs ((cstr template))
    (let ((fd (#_mkstemp cstr)))
      (if (< fd 0)
          (error (ccl::%strerror (ccl::%get-errno)))
          (values fd (ccl:get-foreign-namestring cstr))))))

#+ccl
(defun mkdtemp (template)
  (ccl:with-filename-cstrs ((cstr template))
    (let ((ptr (#_mkdtemp cstr)))
      (if (ccl:%null-ptr-p ptr)
          (error (ccl::%strerror (ccl::%get-errno)))
          (ccl:get-foreign-namestring ptr)))))

#+sbcl
(defun delete-directory (path)
  (sb-ext:delete-directory path :recursive t))

(defmacro with-temporary-directory ((var &key (auto-cleanup t)
                                              (in *tmpdir*)
                                              (prefix "pastelyzer-"))
                                    &body body
                                    &environment env)
  (let ((namestring (gensym "NAMESTRING"))
        (name-form (if (constantp prefix env)
                       (concatenate 'base-string prefix "XXXXXX")
                       `(concatenate 'base-string ,prefix "XXXXXX"))))
    `(let* ((,namestring
              (sys:mkdtemp
               (namestring (merge-pathnames (make-pathname :name ,name-form)
                                            ,in))))
            (,var (probe-file ,namestring)))
       ,(if auto-cleanup
            `(unwind-protect
                  (progn ,@body)
               (sys:delete-directory ,var))
            `(multiple-value-prog1
                 (progn ,@body)
               (sys:delete-directory ,var))))))

(defun %make-fd-stream (fd &key (direction :io)
                                (element-type :default)
                                (external-format :utf-8)
                                (auto-close t)
                                path)
  #+sbcl
  (sb-sys:make-fd-stream fd
                         :pathname path
                         :input (or (eq :input direction)
                                    (eq :io direction))
                         :output (or (eq :output direction)
                                     (eq :io direction))
                         :element-type element-type
                         :external-format external-format
                         :auto-close auto-close)
  #+ccl
  (let* ((args (case element-type
                 (:default
                  (list :element-type '(unsigned-byte 8)
                        :character-p t))
                 (t
                  (list :element-type element-type))))
         (result
           (apply #'ccl::make-fd-stream
                  fd
                  :class 'ccl::basic-file-stream
                  :direction direction
                  :encoding external-format
                  :auto-close auto-close
                  :sharing :external
                  args)))
    (when path
      (setf (ccl::stream-filename result) path))
    result)
  #-(or sbcl ccl)
  (error "Don't know how to make an FD-stream in ~A"
         (lisp-implementation-type)))

(defun open-tmpfile (directory prefix
                     &rest args
                     &key (direction :io)
                          (element-type :default)
                          (external-format :utf-8)
                          (auto-close t))
  (declare (ignorable direction element-type external-format auto-close))
  (let* ((name (concatenate 'base-string prefix "XXXXXX"))
         (path (merge-pathnames (make-pathname :name name)
                                directory)))
    (multiple-value-bind (fd real-path)
        (sys:mkstemp (namestring path))
      (apply #'%make-fd-stream fd :path (pathname real-path) args))))

(defun call-with-temporary-file-stream (function &key template element-type)
  (let ((path (namestring template)))
    (assert (let ((mm (mismatch "XXXXXX" path :from-end t)))
              (or (null mm) (zerop mm))))
    (multiple-value-bind (fd real-path)
        (sys:mkstemp path)
      (let ((stream (%make-fd-stream fd :direction :output
                                        :element-type element-type
                                        :path (pathname real-path))))
        (unwind-protect
             (funcall function stream)
          (close stream))))))

(defmacro with-temporary-file ((var &key directory
                                         element-type
                                         (prefix "pastelyzer-"))
                               &body body
                               &environment env)
  (let* ((name-form (if (constantp prefix env)
                        (concatenate 'base-string prefix "XXXXXX")
                        `(concatenate 'base-string ,prefix "XXXXXX")))
         (template-form
           (if directory
               `(merge-pathnames (make-pathname :name ,name-form)
                                 ,directory)
               `(make-pathname :name ,name-form))))
    `(call-with-temporary-file-stream (lambda (,var) ,@body)
                                      :template ,template-form
                                      :element-type ,element-type)))
#+sbcl
(defun run-program (command
                    &key environment stdin stdout stderr directory timeout)
  (declare (ignore timeout))
  (check-type stdin (or null pathname))
  (check-type stdout (or null stream))
  (check-type stderr (or null stream))
  (check-type directory (or null pathname))
  (let* ((env (mapcar (lambda (cons)
                        (concatenate 'string (car cons) "=" (cdr cons)))
                      environment))
         (proc (sb-ext:run-program (first command)
                                   (rest command)
                                   :search t
                                   :directory directory
                                   :environment env
                                   :input stdin
                                   :output stdout
                                   :error stderr
                                   :wait t)))
    (when stdout
      (finish-output stdout))
    (when stderr
      (finish-output stderr))
    (sb-ext:process-exit-code proc)))

#+ccl
(defun run-program (command
                    &key environment stdin stdout stderr directory timeout)
  (declare (ignore timeout))
  (check-type stdin (or null pathname))
  (check-type stdout (or null stream))
  (check-type stderr (or null stream))
  (check-type directory (or null pathname))
  (let ((proc (ccl:run-program (first command)
                               (rest command)
                               :directory directory
                               :env environment
                               :input stdin
                               :output stdout
                               :error stderr
                               :wait t)))
    (when stdout
      (finish-output stdout))
    (when stderr
      (finish-output stderr))
    (ccl:external-process-status proc)))
