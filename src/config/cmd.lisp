;;; This file is only loaded for SBCL (see system definition) because CCL does
;;; not (yet) support :directory parameter to RUN-PROGRAM.

(defpackage #:pastelyzer.config.cmd
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:read-stream-content-into-string
                #:read-stream-content-into-byte-vector)
  (:import-from #:pastelyzer.log
                #:msg)
  (:local-nicknames (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter)
                    (#:usr #:pastelyzer.config.user)
                    (#:util #:pastelyzer.config.util)
                    (#:loader #:pastelyzer.config.loader))
  (:import-from #:pastelyzer.config.package
                #:user-package
                #:user-identifier
                #:user-identifier-p))

(in-package #:pastelyzer.config.cmd)

(defclass cmd (sink:prototype)
  ((default-environment
    :reader cmd-sink-default-environment
    :type list
    :documentation "External processes are executed in a clean environment
    with only the environment variables listed here present (populated in
    INITIALIZE-INSTANCE :after method).")))

(defmethod initialize-instance :after ((object cmd) &key)
  ;; The default enviroment should be re-generated when configuration
  ;; changes (when we support that).
  (let ((home (namestring (merge-pathnames "")))
        (server (if pastelyzer::*acceptor*
                    (princ-to-string pastelyzer::*web-server-external-uri*)
                    nil)))
    (setf (slot-value object 'default-environment)
         `(("PASTELYZER_HOME" . ,home)
           ,@(when server `(("PASTELYZER_SERVER" . ,server)))))))

(defmethod sink:get-prototype ((name (eql (user-identifier "CMD-SINK"))))
  (make-instance 'cmd))

(defclass finished-process ()
  ((artefact
    :initarg :artefact
    :reader finished-process-artefact)
   (status
    :initarg :status
    :reader finished-process-status)
   (stdout
    :initarg :stdout
    :reader finished-process-stdout)
   (stderr
    :initarg :stderr
    :reader finished-process-stderr)))

(import 'finished-process (user-package))
(export 'finished-process (user-package))

(defun usr::stdout (artefact)
  (finished-process-stdout artefact))

(defun usr::stderr (artefact)
  (finished-process-stderr artefact))

(defun usr::status (artefact)
  (finished-process-status artefact))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :target)) &rest args)
  (list* attribute
         (sink:check-args proto attribute args
                          '((:type (member :artefact :document))))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :command)) &rest args)
  ;; Value of this attribute is a list of strings and expressions that
  ;; evaluate to strings.  The first string is the command to execute,
  ;; and all the rest are command-line parameters for this command.
  (unless args
    (error 'sink:missing-attribute-value
           :sink proto
           :attribute attribute))
  (list attribute
        (if (every #'stringp args)
            args
            (mapcar #'util:parse-dynamic-attribute args))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :stdin)) &rest args)
  ;; What to pass into the standard input of the process.  This should
  ;; be an expression that evaluates to a string or byte vector (i.e.,
  ;; artefact string representation, or, in case of embedded binaries
  ;; an expression that extracts the bytes).  If the attribute is not
  ;; specified then nothing is written to the standard input of the
  ;; process.
  ;;
  ;; Should we provide a stream that emits EOF immediately, or
  ;; /dev/null which just emits zeroes?
  (unless (and (car args) (endp (cdr args)))
    (error 'sink:too-many-attribute-values
           :attribute attribute
           :sink proto))
  (let ((input (first args)))
    (list attribute (util:parse-dynamic-attribute input attribute))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :stdout)) &rest args)
  (list* attribute
         (sink:check-args
          proto attribute args
          '((:type (member :collect-string :collect-bytes :discard))))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :stderr)) &rest args)
  (list* attribute
         (sink:check-args
          proto attribute args
          '((:type (member :collect-string :collect-bytes :discard))))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :environment)) &rest args)
  (list attribute
        (loop for (name value) in args
              collect (cons name
                            (util:parse-dynamic-attribute value attribute)))))

(defmethod sink:parse-sink-attribute
    ((proto cmd) (attribute (eql :action)) &rest args)
  ;; Currently the only supported action is to pass the finished
  ;; process instance to the specified filter.
  (unless (every (lambda (arg)
                   (typep (filter:get-filter arg) 'process-filter))
                 args)
    (error 'sink:invalid-attribute-type
           :attribute attribute
           :sink proto
           :typespec 'process-filter
           :value args))
  (list attribute args))

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :target)))
  :artefact)

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :environment)))
  '())

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :stdin)))
  :null)

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :stdout)))
  :discard)

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :stderr)))
  :discard)

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :action)))
  nil)

(defmethod sink:attribute-value ((cfg cmd) (attribute (eql :time-limit)))
  nil)

;;; XXX: We might want the binding of this be an instance of
;;; EXTERNAL-PROCESS, which would have slots for the (temporary)
;;; directory where it is running, temporary files that have been
;;; created (in this directory), etc.
(defvar *cmd-dir* nil
  "Directory (temporary) where external command is being executed.")

(defmethod dump-to-tmpfile ((seq vector) (directory pathname))
  (let ((element-type (etypecase seq
                        ((vector (unsigned-byte 8)) '(unsigned-byte 8))
                        (base-string                'base-char)
                        (string                     'character))))
    (sys:with-temporary-file (out :directory directory
                                  :element-type element-type)
      (write-sequence seq out)
      (pathname out))))

(defmethod dump-to-tmpfile ((fragment pastelyzer:binary-fragment)
                            (directory pathname))
  (sys:with-temporary-file (out :directory directory
                                :element-type '(unsigned-byte 8))
    (write-sequence (pastelyzer:fragment-body fragment) out)
    (pathname out)))

(defmethod dump-to-tmpfile ((fragment pastelyzer:string-fragment)
                            (directory pathname))
  (sys:with-temporary-file (out :directory directory
                                :element-type 'character)
    (write-sequence (pastelyzer:fragment-body fragment) out)
    (pathname out)))

(defmethod dump-to-tmpfile ((datum (eql :null)) (directory pathname))
  nil)

(defmethod filter:generate-filter-function
    ((operator (eql (user-identifier "STORE-TMPFILE"))) &rest body)
  (check-type body null)
  (filter:make-function store-tmpfile (value cont)
    (unless *cmd-dir*
      (error "STORE-TMPFILE called in invalid context."))
    (funcall cont (namestring (dump-to-tmpfile value *cmd-dir*)))))

(defmethod pastelyzer:artefact-note ((process finished-process))
  (pastelyzer:artefact-note (finished-process-artefact process)))

(defmethod (setf pastelyzer:artefact-note) (note (process finished-process))
  (setf (pastelyzer:artefact-note (finished-process-artefact process))
        note))

(defmethod pastelyzer:important-artefact-p ((process finished-process))
  (pastelyzer:important-artefact-p (finished-process-artefact process)))

(defmethod (setf pastelyzer:important-artefact-p)
    (flag (process finished-process))
  (setf (pastelyzer:important-artefact-p (finished-process-artefact process))
        flag))

(defclass process-filter (filter:filter)
  ())

(defmethod loader:apply-directive
    ((directive (eql (user-identifier "DEFINE-PROCESS-FILTER"))) (args list))
  (destructuring-bind (name code &rest actions)
      args
    (filter:add-filter 'process-filter name code actions)))

(defmethod sink:finish-sink ((proto cmd) (sink sink:sink))
  (let ((stdout (sink:attribute-value sink :stdout))
        (stderr (sink:attribute-value sink :stderr))
        (action (sink:attribute-value sink :action))
        (time (sink:attribute-value sink :time-limit)))
    (labels
        ((open-stream (method dir prefix)
           (ecase method
             (:collect-string
              (sys:open-tmpfile dir prefix :element-type 'character))
             (:collect-bytes
              (sys:open-tmpfile dir prefix :element-type '(unsigned-byte 8)))
             (:discard
              nil)))
         (slurp-stream (method stream)
           (ecase method
             (:collect-string
              (file-position stream 0)
              (prog1 (pastelyzer.util:trim-space
                      (read-stream-content-into-string stream)
                      :both)
                (close stream)))
             (:collect-bytes
              (file-position stream 0)
              (prog1 (read-stream-content-into-byte-vector stream)
                (close stream)))
             (:discard
              nil)))
         (process (item actions stdout stderr)
           (let ((env (append
                       (sink:attribute-value sink :environment)
                       (cmd-sink-default-environment proto)))
                 (stdin (sink:attribute-value-in-context sink :stdin item)))
             (sys:with-temporary-directory (tmpdir)
               (let* ((*cmd-dir* tmpdir)
                      (cmd (sink:attribute-value-in-context sink :command item))
                      (out (open-stream stdout tmpdir "stdout-"))
                      (err (open-stream stderr tmpdir "stderr-")))
                 (multiple-value-bind (status)
                     (sys:run-program cmd
                                      :stdin (dump-to-tmpfile stdin tmpdir)
                                      :stdout out
                                      :stderr err
                                      :environment env
                                      :directory tmpdir
                                      :timeout time)
                   (let ((proc-out (slurp-stream stdout out))
                         (proc-err (slurp-stream stderr err)))
                     (dolist (action actions 42)
                       (filter:apply-filter (filter:get-filter action)
                                            (make-instance 'finished-process
                                                           :artefact item
                                                           :stdout proc-out
                                                           :stderr proc-err
                                                           :status status)
                                            sink)))))))))
      (ecase (sink:attribute-value sink :target)
        (:document
         (process (sink:sink-document sink) action stdout stderr))
        (:artefact
         (mapc (lambda (item) (process item action stdout stderr))
               (sink:sink-artefacts sink)))))))
