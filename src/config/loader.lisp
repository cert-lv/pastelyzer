(defpackage #:pastelyzer.config.loader
  (:use #:common-lisp)
  (:import-from #:pastelyzer.config.sink
                #:make-configuration
                #:register-configuration)
  (:import-from #:pastelyzer.config.filter
                #:add-filter)
  (:import-from #:pastelyzer.log
                #:msg)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter))
  (:export #:load-configuration
           #:apply-directive))

(in-package #:pastelyzer.config.loader)

(defun read-hex-bytes (stream char)
  (assert (char= #\[ char))
  (let* ((*read-base* 16)
         (bytes (read-delimited-list #\] stream t)))
    (assert (every (lambda (value)
                     (typep value '(unsigned-byte 8)))
                   bytes))
    (make-array (length bytes)
                :element-type '(unsigned-byte 8)
                :initial-contents bytes)))

(defun disabled-sharp-dot-reader (stream char n)
  (declare (ignore char n))
  (warn "Ignoring sharp-dot form: ~S" (read stream t nil t)))

(defun create-user-readtable ()
  (let ((readtable (copy-readtable nil)))
    (set-macro-character #\[ #'read-hex-bytes nil readtable)
    (set-macro-character #\] (get-macro-character #\)) nil readtable)
    (set-dispatch-macro-character #\# #\. #'disabled-sharp-dot-reader readtable)
    readtable))

(defun user-readtable ()
  (create-user-readtable))

(defun maybe-continue (condition)
  (let ((continue (find-restart 'skip condition)))
    (when continue
      (invoke-restart continue))))

(defmethod apply-directive ((directive t) (args list))
  (error "Invalid configuration form: (~S~{ ~S~})" directive args))

(defmethod apply-directive ((directive (eql 'usr:define-sink))
                            (args list))
  (destructuring-bind (name (parent) &rest attributes)
      args
    (msg :debug "Defining sink ~S" name)
    (register-configuration
     (make-configuration name parent attributes))))

(defmethod apply-directive ((directive (eql 'usr:define-artefact-filter))
                            (args list))
  (destructuring-bind (name code &rest actions)
      args
    (msg :debug "Defining artefact filter ~S" name)
    (add-filter 'filter:filter name code actions)))

(defmethod load-configuration ((source pathname))
  (with-open-file (stream source :direction :input)
    (load-configuration stream)))

(defun skip-to-char (stream char)
  (loop for next = (read-char stream)
        until (char= char next)
        finally (unread-char next stream)))

(defun read-form (stream)
  "Reads a form from STREAM and returns two values: the form read (or
  EOF-VALUE if end of file was encountered and EOF-ERROR-P is true),
  and position of the form in the STREAM."
  (loop for char = (read-char stream)
        do (case char
             (#\;
              (skip-to-char stream #\newline))
             (#\(
              (unread-char char stream)
              (let ((position (file-position stream)))
                (return (values (read stream)
                                position)))))))

(defun read-configuration (source)
  (let ((*package* (pastelyzer.config.package:user-package))
        (*read-eval* nil)
        (*readtable* (user-readtable)))
    (handler-case
        (loop with eof = '#:eof
              for (form position) = (multiple-value-list (read-form source))
              until (eq form eof)
              do (with-simple-restart
                     (skip "Ignore this directive (~A)." (first form))
                   (handler-bind
                       ((error
                          (lambda (condition)
                            (msg :error "Character ~A: ~A" position condition)
                            ;; XXX: While debugging.
                            (maybe-continue condition))))
                     (apply-directive (first form) (rest form)))))
      (end-of-file ()
        nil))))

(defmethod load-configuration ((source stream))
  ;; This looks like a hack because it is: instead of clobbering the
  ;; existing configuration we load configuration in a "clean"
  ;; environment, and if there are no errors install the new
  ;; configuration.  This is not needed yet, but will be once we allow
  ;; to re-load configuration at run-time.
  (multiple-value-setq (sink::*known-configurations* filter::*filters*)
    (let ((sink::*known-configurations* '())
          (filter::*filters* '()))
      (read-configuration source)
      (values sink::*known-configurations*
              filter::*filters*))))
