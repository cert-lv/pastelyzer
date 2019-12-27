(in-package #:pastelyzer)

#+sbcl
(progn
  (defun bytes-to-string (bytes external-format
                          &optional (errorp nil)
                                    (error-value nil))
    (handler-bind ((sb-int:character-decoding-error
                     #'(lambda (condition)
                         (declare (ignore condition))
                         (unless errorp
                           (return-from bytes-to-string error-value)))))
      (sb-ext:octets-to-string bytes :external-format external-format))))

#+ccl
(progn
  (defun bytes-to-string (bytes external-format
                          &optional (errorp nil)
                                    (error-value nil))
    (handler-bind ((ccl:decoding-problem
                     #'(lambda (condition)
                         (if errorp
                             (error condition)
                             (return-from bytes-to-string error-value))))
                   (type-error
                     ;; Temporary workaround for CCL's UTF-8 decoder
                     ;; issues (the value NNNNNNN is not of the
                     ;; expected type (MOD 1114112)).
                     #'(lambda (condition)
                         (if errorp
                             (error condition)
                             (return-from bytes-to-string error-value)))))
      (ccl:decode-string-from-octets bytes :external-format external-format))))

#-(or sbcl ccl)
(defun bytes-to-string (bytes external-format
                        &optional (errorp nil)
                                  (error-value nil))
  (unless (eq :utf-8 external-format)
    (error "Unsupported string encoding: ~S" external-format))

  (handler-bind ((trivial-utf-8:utf-8-decoding-error
                   #'(lambda (condition)
                       (declare (ignore condition))
                       (unless errorp
                         (return-from bytes-to-string error-value)))))
    (trivial-utf-8:utf-8-bytes-to-string bytes)))

(setf (documentation 'bytes-to-string 'function)
      "Decode BYTES into a string using encoding EXTERNAL-FORMAT.  If
      there are decoding errors the behaviour depends on the value of
      ERRORP parameter: if it is NIL then the ERROR-VALUE is returned;
      otherwise the decoding error is not handled.

      Returns the decoded string.")

(defmacro with-graceful-stop-flag ((var) &body body)
  `(let ((.should-stop. nil))
     (declare (special .should-stop.))
     (symbol-macrolet ((,var (locally
                                 (declare (special .should-stop.))
                               .should-stop.)))
       ,@body)))

(defun gracefully-stop (thread)
  (flet ((thunk ()
           (locally (declare (special .should-stop.))
             (setf .should-stop. t))))
    (bt:interrupt-thread thread #'thunk)))

(defmacro async ((&rest bindings) &body body)
  `(#+sbcl sb-thread:make-thread
    #-sbcl bt:make-thread
    (lambda ()
      ;; We don't use the :initial-bindings parameter of
      ;; bt:make-thread because it uses EVAL.
      (let ,bindings
        (handler-case (progn ,@body)
          (error (condition)
            (msg :error "Async error: ~A" condition)))))))

#+sbcl
(defun resolve-hostname (string)
  (handler-case
      (mapcar #'(lambda (quad)
                  (declare (type (simple-array (unsigned-byte 8) (4)) quad))
                  (ip:ipv4-address-from-quad (aref quad 0)
                                             (aref quad 1)
                                             (aref quad 2)
                                             (aref quad 3)))
              (sb-bsd-sockets:host-ent-addresses
               (sb-bsd-sockets:get-host-by-name string)))
    ((or sb-bsd-sockets:host-not-found-error sb-bsd-sockets:name-service-error)
      ()
      nil)))

#+ccl
(defun resolve-hostname (string)
  (handler-case
      (mapcar (compose #'ip:ipv4-address #'ccl:socket-address-host)
              (ccl:resolve-address :host string
                                   :address-family :internet
                                   :singlep nil))
    (ccl:socket-creation-error ()
      nil)))

#+sbcl
(defmacro with-tcp-connection ((var &key host port) &body body)
  (with-gensyms (socket addr)
    `(let ((,socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream
                                   :protocol :tcp))
           (,addr (sb-bsd-sockets:host-ent-address
                   (sb-bsd-sockets:get-host-by-name ,host))))
       (unwind-protect
            (progn
              (sb-bsd-sockets:socket-connect ,socket ,addr ,port)
              (let ((,var (sb-bsd-sockets:socket-make-stream ,socket
                                                             :input t
                                                             :output t
                                                             :auto-close t)))
                ,@body))
         (sb-bsd-sockets:socket-close ,socket)))))

#+ccl
(defmacro with-tcp-connection ((var &key host port) &body body)
  `(ccl:with-open-socket (,var :type :stream
                               :remote-host ,host
                               :remote-port ,port)
     ,@body))

#+sbcl
(defun (setf thread-name) (name &optional (thread sb-thread:*current-thread*))
  (setf (sb-thread:thread-name thread) name))

#+ccl
(defun (setf thread-name) (name &optional (thread ccl:*current-process*))
  (setf (ccl:process-name thread) name))

#-(or ccl sbcl)
(defun (setf thread-name) (name &optional thread)
  (let ((first-time (load-time-value (cons t nil))))
    (when (car first-time)
      (setf (car first-time) nil)
      (warn "(SETF THREAD-NAME) not implemented for this implementation"))))

#+sbcl
(defun make-named-thread (fn name &rest args)
  (declare (type (or symbol function) fn))
  (sb-thread:make-thread fn :name name :arguments args))

#-sbcl
(progn
  (defmacro make-named-thread (fn name &rest args)
    `(chanl:pexec (:name ,name)
       (funcall ,fn ,@args)))
  (defun make-mailbox (&key name)
    (declare (ignore name))
    (make-instance 'chanl:unbounded-channel))
  (defun mailbox-empty-p (mailbox)
    (chanl:recv-blocks-p mailbox))
  (defun receive-message (mailbox)
    (chanl:recv mailbox))
  (defun send-message (mailbox message)
    (chanl:send mailbox message)))

#+sbcl
(defun inflate (blob)
  (chipz:decompress nil 'chipz:deflate blob))

#-sbcl
(defun inflate (blob)
  (flexi-streams:with-output-to-sequence (out)
    (flexi-streams:with-input-from-sequence (in blob)
      (funcall (find-symbol "INFLATE-ZLIB-STREAM" '#:ql-gunzipper) in out))))

#+sbcl
(defun gunzip (blob)
  (chipz:decompress nil 'chipz:gzip blob))

#-sbcl
(defun gunzip (blob)
  (flexi-streams:with-output-to-sequence (out)
    (flexi-streams:with-input-from-sequence (in blob)
      (funcall (find-symbol "INFLATE-GZIP-STREAM" '#:ql-gunzipper) in out))))

(defun resolve-stream (stream)
  (if (typep stream 'synonym-stream)
      (resolve-stream (symbol-value (synonym-stream-symbol stream)))
      stream))

#+sbcl
(defun isatty (stream &aux (terminal (resolve-stream stream)))
  (and (sb-sys:fd-stream-p terminal)
       (not (zerop (sb-unix:unix-isatty (sb-sys:fd-stream-fd terminal))))))

#-sbcl
(defun isatty (stream)
  (declare (ignore stream))
  nil)
