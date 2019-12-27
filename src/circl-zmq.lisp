(in-package #:pastelyzer)

(defun parse-circl-zmq-message (string)
  (multiple-value-bind (list position)
      (split-sequence #\space string :count 2)
    (destructuring-bind (code file)
        list
      (values (parse-integer code :junk-allowed nil)
              file
              (let ((len (length string)))
                (if (< position len)
                    (base64:base64-string-to-usb8-array
                     (make-array (- len position)
                                 :element-type (array-element-type string)
                                 :displaced-to string
                                 :displaced-index-offset position))
                    nil))))))

(defclass zmq-message ()
  ((code
    :initarg :code
    :reader zmq-message-code
    :type (or fixnum string))
   (file
    :initarg :file
    :reader zmq-message-file
    :type string)
   (data
    :initarg :data
    :reader zmq-message-data
    :type (or null (simple-array (unsigned-byte 8) (*))))
   (time
    :initarg :time
    :reader zmq-message-time
    :initform (local-time:now)
    :type local-time:timestamp)))

(defmethod print-object ((object zmq-message) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A '~A' (~D)"
            (zmq-message-code object)
            (zmq-message-file object)
            (length (zmq-message-data object)))))

(defun process-circl-zmq-message (string handler)
  (handler-case
      (multiple-value-bind (code file data)
          (parse-circl-zmq-message string)
        ;; XXX: For now ignore the messages with code 101 -- those are
        ;; messages with no paste payload preceding messages with code
        ;; 102 which have the pastes (most of the time).
        (when (= 102 code)
          (msg :debug "<< ~D ~A ~/fmt:nbytes/" code file (length data))
          (funcall handler
                   (make-instance 'zmq-message
                                  :code code
                                  :file file
                                  :data data))))
    (error (condition)
      (warn "~A while processing: ~S" condition string))))

(defun fetch-circl-pastes (address handler)
  (pzmq:with-socket socket :sub
    (handler-case
        (progn
          (pzmq:connect socket address)
          (loop
            (multiple-value-bind (string more)
                (pzmq:recv-string socket :encoding :ascii)
              (process-circl-zmq-message string handler)
              (when more
                (warn "Not handling multi-part message")))))
      (error (condition)
        (warn "Problem receiving messages from ~A: ~A"
              address condition)))))

(defgeneric store-in-db (paste)
  (:documentation "Store PASTE in database."))

(defmethod store-in-db ((paste zmq-message))
  (let ((body (when-let ((data (zmq-message-data paste)))
                (handler-case
                    (gunzip data)
                  (end-of-file ()
                    (warn "Broken paste: ~A (~/fmt:nbytes/)"
                          (zmq-message-file paste)
                          (length (zmq-message-data paste)))
                    (return-from store-in-db nil))
                  (error (condition)
                    (warn "~A: ~A" paste condition)
                    (return-from store-in-db nil))))))
    (db:with-connection ()
      (let ((provider "circl")
            (provider-id (zmq-message-file paste))
            (time (zmq-message-time paste)))
        (multiple-value-bind (paste-id content-id)
            (if body
                (db:store-paste body provider provider-id time)
                (values (db:insert-paste provider provider-id :null time)
                        nil))
          (msg :info "~D -> ~A : ~A (~/fmt:nbytes/ -> ~/fmt:nbytes/)"
               paste-id content-id
               (zmq-message-file paste)
               (length (zmq-message-data paste))
               (length body))
          (make-instance 'circl-paste
                         :id paste-id
                         :provider provider
                         :provider-id provider-id
                         :content (if content-id
                                      (make-instance 'content
                                                     :id content-id
                                                     :body body)
                                      nil)))))))
