(in-package #:pastelyzer.log)

(defvar *categories* (make-instance 'cl-log:category-set))

(macrolet ((defcategory (category &optional expands-as)
             `(cl-log:defcategory ,category ,expands-as *categories*)))
  (defcategory :hit)
  (defcategory :fixme)
  (defcategory :critical)
  (defcategory :error (or :error :critical))
  (defcategory :warning (or :warning :error))
  (defcategory :notice (or :notice :warning))
  (defcategory :info (or :info :notice :hit))
  (defcategory :debug (or :debug :info :http :disc))
  (defcategory :http)
  (defcategory :disc))

(defclass timestamped-message (cl-log:formatted-message)
  ())

(defmethod cl-log:format-message ((message timestamped-message))
  (let ((timestamp (cl-log:message-timestamp message))
        (*print-pretty* nil))
    (multiple-value-bind (ss mm hh d m y)
        (decode-universal-time (cl-log:timestamp-universal-time timestamp))
      (format nil "~D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~6,'0D ~A ~?~&"
              y m d hh mm ss
              (cl-log:timestamp-fraction timestamp)
              (cl-log:message-category message)
              (cl-log:message-description message)
              (cl-log:message-arguments message)))))

(defclass plain-message (cl-log:formatted-message)
  ())

(defmethod cl-log:format-message ((message plain-message))
  (let ((*print-pretty* nil))
    (format nil "~A ~?~&"
            (cl-log:message-category message)
            (cl-log:message-description message)
            (cl-log:message-arguments message))))

(defvar *log-manager*
  (make-instance 'cl-log:log-manager
                 :message-class 'plain-message
                 :categories *categories*))

(defun known-log-category-p (keyword)
  (let ((table (cl-log::category-set-categories *categories*)))
    (nth-value 1 (gethash keyword table))))

(defmacro msg (category description &rest args)
  (if (endp args)
      `(cl-log:log-manager-message *log-manager* ,category "~A" ,description)
      `(cl-log:log-manager-message *log-manager* ,category ,description ,@args)))

(defun maybe-log (category description args)
  "Functional version of MSG."
  (when-let ((messengers
              (cl-log::category-messengers category :manager *log-manager*)))
    (cl-log::send-message *log-manager* messengers category description args)))

(defun setup-logging (&key (filter :info)
                           (message-class 'plain-message))
  (when (and *log-manager*
             (not (eq message-class
                      (cl-log:log-manager-message-class *log-manager*))))
    (setf (cl-log:log-manager-message-class *log-manager*) message-class)
    (cl-log:invalidate-log-manager *log-manager*))

  (cl-log:start-messenger 'cl-log:text-stream-messenger
                       :name 'default
                       :manager *log-manager*
                       :stream (make-broadcast-stream *standard-output*)
                       :filter filter))

(defmacro with-logged-warnings (&body body)
  `(handler-bind
       ((warning #'(lambda (condition)
                     (msg :warning "~A" condition)
                     (when-let ((restart (find-restart 'muffle-warning)))
                       (invoke-restart restart)))))
     ,@body))
