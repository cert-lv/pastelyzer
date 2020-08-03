(defpackage #:pastelyzer.config.smtp
  (:use #:common-lisp)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:pastelyzer.config.sink
                #:prototype
                #:get-prototype
                #:check-args
                #:parse-sink-attribute
                #:parse-sink-attribute-value
                #:sink
                #:sink-document
                #:sink-configuration
                #:group-artefacts
                #:attribute-value
                #:attribute-value-in-context
                #:finish-sink
                #:invalid-attribute-type)
  (:local-nicknames (#:util #:pastelyzer.config.util))
  (:import-from #:pastelyzer.config.package
                #:user-identifier))

(in-package #:pastelyzer.config.smtp)

(defclass smtp (prototype)
  ())

(defvar *sink-prototype* nil)

(defmethod get-prototype ((name (eql (user-identifier "SMTP-SINK"))))
  (or *sink-prototype*
      (setq *sink-prototype* (make-instance 'smtp))))

(defmethod parse-sink-attribute ((impl smtp) (attribute symbol)
                                 &rest args)
  (cond ((member attribute '(:server :from))
         (list* attribute (check-args impl attribute args '((:type string)))))
        (t
         (call-next-method))))

(defmethod parse-sink-attribute
    ((impl smtp) (attribute (eql :subject)) &rest args)
  (list attribute (util:parse-user-template args)))

(defmethod parse-sink-attribute ((impl smtp) (attribute (eql :body))
                                 &rest args)
  (list attribute (util:parse-user-template args)))

(defmethod parse-sink-attribute ((impl smtp) (attribute (eql :recipients))
                                 &rest args)
  (list attribute (check-args impl attribute args '(&rest (:type string)))))

(defmethod finish-sink ((proto smtp) (sink sink))
  ;; TODO: The presence of these fields should be checked after
  ;; loading the configuration.
  (flet ((ensure-value (name)
           (let ((value (attribute-value sink name)))
             (cond ((plusp (length value))
                    value)
                   (t
                    (msg :error "Missing value for field ~S in ~S for ~S"
                         name
                         (sink-configuration sink)
                         (sink-document sink))
                    (return-from finish-sink nil))))))
    (cl-smtp:send-email
     (ensure-value :server)
     (ensure-value :from)
     (ensure-value :recipients)
     (ensure-value :subject)
     (ensure-value :body))))
