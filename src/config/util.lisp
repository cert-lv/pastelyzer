(defpackage #:pastelyzer.config.util
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:when-let)
  (:local-nicknames (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter)
                    (#:usr #:pastelyzer.config.user))
  (:export #:parse-item-function
           #:parse-dynamic-attribute
           #:make-attribute-value-composer
           #:parse-user-template))

(in-package #:pastelyzer.config.util)

(defun parse-item-function (code &optional (name '#:anonymous))
  ;; XXX: Filters were not exactly designed for this...
  (pastelyzer.config.filter::parse-filter name code))

(defun make-formatter (control-string &rest args)
  (let ((values (mapcar #'parse-item-function args)))
    (lambda (context)
      (apply #'format nil control-string
             (mapcar (lambda (fn) (funcall fn context)) values)))))

(defun parse-dynamic-attribute (attr &optional name)
  (typecase attr
    (cons
     (case (car attr)
           (usr:fmt
            (apply #'make-formatter (rest attr)))
           (t
            (parse-item-function attr))))
    ((or string keyword)
     attr)
    (null
     nil)
    (t
     (error "Invalid value~@[ for attribute ~S~]: ~S" name attr))))

(defun make-attribute-value-composer (values)
  (lambda (context)
    (with-output-to-string (out)
      (dolist (value values)
        (cond ((eql :nl value)
               (terpri out))
              ((eql :fl value)
               (fresh-line out))
              ((functionp value)
               (when-let (datum (funcall value context))
                 (princ datum out)))
              (t
               (princ value out)))))))

(defun parse-user-template (args)
  (make-attribute-value-composer (mapcar #'parse-dynamic-attribute args)))
