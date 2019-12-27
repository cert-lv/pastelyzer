(defpackage #:pastelyzer.rest
  (:use #:common-lisp)
  (:export #:rest-call
           #:json-call
           #:jsown-call
           #:with-keepalive))

(in-package #:pastelyzer.rest)

(defvar *user-agent* "Pastelyzer")

(defvar *reuse-connection* nil)

(defmacro with-keepalive (&body body)
  "Requests in the scope reuse connection (if possible)."
  `(let ((*reuse-connection* t))
     ,@body))

(defun rest-call (uri method path &rest keys
                                  &key user-agent
                                  &allow-other-keys)
  (let ((uri (puri:merge-uris path uri)))
    (multiple-value-bind (body status headers real-uri stream closep reason)
        (apply #'drakma:http-request uri
               :method method
               :close (if *reuse-connection* nil t)
               :keep-alive (if *reuse-connection* t nil)
               :stream (if (streamp *reuse-connection*) *reuse-connection* nil)
               :user-agent (or user-agent *user-agent*)
               keys)
      (declare (ignore headers real-uri))
      (unwind-protect
           (cond ((= 200 status)
                  body)
                 (t
                  (error "~A: ~S (~A)" uri status reason)))
        (cond (closep
               (close stream))
              (*reuse-connection*
               (setq *reuse-connection* stream)))))))

(defun json-call (uri method path &rest keys)
  (let ((drakma:*text-content-types*
          (list* '("application" . "json") drakma:*text-content-types*)))
    (declare (dynamic-extent drakma:*text-content-types*))
    (apply #'rest-call uri method path
           :content-type "application/json"
           :accept "application/json"
           keys)))

(defun jsown-call (uri method path &rest keys
                                   &key content
                                   &allow-other-keys)
  (let* ((body (apply #'json-call uri method path
                      :content (when content (jsown:to-json content))
                      keys))
         (result (jsown:parse body)))
    (if (and (jsown:keyp result "errors")
             (jsown:val result "errors"))
        (error "~A to ~A~@[ with ~S~] failed: ~S"
               method path content (jsown:val result "errors"))
        result)))
