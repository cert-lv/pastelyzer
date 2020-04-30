(defpackage #:pastelyzer.config.util
  (:use #:common-lisp)
  (:local-nicknames (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter)
                    (#:usr #:pastelyzer.config.user))
  (:export #:parse-item-function
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

(defun parse-user-template (args)
  (sink:make-attribute-value-composer
   (mapcar (lambda (arg)
             (etypecase arg
               ((or string (member :fl :nl))
                arg)
               ((cons (eql usr:fmt))
                (apply #'make-formatter (rest arg)))
               ((cons (eql usr:extract))
                (parse-item-function arg (second arg)))))
           args)))

;;; Extractors.
;;;
;;; TODO: We want a DEFINE-EXTRACTOR macro that would automatilaly put
;;; the symbol into the user package, and also add type and/or context
;;; checking.

(defun usr::source-url (document)
  (pastelyzer:paste-source document))

(defun usr::local-url (sink)
  (let ((document (sink:sink-document sink)))
    (pastelyzer::external-url-to document)))

(defun usr::remote-url (sink)
  (let ((document (sink:sink-document sink)))
    ;; Should cache this result.
    (multiple-value-bind (source url raw-url)
        (pastelyzer::paste-source document)
      (declare (ignore source raw-url))
      (when url
        (puri:render-uri url nil)))))

(defun usr::remote-raw-url (sink)
  (let ((document (sink:sink-document sink)))
    ;; Should use the cached result.
    (multiple-value-bind (source url raw-url)
        (pastelyzer::paste-source document)
      (declare (ignore source url))
      (when raw-url
        (puri:render-uri raw-url nil)))))

(defun usr::artefact-descriptions (sink)
  (with-output-to-string (out)
    (let ((groups (sink:group-artefacts sink)))
      (loop for group in groups
            for (class unique important duplicate-count) = group
            do (terpri out)
               (pastelyzer::summarize-artefact-group out group)
               (terpri out)
               (terpri out)
               (loop for bag being each hash-value in unique
                     for artefact = (first bag)
                     for string = (pastelyzer:artefact-description artefact)
                     do (write-string string out)
                        (terpri out))))))

(defun usr::artefact-summary-by-class (sink)
  (let ((groups (sink:group-artefacts sink)))
    ;; TODO: Waiting for the artefact summarization refactoring.
    (pastelyzer::summarize-artefacts groups :text)))

(defun usr::digits (artefact)
  (pastelyzer:bank-card-number-digits artefact))

(defun usr::note (artefact)
  (slot-value artefact 'pastelyzer::note))
