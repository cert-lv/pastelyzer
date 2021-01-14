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

;;; Extractors.
;;;
;;; TODO: We want a DEFINE-EXTRACTOR macro that would automatilaly put
;;; the symbol into the user package, and also add type and/or context
;;; checking.

(defun usr::source-url (document)
  (warn "SOURCE-URL extractor is deprecated, use ORIGIN instead.")
  (usr::origin document))

(defun usr::origin (document)
  (pastelyzer:paste-source document))

(defun usr::local-url (document)
  (pastelyzer::external-url-to document))

(defun usr::remote-url (document)
  ;; Should cache this result.
  (multiple-value-bind (source url raw-url)
      (pastelyzer::paste-source document)
    (declare (ignore source raw-url))
    (when url
      (puri:render-uri url nil))))

(defun usr::remote-raw-url (document)
  ;; Should use the cached result.
  (multiple-value-bind (source url raw-url)
      (pastelyzer::paste-source document)
    (declare (ignore source url))
    (when raw-url
      (puri:render-uri raw-url nil))))

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
  (pastelyzer:artefact-note artefact))

(defun usr::important (artefact)
  (pastelyzer:important-artefact-p artefact))

(defun usr::source-string (artefact)
  (pastelyzer:artefact-source artefact))

(defun usr::source-context (artefact)
  (pastelyzer:artefact-source-seq artefact))

(defun usr::context-before (artefact)
  (pastelyzer::artefact-context-before artefact))

(defun usr::context-after (artefact)
  (pastelyzer::artefact-context-after artefact))

(defun usr::bytes (artefact)
  (pastelyzer:embedded-binary-bytes artefact))
