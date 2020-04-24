(defpackage #:pastelyzer.config.context
  (:use :common-lisp)
  (:import-from #:pastelyzer
                #:job
                #:job-subject
                #:register-artefact
                #:finish-job)
  (:import-from #:pastelyzer.config.sink
                #:configuration
                #:resolve-configuration
                #:get-prototype
                #:name-of
                #:sink
                #:collect-artefact
                #:add-artefact
                #:finish-sink)
  (:import-from #:pastelyzer.config.filter
                #:apply-filters)
  (:export #:configurable-job))

(in-package #:pastelyzer.config.context)

(defclass configurable-job (job)
  ((sinks
    :initform '())))

;;; Maybe this function's name should include MATERIALIZE or ENSURE?
(defmethod get-sink ((ctx configurable-job) (cfg configuration))
  (let* ((name (name-of cfg))
         (cons (assoc name (slot-value ctx 'sinks))))
    (cond (cons
           (cdr cons))
          (t
           (let ((sink (make-instance 'sink
                                      :configuration cfg
                                      :document (job-subject ctx))))
             (setf (slot-value ctx 'sinks)
                   (acons name sink (slot-value ctx 'sinks)))
             sink)))))

(defmethod register-artefact ((job configurable-job)
                              (artefact t)
                              (source t))
  ;; We might consider creating an instance of DISCARDED-ARTEFACT
  ;; here.  It would give us a chance to gather some statistics
  ;; when generating summary.
  (catch 'discard-artefact
    (apply-filters artefact job)))

(defmethod collect-artefact ((artefact t)
                             (cfg symbol)
                             (ctx configurable-job))
  (collect-artefact artefact (resolve-configuration cfg) ctx))

(defmethod collect-artefact ((artefact t)
                             (cfg configuration)
                             (ctx configurable-job))
  (let ((sink (get-sink ctx cfg)))
    (add-artefact sink artefact)))

(defmethod finish-job ((job configurable-job))
  (loop for (nil . sink) in (slot-value job 'sinks)
        do (finish-sink (get-prototype sink) sink)))
