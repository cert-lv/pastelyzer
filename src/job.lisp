(in-package #:pastelyzer)

(defgeneric job-subject (job))

(defgeneric register-artefact (job artefact source)
  (:documentation
   "Called whenever an ARTEFACT is exctracted (from SOURCE)."))

(defgeneric finish-job (job))

(defgeneric resolve-domains-p (job)
  (:documentation
   "Whether to resolve discovered domains."))

(defclass job ()
  ((subject
    :initarg :subject
    :reader job-subject
    :documentation
    "The object that triggered this job."))
  (:documentation
   "For each item to be processed an instance of this class is
  created.

  Controls what extractors are being used.  For instance when testing
  none of the extractors that connect to external systems are
  active."))

(defmethod print-object ((job job) (stream t))
  (print-unreadable-object (job stream :type t :identity t)
    (princ (job-subject job) stream)))

(defmethod register-artefact ((job job) (artefact t) (source t))
  artefact)

(defmethod finish-job ((job job))
  ;; Do nothing by default.
  nil)

(defmethod resolve-domains-p ((job job))
  *resolve-domains*)
