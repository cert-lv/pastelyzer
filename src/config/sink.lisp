(defpackage #:pastelyzer.config.sink
  (:use :common-lisp)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:pastelyzer.config.package
                #:user-identifier-p)
  (:local-nicknames (#:usr #:pastelyzer.config.user))
  (:export #:prototype
           #:get-prototype
           #:check-args
           #:parse-sink-attribute
           #:parse-sink-attribute-value
           #:parse-action
           #:collect-artefact
           #:configuration-class

           #:configuration
           #:make-configuration
           #:register-configuration
           #:resolve-configuration

           #:sink
           #:add-artefact
           #:sink-configuration
           #:sink-document
           #:sink-artefacts
           #:group-artefacts
           #:attribute-value
           #:attribute-value-in-context
           #:make-attribute-value-composer
           #:finish-sink
           #:run-document-actions
           #:run-item-actions

           #:configuration-error
           #:invalid-attribute-type))

(in-package #:pastelyzer.config.sink)

(defgeneric parse-sink-attribute (prototype attribute &rest args))

(defgeneric parse-action (prototype scope action &rest body)
  (:documentation "Helper function to parse document and item actions."))

(defgeneric attribute-value (sink attribute))

(define-condition configuration-problem ()
  ())

(define-condition configuration-error (configuration-problem error)
  ())

(define-condition file-does-not-exist (configuration-problem)
  ((path
    :initarg :path
    :reader path-of))
  (:report (lambda (condition stream)
             (format stream "File does not exist: ~A" (path-of condition)))))

(define-condition name-problem (configuration-problem)
  ((name
    :initarg :name
    :reader name-of)))

(define-condition unknown-sink-problem (name-problem)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown sink: ~S" (name-of condition)))))

(define-condition unknown-sink-error (unknown-sink-problem configuration-error)
  ())

(define-condition unknown-sink-warning (unknown-sink-problem warning)
  ())

(define-condition sink-problem (configuration-problem)
  ((sink
    :initarg :sink
    :reader sink-of)))

(define-condition attribute-problem (sink-problem)
  ((attribute
    :initarg :attribute
    :reader attribute-of)))

(define-condition attribute-error (attribute-problem configuration-error)
  ())

(define-condition invalid-attribute-used (attribute-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Attempt to use misconfigured attribute ~S of ~A"
                     (attribute-of condition)
                     (name-of (sink-of condition))))))

(define-condition too-many-attribute-values (attribute-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Too many values provided for attribute ~S of ~A"
                     (attribute-of condition)
                     (name-of (sink-of condition))))))

(define-condition invalid-attribute-type (attribute-error)
  ((typespec
    :initarg :typespec
    :reader typespec-of)
   (value
    :initarg :value
    :reader value-of))
  (:report (lambda (condition stream)
             (format stream "~S is not of type ~S (~S attribute of ~A)"
                     (value-of condition)
                     (typespec-of condition)
                     (attribute-of condition)
                     (name-of (sink-of condition))))))

(define-condition missing-attribute-value (attribute-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Missing value for attribute ~S of ~A"
                     (attribute-of condition)
                     (name-of (sink-of condition))))))

(define-condition unknown-attribute (attribute-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown ~S attribute: ~S"
                     (name-of (sink-of condition))
                     (attribute-of condition)))))

(defun resolve-user-value (form)
  (etypecase form
    ((member usr:yes usr:true) t)
    ((member usr:no usr:false) nil)
    ((cons (eql usr:env) (cons string))
     (destructuring-bind (name)
         (rest form)
       (uiop:getenv name)))
    ((cons (eql usr:file-contents) (cons string))
     (destructuring-bind (path)
         (rest form)
       (if (probe-file  path)
           (alexandria:read-file-into-string path)
           (signal 'file-does-not-exist :path path))))
    ((cons (eql usr:or))
     (some #'resolve-user-value (rest form)))
    (string form)))

(defun check-args (sink attribute args specs)
  (labels ((check-arg (value &key type transform)
             (setq value (resolve-user-value value))
             (when (and type (not (typep value type)))
               (error 'invalid-attribute-type
                      :sink sink
                      :attribute attribute
                      :typespec type
                      :value value))
             (if transform
                 (funcall transform value)
                 value)))
    (let ((result '()))
      (loop
        (when (endp specs)
          (if args
              (error 'too-many-attribute-values
                     :sink sink
                     :attribute attribute)
              (return (nreverse result))))
        (let ((spec (pop specs)))
          (when (eq '&rest spec)
            (setq spec (pop specs))
            (assert (not (null spec)))
            (assert (endp specs))
            (return
              (nconc (nreverse result)
                     (mapcar (lambda (value)
                               (apply #'check-arg value spec))
                             args))))
          (unless args
            (error 'missing-sink-attribute-value
                   :sink sink
                   :attribute attribute))
          (push (apply #'check-arg (pop args) spec) result))))))

(defclass sink ()
  ((configuration
    :initarg :configuration
    :reader sink-configuration)
   (document
    :initarg :document
    :reader sink-document)
   (artefacts
    :initarg :artefacts
    :reader sink-artefacts
    :initform '())
   (groups))
  (:documentation
   "Subclasses of this class are instantiated for each processed
  document (on-demand) to collect relevant artefacts."))

(defmethod attribute-value :around ((ctx t) (attribute t))
  ;; Fix the origin of unknown-attribute error.
  (handler-case (call-next-method)
    (unknown-attribute ()
      (error 'unknown-attribute
             :sink ctx
             :attribute attribute))))

(defmethod attribute-value ((sink sink) (attribute t))
  (attribute-value (sink-configuration sink) attribute))

(defmethod add-artefact ((sink sink) (artefact t))
  (push artefact (slot-value sink 'artefacts)))

(defun group-artefacts (sink)
  (if (slot-boundp sink 'groups)
      (slot-value sink 'groups)
      (setf (slot-value sink 'groups)
            (pastelyzer::group-artefacts (sink-artefacts sink)))))

(defclass prototype ()
  ())

(defmethod name-of ((object prototype))
  (class-name (class-of object)))

(defclass configuration ()
  ((name
    :initarg :name
    :reader name-of
    :type symbol)
   (parent
    :initarg :parent
    :reader parent-of
    :type (or configuration prototype))
   (attributes
    :initarg :attributes
    :reader attributes-of
    :type list
    :initform '())
   (document-actions
    :initarg :document-actions
    :reader document-actions
    :type list
    :initform '())
   (item-actions
    :initarg :item-actions
    :reader item-actions
    :type list
    :initform '())))

(defmethod print-object ((object configuration) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (slot-value object 'name))))

(defmethod initialize-instance :around
    ((instance configuration) &rest initargs &key attributes &allow-other-keys)
  ;; We might get rid of this method if we make PARSE-SINK-ATTRIBUTE
  ;; into PROCESS-CONFIGURATION-OPTION that would act directly on a
  ;; CONFIGURATION instance.
  (let* ((document-actions '())
         (item-actions '())
         (attrs (remove-if (lambda (attr)
                             (case (first attr)
                               (:document-action
                                (push (second attr) document-actions)
                                t)
                               (:item-action
                                (push (second attr) item-actions)
                                t)))
                           attributes)))
    (apply #'call-next-method instance
           :document-actions (nreverse document-actions)
           :item-actions (nreverse item-actions)
           :attributes attrs
           initargs)))

(defmethod configuration-class ((proto prototype))
  'configuration)

(defmethod attribute-value ((cfg configuration) (attribute t))
  (let ((spec (find attribute (attributes-of cfg) :key #'car)))
    (if spec
        (second spec)
        (attribute-value (parent-of cfg) attribute))))

;;; XXX: Consider using symbol plist for this.
(defvar *known-configurations* nil
  "An association list of known sink configurations.")

(defun register-configuration (sink &aux (name (name-of sink)))
  (let ((cons (assoc name *known-configurations*)))
    (cond (cons
           (warn "Replacing existing sink configuraton ~A" name)
           (setf (cdr cons) sink))
          (t
           (setq *known-configurations*
                 (acons name sink *known-configurations*))))))

(defun resolve-configuration (name)
  (let ((cons (assoc name *known-configurations*)))
    (if cons
        (cdr cons)
        (get-prototype name))))

(defmethod parse-sink-attribute :around
    ((proto prototype) (attribute t) &rest args)
  (declare (ignore args))
  (with-simple-restart
      (continue "Leave the attribute unconfigured (~S)." attribute)
    (return-from parse-sink-attribute (call-next-method)))
  (cons attribute (lambda (context)
                    (error 'invalid-attribute-used
                           :sink context
                           :attribute attribute))))

(defun make-configuration (name parent attributes)
  (let* ((proto (get-prototype parent))
         (attributes
           (mapcar (lambda (spec)
                     (apply #'parse-sink-attribute proto spec))
                   attributes)))
    (make-instance (configuration-class proto)
                   :name name
                   :parent (resolve-configuration parent)
                   :attributes attributes)))

(defun attribute-value-in-context (cfg attribute context)
  (flet ((resolve (value)
           (if (functionp value)
               (funcall value context)
               value)))
    (let ((value (attribute-value cfg attribute)))
      (if (consp value)
          (mapcar #'resolve value)
          (resolve value)))))

(defmethod attribute-value ((cfg prototype) (attribute t))
  (error 'unknown-attribute
         :sink cfg
         :attribute attribute))

(defmethod attribute-value ((cfg null) (attribute t))
  (error "Programming is hard"))

;;; XXX FIXME: Prevent recursive definitions.
(defmethod get-prototype ((instance configuration))
  (get-prototype (parent-of instance)))

(defmethod get-prototype ((instance sink))
  (get-prototype (sink-configuration instance)))

(defmethod get-prototype ((instance prototype))
  instance)

(defmethod get-prototype ((name symbol))
  (let ((cons (assoc name *known-configurations*)))
    (if cons
        (get-prototype (cdr cons))
        (error 'unknown-sink-error :name name))))

(defmethod parse-sink-attribute ((impl prototype) (attribute symbol)
                                 &rest args)
  (declare (ignore args))
  (error 'unknown-attribute
         :sink impl
         :attribute attribute))

(defmethod parse-sink-attribute ((impl prototype) (attribute (eql :deduplicate))
                                 &rest args)
  (list* attribute (check-args impl attribute args '((:type boolean)))))

(defmethod parse-sink-attribute
    ((impl prototype) (attribute (eql :document-action)) &rest args)
  (check-type args (cons cons null))
  (destructuring-bind ((action &rest body))
      args
    (assert (user-identifier-p action))
    (list attribute (apply #'parse-action impl :document action body))))

(defmethod parse-sink-attribute
    ((impl prototype) (attribute (eql :item-action)) &rest args)
  (check-type args (cons cons null))
  (destructuring-bind ((action &rest body))
      args
    (assert (user-identifier-p action))
    (list attribute (apply #'parse-action impl :item action body))))

(defgeneric finish-sink (proto sink))

(defgeneric run-document-actions (prototype sink &key &allow-other-keys))
(defgeneric run-item-actions (prototype sink &key &allow-other-keys))

;;; We might want to collect document and item actions across
;;; configuration inheritance tree.  But then again there should be a
;;; way to disable/override some actions from parent configuration,
;;; which means the actions should be named.  Currently only actions
;;; from sink configuration are used.
(defmethod run-document-actions
    ((prototype prototype) (sink sink) &rest args)
  (loop with cfg = (sink-configuration sink)
        for action in (document-actions cfg)
        do (apply action (sink-document sink) args)))

(defmethod run-item-actions
    ((prototype prototype) (sink sink) &rest args)
  (let* ((cfg (sink-configuration sink))
         (actions (item-actions cfg)))
    (loop for artefact in (sink-artefacts sink)
          do (loop for action in actions
                   do (apply action artefact args)))))
