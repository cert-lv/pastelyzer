(defpackage #:pastelyzer.modules.misp
  (:use #:common-lisp
        #:pastelyzer)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:pastelyzer.rest
                #:jsown-call
                #:with-keepalive)
  (:import-from #:pastelyzer.config.package
                #:user-identifier)
  (:local-nicknames (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter)
                    (#:util #:pastelyzer.config.util)
                    (#:cfg #:pastelyzer.config.package)
                    (#:usr #:pastelyzer.config.user)))

(in-package #:pastelyzer.modules.misp)

(defclass misp-server (sink:configuration)
  ((ssl-headers
    :initarg :ssl-headers
    :reader misp-server-ssl-headers
    :type list)))

(defmethod slot-unbound
    ((class t) (instance misp-server) (name (eql 'ssl-headers)))
  (setf (slot-value instance 'ssl-headers)
        (loop for (attr . key) in '((:ca-cert . :ca-file)
                                    (:user-cert . :certificate)
                                    (:user-key-pass  . :certificate-password)
                                    (:user-key . :key))
              for value = (sink:attribute-value instance attr)
              when value
                collect key and collect value)))

(defun api-call (endpoint path method &optional content)
  (let ((uri (sink:attribute-value endpoint :server))
        (api-key (sink:attribute-value endpoint :api-key))
        (ssl-headers (misp-server-ssl-headers endpoint)))
    (apply #'jsown-call uri method path
           :content content
           :additional-headers `(("Authorization" . ,api-key))
           ssl-headers)))

(defun find-sharing-group-id (misp name)
  (loop for json in (jsown:filter (api-call misp "sharing_groups/index" :get)
                                  "response" map "SharingGroup")
        when (string= (jsown:val json "name") name)
          do (return (jsown:val json "id"))))

(defun add-event (misp info &key (distribution "1")
                                 (sharing-group nil)
                                 date
                                 (analysis "2")
                                 (threat-level "4"))
  (let ((content
          `(:obj
            ("analysis" . ,analysis)
            ("threat_level_id" . ,threat-level)
            ("info" . ,info)
            ,@(if sharing-group
                  `(("distribution" . "4")
                    ("sharing_group_id" . ,sharing-group))
                  `(("distribution" . ,distribution)))
            ,@(when date
                `(("date" . ,date))))))
    (let* ((res (api-call misp "events" :post content))
           (id (jsown:filter res "Event" "id"))
           (uuid (jsown:filter res "Event" "uuid")))
      (msg :info "Created MISP event ~A (~A)" id uuid)
      (values id uuid))))

(defun publish-event (misp id &key (alert nil))
  (api-call misp (format nil "events/~:[publish~;alert~]/~A" alert id) :post))

(defun add-event-attribute (misp event-id category type value &optional comment)
  (api-call misp (format nil "attributes/add/~A" event-id)
            :post
            `(:obj
              ("category" . ,category)
              ("type" . ,type)
              ("value" . ,value)
              ("comment" . ,comment))))

(defun add-tag (misp uuid tag)
  (api-call misp "tags/attachTagToObject"
            :post
            `(:obj
              ("uuid" . ,uuid)
              ("tag" . ,tag))))

(defvar *sink-prototype* nil)

(defclass proto-misp (sink:prototype)
  ())

(defmethod sink:get-prototype
    ((name (eql (user-identifier "MISP-SINK"))))
  (or *sink-prototype*
      (setq *sink-prototype* (make-instance 'proto-misp))))

(defmethod sink:configuration-class ((proto proto-misp))
  'misp-server)

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute (eql :server)) &rest args)
  (list* attribute
         (sink:check-args impl attribute args
                          '((:type string :transform puri:parse-uri)))))

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute symbol) &rest args)
  (let ((string-attrs '(:api-key :ca-cert :user-cert :user-key :user-key-pass)))
    (cond ((member attribute string-attrs)
           (list* attribute
                  (sink:check-args impl attribute args '((:type string)))))
          (t
           (call-next-method)))))

(defmethod sink:attribute-value ((cfg proto-misp) (attribute (eql :alert)))
  ;; Default value for :alert parameter.
  nil)

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute (eql :alert)) &rest args)
  (list* attribute (sink:check-args impl attribute args '((:type boolean)))))

(defmethod sink:attribute-value ((cfg proto-misp) (attribute (eql :publish)))
  ;; Default value for :publish parameter.
  t)

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute (eql :publish)) &rest args)
  (list* attribute (sink:check-args impl attribute args '((:type boolean)))))

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute (eql :sharing-group)) &rest args)
  (list* attribute (sink:check-args impl attribute args '((:type string)))))

(defmethod filter:generate-filter-function
    ((op (eql (cfg:user-identifier "UNIQUE-COUNT"))) &rest body)
  (check-type body (cons symbol null))
  (let ((class (first body)))
    (filter:make-function unique-count (sink cont)
      (let* ((groups (sink:group-artefacts sink))
             (group (assoc class groups)))
        (funcall cont
                 (if group
                     (hash-table-count (second group))
                     0))))))

(defmethod sink:parse-sink-attribute
    ((impl proto-misp) (attribute (eql :title)) &rest args)
  (list attribute (util:parse-user-template args)))

(defmethod sink:parse-action ((impl proto-misp)
                              (scope (eql :document))
                              (action (eql (user-identifier "ADD-TAGS")))
                              &rest args)
  (loop for tag in args
        unless (stringp tag)
          do (error 'sink:invalid-attribute-type
                    :sink impl
                    :attribute :document-action
                    :typespec 'string
                    :value tag))
  (lambda (document &key misp event-id uuid)
    (declare (ignore document event-id))
    (dolist (tag args)
      (add-tag misp uuid tag))))

(defun make-action/add-attribute (&key category type value comment)
  (flet ((attribute-retriever (datum)
           (let ((parsed (util:parse-dynamic-attribute datum)))
             (if (functionp parsed)
                 parsed
                 (lambda (context)
                   (declare (ignore context))
                   parsed)))))
    (let ((value (attribute-retriever value))
          (comment (attribute-retriever comment)))
      (lambda (item &key misp event-id uuid)
        (declare (ignore uuid))
        (add-event-attribute misp event-id category type
                             (funcall value item)
                             (funcall comment item))))))

(defmethod sink:parse-action ((impl proto-misp)
                              (scope (eql :document))
                              (action (eql (user-identifier "ADD-ATTRIBUTE")))
                              &rest args)
  (apply #'make-action/add-attribute args))

(defmethod sink:parse-action ((impl proto-misp)
                              (scope (eql :item))
                              (action (eql (user-identifier "ADD-ATTRIBUTE")))
                              &rest args)
  (apply #'make-action/add-attribute args))

(defmethod sink:finish-sink
    ((proto proto-misp) (sink sink:sink))
  (let* ((date (multiple-value-bind (sec min hour date month year)
                   (get-decoded-time)
                 (declare (ignore sec min hour))
                 (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))
         (title (sink:attribute-value sink :title))
         (misp (sink:sink-configuration sink)))
    (with-keepalive
      ;; FIXME: The :sharing-group configuration parameter should be
      ;; renamed to :distribution.  A named :sharing-group could be
      ;; just one option.  There are also other parameters (like
      ;; :analysis and :threat-level) that need to be handled.
      (let* ((sharing-group (sink:attribute-value sink :sharing-group))
             (sharing-group-id (find-sharing-group-id misp sharing-group)))
        (multiple-value-bind (event-id uuid)
            (add-event misp title
                       :sharing-group sharing-group-id
                       :date date)
          (sink:run-document-actions proto sink
                                     :misp misp
                                     :event-id event-id
                                     :uuid uuid)
          ;; XXX: We only want to do this for _unique_ artefacts.
          ;; This is probalby where the :deduplicate configuration
          ;; option comes in.
          (sink:run-item-actions proto sink
                                 :misp misp
                                 :event-id event-id
                                 :uuid uuid)
          (when (sink:attribute-value sink :publish)
            (publish-event misp event-id
                           :alert (sink:attribute-value sink :alert))))))))
