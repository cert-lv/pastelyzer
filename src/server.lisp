(in-package #:pastelyzer)

(define-condition record-not-found-error (simple-error)
  ((key :initarg :key :reader record-not-found-error-key)
   (table :initarg :table :reader record-not-found-error-table))
  (:report
   (lambda (condition stream)
     (with-standard-io-syntax
       (format stream "Record ~A not found in ~:@(~A~)"
               (record-not-found-error-key condition)
               (record-not-found-error-table condition))))))

(defun fetch-paste (id &optional (not-found-error-p t) not-found-value)
  (let ((row (db:with-connection () (db:paste-with-content id))))
    (cond (row
           (destructuring-bind (content-id body provider provider-id)
               row
             (make-instance (cond ((string= "circl" provider)
                                   'circl-paste)
                                  ((string= "web" provider)
                                   'web-paste)
                                  (t
                                   'paste))
                            :id id
                            :provider provider
                            :provider-id provider-id
                            :content (if (eq :null body)
                                         nil
                                         (make-instance 'content
                                                        :id content-id
                                                        :body body)))))
          (not-found-error-p
           (error 'record-not-found-error :table "pastes" :key id))
          (t
           not-found-value))))

(defun fetch-content (id &optional (not-found-error-p t) not-found-value)
  (let ((body (db:with-connection () (db:content-body id))))
    (cond (body
           (make-instance 'content
                          :id id
                          :body (if (eq :null body) nil body)))
          (not-found-error-p
           (error 'record-not-found-error :table "contents" :key id))
          (t
           not-found-value))))

(defclass reply (ht:reply)
  ())

(defmethod initialize-instance :after ((reply reply) &key)
  ;; Don't include the "Server" header in response.
  (setf (ht:header-out :server reply) nil))

(defclass acceptor (hs:websocket-acceptor)
  ()
  (:default-initargs
   :access-log-destination nil
   :document-root nil
   :error-template-directory nil
   ;; Hunchensocket customizes this too, but the customization is not
   ;; needed since Hunchentoot's default reply-external-format already
   ;; is UTF-8 with LF line endings.  hs::websocket-reply is also not
   ;; exported.
   :reply-class 'reply))

(defmethod ht:acceptor-log-access ((acceptor acceptor) &key return-code)
  (msg :http "~A ~A ~A ~A"
       (ht:remote-addr*)
       return-code
       (ht:request-method*)
       (ht:script-name*)))

(defmethod ht:acceptor-log-message ((acceptor acceptor)
                                    category format &rest args)
  (maybe-log category format args))

(defmethod ht:acceptor-status-message ((acceptor acceptor) status-code
                                       &key &allow-other-keys)
  ;; Don't include any extra information in response body unless the
  ;; handler does it explicitly.
  nil)

(defvar *static-dispatcher*
  (load-time-value
   (ht:create-folder-dispatcher-and-handler "/static/" #p"public/")))

(defun store-document (req)
  (let ((time (local-time:now))
        (data (ht:post-parameter "data" req))
        (source (ht:post-parameter "source" req))
        (id (ht:post-parameter "id" req)))
    (unless (consp data)
      (setf (ht:return-code*) ht:+http-bad-request+)
      (return-from store-document "File data missing."))
    (let ((provider (or source "store"))
          (provider-id (or id (second data) ""))
          (body (read-file-into-byte-vector (first data))))
      (multiple-value-bind (paste-id content-id)
          (db:with-connection ()
            (db:store-paste body provider provider-id time))
        (msg :info "~D -> ~A : store : ~A (~/fmt:nbytes/)"
             paste-id content-id provider-id (length body))
        (let* ((content (make-instance 'content
                                      :id content-id
                                      :body body))
               (paste (make-instance 'paste
                                     :id paste-id
                                     :provider provider
                                     :provider-id provider-id
                                     :content content)))
          (send-message (get-queue :process) paste))
        (setf (ht:return-code*) ht:+http-see-other+)
        (setf (ht:header-out :location)
              (puri:merge-uris (format nil "content/~A" content-id)
                               (ht:script-name req)))
        ""))))

(defmethod ht:acceptor-dispatch-request ((acceptor acceptor)
                                         (req ht:request))
  (block nil
    (let ((path (ht:script-name req)))
      (ppcre:register-groups-bind ((#'parse-integer id))
          ("^/content/(\\d+)$" path)
        (let ((content (fetch-content id nil)))
          (cond (content
                 (setf (ht:content-type*) "text/plain; charset=utf-8")
                 (return (content-body content)))
                (t
                 (setf (ht:return-code*) ht:+http-not-found+)
                 (return nil)))))

      (ppcre:register-groups-bind ((#'parse-integer id))
          ("^/paste/(\\d+)$" path)
        (let ((paste (fetch-paste id nil)))
          (cond (paste
                 (setf (ht:content-type*) "text/plain; charset=utf-8")
                 (return (if-let (content (paste-content paste))
                           (content-body content)
                           "")))
                (t
                 (setf (ht:return-code*) ht:+http-not-found+)
                 (return nil)))))

      (when (and (string= "/store" path)
                 (eql :post (ht:request-method req)))
        (return (store-document req)))

      (when (and (or (string= "/analyze" path)
                     (string= "/analyse" path))
                 (eql :post (ht:request-method req)))
        (return (analyze-document req)))

      (when (string= "/" path)
        (return
          (ht:handle-static-file #p"public/dash.html"
                                 "text/html; charset=utf-8")))

      (when-let ((handler (funcall *static-dispatcher* req)))
        (return (funcall handler)))

      (call-next-method))))

(defun message (type &optional data)
  (jsown:to-json
   (list* :obj
          (cons "type" type)
          (when data
            (list (cons "data" data))))))

(defclass resource (hs:websocket-resource pastelyzer.config.sink:prototype)
  ((recent-announcements
    :accessor resource-recent-announcements
    :type sq:speedy-queue
    :initform (sq:make-queue 20)))
  (:default-initargs
   :client-class 'client))

(defclass client (hs:websocket-client)
  ())

(defmethod hs:client-connected ((resource resource) (client client))
  (msg :info "WS connect: ~A:~A"
       (ht:remote-addr (hs:client-request client))
       (ht:remote-port (hs:client-request client)))
  (loop for recent in (sq:queue-contents
                       (resource-recent-announcements resource))
        do (hs:send-text-message client (message "bulk_add_hit" recent))))

(defmethod hs:client-disconnected ((resource resource) (client client))
  (msg :info "WS disconnect: ~A:~A"
       (ht:remote-addr (hs:client-request client))
       (ht:remote-port (hs:client-request client))))

(defmethod hs:text-message-received ((resource resource) (client client) string)
  (let* ((msg (jsown:parse string))
         (type (jsown:val msg "type")))

    (cond ((string= type "ping")
           (hs:send-text-message client (message "pong")))

          (t
           (msg :http "~A:~A says: ~A"
                (ht:remote-addr (hs:client-request client))
                (ht:remote-port (hs:client-request client))
                string)))))

(defvar *ws-resource*
  (make-instance 'resource))

(defmethod pastelyzer.config.sink:get-prototype
    ((name (eql (pastelyzer.config.package:user-identifier "WEB-SINK"))))
  *ws-resource*)

(defun find-ws-handler (request)
  ;; It would be nice if websocket stuff was associated with an
  ;; acceptor directly, not through a special variable.  Until then
  ;; there's this function and check against *acceptor*.
  (if (and (string= (ht:script-name request) "/ws")
           (eq *acceptor* (ht:request-acceptor request)))
      *ws-resource*
      nil))

(defmethod pastelyzer.config.sink:finish-sink
    ((proto resource) (sink pastelyzer.config.sink:sink))
  (let ((paste (pastelyzer.config.sink:sink-document sink))
        (groups (pastelyzer.config.sink:group-artefacts sink)))
    (announce-artefacts/web paste :grouped groups)))

(defun announce (string &key (resource *ws-resource*))
  (sq:force-enqueue string (resource-recent-announcements resource))
  (let ((message (message "add_hit" string)))
    (loop for peer in (hs:clients resource)
          do (hs:send-text-message peer message))))

(defun announce-artefacts/web (paste
                               &key
                                 grouped
                                 (group-limit 6)
                               &aux
                                 (content (paste-content paste))
                                 (id (content-id content))
                                 (body (content-body content)))
  ;; XXX: For now generate HTML, but send JSON messages when
  ;; implemented on the other end.
  (labels ((format-group (stream class unique important duplicates)
             (let* ((unique-count (hash-table-count unique))
                    (total-count (+ unique-count duplicates)))
               (who:with-html-output (stream)
                 ((:li :class (if (plusp (hash-table-count important))
                                  "artefact-group important"
                                  "artefact-group"))
                  (who:str (string class)) ": " (who:str total-count)
                  (when (< unique-count total-count)
                    (who:htm
                     " (" (who:str unique-count) " unique)"))
                  ((:ul :class "artefact-list")
                   (let ((space-left group-limit))
                     (loop for bag being each hash-value in important
                           do (format-artefact stream (first bag)
                                               :important t)
                              (decf space-left))
                     (when (< space-left unique-count)
                       ;; Reserve room for ellipsis.
                       (decf space-left))
                     (loop with space = space-left
                           for bag being each hash-value in unique
                           for artefact = (first bag)
                           while (plusp space)
                           when (not (gethash (artefact-key artefact)
                                              important))
                             do (format-artefact stream artefact)
                                (decf space))
                     (when (< space-left unique-count)
                       (who:htm
                        ((:li :class "artefact")
                         (who:str "&hellip;"))))))))))
           (format-artefact (stream artefact &key important)
             (let ((before (artefact-context-before artefact :limit 30))
                   (after (artefact-context-after artefact :limit 30)))
              (who:with-html-output (stream)
                ((:li :class (if important "artefact important" "artefact")
                      :title (who:escape-string-minimal-plus-quotes
                              (artefact-description artefact)))
                 (when before
                   (who:htm ((:span :class "context") (who:esc before))))
                 ((:span :class "source-string")
                  (who:esc (one-line (artefact-source artefact)
                                     :limit 40
                                     :replace-invisible nil
                                     :continuation "…"
                                     :mode :squeeze)))
                 (when after
                   (who:htm ((:span :class "context") (who:esc after))))
                 (who:str #\newline))))))
    (announce
     (let ((*print-pretty* nil))
       (who:with-html-output-to-string (html nil :prologue nil :indent nil)
         ((:div :class "hit" :id id)
          ((:span :class "description")
           (multiple-value-bind (ss mm hh d m y)
               (get-decoded-time)
             (who:fmt "~D-~2,'0D-~2,'0D&nbsp;~2,'0D:~2,'0D:~2,'0D - "
                      y m d hh mm ss))
           (let ((bytes (length body)))
             (who:htm
              ((:span :title (format nil "~D byte~:P" bytes))
               (who:fmt "~/fmt:nbytes/" (length body)))))
           " : "
           ((:a :href (format nil "/content/~A" id) :target "_blank")
            (who:str id))
           " : "
           (multiple-value-bind (source url raw-url)
               (paste-source paste)
             (declare (ignore source))
             (when url
               (who:htm
                ((:a :href url :target "_blank")
                 (who:fmt "~A~A" (puri:uri-host url) (puri:uri-path url)))))
             (when raw-url
               (if url
                   (who:htm
                    " : "
                    ((:a :href raw-url :target "_blank") "raw"))
                   (who:htm
                    ((:a :href raw-url :target "_blank")
                     (who:fmt "~A~A"
                              (puri:uri-host raw-url)
                              (puri:uri-path raw-url))))))))
          (when grouped
            (who:htm
             ((:ul :class "fragment-artefacts")
              (mapc #'(lambda (group)
                        (apply #'format-group html group))
                    grouped))))))))
    nil))

(defun stop-web-server (&key force)
  (when *ws-resource*
    (loop for client in (hs:clients *ws-resource*)
          do (hs:close-connection client)))

  (when (and *acceptor* (ht:started-p *acceptor*))
    (ht:stop *acceptor* :soft (not force))))

(defun start-web-server (&key force (port 7000))
  (stop-web-server :force force)
  (let ((taskmaster (make-instance 'ht:one-thread-per-connection-taskmaster
                                   :max-thread-count 16)))
    (setf *acceptor*
          (make-instance 'acceptor
                         :taskmaster taskmaster
                         :port port))
    (pushnew 'find-ws-handler hs:*websocket-dispatch-table*)
    (ht:start *acceptor*)))

(defclass web-job (job)
  ((artefacts
    :type list
    :initform '())))

(defmethod resolve-domains-p ((job web-job))
  nil)

(defmethod applicable-extractors :around ((target string-fragment)
                                          (job web-job))
  (set-difference (call-next-method)
                  '(:m3u-entries
                    :windows-internals)))

(defmethod register-artefact ((job web-job)
                              (artefact string-artefact)
                              (source t))
  (push artefact (slot-value job 'artefacts))
  artefact)

(defun analyze-document (req)
  (setf (ht:header-out :content-type) "application/json")
  (let ((data (ht:post-parameter "data" req))
        (context
          (let ((value (ht:post-parameter "context" req)))
            (cond ((or (null value)
                       (string-equal "after" value))
                   :after)
                  ((string-equal "before" value)
                   :before)
                  ((string-equal "both" value)
                   :both)
                  ((or (string= "" value)
                       (string-equal "none" value))
                   nil)))))
    (cond ((consp data)
           (let ((bytes (read-file-into-byte-vector (first data))))
             (process-posted-content
              (make-instance 'binary-fragment :body bytes)
              :context context)))
          ((stringp data)
           (process-posted-content
            (make-instance 'string-fragment :body data)
            :context context))
          (t
           (setf (ht:return-code*) ht:+http-bad-request+)
           (jsown:to-json
            '(:obj
              ("type" . "error")
              ("value" . "No data field.")))))))

(defun process-posted-content (fragment &key context)
  (flet ((to-jsown (artefact type)
           `(:obj
             ,@(when (or (eq context :after)
                         (eq context :both))
                 `(("after" . ,(artefact-context-after artefact
                                                       :limit nil
                                                       :eol t))))
             ,@(when (or (eq context :before)
                         (eq context :both))
                 `(("before" . ,(artefact-context-before artefact
                                                         :limit nil
                                                         :bol t))))
             ("type" . ,type)
             ("data" . ,(artefact-description artefact)))))
    (let* ((job (process (make-instance 'web-job :subject fragment)))
           (groups (group-artefacts (slot-value job 'artefacts)))
           (result '())
           (types '((domain . "domain")
                    (email . "email")
                    (ip-address . "ip")
                    (uri . "url"))))
      (loop for (class unique) in groups
            for type = (assoc class types)
            when type
              do (loop for bag being each hash-value in unique
                       for artefact = (first bag)
                       do (push (to-jsown artefact (cdr type))
                                result)))
      (jsown:to-json result))))
