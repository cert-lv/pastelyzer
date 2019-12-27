(in-package #:pastelyzer)

;;; TODO:
;;;
;;; - Don't use error pages (i.e., document-root or
;;;   error-template-directory).
;;;
;;; - We don't want to include Server software/version in any
;;;   response.
;;;
;;; - Get rid of find-ws-handler.
;;;
;;; - Handle WebSockets in a separate server (because
;;;   websocket-acceptor has its own request and reply classes).
;;;

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

(defclass acceptor (hs:websocket-acceptor)
  ()
  (:default-initargs
   :access-log-destination nil))

(defmethod ht:acceptor-log-access ((acceptor acceptor) &key return-code)
  (msg :http "~A ~A ~A ~A"
       (ht:remote-addr*)
       return-code
       (ht:request-method*)
       (ht:script-name*)))

(defmethod ht:acceptor-log-message ((acceptor acceptor)
                                    category format &rest args)
  (maybe-log category format args))

(defvar *static-dispatcher*
  (load-time-value
   (ht:create-folder-dispatcher-and-handler "/static/" #p"public/")))

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
          ("^/show/(\\d+)$" path)
        (setf (ht:return-code*) ht:+http-moved-permanently+)
        (setf (ht:header-out :location) (format nil "/paste/~A" id))
        (return ""))

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

      (ppcre:register-groups-bind (id) ("/show\\?id=(\\d+)" path)
        (setf (ht:return-code*) ht:+http-moved-permanently+)
        (setf (ht:header-out :location) (format nil "/show/~A" id))
        (return ""))

      (when (and (or (string= "/analyze" path)
                     (string= "/analyse" path))
                 (eql :post (ht:request-method req)))
        (setf (ht:header-out :content-type) "application/json")
        (return
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
                      ("value" . "No data field."))))))))

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
  (if (string= (ht:script-name request) "/ws")
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

;;; XXX: Move to start-server.  Or find a way to integrate websocket
;;; handling with our own dispatcher.  Looking around it seems this is
;;; not feasible (see acceptor-dispatch-request method on
;;; websocket-acceptor).
(eval-when (:load-toplevel :execute)
  (pushnew 'find-ws-handler hs:*websocket-dispatch-table*))

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
    (ht:start *acceptor*)))

(defclass web-job (job)
  ())

(defmethod applicable-extractors :around ((target string-fragment)
                                          (job web-job))
  (set-difference (call-next-method)
                  '(:m3u-entries
                    :windows-internals)))

(defmethod artefact-to-json ((artefact t))
  nil)

(defmethod artefact-to-json ((artefact uri))
  (list (cons "data" (artefact-source artefact))
        '("type" . "url")))

(defmethod artefact-to-json ((artefact domain))
  (list (cons "data" (artefact-source artefact))
        '("type" . "domain")))

(defmethod artefact-to-json ((artefact ip-address))
  (list (cons "data" (princ-to-string (artefact-address artefact)))
        '("type" . "ip")))

(defmethod artefact-to-json ((artefact resolved-ip-address))
  nil)

(defmethod artefact-to-json ((artefact email))
  (list (cons "data" (artefact-source artefact))
        '("type" . "email")))

(defun process-posted-content (fragment &key context)
  (let* ((artefacts (process (make-instance 'web-job :subject fragment)))
         (groups (group-artefacts artefacts))
         (result '()))
    (loop for (nil unique nil nil) in groups
          do (loop for bag being each hash-value in unique
                   for artefact = (first bag)
                   do (when-let (json (artefact-to-json artefact))
                        (push (list* :obj
                                     (append
                                      (when (or (eq context :after)
                                                (eq context :both))
                                        `(("after" . ,(artefact-context-after artefact :limit nil :eol t))))
                                      (when (or (eq context :before)
                                                (eq context :both))
                                        `(("before" . ,(artefact-context-before artefact :limit nil :bol t))))
                                      (artefact-to-json artefact)))
                              result))))
    (jsown:to-json result)))
