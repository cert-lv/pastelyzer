(in-package #:pastelyzer)

(defgeneric remote-content-location (instance))

(defclass remote-content ()
  ())

(defclass http-content (remote-content)
  ((uri
    :initarg :uri
    :reader remote-content-location
    :type (or puri:uri string))))

(defmethod print-object ((object http-content) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (puri:render-uri (remote-content-location object) stream)))

(defclass link-only-http-content (http-content)
  ())

(defmethod content-body ((content link-only-http-content))
  nil)

(defclass content (binary-fragment)
  ((id
    :initarg :id
    :reader content-id
    :type unsigned-byte)
   (body
    :reader content-body)))

(defmethod print-object ((object content) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A (~/fmt:nbytes/)"
            (content-id object)
            (length (content-body object)))))

(defgeneric paste-source (paste)
  (:documentation "Returns 3 values: Source name, URL for the paste
  and URL for raw paste content (on the source)."))

(defgeneric paste-origins (paste)
  (:documentation "Return a list of places where PASTE can be
  retrieved from."))

(defmethod paste-source ((paste t))
  (values "Unknown" nil nil))

(defclass paste ()
  ((id
    :initarg :id
    :reader paste-id
    :type (or null unsigned-byte))
   (provider
    :initarg :provider
    :reader paste-provider
    :type string)
   (provider-id
    :initarg :provider-id
    :reader paste-provider-id
    :type string)
   (content
    :initarg :content
    :reader paste-content
    :type (or null content))))

(defmethod print-object ((paste paste) (stream t))
  (if (or *print-escape* *print-readably*)
      (print-unreadable-object (paste stream :type t :identity t)
        (format stream "~A" (paste-id paste))
        (when-let (content (paste-content paste))
          (format stream " -> ~A (~/fmt:nbytes/)"
                  (content-id content)
                  (length (content-body content)))))
      (format stream "~A ~A~@[ -> ~A~]"
              (class-name (class-of paste))
              (paste-id paste)
              (when-let (content (paste-content paste))
                (content-id content)))))

(defmethod paste-source ((paste paste))
  (values
   (concatenate 'string (paste-provider paste) ":" (paste-provider-id paste))
   nil
   nil))

;;; Two convenience methods.
(defmethod content-id ((paste paste))
  (if-let (content (paste-content paste))
    (content-id content)
    (error "~S has no content" paste)))

(defmethod content-body ((paste paste))
  (if-let (content (paste-content paste))
    (content-body content)
    (error "~S has no content" paste)))

(defmethod paste-content ((content content))
  content)

(defclass circl-paste (paste)
  ()
  (:default-initargs
   :provider "circl"))

(defun parse-circl-paste-id (string)
  (ppcre:register-groups-bind (site id)
      ("/([^/]+)/\\d{4}/\\d{2}/\\d{2}/(.+)\\.gz$" string)
    (return-from parse-circl-paste-id (values site id)))
  (error "Unrecognized paste file: ~S" string))

(defmethod paste-origin ((paste circl-paste))
  (circl-provider-id-to-url (paste-provider-id paste)))

(defun circl-provider-id-to-url (provider-id)
  (multiple-value-bind (site id)
      (parse-circl-paste-id provider-id)
    (flet ((url (scheme host &rest path)
             (make-instance 'puri:uri
                            :scheme scheme
                            :host host
                            :path (apply #'concatenate 'string path))))
      (cond ((or (string= "pastebin.com_pro" site)
                 (string= "pastebin.com" site))
             (values
              (make-instance 'http-content
                             :uri (url :https "pastebin.com" "/raw/" id))
              (make-instance 'link-only-http-content
                             :uri (url :https "pastebin.com" "/" id))))

            ((string= "lpaste.net" site)
             (values
              (make-instance 'http-content
                             :uri (url :http site "/raw/" id))
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))

            ((string= "gist.github.com" site)
             (destructuring-bind (user hash)
                 (split-sequence #\_ id)
               (values
                (make-instance 'link-only-http-content
                               :uri (url :https site "/" user "/" hash)))))

            ((string= "snipplr.com" site)
             (destructuring-bind (snipplr-id slug tail)
                 (split-sequence #\_ id)
               (when (string/= "" tail)
                 (warn "Unexpected snipplr.com paste id: ~S" id))
               (values
                (make-instance 'link-only-http-content
                               :uri (url :http site
                                         "/view/" snipplr-id "/" slug "/")))))

            ((or (string= "paste.debian.net" site)
                 (string= "ideone.com" site))
             (values
              (make-instance 'http-content
                             :uri (url :http site "/plain/" id))
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))

            ((string= "slexy.org" site)
             (values
              (make-instance 'link-only-http-content
                             :uri (url :https site "/view/" id))))

            ((string= "paste.org.ru" site)
             (values
              (make-instance 'link-only-http-content
                             :uri (url :http site "/?" id))))

            ((string= "paste.opensuse.org" site)
             (values
              (make-instance 'http-content
                             :uri (url :http site "/view/raw/" id))
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))

            ((string= "kpaste.net" site)
             (values
              (make-instance 'http-content
                             :uri (url :http site "/" id "?raw"))
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))

            ((string= "pastebin.ru" site)
             (values
              (make-instance 'http-content
                             :uri (url :http site "/" id "/d/"))
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))

            ((or (string= "justpaste.it" site)
                 (string= "paste.kde.org" site))
             (values
              (make-instance 'link-only-http-content
                             :uri (url :https site "/" id))))

            (t
             (values
              (make-instance 'link-only-http-content
                             :uri (url :http site "/" id))))))))

(defmethod paste-source ((paste circl-paste))
  (multiple-value-bind (first other)
      (paste-origin paste)
    (let* ((first-url (remote-content-location first))
           (host (puri:uri-host first-url)))
      (if other
          (values host (remote-content-location other) first-url)
          (values host first-url nil)))))

(defclass web-server-state ()
  ((host
    :initarg :host
    :reader web-server-state-host)
   (cookie-jar
    :initarg :cookie-jar
    :reader web-server-state-cookie-jar
    :initform (make-instance 'drakma:cookie-jar))))

(defclass rate-controlled-web-server-state (web-server-state)
  ((interval
    :initarg :interval
    :type fixnum
    :initform (* 8 internal-time-units-per-second)
    :documentation "Minimum time between requests using this state, in
    INTERNAL-TIME-UNITS-PER-SECOND.")
   (last-activity
    :type unsigned-byte
    :initform 0)))

(defmethod print-object ((object web-server-state) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (princ (web-server-state-host object) stream)))

(defmethod fetch-web-page :around ((state rate-controlled-web-server-state)
                                   (uri t)
                                   &key &allow-other-keys)
  (with-slots (interval last-activity)
      state
    (let ((time (get-internal-real-time))
          (next-activity (+ last-activity interval)))
      (when (< time next-activity)
        ;; Pause for INTERVAL + 30%.
        (let ((seconds (/ (+ (- next-activity time)
                             (random (* interval 0.3)))
                          internal-time-units-per-second)))
          (msg :debug "Waiting ~,3Fs on ~A" seconds state)
          (sleep seconds)))
      (unwind-protect
           (call-next-method)
        (setf last-activity (get-internal-real-time))))))

(defmethod fetch-web-page ((state web-server-state) (uri puri:uri)
                           &rest keys &key &allow-other-keys)
  (with-slots (cookie-jar)
      state
    (multiple-value-bind (body status headers real-uri stream closep reason)
        (apply #'drakma:http-request uri
               :want-stream t
               :redirect 1
               :user-agent *default-http-user-agent*
               :cookie-jar cookie-jar
               keys)
      (unwind-protect
           (cond ((= 200 status)
                  (values (read-stream-content-into-byte-vector
                           (flexi-streams:flexi-stream-stream body))
                          real-uri
                          headers))
                 (t
                  (error "Unexpected response: ~S (~A) from ~A"
                         status reason uri)))
        (when closep
          (close stream))))))

;;; XXX: This is not thread-safe!
(defvar *server-states* (make-hash-table :test 'equal))

(defmethod initial-server-state-for ((content http-content))
  (make-instance 'rate-controlled-web-server-state
                 :host (puri:uri-host (remote-content-location content))))

(defmethod content-body ((content http-content))
  (let ((key (puri:uri-host (remote-content-location content))))
    (fetch-web-page (or (gethash key *server-states*)
                        (setf (gethash key *server-states*)
                              (initial-server-state-for content)))
                    (remote-content-location content))))

(defclass fetched-content (http-content)
  ((origin
    :initarg :origin
    :reader fetched-content-origin
    :type (or null string)
    :documentation "Where the data was fetched from.")))

(defmethod retrieve-original ((paste circl-paste))
  (let ((origin (paste-origin paste)))
    (unless (find (puri:uri-host (remote-content-location origin))
                  *ignored-paste-sites*
                  :test #'string=)
      (multiple-value-bind (data uri headers)
          (content-body origin)
        (when data
          (msg :debug "Retrieved ~A (~/fmt:nbytes/)"
               uri (length data))
          (return-from retrieve-original
            (values data uri headers)))))))

(defclass web-paste (paste)
  ()
  (:default-initargs
   :provider "web"))

(defmethod paste-source ((paste web-paste))
  (let ((uri (puri:uri (paste-provider-id paste))))
    (values (puri:uri-host uri) nil uri)))

(defun store-fixed-content (&key broken-id uri data)
  (declare (type unsigned-byte broken-id)
           (type puri:uri uri)
           (type (or null vector) data))
  (let ((url (puri:render-uri uri nil))
        (time (local-time:now)))
    (db:with-connection ()
      (db:with-transaction ()
        (multiple-value-bind (paste-id content-id)
            (db:store-paste data "web" url time)
          (db:insert-content-fix :broken-id broken-id
                                 :fixed-id content-id)
          (msg :notice "Fixed ~A -> ~A (~A)" broken-id content-id url)
          (make-instance 'web-paste
                         :id paste-id
                         :provider-id url
                         :content (make-instance 'content
                                                 :id content-id
                                                 :body data)))))))

(defun fix-paste (paste)
  (multiple-value-bind (data uri headers)
      (retrieve-original paste)
    (declare (ignore headers))
    (let ((broken-id (content-id paste)))
      (cond (data
             (store-fixed-content :broken-id broken-id
                                  :data data
                                  :uri uri))
            (t
             (msg :notice "Can't fix ~A" paste)
             (db:with-connection ()
               (db:register-broken-content broken-id))
             nil)))))

(defun fetch-broken-pastes-loop (in out)
  ;; XXX: This is very much synchronous, i.e., broken pastes are
  ;; fetched (including waiting for server cooldown) and stored one by
  ;; one.
  (with-logged-warnings
    (loop for paste = (receive-message in)
          while paste
          do (handler-case
                 (when-let (fixed (fix-paste paste))
                   (send-message out fixed))
               (serious-condition (condition)
                 (msg :error "Problem fixing ~A: ~A" paste condition)
                 (db:with-connection ()
                   (db:register-broken-content (content-id paste))))))))
