(in-package #:pastelyzer)

(defun provider-urls (provider id)
  (cond ((string= "circl" provider)
         (mapcar #'remote-content-location
                 (multiple-value-list (circl-provider-id-to-url id))))
        ((string= "web" provider)
         (list id))
        (t
         nil)))

(defmethod content-uri ((content content))
  (content-uri (content-id content)))

(defmethod content-uri ((content integer))
  (puri:merge-uris (format nil "/content/~D/" content)
                   *web-server-external-uri*))

(defmethod paste-uri ((paste paste))
  (paste-uri (paste-id paste)))

(defmethod paste-uri ((paste integer))
  (puri:merge-uris (format nil "/paste/~D/" paste)
                   *web-server-external-uri*))

(defun artefact-to-jsown (content-id type value extra important note)
  `(:obj ("type" . ,type)
         ("value" . ,value)
         ,@(when important
             `(("important" . :true)))
         ,@(unless (eq :null note)
             `(("note" . ,note)))
         ,@(when (and extra (not (eq :null extra)))
             `(("extra" . ,extra)))
         ("source" . ,(princ-to-string (content-uri content-id)))))

(defun artefact-to-jsown* (content-id type value extra important note)
  (let ((fields (string-case (type)
                  ("BANK-CARD-NUMBER"
                   `(("type" . "cc-number")
                     ("digits" . ,value)))
                  ("DOMAIN"
                   `(("type" . "domain")
                     ("domain" . ,value)))
                  ("ONION"
                   `(("type" . "onion")
                     ("address" . ,value)))
                  ("EMAIL"
                   `(("type" . "email")
                     ("email" . ,value)))
                  ("CREDENTIAL"
                   `(("type" . "credential")
                     ("username" . ,value)
                     ("passphrase" . ,extra)))
                  ("IP-ADDRESS"
                   `(("type" . "ip")
                     ("address" . ,value)))
                  ("IP-SERVICE"
                   `(("type" . "service")
                     ("address" . ,value)
                     ("port" . ,(parse-integer extra))))
                  ("RESOLVED-IP-ADDRESS"
                   `(("type" . "ip")
                     ("address" . ,value)
                     ("domain" . ,extra)))
                  ("BASE64-BLOB"
                   `(("type" . "blob")
                     ("sha1" . ,value)
                     ("encoding" . "base64")
                     ("start" . ,extra)))
                  ("HEX-BLOB"
                   `(("type" . "blob")
                     ("sha1" . ,value)
                     ("encoding" . "hex")
                     ("start" . ,extra)))
                  ("BINARY-BLOB"
                   `(("type" . "blob")
                     ("sha1" . ,value)
                     ("encoding" . "binary")
                     ("start" . ,extra)))
                  (t
                   `(("type" . ,type)
                     ("value" . ,value)
                     ,@(when (and extra (not (eq :null extra)))
                         `(("extra" . ,extra))))))))
    `(:obj ,@fields
           ,@(when important
               `(("important" . :true)))
           ,@(unless (eq :null note)
               `(("note" . ,note)))
           ("source" . ,(princ-to-string (content-uri content-id))))))

(defun paste-to-jsown (paste-id provider provider-id timestamp content-id)
  `(:obj ("type" . "paste")
         ("id" . ,paste-id)
         ("location" . ,(princ-to-string (paste-uri paste-id)))
         ("provider" . ,provider)
         ("provider-id" . ,provider-id)
         ("timestamp" . ,(princ-to-string timestamp))
         ("urls" . ,(mapcar #'princ-to-string
                            (provider-urls provider provider-id)))
         ("content" . ,(princ-to-string (content-uri content-id)))))

(defun parse-limit (value)
  (cond ((null value)
         500)
        ((string= "" value)
         nil)
        ((every #'digit-char-p value)
         (parse-integer value))
        (t
         :invalid)))

(define-handler filter-artefacts :post "/artefacts"
  (let* ((value (ht:parameter "value"))
         (extra (ht:parameter "extra"))
         (limit (ht:parameter "limit"))
         (parsed-limit (parse-limit limit)))
    (cond ((eq :invalid parsed-limit)
           (msg :notice "~A -> /artefacts: invalid limit value: ~S"
                (ht:remote-addr*) limit)
           (values nil ht:+http-bad-request+))
          ((or value extra)
           (let ((artefacts
                   (db:with-connection ()
                     (db:search-artefacts :value value
                                          :extra extra
                                          :limit limit))))
             (setf (ht:header-out :content-type) "application/json")
             (jsown:to-json
              (loop for artefact in artefacts
                    collect (apply #'artefact-to-jsown artefact)))))
          (t
           (msg :notice "~A -> /artefacts: missing query parameters"
                (ht:remote-addr*))
           (values nil ht:+http-bad-request+)))))

(define-handler content-artefacts :get
    ("^/content/(\\d+)/artefacts(/typed)?$" content-id typed)
  (let ((artefacts
          (db:with-connection ()
            (db:content-artefacts content-id))))
    ;; XXX: What if the content does not exist?  Should we respond
    ;; with not-found instead of an empty set?
    (setf (ht:header-out :content-type) "application/json")
    (jsown:to-json
     (loop for artefact in artefacts
           collect (apply (if typed #'artefact-to-jsown* #'artefact-to-jsown)
                          artefact)))))

(define-handler content-meta :get
    ("^/content/(\\d+)/?$" (#'parse-integer content-id))
  (let ((sources
          (db:with-connection ()
            (db:content-sources content-id)))
        (content-uri (content-uri content-id)))
    ;; XXX: This should definitely respond with not-found if the
    ;; content does not exist?
    (setf (ht:header-out :content-type) "application/json")
    (jsown:to-json
     `(:obj
       ("type" . "content")
       ("id" . ,content-id)
       ("location" . ,(princ-to-string content-uri))
       ("body" . ,(princ-to-string (puri:merge-uris "body" content-uri)))
       ("sources" . ,(loop for paste in sources
                           collect (apply #'paste-to-jsown paste)))))))

(define-handler paste-meta :get
    ("^/paste/(\\d+)/?$" (#'parse-integer id))
  (let ((row
          (db:with-connection ()
            (db:get-paste id))))
    (cond (row
           (setf (ht:header-out :content-type) "application/json")
           (jsown:to-json (apply #'paste-to-jsown row)))
          (t
           (values nil ht:+http-not-found+)))))

(define-handler filter-artefacts/typed :post "/artefacts/typed"
  (let ((parameters '())
        (options '(:limit 500)))
    (loop for (name . value) in (ht:post-parameters*)
          do (string-case (name)
               ("limit"
                (let ((parsed (parse-limit value)))
                  (cond ((eq :invalid parsed)
                         (msg :notice
                              "~A -> /artefacts: invalid limit value: ~S"
                              (ht:remote-addr*) value)
                         (return-from filter-artefacts/typed
                           (values nil ht:+http-bad-request+)))
                        (t
                         (setq options (list* :limit parsed options))))))
               ("ip"
                (push (list :types '("IP-ADDRESS"
                                     "IP-SERVICE"
                                     "RESOLVED-IP-ADDRESS")
                            :value value)
                      parameters))
               ("domain"
                (push (list :type "DOMAIN"
                            :value value)
                      parameters))
               ("email"
                ;; XXX: For now all our usernames in credentials are
                ;; emails.
                (push (list :type "EMAIL"
                            :value value)
                      parameters))
               ("credential"
                (push (list :type "CREDENTIAL"
                            :value value)
                      parameters))
               ("cc-number"
                (push (list :type "BANK-CARD-NUMBER"
                            :value value)
                      parameters))
               ("onion"
                (push (list :type "ONION"
                            :value value)
                      parameters))
               ("sha1"
                (push (list :types '("BASE64-BLOB" "HEX-BLOB" "BINARY-BLOB")
                            :value value)
                      parameters))
               ("any"
                (push (list :value value)
                      parameters))
               (t
                (msg :notice "~A -> /artefacts/typed: unknown parameter: ~A=~A"
                     (ht:remote-addr*) name value)
                (return-from filter-artefacts/typed
                  (values nil ht:+http-bad-request+)))))
    (let ((artefacts
            (db:with-connection ()
              (apply #'db:search-artefacts-multi parameters options))))
      (setf (ht:header-out :content-type) "application/json")
      (jsown:to-json
       (loop for artefact in artefacts
             collect (apply #'artefact-to-jsown* artefact))))))
