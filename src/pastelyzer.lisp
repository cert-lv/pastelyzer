(in-package #:pastelyzer)

(defun group-artefacts (artefacts)
  (flet ((process-group (artefacts)
           (let ((unique (make-hash-table :test 'equal))
                 (important (make-hash-table :test 'equal))
                 (duplicate-count 0))
             (dolist (artefact artefacts)
               (let* ((key (artefact-key artefact))
                      (list (push artefact (gethash key unique))))
                 (when (cdr list)
                   (incf duplicate-count))
                 (when (important-artefact-p artefact)
                   (push artefact (gethash key important)))))
             (list unique important duplicate-count))))
    (loop for (class . artefacts) in (group artefacts :key #'type-of)
          collect (list* class (process-group artefacts)))))

(defgeneric summarize-artefacts (list view &key))

(defun summarize-artefact-group (stream group &key (include-important nil))
  (destructuring-bind (class unique important duplicates)
      group
    (format stream "~A: ~D"
            class
            (+ (hash-table-count unique) duplicates))
    (when (or (and (plusp (hash-table-count important))
                   include-important)
              (not (zerop duplicates)))
      (write-string " (" stream)
      (unless (zerop duplicates)
        (format stream "~D unique~@[; ~{~A~^, ~}~]"
                (hash-table-count unique)
                (and include-important
                     (mapcar (lambda (bag)
                               (artefact-description (first bag)))
                             (hash-table-values important)))))
      (write-string ")" stream))
    stream))

(defmethod summarize-artefacts ((groups list) (view (eql :text))
                                &key (include-important nil))
  (let ((*print-pretty* nil))
    (with-output-to-string (out)
      (loop for (group . more) on groups
            while group
            do (summarize-artefact-group
                out group :include-important include-important)
               (when more (write-string ", " out))))))

(defgeneric external-url-to (thing))

(defmethod external-url-to ((content content))
  (puri:merge-uris (format nil "content/~A" (content-id content))
                   *web-server-external-uri*))

(defmethod external-url-to ((paste paste))
  (if-let (content (paste-content paste))
    (external-url-to content)
    (puri:merge-uris (format nil "paste/~A" (paste-id content))
                     *web-server-external-uri*)))

(defun announce-artefacts (paste artefacts grouped)
  (mapc #'(lambda (announcer)
            (funcall announcer paste
                     :artefacts artefacts
                     :grouped grouped))
        *announcers*))

(defun log-hit (paste &key artefacts grouped)
  (declare (ignore artefacts))
  (msg :hit "~A - ~A" (content-id paste) (summarize-artefacts grouped :text)))

(defmethod summarize-artefacts ((groups list) (view (eql :json)) &key)
  (flet ((to-json (group)
           (destructuring-bind (class unique important duplicate)
               group
             (let* ((unique (hash-table-count unique))
                    (important (hash-table-count important)))
               (cons (string class)
                     `(:obj ("UNIQUE" . ,unique)
                            ,@(unless (zerop duplicate)
                                (list (cons "DUPLICATE" duplicate)))
                            ,@(unless (zerop important)
                                (list (cons "IMPORTANT" important)))))))))
    (jsown:to-json
     (list* :obj (mapcar #'to-json groups)))))

(defclass batch-job (pastelyzer.config.context:configurable-job)
  ())

(defmethod process :around ((job batch-job))
  (setf (thread-name) (format nil "Processing ~A" job))
  (let ((success nil))
    (unwind-protect
         (prog1 (let ((*seen-hostnames* (make-hash-table :test 'equal)))
                  (call-next-method))
           (setf (thread-name) (format nil "Done processing ~A" job)
                 success t))
      (unless success
        (setf (thread-name) (format nil "Failed to process ~A" job))))))

(defmethod run-extractor :around ((extractor t) (target t) (job t))
  (handler-case
      (call-next-method)
    (error (condition)
      (let ((root (job-subject job)))
        (msg :error "Problem recognising ~A in ~:[~A / ~A~;~*~A~]: ~A"
             extractor (eq target root) root target condition)))))

(defgeneric log-artefacts (source))

(defgeneric analyse (source &key force))

(defgeneric artefact-store-value (artefact))

(defgeneric artefact-store-extra (artefact))

(defmethod artefact-store-value ((artefact string-artefact))
  (artefact-source artefact))

(defmethod artefact-store-extra ((artefact artefact))
  (artefact-note artefact))

(defmethod artefact-store-value ((artefact bank-card-number))
  (bank-card-number-digits artefact))

(defmethod artefact-store-value ((artefact credential))
  (credential-username artefact))

(defmethod artefact-store-extra ((artefact credential))
  (credential-passphrase artefact))

(defmethod artefact-store-value ((artefact domain))
  (call-next-method))

(defmethod artefact-store-value ((artefact ip-address))
  (princ-to-string (artefact-address artefact)))

(defmethod artefact-store-extra ((artefact ip-service))
  (princ-to-string (ip-service-port artefact)))

(defmethod artefact-store-value ((artefact embedded-binary))
  (let* ((bytes (fragment-body (embedded-binary-bytes artefact)))
         (hash (ironclad:digest-sequence 'ironclad:sha1 bytes)))
    (format nil "~/fmt:bytes/" hash)))

(defmethod artefact-store-extra ((artefact embedded-binary))
  (let* ((bytes (fragment-body (embedded-binary-bytes artefact)))
         (prefix (subseq bytes 0 (min 4 (length bytes)))))
    (format nil "~/fmt:bytes/" prefix)))

(defun register-artefacts-from-groups (content-id groups)
  (loop for (class unique) in groups
        do (loop for bag being each hash-value in unique
                 for artefact = (first bag)
                 do (let ((type (string class))
                          (value (artefact-store-value artefact))
                          (extra (artefact-store-extra artefact)))
                      (db:register-artefact content-id type value extra)))))

(defmethod analyse ((target paste) &key (force nil))
  (let* ((content-id (content-id target))
         (started (db:with-connection ()
                    (db:initiate-analysis content-id :force force))))
    (if started
        (let ((finishedp nil))
          (unwind-protect
               (let ((groups (log-artefacts target)))
                 (setq finishedp t)
                 (let ((summary (summarize-artefacts groups :json)))
                   (db:with-connection ()
                     (db:finish-analysis content-id summary)
                     (db:with-transaction ()
                       (db:flush-content-artefacts content-id)
                       (register-artefacts-from-groups content-id groups))))
                 groups)
            (unless finishedp
              (db:with-connection ()
                (db:finish-analysis content-id :null)))))
        (msg :debug "~A already [being] processed, skipping." target))))

(defmethod log-artefacts ((source paste))
  (let ((job (make-instance 'batch-job :subject source)))
    (process job)
    (let* ((artefacts (pastelyzer.config.context:job-artefacts job))
           (groups (group-artefacts artefacts)))
      (when groups
        (announce-artefacts source artefacts groups))
      (values groups job))))

(defun fetch-circl-pastes-loop (queue)
  (with-logged-warnings
    (with-simple-restart (abort "Stop CIRCL paste fetching.")
      (loop
        (fetch-circl-pastes *circl-zmq-address*
                            #'(lambda (paste)
                                (send-message queue paste)))
        (sleep 1)))))

(defun store-pastes-loop (in out slow-out)
  (with-logged-warnings
    (prog (msg)
     next
       (setq msg (receive-message in))
       (unless msg (return))
     retry
       (restart-case
           (handler-case
               (db:with-auto-reconnect (:interval 3)
                 (when-let* ((paste (store-in-db msg))
                             (content (paste-content paste))
                             (body (content-body content))
                             (id (content-id paste)))
                   (cond ((<= *huge-fragment-bytes* (length body))
                          (msg :info "~A too big, not processing" paste))
                         ((<= *big-fragment-bytes* (length body))
                          (msg :debug "Processing ~A in slow queue" paste)
                          (send-message slow-out paste))
                         (t
                          (send-message out paste)))))
             (db:congested-content (condition)
               (msg :debug "~A already being processed, skipping."
                    (db:congested-content-id condition))))
         (retry ()
           :report (lambda (stream)
                     (format stream "Retry processing ~S." msg))
           (go retry))
         (postpone ()
           :report (lambda (stream)
                     (format stream "Process ~S later." msg))
           (send-message in msg))
         (ignore ()
           :report (lambda (stream)
                     (format stream "Don't process ~S." msg))))
       (go next))))

(defun process-pastes-loop (in broken process-broken-p)
  (with-logged-warnings
    (loop for item = (receive-message in)
          while item
          do (block nil
               (handler-case
                   (db:with-auto-reconnect (:interval 3)
                     (let ((known-broken-p nil))
                       (handler-bind
                           ((broken-utf-8
                              (lambda (condition)
                                (declare (ignore condition))
                                (unless known-broken-p
                                  (setq known-broken-p t)
                                  (msg :info "Broken UTF-8 in ~A" item)
                                  (send-message broken item)
                                  (unless process-broken-p
                                    (return))))))
                         (analyse item))))
                 (serious-condition (condition)
                   (msg :error "Problem processing ~A: ~A" item condition)))))))

(defvar *queues* '())

(defun create-queue (name)
  (let ((mailbox (make-mailbox :name name)))
    (setq *queues* (acons name mailbox *queues*))
    mailbox))

(defun get-queue (name &optional (errorp t) (error-value nil))
  (let ((cell (assoc name *queues*)))
    (cond (cell
           (cdr cell))
          ((not errorp)
           error-value)
          (t
           (error "Unknown queue: ~S" name)))))

(defun debugger-hook (condition old-hook)
  (declare (ignore old-hook))
  (when (typep condition #+ccl 'ccl:interrupt-signal-condition
                         #+sbcl 'sb-sys:interactive-interrupt)
    (throw 'exit-on-interrupt t))

  ;; Wait for swank connection to become active.  Not very pretty, but
  ;; swank currently does not offer any functionality to wait for
  ;; connected clients.
  (unless swank::*connections*
    ;; XXX: We should also send an email, either here or in all
    ;; :critical cases.
    (msg :critical "Waiting for a human operator to handle ~A" condition)
    (loop while (endp swank::*connections*)
          do (sleep 1)))
  ;; If we lose the connection at this point swank will drop into the
  ;; default debugger, and we do not want to do that.  We probably
  ;; want to re-bind *debugger-hook* and do the whole thing in a loop.
  ;; But then again -- what happens if we lose connection when we're
  ;; already in Slime dubugger?  Should handle both of these problems
  ;; together.
  (swank:invoke-slime-debugger condition))

(defun setup-swank (port)
  (setq swank:*global-debugger* nil)
  (swank:create-server :style :spawn :dont-close t :port port)
  #+sbcl
  ;; SBCL debugger is disabled in non-interactive mode.  But if we've
  ;; been asked to start swank, we want the debugger.
  (setq sb-ext:*invoke-debugger-hook* nil)
  ;; This can be done per-thread.
  (setq *debugger-hook* #'debugger-hook))

(defun run-server (&key (parallel 4)
                        (process-broken-p t)
                        (swank-port nil)
                        (web-server-port nil)
                        (web-server-external-host nil)
                        (web-server-external-port nil))
  (check-type parallel fixnum)
  (check-type swank-port (or null fixnum))
  (let ((db-params (append (list (or (uiop:getenv "DB_NAME") "pastelyzer")
                                 (or (uiop:getenv "DB_USER") "pastelyzer")
                                 (or (uiop:getenv "DB_PASS") "")
                                 (or (uiop:getenv "DB_HOST") "localhost"))
                           (list :port
                                 (let ((port-string (uiop:getenv "DB_PORT")))
                                   (if port-string
                                       (parse-integer port-string)
                                       5432)))
                           (list :pooled-p t))))
    (db:initialize :connection-parameters db-params
                   :release *release*))
  (setq *circl-zmq-address*
        (or (uiop:getenv "CIRCL_ZMQ_ADDRESS")
            "tcp://crf.circl.lu:5556"))
  (when-let ((string (uiop:getenv "IGNORED_PASTESITES")))
    (setq *ignored-paste-sites* (split-sequence #\, string)))
  (when swank-port
    (setup-swank swank-port))
  (when web-server-external-host
    (setf (puri:uri-host *web-server-external-uri*) web-server-external-host))
  (when-let ((port (or web-server-external-port web-server-port)))
    (setf (puri:uri-port *web-server-external-uri*) port))

  ;; We unload foreign libraries required to run in server mode before
  ;; dumping the image so that the CLI version can be used without
  ;; these.
  (cl+ssl:reload)
  (cffi:load-foreign-library 'pzmq::libzmq)

  (let* ((store-queue (create-queue :store))
         (process-queue (create-queue :process))
         ;; Separate queue for big pastes.
         (slow-queue (create-queue :slow))
         (broken-queue (create-queue :broken))
         (workers ()))
    (push (make-named-thread 'store-pastes-loop
                             "store pastes"
                             store-queue
                             process-queue
                             slow-queue )
          workers)
    (push (make-named-thread 'fetch-broken-pastes-loop
                             "fetch broken pastes"
                             broken-queue
                             process-queue)
          workers)
    (loop repeat (1- parallel)
          do (push (make-named-thread 'process-pastes-loop
                                      "process pastes"
                                      process-queue
                                      broken-queue
                                      process-broken-p)
                   workers))
    (push (make-named-thread 'process-pastes-loop
                             "process big pastes"
                             slow-queue
                             broken-queue
                             nil)
          workers)
    (when web-server-port
      (start-web-server :port web-server-port))
    (unwind-protect
         ;; XXX: Do this in the main thread for now.  In the
         ;; future when we have multiple sources we will run them
         ;; in their own threads, and have a supervisor thread.
         (fetch-circl-pastes-loop store-queue)
      (stop-web-server)
      (send-message store-queue nil)
      (loop repeat (1- parallel)
            do (send-message process-queue nil))
      ;; Wait a while for queued tasks to finish naturally.
      (loop repeat 15
            until (and (mailbox-empty-p store-queue)
                       (mailbox-empty-p process-queue)
                       (mailbox-empty-p slow-queue)
                       (mailbox-empty-p broken-queue))
            do (sleep 0.2)))))

(defun optionp (string)
  (and (< 1 (length string))
       (char= #\- (char string 0))))

(defun resolve-log-level (string)
  (list* 'or (mapcan (lambda (category)
                       (let ((keyword (find-symbol (string-upcase category)
                                                   "KEYWORD")))
                         (if (and keyword
                                  (pastelyzer.log:known-log-category-p keyword))
                             (list keyword)
                             (warn "Invalid log-level: ~A" category))))
                     (split-sequence #\, string :remove-empty-subseqs t))))

(defun fail (format &rest args)
  (apply #'format t format args)
  (fresh-line)
  (uiop:quit 1))

(defun parse-cmdline (args)
  (let ((keys '())
        (paths '()))
    (flet ((collect (key value)
             (setq keys (list* key value keys))))
      (loop
        (when (endp args)
          (return))
        (let ((arg (pop args)))
          (cond ((string= "--interactive" arg)
                 (collect :interactive t))
                ((string= "--log-level" arg)
                 (when-let ((level (resolve-log-level (pop args))))
                   (collect :log-level level)))
                ((or (string= "-c" arg)
                     (string= "--config" arg))
                 (collect :config (pop args)))
                ((or (string= "-w" arg)
                     (string= "--workers" arg))
                 (let ((arg (pop args)))
                   (cond ((and arg (every #'digit-char-p arg))
                          (collect :parallel (parse-integer arg)))
                         (t
                          (warn "Invalid parallel value: ~A" arg)))))
                ((string= "--swank-port" arg)
                 (let ((arg (pop args)))
                   (cond ((and arg (every #'digit-char-p arg))
                          (collect :swank-port (parse-integer arg)))
                         (t
                          (warn "Invalid swank port value: ~A" arg)))))
                ((string= "--resolve-domains" arg)
                 (collect :resolve-domains t))
                ((string= "--no-resolve-domains" arg)
                 (collect :resolve-domains nil))
                ((string= "--tlds-file" arg)
                 (let ((arg (pop args)))
                   (warn "~
--tlds-file option has been removed.  Use configuration like the
following instead:

  (define-set tlds (super-domains)
    :file \"~A\")

  (define-artefact-filter check-tld
      (and (type? domain)
           (not (member? tlds)))
    (discard \"Unknown TLD\"))~%" arg)))
                ((string= "--important-cc-bins" arg)
                 (let ((arg (pop args)))
                   (warn "~
--important-cc-bins option has been removed.  Use configuration like the
following instead:

  (define-set bins (cc-bins)
    :file \"~A\")

  (define-artefact-filter check-cc-bin
      (and (type? bank-card-number)
           (not (member? bins)))
    (set-important))~%" arg)))
                ((string= "--interesting-tlds" arg)
                 (let ((arg (pop args)))
                   (warn "~
--interesting-tlds option has been removed.  Use configuration like the
following instead:

  (define-set interesting-tlds (cc-bins)
    :entries (~{~S~^ ~}))

  (define-artefact-filter interesting-tld
      (and (type? domain)
           (member? interesting-tlds))
    (set-important))~%" (split-sequence #\, arg :remove-empty-subseqs t))))
                ((string= "--server-port" arg)
                 (let ((arg (pop args)))
                   (cond ((and arg (every #'digit-char-p arg))
                          (collect :web-server-port (parse-integer arg)))
                         (t
                          (warn "Invalid port value: ~A" arg)))))
                ((string= "--server-ext-host" arg)
                 (let ((arg (pop args)))
                   (cond (arg
                          (collect :web-server-external-host arg))
                         (t
                          (warn "Invalid host value: ~A" arg)))))
                ((string= "--server-ext-port" arg)
                 (let ((arg (pop args)))
                   (cond ((and arg (every #'digit-char-p arg))
                          (collect :web-server-external-port
                            (parse-integer arg)))
                         (t
                          (warn "Invalid port value: ~A" arg)))))
                ((string= "--process-broken" arg)
                 (collect :process-broken-p t))
                ((string= "--no-process-broken" arg)
                 (collect :process-broken-p nil))
                ((or (string= "-h" arg)
                     (string= "--help" arg))
                 (usage)
                 (uiop:quit))
                ((or (string= "-v" arg)
                     (string= "--version" arg))
                 (format t "~&Pastelyzer ~A~@[ (~A)~]~%~A ~A~%"
                         *release*
                         *build-id*
                         (lisp-implementation-type)
                         (lisp-implementation-version))
                 (uiop:quit))
                ((string= "--server" arg)
                 (collect :mode :server))
                ((or (string= "-C" arg)
                     (string= "--color" arg)
                     (string= "--colour" arg))
                 (collect :colour t))
                ((or (string= "+C" arg)
                     (string= "--no-color" arg)
                     (string= "--no-colour" arg))
                 (collect :colour nil))
                ((string= "--" arg)
                 (return))
                (t
                 (if (or (string= "-" arg)
                         (ignore-errors (probe-file arg)))
                     (push arg paths)
                     (fail "Invalid argument: ~A" arg))))))
      (setq paths (nreverse paths)))
    (when args
      (setf paths (nconc paths args)))
    (when paths
      (if (eq :server (getf keys :mode))
          (fail "Unexpected argument~P in server mode: ~{~A~^, ~}"
                (length paths) paths)
          (setf (getf keys :paths) paths
                (getf keys :mode) :cli)))
    keys))

(defun usage ()
  (format t "~
Usage:
  pastelyzer <options> [-|path+]
  pastelyzer <options> --server

Generic options:
  --[no-]resolve-domains resolve domains; defaults to yes if --networks-file
                         is specified, no otherwise

CLI options:
  -C, --color            colorize output
  +C, --no-color         don't colorize output

Server options:
  -c, --config           path to configuration file
  -w, --workers          use N worker threads (default: 4)
  --server-port          start web server on port N (default: 7000)
  --server-ext-host      web server host (for emails; default: \"localhost\")
  --server-ext-port      web server port (for emails; no default)
  --[no-]process-broken  process pastes with broken UTF-8 content (in addition
                         to trying to fix them; defaults to yes)

Miscellaneous options:
  --log-level         set logging level; allowed values: DEBUG, INFO, NOTICE,
                      WARNING (default), ERROR, CRITICAL
  --swank-port        start swank server (SLIME) on this port
  --interactive       debugger will be entered on errors

Environment variables:

  DB_NAME             database name (default: \"pastelyzer\")
  DB_USER             database user (default: \"pastelyzer\")
  DB_PASS             database password (default: empty)
  DB_HOST             database host (default: \"localhost\")
  DB_PORT             database port (default: 5432)
  CIRCL_ZMQ_ADDRESS   paste feed endpoint (default \"tcp://crf.circl.lu:5556\")
  IGNORED_PASTESITES  Comma-separated list of paste sites to not re-fetch
                      broken pastes from
  HTTP_USER_AGENT     User agent to use when fetching web pages
"))

(defun read-config (namestring)
  (msg :info "Reading configuration from ~A" namestring)
  (handler-case
      (let ((pathname (parse-namestring namestring)))
        (pastelyzer.config.loader:load-configuration pathname))
    (error (condition)
      (msg :error "~A" condition)
      (format *error-output*
              "~&Could not read configuration from '~A'. ~
               Please ensure that the file exists and is readable.~%"
              namestring)
      (finish-output *error-output*)
      (uiop:quit 1))))

(defun run (&rest keys
            &key interactive
                 (log-level :warning)
                 config
                 (resolve-domains nil resolve-domains-supplied-p)
                 mode
            &allow-other-keys)
  (setup-logging :filter log-level
                 :message-class (if interactive
                                    'pastelyzer.log:timestamped-message
                                    'pastelyzer.log:plain-message))
  (with-logged-warnings
    (when config
      (read-config config))
    (setq *resolve-domains*
          (cond (resolve-domains-supplied-p
                 resolve-domains)
                (*interesting-networks*
                 t)
                (t nil)))
    (setq keys (delete-from-plist keys
                                  :mode
                                  :interactive
                                  :log-level
                                  :config
                                  :resolve-domains))
    (setq *default-http-user-agent*
          (or (uiop:getenv "HTTP_USER_AGENT")
              (format nil "Pastelyzer~@[-~A~]" *build-id*)))
    (case mode
      (:server
       (apply #'run-server keys))
      (:cli
       (apply #'run-cli keys))
      (t
       (cond ((isatty *standard-input*)
              (usage)
              (uiop:quit))
             (t
              (apply #'run-cli :paths '("-") keys)))))))

(defun run-standalone (&optional (args (uiop:command-line-arguments)))
  (let ((real-args (append (parse-cmdline args)
                           '(:process-broken-p t))))
    (catch 'exit-on-interrupt
      (cond ((getf real-args :interactive)
             #+sbcl (sb-ext:enable-debugger)
             (apply #'run real-args))
            (t
             (flet ((run ()
                      (handler-case
                          (apply #'run real-args)
                        (serious-condition (condition)
                          (princ condition t)))))
               #-ccl
               (run)
               #+ccl
               (let ((ccl::*invoke-debugger-hook-on-interrupt* t))
                 (run))))))))
