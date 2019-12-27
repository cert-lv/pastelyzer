(in-package #:pastelyzer.db)

(defvar *db-params* nil)

(defvar *current-version-id*)

(defmacro with-connection (() &body body)
  `(pomo:with-connection *db-params*
     ,@body))

(defun call-with-auto-reconnect (thunk &key (retries nil) (interval 1))
  (let ((reconnects 0))
    (handler-bind
        ((cl-postgres:database-connection-error
           #'(lambda (condition)
               (let ((restart (find-restart :reconnect condition)))
                 (when (and restart
                            (or (null retries)
                                (<= (incf reconnects) retries)))
                   (msg :error "DB connection lost. Reconnect attempt ~D"
                        reconnects)
                   (sleep interval)
                   (invoke-restart restart))))))
      (funcall thunk))))

(defmacro with-auto-reconnect ((&rest keys &key retries interval) &body body)
  (declare (ignore retries interval))
  `(call-with-auto-reconnect #'(lambda () ,@body) ,@keys))

#+sbcl
(progn
  (defun sha256-hash (sequence)
    ;; For SBCL ironclad accepts non-simple arrays.
    (ironclad:digest-sequence 'ironclad:sha256 sequence)))

#-sbcl
(defun sha256-hash (sequence)
  (check-type sequence (array (unsigned-byte 8) (*)))
  (ironclad:digest-sequence 'ironclad:sha256
                            (make-array (length sequence)
                                        :element-type '(unsigned-byte 8)
                                        :initial-contents sequence)))

(pomo:defprepared-with-names get-version-id (title)
    ("SELECT id FROM versions WHERE title = $1" title)
    :single)

(pomo:defprepared-with-names insert-version (title description)
    ("INSERT INTO versions (title, description) VALUES ($1, $2) RETURNING id"
     title (or description :null))
    :single)

(defun ensure-version (title &optional description)
  (or (get-version-id title)
      (handler-case
          (insert-version title description)
        (cl-postgres-error:unique-violation ()
          (get-version-id title)))))

(defun initialize (&key connection-parameters release)
  (setq *db-params* connection-parameters)
  (local-time:set-local-time-cl-postgres-readers)
  ;; Since there is no way to hook into Postmodern to do this upon
  ;; connection, this function must be called before using the DB.
  (with-connection ()
    (setq *current-version-id* (ensure-version release))))

(pomo:defprepared-with-names insert-content-fix (&key broken-id fixed-id)
    ("
INSERT INTO content_fixes (broken_id, fixed_id)
VALUES ($1, $2)
ON CONFLICT DO NOTHING"
     broken-id fixed-id)
    :single)

(pomo:defprepared-with-names register-broken-content (broken-id)
    ("INSERT INTO content_fixes (broken_id) VALUES ($1)" broken-id)
    :single)

(pomo:defprepared-with-names insert-content (content hash)
    ("INSERT INTO contents (body, hash) VALUES ($1, $2) RETURNING id"
     content hash)
    :single)

(pomo:defprepared-with-names content-id-from-hash (hash)
    ("SELECT id FROM contents WHERE hash = $1" hash)
    :single)

(defun store-content (content &optional hash)
  (unless hash
    (setq hash (ironclad:digest-sequence 'ironclad:sha256 content)))
  (handler-case
      (insert-content content hash)
    (cl-postgres-error:unique-violation ()
      (content-id-from-hash hash))))

(define-condition congested-content (condition)
  ((content-id
    :initarg :content-id
    :reader congested-content-id))
  (:report (lambda (condition stream)
             (format stream "~A is congested."
                     (congested-content-id condition)))))

(defun store-paste (content provider provider-id time)
  (let* ((hash (sha256-hash content))
         (content-id (content-id-from-hash hash)))
    (unless content-id
      (setq content-id (store-content content hash)))
    (multiple-value-bind (paste-id count)
        (insert-paste provider provider-id content-id time)
      (cond ((zerop count)
             ;; Inserting failed, which means the paste has been
             ;; inserted recently (see the time interval in
             ;; INSERT-PASTE).
             (signal 'congested-content :content-id content-id)
             (values (pomo:query "
SELECT id
  FROM pastes
 WHERE content_id = $1 AND provider = $2 AND provider_id = $3"
                                 content-id provider provider-id
                                 :single)
                     content-id))
            (t
             (values paste-id content-id))))))

(pomo:defprepared-with-names insert-paste (provider provider-id content-id time)
    ("
INSERT INTO pastes AS p (provider, provider_id, content_id, updates)
     VALUES ($1, $2, $3, ARRAY[$4::timestamptz])
ON CONFLICT (content_id, provider, provider_id)
  DO UPDATE
        SET updates = array_prepend($4, p.updates)
      WHERE p.updates[1] < $4 - interval '0.4 seconds'
  RETURNING id"
     provider provider-id content-id time)
    :single)

(pomo:defprepared-with-names content-body (content-id)
    ("SELECT body FROM contents WHERE id = $1" content-id)
    :single)

(pomo:defprepared-with-names paste-with-content (source-id)
    ("
SELECT c.id, c.body, s.provider, s.provider_id
FROM pastes AS s
LEFT JOIN contents AS c ON (c.id = s.content_id)
WHERE s.id = $1"
     source-id)
    :row)

(pomo:defprepared-with-names insert-analysis (content-id version-id)
    ("
INSERT INTO analysis (content_id, version_id, created_at)
VALUES ($1, $2, now())
RETURNING created_at"
     content-id version-id)
    :single)

(defun initiate-analysis (content-id &key (version-id *current-version-id*)
                                          (force nil))
  (handler-case
      (values (insert-analysis content-id version-id))
    (cl-postgres-error:unique-violation ()
      ;; Analysis already performed or in the process.
      (cond (force
             (values (pomo:query "
UPDATE analysis
   SET created_at = now(), updated_at = NULL
 WHERE content_id = $1 AND version_id = $2
RETURNING created_at"
                          content-id version-id :single)))
            (t nil)))))

(pomo:defprepared-with-names finish-analysis
    (content-id summary &key (version-id *current-version-id*))
    ("
UPDATE analysis
   SET updated_at = now(), summary = $2
 WHERE content_id = $1
   AND version_id = $3
RETURNING updated_at"
     content-id summary version-id)
    :single)
