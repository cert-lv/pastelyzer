(in-package #:pastelyzer)

;;; System release version.
(defvar *release* "0.9")

;;; Set by build script.
(defvar *build-id* nil)

(defvar *circl-zmq-address*)

(defvar *interesting-networks* nil)

(defvar *resolve-domains* nil
  "Whether to resolve domains.")

(defvar *log-artefacts-threshold* 3
  "Include artefacts in the log output if there are fewer than this
  number of them")

(defvar *acceptor* nil)

(defvar *web-server-external-uri*
  (make-instance 'puri:uri :scheme :http :host "localhost"))

;; XXX: This is a hack to avoid resolving same domains over and over
;; again (in the scope of a single paste for now; see PROCESS generic
;; function).  Until we have our own resolver in place.
(defvar *seen-hostnames* nil)

(defvar *big-fragment-bytes* (* 1024 1024)
  "Size of a fragment that is considered big and is processed in a
  separate queue.")

(defvar *huge-fragment-bytes* (* 16 1024 1024)
  "Size of a fragment that is considered too big to process.")

(defvar *announcers*
  '(log-hit)
  "A list of functions that are called with the results of paste analysis.")

(defvar *ignored-paste-sites*
  '()
  "Paste sites to ignore when re-fetching broken pastes.")

(defvar *default-http-user-agent* nil)

;;; Ensure that artefacts of these classes have methods on
;;; ARTEFACT-STORE-VALUE and ARTEFACT-STORE-EXTRA generic functions.
(defvar *stored-artefact-classes*
  '(bank-card-number
    domain
    onion
    email
    credential
    ip-address
    ip-service
    resolved-ip-address
    base64-blob
    hex-blob
    binary-blob)
  "Classes of artefacts that are stored in the database.")
