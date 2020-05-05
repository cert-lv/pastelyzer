(defpackage #:pastelyzer.config.sets
  (:use :common-lisp)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:pastelyzer.config.filter
                #:make-function)
  (:local-nicknames (#:util #:pastelyzer.util)
                    (#:usr #:pastelyzer.config.user)
                    (#:filter #:pastelyzer.config.filter)
                    (#:loader #:pastelyzer.config.loader)))

(in-package #:pastelyzer.config.sets)

(defgeneric load-set (type source &key &allow-other-keys))
(defgeneric contains? (datum set))

(defclass lookup-table ()
  ((source
    :initarg :source
    :reader lookup-table-source
    :type (or null pathname))
   (entries
    :initarg :entries
    :reader lookup-table-entries)))

(defmethod contains? ((artefact pastelyzer:string-artefact) (set lookup-table))
  (contains? (pastelyzer:artefact-source artefact) set))

(defclass string-set (lookup-table)
  ((entries
    :type hash-table)))

(defmethod load-set ((type (eql 'usr:strings)) (path pathname)
                     &rest keys
                     &key (comment-start "#") (attach-comments nil))
  (unless (and (string= "#" comment-start) (null attach-comments))
    (warn "Unsupported options for STRINGS: ~S" keys))
  (let ((table (make-hash-table :test 'equal)))
    (util:map-lines path
                    (lambda (entry)
                      (setf (gethash entry table) t))
                    :ignore-comment-lines t
                    :trim-space t)
    (make-instance 'string-set
                   :source path
                   :entries table)))

(defmethod contains? ((string string) (set string-set))
  (gethash string set))

(defclass cc-bin-set (lookup-table)
  ((entries
    :initarg :bins
    :reader cc-bin-set-bins
    :type list)))

(defmethod contains? ((digits string) (set cc-bin-set))
  (let ((length (length digits)))
    (loop for (ndigits plength table) in (cc-bin-set-bins set)
          when (= ndigits length)
            do (multiple-value-bind (note foundp)
                   (gethash (subseq digits 0 plength) table)
                 (when foundp
                   (return (if note note t)))))))

(defmethod contains? ((artefact pastelyzer:bank-card-number) (set cc-bin-set))
  (contains? (pastelyzer:bank-card-number-digits artefact) set))

(defmethod load-set ((type (eql 'usr:cc-bins)) (path pathname)
                     &rest keys
                     &key (comment-start "#") (attach-comments t))
  (unless (and (string= "#" comment-start) (eql 't attach-comments))
    (warn "Unsupported options for CC-BINS: ~S" keys))
  (make-instance 'cc-bin-set
                 :source path
                 :bins (pastelyzer::read-bins path)))

(defclass ipv4-network-set (lookup-table)
  ((entries
    ;; An alist mapping prefix-length to a hash-table.
    ;;
    ;; Theoretically the networks should not overlap, but it seems a
    ;; good idea to allow it so that known finer-grained networks can
    ;; be reported, and the bigger prefixes left as a fall-back.
    ;; Entries are be stored (and therefore also checked) with the
    ;; longest prefixes first.
    :type list
    :initform '())))

(defmethod contains? ((artefact pastelyzer:ip-address) (set ipv4-network-set))
  (contains? (pastelyzer::artefact-address artefact) set))

(defmethod add-entry ((set ipv4-network-set) (network ip:ipv4-network))
  (with-slots (entries)
      set
    (let* ((provided-bits (ip:ipv4-network-bits network))
           (prefix (ip:ipv4-network-prefix network))
           (bits (mask-field (byte prefix (- 32 prefix)) provided-bits))
           (cell (assoc prefix entries))
           (table (if cell
                      (cdr cell)
                      (let ((table (make-hash-table)))
                        (setf entries
                              (sort (acons prefix table entries) #'>
                                    :key #'car))
                        table))))
      (if (gethash bits table)
          (warn "Network ~S already present in ~S" network set)
          (setf (gethash bits table) t)))))

(defmethod contains? ((address ip:ip-address) (set ipv4-network-set))
  (loop with entries = (slot-value set 'entries)
        with address-bits = (ip:ipv4-address-bits address)
        for (prefix . table) of-type ((integer 1 32) . hash-table) in entries
        do (let ((bits (mask-field (byte prefix (- 32 prefix)) address-bits)))
             (when (gethash bits table)
               (return t)))))

(defmethod load-set ((type (eql 'usr:ipv4-networks)) (list cons)
                     &rest keys)
  (when keys
    (warn "Unsupported options for IPv4-NETWORKS: ~S" keys))
  (loop with result = (make-instance 'ipv4-network-set :source nil)
        for string in list
        do (add-entry result (ip:parse-address string :network))
        finally (return result)))

(defmethod load-set ((type (eql 'usr:ipv4-networks)) (path pathname)
                     &rest keys
                     &key (comment-start "#") (attach-comments nil))
  (unless (and (string= "#" comment-start) (eql 'nil attach-comments))
    (warn "Unsupported options for IPv4-NETWORKS: ~S" keys))
  (let ((result (make-instance 'ipv4-network-set :source path)))
    (util:map-lines path
                    (lambda (string)
                      (with-simple-restart
                          (continue "Ignore the invalid network.")
                        (add-entry result (ip:parse-address string :network))))
                    :trim-space t
                    :ignore-comment-lines t)
    result))

(defclass super-domain-set (lookup-table)
  ((entries
    :initarg :entries
    :reader super-domain-set-entries
    :type hash-table)))

(defun hashtree-add-path (tree path)
  (check-type tree hash-table)
  (check-type path list)
  (if (null (cdr path))
      (setf (gethash (car path) tree) 't)
      (multiple-value-bind (table foundp)
          (gethash (car path) tree)
        (if foundp
            (if (eq 't table)
                (warn "Wildcard entry already present for ~S; ignoring ~S"
                      (car path) (cdr path))
                (hashtree-add-path table (cdr path)))
            (let ((new (setf (gethash (car path) tree)
                             (make-hash-table :test 'equal))))
              (hashtree-add-path new (cdr path)))))))

(defun hashtree-present-p (tree path)
  (multiple-value-bind (value foundp)
      (gethash (car path) tree)
    (if foundp
        (cond ((eql 't value)
               ;; Terminating table — all sub-searches match.
               t)
              ((null (cdr path))
               ;; Path too short.
               nil)
              (t
               (hashtree-present-p value (cdr path))))
        nil)))

(defmethod contains? ((domain string) (set super-domain-set))
  (hashtree-present-p (lookup-table-entries set)
                      (reverse (split-sequence #\. domain))))

(defmethod load-set ((type (eql 'usr:super-domains)) (list cons)
                     &rest keys)
  (when keys
    (warn "Unsupported options for SUPER-DOMAINS: ~S" keys))
  (let ((tree (make-hash-table :test 'equal)))
    (dolist (entry list)
      (hashtree-add-path tree (reverse (split-sequence #\. entry))))
    (make-instance 'super-domain-set
                   :source nil
                   :entries tree)))

(defmethod load-set ((type (eql 'usr:super-domains)) (path pathname)
                     &rest keys
                     &key (comment-start "#") (attach-comments nil))
  (unless (and (string= "#" comment-start) (eql 'nil attach-comments))
    (warn "Unsupported options for SUPER-DOMAINS: ~S" keys))
  (let ((tree (make-hash-table :test 'equal)))
    (util:map-lines path
                    (lambda (line)
                      (hashtree-add-path tree
                                         (reverse (split-sequence #\. line))))
                    :ignore-comment-lines t
                    :trim-space t)
    (make-instance 'super-domain-set
                   :source path
                   :entries tree)))

(defvar *known-sets* '())

(defun resolve-set (name)
  (cdr (assoc name *known-sets*)))

(defun register-set (name datum)
  (let ((cell (assoc name *known-sets*)))
    (if cell
        (setf (cdr cell) datum)
        (setq *known-sets* (acons name datum *known-sets*)))))

(defmethod loader::apply-directive ((directive (eql 'usr:define-set))
                                    (args list))
  (destructuring-bind
      (name (type) &rest keys
                   &key file entries auto-reload comment-start attach-comments)
      args
    (declare (ignore auto-reload comment-start attach-comments))
    (msg :debug "Defining ~(~S~) set ~S" type name)
    (when (and file entries)
      (error ":FILE and :ENTRIES are mutually exclusive"))
    (cond (entries
           (register-set name
                         (apply #'load-set type entries
                                (alexandria:remove-from-plist keys :entries))))
          (file
           (unless (pathnamep file)
             (setq file (parse-namestring file)))
           (register-set name
                         (apply #'load-set type file
                                (alexandria:remove-from-plist keys :file))))
          (t
           (error "Need either :FILE or :ENTRIES")))))

(defmethod filter:generate-filter-function ((operator (eql 'usr:member?))
                                            &rest body)
  (check-type body (cons symbol null))
  (let* ((identifier (first body))
         (set (resolve-set identifier)))
    (make-function member? (artefact cont)
      (funcall cont (contains? artefact set)))))
