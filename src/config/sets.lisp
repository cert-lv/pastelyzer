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

(defun cfg-bool (value)
  (ecase value
    ((cl:nil usr:no usr:false) nil)
    ((cl:t usr:yes usr:true) t)))

(defgeneric add-entry (set key &optional value))
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

(defmethod contains? :around ((artefact pastelyzer:artefact) (set lookup-table))
  (multiple-value-bind (found note)
      (call-next-method)
    (when note
      (setf (pastelyzer:artefact-note artefact) note))
    found))

(defmethod populate-set ((set lookup-table) (list cons) &rest keys)
  (when keys
    (warn "Options not supported for ~S: ~S" set keys))
  (loop for entry in list
        do (multiple-value-bind (value comment)
               (etypecase entry
                 (cons
                  (values-list entry))
                 (string
                  (values entry nil)))
             (add-entry set value comment)))
  set)

(defmethod populate-set ((set lookup-table) (path pathname)
                         &key (comment-start "#")
                              (attach-comments nil)
                              (trim-space t))
  (with-open-file (in path)
    (loop with note = nil
          for line = (read-line in nil nil)
          while line
          do (when trim-space
               (setq line (util:trim-space line :both)))
             (if (zerop (length line))
                 (setq note nil)
                 (let ((mm (mismatch comment-start line)))
                   (cond ((null mm)
                          ;; Empty comment line.
                          (setq note nil))
                         ((zerop mm)
                          ;; Not a comment line.
                          (with-simple-restart
                              (continue "Ignore invalid entry.")
                            (add-entry set line note)))
                         ((cfg-bool attach-comments)
                          (let ((start (position-if-not #'util:whitespace-char-p
                                                        line
                                                        :start mm)))
                            (setq note (if start
                                           (subseq line start)
                                           nil)))))))))
  set)

(defclass string-set (lookup-table)
  ((entries
    ;; A simple hash table mapping strings to user supplied values.
    :type hash-table
    :initform (make-hash-table :test 'equal))))

(defmethod add-entry ((set string-set) (string string) &optional note)
  (setf (gethash string (lookup-table-entries set)) note))

(defmethod load-set ((type (eql 'usr:strings)) (list cons)
                     &rest keys)
  (apply #'populate-set
         (make-instance 'string-set :source nil)
         list
         keys))

(defmethod load-set ((type (eql 'usr:strings)) (path pathname) &rest keys)
  (apply #'populate-set
         (make-instance 'string-set :source path)
         path
         keys))

(defmethod contains? ((string string) (set string-set))
  (multiple-value-bind (value found)
      (gethash string (lookup-table-entries set))
    (values found value)))

(defclass cc-bin-set (lookup-table)
  ((entries
    ;; A two-level alist.  The first level keys are card number
    ;; lengths.  The second level maps prefix (BIN) length to a
    ;; hash-table.  In this table BINs (strings of digits) are mapped
    ;; to user supplied values (notes).
    :reader cc-bin-set-bins
    :type list
    :initform '())))

(defmethod add-entry ((set cc-bin-set) (pattern string) &optional note)
  (let* ((pattern (remove-if #'util:whitespace-char-p pattern))
         (length (length pattern))
         (end (position-if-not #'digit-char-p pattern)))
    (when (zerop end)
      (error "Invalid bin pattern: ~S" pattern))
    (with-slots (entries)
        set
      (let ((cell (assoc length entries)))
        (unless cell
          (setq cell (cons length '()))
          (push cell entries))
        (let* ((prefix (subseq pattern 0 end))
               (plength (length prefix))
               (table (cdr (assoc plength (cdr cell)))))
          (unless table
            (setq table (make-hash-table :test 'equal))
            (setf (cdr cell) (acons plength table (cdr cell))))
          (multiple-value-bind (value found)
              (gethash prefix table)
            (when found
              (warn "BIN ~A already in set~@[ (~A)~]" pattern value)))
          (msg :debug "Adding CC bin ~A/~D~@[: ~A~]" prefix length note)
          (setf (gethash prefix table) note))))))

(defmethod load-set ((type (eql 'usr:cc-bins)) (list cons) &rest keys)
  (apply #'populate-set
         (make-instance 'cc-bin-set :source nil)
         list
         keys))

(defmethod load-set ((type (eql 'usr:cc-bins)) (path pathname) &rest keys)
  (msg :info "Reading important CC bins from ~A" path)
  (let* ((result (apply #'populate-set
                        (make-instance 'cc-bin-set :source path)
                        path
                        keys))
         (count (loop for (nil . bins) in (lookup-table-entries result)
                       sum (loop for (nil . table) in bins
                                 sum (hash-table-count table)))))
    (msg :info "Read ~D bin~:P from ~A" count path)
    result))

(defmethod contains? ((digits string) (set cc-bin-set))
  (with-slots (entries)
      set
    (loop with bins = (cdr (assoc (length digits) entries))
          for (plength . table) in bins
          do (multiple-value-bind (note found)
                 (gethash (subseq digits 0 plength) table)
               (when found
                 (return (values found note)))))))

(defclass ipv4-network-set (lookup-table)
  ((entries
    ;; An alist mapping prefix-length to a hash-table.  Hash table key
    ;; is a prefix, and value is the user supplied note (can be NIL).
    ;;
    ;; Theoretically the networks should not overlap, but it seems a
    ;; good idea to allow it so that known finer-grained networks can
    ;; be reported, and the bigger prefixes left as a fall-back.
    ;; Entries are be stored (and therefore also checked) with the
    ;; longest prefixes first.
    :type list
    :initform '())))

(defmethod add-entry ((set ipv4-network-set) (network ip:ipv4-network)
                      &optional note)
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
      (multiple-value-bind (value found)
          (gethash bits table)
        (declare (ignore value))
        (when found
          (warn "Network ~S already present in ~S" network set)))
      (setf (gethash bits table) note)
      set)))

(defmethod add-entry ((set ipv4-network-set) (network string) &optional note)
  (add-entry set (ip:parse-address network :network) note))

(defmethod load-set ((type (eql 'usr:ipv4-networks)) (list cons)
                     &rest keys)
  (apply #'populate-set
         (make-instance 'ipv4-network-set :source nil)
         list
         keys))

(defmethod load-set ((type (eql 'usr:ipv4-networks)) (path pathname) &rest keys)
  (apply #'populate-set
         (make-instance 'ipv4-network-set :source path)
         path
         keys))

(defmethod contains? ((address ip:ip-address) (set ipv4-network-set))
  (loop with entries = (slot-value set 'entries)
        with address-bits = (ip:ipv4-address-bits address)
        for (prefix . table) of-type ((integer 1 32) . hash-table) in entries
        do (let ((bits (mask-field (byte prefix (- 32 prefix)) address-bits)))
             (multiple-value-bind (value found)
                 (gethash bits table)
               (when found
                 (return (values t value)))))))

(defmethod contains? ((artefact pastelyzer:ip-address) (set ipv4-network-set))
  (contains? (pastelyzer::artefact-address artefact) set))

(defclass super-domain-set (lookup-table)
  ((entries
    ;; A tree of hash tables.  Key is a domain label.  Value is either
    ;; another hash table, or a value to return for the entry (a note
    ;; or NIL).
    :initarg :entries
    :reader super-domain-set-entries
    :type hash-table
    :initform (make-hash-table :test 'equalp))))

(defun hashtree-add-path (tree path &optional value)
  (check-type tree hash-table)
  (check-type path list)
  (if (null (cdr path))
      (setf (gethash (car path) tree) value)
      (multiple-value-bind (table found)
          (gethash (car path) tree)
        (if found
            (if (typep table 'hash-table)
                (hashtree-add-path table (cdr path) value)
                (warn "Entry already present for ~S (~A); ignoring ~S (~A)"
                      (car path) table (cdr path) value))
            (let ((new (setf (gethash (car path) tree)
                             (make-hash-table :test 'equalp))))
              (hashtree-add-path new (cdr path) value))))))

(defun hashtree-present-p (tree path)
  (multiple-value-bind (value found)
      (gethash (car path) tree)
    (if found
        (cond ((not (typep value 'hash-table))
               ;; Terminating table — all sub-searches match.
               (values t value))
              ((null (cdr path))
               ;; Path too short.
               nil)
              (t
               (hashtree-present-p value (cdr path))))
        nil)))

(defmethod contains? ((domain string) (set super-domain-set))
  (hashtree-present-p (lookup-table-entries set)
                      (reverse (split-sequence #\. domain))))

(defmethod add-entry ((set super-domain-set) (value string) &optional note)
  (with-slots (entries)
      set
    (let ((labels (split-sequence #\. value :remove-empty-subseqs t)))
      (hashtree-add-path entries (reverse labels) note))))

(defmethod load-set ((type (eql 'usr:super-domains)) (list cons) &rest keys)
  (apply #'populate-set
         (make-instance 'super-domain-set
                        :source nil)
         list
         keys))

(defmethod load-set ((type (eql 'usr:super-domains)) (path pathname) &rest keys)
  (apply #'populate-set
         (make-instance 'super-domain-set :source path)
         path
         keys))

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
