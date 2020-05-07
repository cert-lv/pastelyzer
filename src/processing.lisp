(in-package #:pastelyzer)

(defgeneric fragment-body (fragment))

(defgeneric fragment-artefacts (fragment))

(defclass fragment ()
  ())

(defclass string-fragment (fragment)
  ((body
    :initarg :body
    :reader fragment-body
    :type string)))

(defclass binary-fragment (fragment)
  ((body
    :initarg :body
    :reader fragment-body
    :type (vector (unsigned-byte 8)))))

(defgeneric artefact-source (artefact)
  (:documentation
   "Representation of the ARTEFACT in the sequence this artefact has
  been detected in.  Usually a displaced array."))

(defgeneric artefact-parent (artefact)
  (:documentation
   "Where was this artefact found."))

(defgeneric artefact-source-seq (artefact)
  (:documentation
   "The sequence where the ARTEFACT has been detected."))

(defgeneric artefact-source-seq-start (artefact)
  (:documentation
   "Start position of the ARTEFACT in ARTEFACT-SOURCE-SEQ."))

(defgeneric artefact-source-seq-end (artefact)
  (:documentation
   "End position of the ARTEFACT in ARTEFACT-SOURCE-SEQ."))

(defgeneric artefact-source-seq-bounds (artefact)
  (:documentation
   "Start and end positions of ARTEFACT in ARTEFACT-SOURCE-SEQ as
   multiple values."))

(defgeneric artefact-description (artefact)
  (:documentation "A string representation of ARTEFACT."))

(defgeneric artefact-context-before (artefact &key limit bol trim-space))

(defgeneric artefact-context-after (artefact &key limit eol trim-space))

(defgeneric artefact-key (artefact)
  (:documentation
   "Calculate a unique key for ARTEFACT.  Used for comparing artefacts
  or as key function when storing artefacts in a hash table.  The
  comparison function is CL:EQUAL."))

(defclass artefact ()
  ((start
    :initarg :start
    :reader artefact-source-seq-start
    :type array-index)
   (end
    :initarg :end
    :reader artefact-source-seq-end
    :type array-index)
   (important
    :initarg :important
    :accessor important-artefact-p
    :type boolean
    :initform nil)
   (note
    :initarg :note
    :accessor artefact-note
    :type (or null string)
    :initform nil)))

(defmethod initialize-instance :after ((artefact artefact) &key)
  (unless (slot-boundp artefact 'start)
    (setf (slot-value artefact 'start) 0))
  (unless (slot-boundp artefact 'end)
    (setf (slot-value artefact 'end)
          (length (artefact-source-seq artefact)))))

(defmethod artefact-source-seq ((artefact artefact))
  (fragment-body (artefact-parent artefact)))

(defmethod artefact-source-seq-bounds ((artefact artefact))
  (with-slots (start end)
      artefact
    (values start end)))

(defmethod artefact-source ((artefact artefact))
  (multiple-value-bind (start end)
      (artefact-source-seq-bounds artefact)
    (dsubseq (artefact-source-seq artefact) start end)))

(defmethod artefact-key ((artefact artefact))
  (artefact-source artefact))

(defgeneric process (job))

(defgeneric analysable-parts (target job))

(defgeneric applicable-extractors (target job))

(defgeneric run-extractor (extractor target job))

(defgeneric extract-artefacts (target job)
  (:documentation
   "Convenience function that collects all artefacts from TARGET by calling
  RUN-EXTRACTOR on the extractors obtained by APPLICABLE-EXTRACTORS."))

(defgeneric add-artefact (job source class &key location &allow-other-keys))

(defmethod add-artefact ((job t) (source fragment) (class symbol)
                         &rest initargs)
  (let ((artefact (apply #'make-instance class :source source initargs)))
    (register-artefact job artefact source)))

(defmethod analysable-parts ((target artefact) (job t))
  "By default artefacts don't have any analysable parts."
  '())

(defmethod analysable-parts ((target paste) (job t))
  (if-let (content (paste-content target))
    (list content)
    '()))

(defgeneric postprocess (artefact job)
  (:method ((artefact t) (job t))
    nil))

(defmethod extract-artefacts ((target t) (job t))
  (loop for part in (analysable-parts target job)
        nconc (loop with done = nil
                    for extractor in (applicable-extractors part job)
                    append (multiple-value-bind (artefacts inhibit-next-p)
                               (run-extractor extractor part job)
                             (setq done inhibit-next-p)
                             ;; XXX: Until we have proper decorators.
                             (mapcan (lambda (artefact)
                                       (when artefact
                                         (list* artefact
                                                (postprocess artefact job))))
                                     artefacts))
                    until done)))

(defmethod process ((job job))
  (let ((queue (list (job-subject job)))
        (result '()))
    (loop for node = (pop queue)
          while node
          do (when (typep node 'artefact)
               (push node result))
             (let ((artefacts (extract-artefacts node job)))
               (setq queue (append queue artefacts))))
    result))

(defmethod run-extractor ((extractor t) (target t) (job t))
  (msg :debug "~S extractor not implemented for ~S/~S" extractor target job))

(defmethod analysable-parts ((target fragment) (job t))
  (list target))

;; XXX: [Maybe] temporary method until we update the callers of this
;; GF so that they deal properly with list items in result of
;; ANALYSABLE-PARTS.
(defmethod applicable-extractors ((part cons) (job t))
  (cdr part))

;; XXX: Temporary method until we update the callers of this GF so
;; that they deal properly with list results of ANALYSABLE-PARTS.
(defmethod run-extractor ((extractor t) (target cons) (job t))
  (assert (member extractor (cdr target))
          (target extractor)
          "Unexpected extractor ~S for ~S." extractor target)
  (run-extractor extractor (car target) job))

(defmethod applicable-extractors ((target binary-fragment) (job t))
  (unless (zerop (length (fragment-body target)))
    ;; TODO: These formats are not compatible, so we want to
    ;; short-cirtuct on the first extractor that returns a value.
    '(:check-gzip
      :check-zlib
      :check-string
      #+(or sbcl ccl) :check-utf-16le
      #+(or sbcl ccl) :check-utf-16be
      :check-deflate)))

(defmethod analysable-parts ((target circl-paste) (job t))
  ;; CIRCL pastes are known to be UTF-8 encoded.
  (mapcar (lambda (part) (list part :utf-8-string))
          (call-next-method)))

(defmethod run-extractor ((what (eql :utf-8-string))
                          (fragment binary-fragment)
                          (job t)
                          &aux (bytes (fragment-body fragment)))
  (if-let (string (bytes-to-string bytes :utf-8))
    (list (add-artefact job fragment 'encoded-string
                        :encoding :utf-8
                        :string (make-instance 'string-fragment
                                               :body string)))
    (warn "Cannot decode ~S as UTF-8" fragment)))

(defun is-binary-p (blob)
  (check-type blob (vector (unsigned-byte 8)))
  ;; XXX: This is very oversimplified.
  (loop with count = 0
        for x across blob
        do (case x
             ;; NUL, BEL, BS, SUB, ESC, DEL.
             ((0 7 8 26 27 127)
              (incf count)))
           (when (<= 3 count)
             (return-from is-binary-p t))))

(defclass binary-artefact (artefact)
  ((parent
    :initarg :source
    :reader artefact-parent
    :type binary-fragment)))

(defmethod artefact-description ((artefact binary-artefact))
  (format nil "~/fmt:bytes/" (artefact-source artefact)))

(defclass encoded-string (binary-artefact)
  ((encoding
    :initarg :encoding
    :reader encoded-string-encoding)
   (string
    :initarg :string
    :type string-fragment)))

(defmethod artefact-description ((artefact encoded-string))
  (format nil "~A, ~A"
          (encoded-string-encoding artefact)
          (fragment-body (slot-value artefact 'string))))

(defmethod analysable-parts ((target encoded-string) (job t))
  (list (slot-value target 'string)))

(defmethod run-extractor ((what (eql :check-string))
                          (fragment binary-fragment)
                          (job t)
                          &aux (data (fragment-body fragment)))
  (when-let ((string (and (not (is-binary-p data))
                          (bytes-to-string data :utf-8 nil))))
    (values
     (list (add-artefact job fragment 'encoded-string
                         :encoding :utf-8
                         :string (make-instance 'string-fragment
                                                :body string)))
     t)))

(defmethod run-extractor ((what (eql :check-utf-16le))
                          (fragment binary-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (let ((length (length target)))
    (when (and (evenp length)
               (loop for i from 1 below length by 2
                     always (zerop (aref target i))))
      (when-let ((string (bytes-to-string target :utf-16le)))
        (values
         (list (add-artefact job fragment 'encoded-string
                             :encoding :utf-16le
                             :string (make-instance 'string-fragment
                                                    :body string)))
         t)))))

(defmethod run-extractor ((what (eql :check-utf-16be))
                          (fragment binary-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (let ((length (length target)))
    (when (and (evenp length)
               (loop for i from 0 below length by 2
                     always (zerop (aref target i))))
      (when-let ((string (bytes-to-string target :utf-16be)))
        (values
         (list (add-artefact job fragment 'encoded-string
                             :encoding :utf-16be
                             :string (make-instance 'string-fragment
                                                    :body string)))
         t)))))

(defclass compressed-blob (binary-artefact)
  ((method
    :initarg :method
    :reader compressed-blob-method)
   (bytes
    :initarg :bytes
    :reader compressed-blob-bytes
    :type binary-fragment)))

(defmethod artefact-description ((artefact compressed-blob))
  (format nil "~A, ~A -> ~A bytes"
          (compressed-blob-method artefact)
          (length (artefact-source artefact))
          (length (fragment-body (compressed-blob-bytes artefact)))))

(defmethod analysable-parts ((artefact compressed-blob) (job t))
  (list (compressed-blob-bytes artefact)))

(defmethod run-extractor ((what (eql :check-gzip))
                          (fragment binary-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (when (and (< 2 (length target))
             (= (aref target 0) #x1f)
             (= (aref target 1) #x8b))
    (handler-case
        (values
         (list (add-artefact job fragment 'compressed-blob
                             :method :gzip
                             :bytes (make-instance 'binary-fragment
                                                   :body (gunzip target))))
         t)
      (error (condition)
        (msg :debug "Invalid gzip data: ~S (~A)" target condition)
        nil))))

(defmethod run-extractor ((what (eql :check-zlib))
                          (fragment binary-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (when (and (< 6 (length target))
             (= 8 (ldb (byte 4 0) (aref target 0)))
             (zerop (mod (+ (* (aref target 0) 256) (aref target 1)) 31)))
    (handler-case
        (values
         (list (add-artefact job fragment 'compressed-blob
                             :method :zlib
                             :bytes (make-instance 'binary-fragment
                                                   :body (inflate target))))
         t)
      (error (condition)
        (msg :debug "Invalid zlib data: ~S (~A)" target condition)
        nil))))

(defmethod run-extractor ((what (eql :check-deflate))
                          (fragment binary-fragment)
                          (job t))
  (let ((bytes (fragment-body fragment)))
    (handler-case
        (values
         (list (add-artefact job fragment 'compressed-blob
                             :method :deflate
                             :bytes (make-instance 'binary-fragment
                                                   :body (inflate bytes))))
         t)
      (chipz:decompression-error ()
        nil)
      (error (condition)
        (msg :fixme "In ~A: ~A" (job-subject job) condition)))))

(defclass string-artefact (artefact)
  ((parent
    :initarg :source
    :reader artefact-parent
    :type string-fragment)))

(defmethod artefact-description ((artefact string-artefact))
  (artefact-source artefact))

(defmethod print-object ((object string-artefact) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (multiple-value-bind (start end)
        (artefact-source-seq-bounds object)
      (format stream "[~D,~D] ~A" start end (artefact-description object)))))

(defmethod artefact-context-before ((artefact string-artefact)
                                    &rest keys
                                    &key limit bol trim-space)
  (declare (ignorable limit bol trim-space))
  (apply #'string-context-before
         (artefact-source-seq artefact)
         (artefact-source-seq-start artefact)
         keys))

(defmethod artefact-context-after ((artefact string-artefact)
                                   &rest keys
                                   &key limit eol trim-space)
  (declare (ignorable limit eol trim-space))
  (apply #'string-context-after
         (artefact-source-seq artefact)
         (artefact-source-seq-end artefact)
         keys))

(define-condition broken-fragment (condition)
  ((datum
    :initarg :datum
    :reader broken-fragment-datum)
   (locations
    :initarg :locations
    :reader broken-fragment-locations
    :initform '())))

(define-condition broken-utf-8 (broken-fragment)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "~S has broken UTF-8-encoded characters~@[ ~A~]."
                     (broken-fragment-datum condition)
                     (broken-fragment-locations condition)))))

(defmethod run-extractor ((what (eql :utf-8-string))
                          (fragment content)
                          (job t))
  ;; Theoretically this can be done with byte vector, but this is way
  ;; more convenient (at the moment).
  ;;
  ;; Notice that this method is specialised for :utf-8-string and
  ;; CONTENT, a combination only suggested when an instance of
  ;; CIRCL-PASTE is processed.
  (let ((artefacts (call-next-method)))
    (dolist (artefact artefacts)
      (when (and (typep artefact 'encoded-string)
                 (eq :utf-8 (encoded-string-encoding artefact)))
        (let ((target (fragment-body (slot-value artefact 'string)))
              (count 0)
              (positions '()))
          (ppcre:do-matches (start end "\\W�{2,}\\W" target)
            (incf count)
            (push start positions)
            (when (<= 3 count)
              (signal 'broken-utf-8
                      :datum target
                      :locations (nreverse positions))
              (return))))))
    artefacts))

(defmethod applicable-extractors ((target string-fragment) (job t))
  (unless (zerop (length (fragment-body target)))
    '(:encoded-blob
      :encoded-blob/0x
      :uri
      :domain
      :credential
      :IPv4-address
      :bank-card
      :m3u-entries
      :windows-internals)))

(defclass embedded-binary (string-artefact)
  ((bytes
    :initarg :bytes
    :reader embedded-binary-bytes
    :type binary-fragment)))

(defmethod analysable-parts ((blob embedded-binary) (job t))
  (list (embedded-binary-bytes blob)))

(defmethod print-object ((object embedded-binary) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (let* ((fragment (embedded-binary-bytes object))
           (bytes (fragment-body fragment)))
      (format stream "[~D byte~:P]" (length bytes)))))

(defmethod artefact-description ((artefact embedded-binary))
  (let* ((fragment (embedded-binary-bytes artefact))
         (bytes (fragment-body fragment))
         (length (length bytes)))
    (with-output-to-string (out)
      (format out "~D byte~:P " length)
      (if (<= length 16)
          (format out "(~/fmt:bytes/)" bytes)
          (format out "(~/fmt:bytes/..) SHA1: ~/fmt:bytes/"
                  (subseq bytes 0 4)
                  (ironclad:digest-sequence 'ironclad:sha1 bytes))))))

(defclass binary-blob (embedded-binary)
  ())

(defclass hex-blob (embedded-binary)
  ())

(defclass base64-blob (embedded-binary)
  ())

(defun base64-space-p (char)
  (declare (type character char))
  ;; Space is not really matched by our Base64 detection regex, but it
  ;; will not hurt.
  (case char
    ((#\linefeed #\return #\space) t)
    (otherwise nil)))

(defun decode/bin (string &optional ignore-whitespace
                          &aux (ndigits (length string)))
  (declare (type string string))
  (when ignore-whitespace
    (setf ndigits (- ndigits (count-if #'base64-space-p string))))
  (multiple-value-bind (nbytes extra)
      (truncate ndigits 8)
    (unless (zerop extra)
      (warn "Odd length (~D) for binary-encoded string: ~S"
            ndigits string))

    (loop with byte of-type (unsigned-byte 8) = 0
          with bit of-type (integer 0 7) = 7
          with result = (make-array nbytes :element-type '(unsigned-byte 8))
          with i of-type fixnum = 0
          for digit across string
          when (or (not ignore-whitespace)
                   (not (base64-space-p digit)))
            do (ecase digit
                 (#\1 (setf (ldb (byte 1 bit) byte) 1))
                 (#\0))
               (cond ((zerop bit)
                      (setf bit 7)
                      (shiftf (aref result i) byte 0)
                      (incf i))
                     (t
                      (decf bit)))
          finally (return result))))

(defun decode/hex (string &optional ignore-whitespace
                          &aux (ndigits (length string)))
  (declare (type string string))
  (when ignore-whitespace
    (setf ndigits (- ndigits (count-if #'base64-space-p string))))
  (multiple-value-bind (nbytes extra)
      (truncate ndigits 2)
    (unless (zerop extra)
      (warn "Odd length (~D) for hex-encoded string: ~S"
            ndigits string))

    (loop with byte of-type (unsigned-byte 8) = 0
          with result = (make-array nbytes :element-type '(unsigned-byte 8))
          with i of-type fixnum = 0
          with quad of-type (integer 0 1) = 1
          for digit across string
          when (or (not ignore-whitespace)
                   (not (base64-space-p digit)))
            do (let ((weight (digit-char-p digit 16)))
                 (cond (weight
                        (setf (ldb (byte 4 (* quad 4)) byte) weight))
                       (t
                        (error "Invalid hexadecimal digit: ~S" digit))))
               (when (zerop quad)
                 (shiftf (aref result i) byte 0)
                 (incf i))
               (setf quad (- 1 quad))
          finally (return result))))

(defun decode-by-alphabet (string)
  (declare (type string string))
  (flet ((give-up (condition)
           (declare (ignore condition))
           (return-from decode-by-alphabet (values nil :invalid))))
    (let ((base64-alphabet
            "+/0123456789=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
          (chars (remove-if #'base64-space-p (alphabet string))))
      (cond ((sub-alphabet-p chars "01")
             ;; Ignore boring cases: all zeroes or ones (although this
             ;; is not the right place to decide what is boring and
             ;; what isn't).
             (when (= 2 (length chars))
               (handler-bind ((simple-warning #'give-up))
                 (values (decode/bin string t) :bin))))

            ((sub-alphabet-p chars "0123456789")
             ;; Just a long number.
             nil)

            ((or (sub-alphabet-p chars "0123456789ABCDEF")
                 (sub-alphabet-p chars "0123456789abcdef"))
             (handler-bind ((simple-warning #'give-up))
               (values (decode/hex string t) :hex)))

            ((and (sub-alphabet-p chars base64-alphabet)
                  ;; Require at least some variety.
                  (<= 16 (length chars)))
             (handler-case
                 (values (base64:base64-string-to-usb8-array string) :base64)
               (error ()
                 (msg :debug "Broken Base64: ~A" (one-line string))
                 nil)))))))

(defmethod run-extractor ((what (eql :encoded-blob))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  ;; XXX: What we actually want is to incrementally try and
  ;; base64-decode until we get something useful.
  ;;
  ;; XXX: As an experiment could run a test on english dictionary to
  ;; see how many words are valid base64-encodings.
  (let ((regex (load-time-value
                (ppcre:create-scanner
                 '(:sequence
                   (:regex "[0-9A-Za-z+/]{32,}")
                   (:greedy-repetition 0 nil
                    (:sequence
                     (:alternation
                      (:sequence #\return #\linefeed) #\linefeed)
                     (:regex "[0-9A-Za-z+/]+")))
                   (:greedy-repetition 0 2 #\=)))))
        (result '()))
    (ppcre:do-matches (start end regex target)
      (when-let ((encoded (and (<= 72 (- end start))
                               (dsubseq target start end))))
        (multiple-value-bind (decoded kind)
            (decode-by-alphabet encoded)
          (when decoded
            (let ((class (ecase kind
                           (:bin 'binary-blob)
                           (:hex 'hex-blob)
                           (:base64 'base64-blob))))
              (push (add-artefact job fragment class
                                  :start start
                                  :end end
                                  :bytes (make-instance 'binary-fragment
                                                        :body decoded))
                    result))))))
    result))

(defmethod run-extractor ((what (eql :encoded-blob/0x))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex (load-time-value
                (ppcre:create-scanner
                 "0x[0-9a-f]{2}(?:,\\s*0x[0-9a-f]{2})+"
                 :case-insensitive-mode t)))
        (result '()))
    (ppcre:do-matches (ms me regex target)
      (let ((bytes (make-array (truncate (- me ms) 4)
                               :element-type '(unsigned-byte 8)
                               :adjustable t
                               :fill-pointer 0))
            (start ms))
        (loop
          (let ((position (search "0x" target :start2 start :end2 me)))
            (unless position
              (return))
            (vector-push-extend
             (parse-integer target :start (+ position 2)
                                   :end (+ position 4)
                                   :radix 16)
             bytes)
            (setq start (+ position 5))
            (when (<= me start)
              (return))))
        (when (<= 2 (fill-pointer bytes))
          (push (add-artefact job fragment 'hex-blob
                              :start ms
                              :end me
                              :bytes (make-instance 'binary-fragment
                                                    :body (copy-seq bytes)))
                result))))
    result))

(defclass m3u-entry (string-artefact)
  ())

(defmethod run-extractor ((what (eql :m3u-entries))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex (load-time-value
                (ppcre:create-scanner "^#EXTINF:(?:-1|0).*,(.*?)\\r?$"
                                      :multi-line-mode t)))
        (result '()))
    (ppcre:do-scans (start end reg-starts reg-ends regex target)
      (push (add-artefact job fragment 'm3u-entry
                          :start (aref reg-starts 0)
                          :end (aref reg-ends 0))
            result))
    result))

(defclass windows-internal (string-artefact)
  ())

(defmethod run-extractor ((what (eql :windows-internals))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex (load-time-value
                (ppcre:create-scanner
                 '(:sequence
                   :word-boundary
                   (:alternation
                    ;; "kernel32.dll"
                    ;; "msvcrt.dll"
                    ;; "rundll32.exe"
                    "CreateProcess"
                    "CreateRemoteThread"
                    "DefineDynamicAssembly"
                    ;; "DllImport"
                    "GetDelegateForFunctionPointer"
                    "GetModuleHandle"
                    "GetProcAddress"
                    "GetThreadContext"
                    "NtAllocateVirtualMemory"
                    "NtCreateUserProcess"
                    "NtGetContextThread"
                    "NtUnmapViewOfSection"
                    "NtWriteVirtualMemory"
                    "PointFunctionCall"
                    "QueueUserAPC"
                    "ResumeThread"
                    "SetThreadContext"
                    "VirtualAlloc"
                    "VirtualAllocEx"
                    "WriteProcessMemory")
                   :word-boundary))))
        (result '()))
    (ppcre:do-matches (start end regex target)
      (push (add-artefact job fragment 'windows-internal
                          :start start
                          :end end)
            result))
    result))

(defclass email (string-artefact)
  ())

(defclass credential (string-artefact)
  ((username
    :initarg :username
    :reader credential-username
    :type string)
   (passphrase
    :initarg :passphrase
    :reader credential-passphrase
    :type string)))

(defmethod artefact-key ((artefact credential))
  (cons (credential-username artefact)
        (credential-passphrase artefact)))

(defmethod run-extractor ((what (eql :credential))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex
          (load-time-value
           (ppcre:create-scanner
            '(:sequence
              (:greedy-repetition 0 1
               (:register (:alternation #\" #\' #\` #\< #\[ #\()))
              (:negative-lookbehind (:alternation #\~ #\!))
              :word-boundary
              (:register
               (:sequence
                (:regex "[A-Za-z0-9._%+-]{1,255}@")
                (:register
                 (:sequence
                  (:regex "[A-Za-z0-9-]{2,64}")
                  (:greedy-repetition 0 nil
                   (:sequence #\. (:regex "[A-Za-z0-9-]{2,64}")))))))
              :word-boundary
              ;; [Optional] password.
              (:greedy-repetition 0 1
               (:sequence
                ;; Separated from email with these.
                (:register (:alternation #\: #\; #\| #\tab #\,))
                (:register
                 ;; We allow all these characters in password.  No
                 ;; spaces, though.
                 (:regex
                  "[A-Za-z0-9_!:;@#$%^&*()\\\\/\\|\\<>\\[\\]{},.~'`\"+=?-]{3,}?"))
                ;; Running up to the next passphrase separator
                ;; character or end of line.
                (:alternation
                 (:back-reference 4)
                 #\space #\tab
                 ;; Ignore comma at the end.
                 (:sequence
                  (:regex "(?:,?\\s*)")
                  (:alternation #\return #\linefeed :end-anchor)))))))))
        (result '()))
    (ppcre:do-scans (start end reg-starts reg-ends regex target)
      (when (viable-domain-p target (aref reg-starts 2) (aref reg-ends 2))
        (cond ((aref reg-starts 4)
               (let ((pass-start (aref reg-starts 4))
                     (pass-end (aref reg-ends 4)))
                 (when-let ((qpos (aref reg-starts 0)))
                   ;; Handle quotation.
                   (let* ((start-quote (char target qpos))
                          (end-quote (case start-quote
                                       (#\< #\>)
                                       (#\[ #\])
                                       (#\( #\))
                                       (t start-quote))))
                     (let ((position (position end-quote target
                                               :start (1+ pass-start)
                                               :end pass-end)))
                       (when (and position
                                  (<= 4 (- pass-end pass-start)))
                         (setf pass-end position)))))

                 (push (add-artefact job fragment 'credential
                                     :start (aref reg-starts 1)
                                     :end pass-end
                                     :username (subseq target
                                                       (aref reg-starts 1)
                                                       (aref reg-ends 1))
                                     :passphrase (subseq target
                                                         (aref reg-starts 4)
                                                         pass-end))
                       result)))
              (t
               (push (add-artefact job fragment 'email
                                   :start (aref reg-starts 1)
                                   :end (aref reg-ends 1))
                     result)))))
    result))

(defclass ip-address (string-artefact)
  ((address
    :initarg :address
    :reader artefact-address
    :type ip:ip-address)))

(defmethod artefact-description ((artefact ip-address))
  (princ-to-string (artefact-address artefact)))

(defmethod print-object ((object ip-address) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (princ (artefact-address object) stream)))

(defmethod artefact-key ((artefact ip-address))
  (ip:ipv4-address-bits (artefact-address artefact)))

(defclass ip-service (ip-address)
  ((port
    :initarg :port
    :reader ip-service-port
    :type (unsigned-byte 16))))

(defmethod print-object ((object ip-service) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (address port) object
      (format stream "~A:~D" address port))))

(defmethod artefact-description ((artefact ip-service))
  (with-slots (address port) artefact
    (format nil "~A:~D" address port)))

(defmethod artefact-key ((artefact ip-service))
  (with-slots (address port) artefact
    (cons (ip:ipv4-address-bits address) port)))

(defclass resolved-ip-address (ip-address)
  ((note
    :initarg :domain
    :type string)))

(defmethod artefact-domain ((artefact resolved-ip-address))
  (slot-value artefact 'note))

(defmethod artefact-description ((artefact resolved-ip-address))
  (with-accessors ((address artefact-address)
                   (domain artefact-domain))
      artefact
    (format nil "~A (~A)" address domain)))

(defclass domain (string-artefact)
  ())

(defclass onion (domain)
  ())

(defparameter *known-file-extensions*
  (load-time-value
   (let ((table (make-hash-table :test 'equal))
         ;; XXX: Who knows when any of these become TLDs?
         (extensions '("apk" "asp" "aspx" "avi"
                       "bat" "bin"
                       "cfg" "cfm" "clj" "conf" "cpp" "crt" "css" "csv"
                       "dat" "db" "deb" "dex" "diff" "dll" "dmg" "doc" "docbook" "docx" "dot" "dtd"
                       "exe"
                       "gif" "gz"
                       "htm" "html"
                       "ico" "ics" "img" "inc" "ini"
                       "jar" "jpg" "jpeg" "js" "json" "jsp" "jsr" "jsx"
                       "lib" "lisp" "list" "log"
                       "mdb" "mk" "mkv" "mp3" "mp4" "mov" "mpg"
                       "nsf"
                       "ogg" "otf"
                       "patch" "pdf" "pem" "php" "php3" "png" "ppt" "pptx" "py"
                       "rar" "rb" "rtf"
                       "sass" "scss" "shtml" "sml" "sql" "svg" "swf"
                       "tar" "tgz" "tif" "tiff" "ttf" "txt"
                       "vb"
                       "wav" "webp" "wmv"
                       "xhtml" "xls" "xlsx" "xml" "xpi" "xsl"
                       "yml" "yaml"
                       "zip")))
     (dolist (ext extensions table)
       (setf (gethash ext table) t)))))

(defun known-file-extension-p (string)
  (values (gethash string *known-file-extensions*)))

(defun valid-domain-label-p (label &aux (length (length label)))
  (and (<= 1 length 63)
       (not (char= #\- (char label 0)))
       (not (char= #\- (char label (1- length))))
       (not (find #\_ label))))

;;; This is a hack, until we can better "classify" content.
(defun viable-domain-p (string start end &aux (length (length string)))
  (let ((tld-re (ppcre:create-scanner
                 "^(?:xn--[A-Za-z0-9]{2,60}|[A-Za-z]{2,64})$")))
    (flet ((accept (reason)
             (return-from viable-domain-p (values t reason)))
           (reject (reason)
             (return-from viable-domain-p (values nil reason)))
           (preceded-by (&rest prefixes)
             (dolist (prefix prefixes nil)
               (when (etypecase prefix
                       (character
                        (and (<= 1 start)
                             (char= prefix (char string (1- start)))))
                       (string
                        (let* ((len (length prefix))
                               (start2 (- start len)))
                          (and (<= 0 start2)
                               (null (mismatch prefix string
                                               :start2 start2
                                               :end2 start))))))
                 (return t))))
           (followed-by (&rest postfixes)
             (dolist (postfix postfixes nil)
               (when (etypecase postfix
                       (character
                        (and (< end length)
                             (char= postfix (char string end))))
                       (string
                        (let* ((len (length postfix))
                               (end2 (+ end len)))
                          (and (<= end2 length)
                               (null (mismatch postfix string
                                               :start2 end
                                               :end2 end2))))))
                 (return t)))))

      (let* ((labels (split-sequence #\. string :start start :end end))
             (rlabels (reverse labels)))
        (unless (and (<= (- end start) 253)
                     (<= 2 (length labels))
                     (every #'valid-domain-label-p rlabels)
                     (not (every #'digit-char-p (second rlabels)))
                     (<= 2 (length (second rlabels))))
          (reject :invalid-labels))

        (when (and (string-equal "onion" (first rlabels))
                   ;; Could also check for valid base32 encoding.
                   (or (= 16 (length (second rlabels)))
                       (= 56 (length (second rlabels))))
                   ;; Not sure which is better: (null (cddr rlabels))
                   ;; or this:
                   (eq (first labels) (second rlabels)))
          (accept :onion))

        (unless (ppcre:scan tld-re (first rlabels))
          (reject :invalid-tld))

        (when (string= "service" (first rlabels))
          (reject :service))

        (when (preceded-by "://")
          ;; Looks very much like a URL.
          (accept :url-like))

        (when (or (preceded-by "import " "package ")
                  (and (preceded-by "from ")
                       (followed-by " import")))
          (reject :import))

        ;; XXX: This stinks.
        (let ((first (string-downcase (first labels))))
          (when (or (string= "self" first)
                    (string= "this" first))
            (reject :self)))

        (when (known-file-extension-p (string-downcase (first rlabels)))
          ;; Looks like a file name.
          (reject :file-name))

        (when (and (preceded-by #\@)
                   ;; Special case Ruby ranges.
                   (not (preceded-by "..@")))
          (accept :email-like))

        (when (preceded-by #\\ #\/)
          (reject :path))

        (when (followed-by #\( " ()")
          (reject :function-call))

        (when (or (followed-by #\[)
                  (and (preceded-by #\[)
                       (followed-by #\])))
          (reject :array-subscript))

        (when (and (preceded-by #\( #\, "( " ", ")
                   (followed-by #\, #\) " ," " )"))
          (reject :function-parameter))

        (when (or (followed-by #\= #\+ #\* #\< #\> #\^ #\& #\?
                               " = " " + " " - " " / " " * " " < " " > " " ^ "
                               "!=" " != " "||" " || " " & ")
                  (preceded-by #\! #\= #\+ #\- #\* #\< #\> #\^ #\$ #\&
                               " = " " + " " - " " / " " * " " < " " > " " ^ "
                               "${" "!=" " != " "||" " || " " & "))
          (reject :expression))

        ;; No heuristics left.
        (values t :undecided)))))

(defmethod run-extractor ((what (eql :domain))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex
          (load-time-value
           (ppcre:create-scanner
            '(:sequence
              :word-boundary
              (:negative-lookbehind #\.)
              ;; We intentionally have a very lax syntax here so that
              ;; we match longer identifiers which might contain
              ;; domain-looking parts only by accident.  See
              ;; VIABLE-DOMAIN-P function.
              (:regex "[A-Za-z0-9-]{1,64}")
              (:greedy-repetition 1 nil
               (:sequence #\. (:regex "[A-Za-z0-9-]{1,64}")))
              (:negative-lookahead (:alternation #\; #\@))
              :word-boundary))))
        (result '()))
    (ppcre:do-matches (start end regex target)
      (let ((key (string-downcase (subseq target start end))))
        (multiple-value-bind (initargs seen)
            (if *seen-hostnames*
                (gethash key *seen-hostnames*)
                (values nil nil))
          (cond ((and seen initargs)
                 (push (apply #'add-artefact job fragment (first initargs)
                              :start start
                              :end end
                              (rest initargs))
                       result))

                (seen
                 ;; Something unworthy already seen.
                 )

                ((not (viable-domain-p target start end))
                 ;; This is how stuff becomes unworthy.
                 (when *seen-hostnames*
                   (setf (gethash key *seen-hostnames*) nil)))

                ((ends-with-subseq ".onion" key :test #'char-equal)
                 (when *seen-hostnames*
                   (setf (gethash key *seen-hostnames*)
                         (list 'onion)))
                 (push (add-artefact job fragment 'onion
                                     :start start
                                     :end end)
                       result))

                (t
                 (when *seen-hostnames*
                   (setf (gethash key *seen-hostnames*)
                         (list 'domain)))
                 (push (add-artefact job fragment 'domain
                                     :start start
                                     :end end)
                       result))))))
    result))

(defmethod postprocess ((artefact domain) (job t))
  ;; XXX: This really is a hack to only resolve domains that have not
  ;; been discarded by filters.
  (when (resolve-domains-p job)
    (let* ((domain (artefact-source artefact))
           (addresses (resolve-hostname domain))
           (fragment (artefact-parent artefact))
           (start (artefact-source-seq-start artefact))
           (end (artefact-source-seq-end artefact))
           (result '()))
      (loop for address in addresses
            do (push (add-artefact job fragment 'resolved-ip-address
                                   :address address
                                   :domain domain
                                   :start start
                                   :end end)
                     result))
      result)))

;;; URI extractor.
;;;
;;; https://tools.ietf.org/html/rfc3986#appendix-A
;;; https://tools.ietf.org/html/rfc2234

(defclass uri (string-artefact)
  ())

;;; unreserved  = ALPHA | DIGIT | "-" | "." | "_" | "~"
(ppcre:define-parse-tree-synonym unreserved
    (:regex "[a-z0-9-._~]"))

(ppcre:define-parse-tree-synonym unreserved-nd
    ;; Same as unreserved, sans period.
    (:regex "[a-z0-9-_~]"))

;;; pct-encoded = "%" HEXDIG HEXDIG
(ppcre:define-parse-tree-synonym pct-encoded
    (:regex "%[0-9a-f]{2}"))

;;; sub-delims  = "!" | "$" | "&" | "'" | "(" | ")"
;;;             | "*" | "+" | "," | ";" | "="
(ppcre:define-parse-tree-synonym sub-delim
    (:regex "[!$&'()*+,;=]"))

;;; pchar = unreserved | pct-encoded | sub-delims | ":" | "@"
(ppcre:define-parse-tree-synonym pchar
    (:alternation unreserved pct-encoded sub-delim #\: #\@))

;;; segment-nz    = 1*pchar
(ppcre:define-parse-tree-synonym segment-nz
    (:greedy-repetition 1 nil pchar))

;;; dec-octet = DIGIT                 ; 0-9
;;;           | %x31-39 DIGIT         ; 10-99
;;;           | "1" 2DIGIT            ; 100-199
;;;           | "2" %x30-34 DIGIT     ; 200-249
;;;           | "25" %x30-35          ; 250-255
(ppcre:define-parse-tree-synonym dec-octet
    (:alternation
     (:sequence #\2 #\5 (:regex "[0-5]"))
     (:sequence #\2 (:regex "[0-4]") :digit-class)
     (:sequence #\1 :digit-class :digit-class)
     (:sequence (:regex "[1-9]") :digit-class)
     :digit-class))

(ppcre:define-parse-tree-synonym IPv4address
    ;; NOTE: Not exactly like in the spec, because we want to allow
    ;; shortuct syntax like 127.1 and 127.0.1.
    (:alternation
     (:sequence dec-octet (:greedy-repetition 1 3 (:sequence #\. dec-octet)))
     ;; Theoretically this will fail for .onion links if the first 8
     ;; characters are digits.  Not sure this is even possible with
     ;; base32 encoding.
     (:sequence (:greedy-repetition 8 10 :digit-class)
                (:negative-lookahead #\.))))

(ppcre:define-parse-tree-synonym h16
    (:greedy-repetition 1 4 (:regex "[0-9a-f]")))

(ppcre:define-parse-tree-synonym ls32
    (:alternation (:sequence h16 #\: h16) IPv4address))

(ppcre:define-parse-tree-synonym IPv6address
    (:alternation
     ;;                            6( h16 ":" ) ls32
     (:sequence (:greedy-repetition 6 6 (:sequence h16 #\:)) ls32)
     ;;                       "::" 5( h16 ":" ) ls32
     (:sequence "::"
                (:greedy-repetition 5 5 (:sequence h16 #\:)) ls32)
     ;; [               h16 ] "::" 4( h16 ":" ) ls32
     (:sequence (:greedy-repetition 0 1 h16)
                "::"
                (:greedy-repetition 4 4 (:sequence h16 #\:)) ls32)
     ;; [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 1 (:sequence h16 #\:)) h16))
                "::"
                (:greedy-repetition 3 3 (:sequence h16 #\:)) ls32)
     ;; [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 2 (:sequence h16 #\:)) h16))
                "::"
                (:greedy-repetition 2 2 (:sequence h16 #\:)) ls32)
     ;; [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 3 (:sequence h16 #\:)) h16))
                "::" h16 #\: ls32)
     ;; [ *4( h16 ":" ) h16 ] "::"              ls32
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 4 (:sequence h16 #\:)) h16))
                "::" ls32)
     ;; [ *5( h16 ":" ) h16 ] "::"              h16
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 5 (:sequence h16 #\:)) h16))
                "::" h16)
     ;; [ *6( h16 ":" ) h16 ] "::"
     (:sequence (:greedy-repetition 0 1 (:sequence (:greedy-repetition 0 6 (:sequence h16 #\:)) h16))
                "::")))

(ppcre:define-parse-tree-synonym ip-literal
    ;; NOTE: Not including IPvFuture.
    (:sequence #\[ (:register IPv6address) #\]))

(ppcre:define-parse-tree-synonym reg-name
    ;; NOTE: Looks like RFC allows domains with escapes (pct-encoded),
    ;; and such domains work from browsers, but not in curl.  Could be
    ;; a simple obfuscation mechanism.  Not sure I've seen domains
    ;; containing sub-delims, but that's probably for non-DNS hosts...
    ;;
    ;; TODO: We want to add constraints on the parts
    ;; between periods, as in the domain regexp:
    ;;
    ;;  - Does not start with a dash, etc.
    ;;  - Length limited to 2-64 characters.
    ;;
    ;; NOTE: The spec says 1*, not 4*255.
    (:greedy-repetition 4 255 (:alternation unreserved pct-encoded sub-delim)))

(ppcre:define-parse-tree-synonym host
    ;; IPv6 address registered without square brackets.
    (:alternation ip-literal
                  (:register IPv4address)
                  (:register reg-name)))

(ppcre:define-parse-tree-synonym query
    ;; query = *( pchar | "/" | "?" )
    ;;
    ;; Apparently query can contain a question mark...
    (:greedy-repetition
     0 1 (:sequence #\?
                    (:register
                     (:greedy-repetition
                      0 nil (:alternation pchar #\/ #\?))))))

(ppcre:define-parse-tree-synonym fragment
    ;; fragment = *( pchar / "/" / "?" )
    (:greedy-repetition
     0 1 (:sequence #\#
                    (:register
                     (:greedy-repetition
                      0 nil (:alternation pchar #\/ #\?))))))

(ppcre:define-parse-tree-synonym uri
    (:sequence
     (:regex "\\b(?:https?|hxxps?|ftp)://")
     ;; authority = [ userinfo "@" ] host [ ":" port ]
     ;; userinfo  = *( unreserved | pct-encoded | sub-delims | ":" )
     (:greedy-repetition
      0 1 (:sequence
           (:register
            (:greedy-repetition
             1 nil (:alternation unreserved pct-encoded sub-delim)))
           (:greedy-repetition
            0 1 (:sequence
                 #\:
                 (:register
                  (:greedy-repetition
                   1 nil (:alternation unreserved pct-encoded sub-delim)))))
           #\@))
     ;; host     = IP-literal | IPv4address | reg-name
     ;; reg-name = *( unreserved | pct-encoded | sub-delims )
     ;;
     host
     ;; port = *DIGIT
     ;;
     ;; XXX: According to the spec it seems digits after the
     ;; colon are optional.  Another obfuscation method to
     ;; hide valid URLs in plain sight from simplistic
     ;; regular-expression based extractors.
     (:greedy-repetition 0 1 (:sequence #\: (:register (:regex "[0-9]*"))))
     ;; path-abempty  = *( "/" segment )
     ;; path-absolute = "/" [ segment-nz *( "/" segment ) ]
     ;; path-rootless = segment-nz *( "/" segment )
     ;; path-empty    = 0<pchar>
     ;;
     ;; Note: path-rootless probably does not apply to web
     ;; URLs, and at least FireFox does not like it.  Not
     ;; sure what's the meaning of path-empty in the presence
     ;; of path-abempty.
     ;;
     ;; We use an optional path-absolute here.  We also allow
     ;; consecutive slashes, even the ones after authority
     ;; part (the above rules preclude this).
     (:register
      (:greedy-repetition
       0 nil (:sequence #\/
                        (:greedy-repetition
                         0 1 (:sequence segment-nz
                                        (:greedy-repetition
                                         0 nil (:sequence #\/ segment-nz))))
                        query
                        fragment)))))

(defmethod run-extractor ((what (eql :uri))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex (load-time-value
                (ppcre:create-scanner 'uri :case-insensitive-mode t)))
        (result '()))
    (ppcre:do-scans (start end rs re regex target)
      ;; 0    1    2    3    4      5    6    7     8
      ;; user pass IPv6 IPv4 domain port path query fragment
      (when (or (aref rs 2)
                (aref rs 3)
                (and (aref rs 4)
                     (viable-domain-p target (aref rs 4) (aref re 4))))
        (push (add-artefact job fragment 'uri :start start :end end)
              result))
      ;; XXX: The following are actually sub-artefacts of URI, not
      ;; TARGET.
      (when (and (aref rs 0)
                 (aref rs 1))
        (push (add-artefact job fragment 'credential
                            :username (subseq target (aref rs 0) (aref re 0))
                            :passphrase (subseq target (aref rs 1) (aref re 1))
                            :start (aref rs 0)
                            :end (aref re 1))
              result))
      ;; TODO: When domain resolution is decoupled from :domain
      ;; extractor.  This should also take precedence over :domain
      ;; extractor.  Also be careful with the class of the artefact in
      ;; case of .onion TLD.
      #-(and)
      (add-artefact job fragment 'domain
                    :start (aref rs 3)
                    :end (aref re 3)))
    result))

(defmethod run-extractor ((what (eql :IPv4-address))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex
          (load-time-value
           (ppcre:create-scanner
            (concatenate 'string
                         "\\b(?<![.-])"
                         "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
                         "(?![/.-])\\b"))))
        (result '()))
    (ppcre:do-matches (start end regex target)
      (handler-case
          (let ((address (ip:parse-address (subseq target start end) :address))
                (port nil))
            (if (and (<= (+ end 2) (length target))
                     (char= #\: (schar target end))
                     (digit-char-p (schar target (1+ end)))
                     (multiple-value-bind (n pos)
                       (parse-integer target :start (1+ end) :junk-allowed t)
                       (when (and n (<= 1 n #xFFFF))
                         (setq port n end pos))))
                (push (add-artefact job fragment 'ip-service
                                    :start start
                                    :end end
                                    :address address
                                    :port port)
                      result)
                (push (add-artefact job fragment 'ip-address
                                    :start start
                                    :end end
                                    :address address)
                      result)))
        (error (condition)
          (msg :debug "In ~A: ~A" (job-subject job) condition))))
    result))

(defclass bank-card-number (string-artefact)
  ((digits
    :initarg :digits
    :reader bank-card-number-digits
    :type simple-string)))

(defmethod artefact-description ((artefact bank-card-number))
  (format nil "~A~@[: ~A~]"
          (call-next-method)
          (slot-value artefact 'note)))

(defmethod artefact-key ((artefact bank-card-number))
  (bank-card-number-digits artefact))

(defun luhn-checksum-valid-p (digits &aux (length (length digits)))
  ;; https://en.wikipedia.org/wiki/Luhn_algorithm
  (declare (type simple-string digits)
           (optimize (speed 3) (safety 1)))
  (let ((sum (loop for set of-type bit = (if (oddp length) 0 1) then (- 1 set)
                   for i from 0 below length
                   sum (let ((digit (digit-char-p (schar digits i))))
                         (declare (type (integer 0 9) digit))
                         (if (zerop set)
                             digit
                             (let ((digit (* digit 2)))
                               (if (<= digit 9)
                                   digit
                                   (- digit 9)))))
                   ;; The declaration limits our input string length to
                   ;; around ~56-bit integer.  We will revisit this when
                   ;; we have computers with 7PB memory and each such
                   ;; computer is used to store one bank card number.
                     of-type (unsigned-byte 60))))
    (zerop (rem sum 10))))

(defun all-same-p (sequence &key ((:test test-fn) #'eql)
                                 ((:key key-fn) #'identity))
  (declare (type sequence sequence)
           (type (or function symbol) test-fn key-fn))
  (etypecase sequence
    (list
     (cond ((or (null sequence)
                (null (cdr sequence))))
           (t
            (not (find (car sequence)
                       (cdr sequence)
                       :test-not test-fn
                       :key key-fn)))))
    (vector
     (cond ((<= (length sequence) 1))
           (t
            (not (find (aref sequence 0)
                       sequence
                       :test-not test-fn
                       :key key-fn)))))))

(defmethod run-extractor ((what (eql :bank-card))
                          (fragment string-fragment)
                          (job t)
                          &aux (target (fragment-body fragment)))
  (declare (type string target))
  (let ((regex
          (load-time-value
           (ppcre:create-scanner
            '(:sequence
              (:negative-lookbehind (:sequence :digit-class #\.))
              :word-boundary
              (:alternation
               (:register (:regex "[1-9]\\d{13,15}"))

               (:sequence
                (:register (:regex "[1-9]\\d{3}"))
                (:register (:alternation #\- (:greedy-repetition 1 2 #\space)))
                (:alternation
                 ;; Ordinary 16 digit number, 4 groups of 4 digits.
                 (:sequence
                  (:register (:greedy-repetition 4 4 :digit-class))
                  (:back-reference 3)
                  (:register (:greedy-repetition 4 4 :digit-class))
                  (:back-reference 3)
                  (:register (:greedy-repetition 4 4 :digit-class)))

                 ;; American express, grouped 4-6-5.
                 (:sequence
                  (:register (:greedy-repetition 6 6 :digit-class))
                  (:back-reference 3)
                  (:register (:greedy-repetition 5 5 :digit-class))))))
              :word-boundary
              (:negative-lookahead (:sequence #\. :digit-class))))))
        (result '()))
    (ppcre:do-scans (start end rs re regex target)
      (flet ((collect (digits)
               (declare (type simple-string digits))
               (when (and (not (all-same-p digits))
                          (luhn-checksum-valid-p digits))
                 (push (add-artefact job fragment 'bank-card-number
                                     :digits digits
                                     :start start
                                     :end end)
                       result))))
        (cond ((aref rs 0)
               (collect (subseq target start end)))

              ((aref rs 3)
               (collect
                   (concatenate 'string
                                (subseq target (aref rs 1) (aref re 1))
                                (subseq target (aref rs 3) (aref re 3))
                                (subseq target (aref rs 4) (aref re 4))
                                (subseq target (aref rs 5) (aref re 5)))))
              ((aref rs 6)
               (collect
                   (concatenate 'string
                                (subseq target (aref rs 1) (aref re 1))
                                (subseq target (aref rs 6) (aref re 6))
                                (subseq target (aref rs 7) (aref re 7))))))))

    result))
