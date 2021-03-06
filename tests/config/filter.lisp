(defpackage #:pastelyzer.tests.config.filter
  (:use #:common-lisp #:2am)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter)
                    (#:cmd #:pastelyzer.config.cmd)
                    (#:cfg.util #:pastelyzer.config.util)
                    (#:ctx #:pastelyzer.config.context))
  (:export #:tests))

(in-package #:pastelyzer.tests.config.filter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-user-symbols (tree)
    (sublis '((or . usr:or)
              (and . usr:and)
              (not . usr:not)
              (= . usr:=)
              (> . usr:>)
              (< . usr:<)
              (type? . usr:type?)
              (exact-type? . usr:exact-type?)
              (extract . usr:extract)
              (-> . usr:->)
              (length . usr:length)
              (starts-with? . usr:starts-with?)
              (ends-with? . usr:ends-with?)
              (contains? . usr:contains?)
              (mixed-case? . usr:mixed-case?)
              (member? . usr:member?)
              (note . usr:note)
              (important . usr:important)
              (^ . usr:^)
              (store-tmpfile . usr:store-tmpfile)

              (define-set . usr:define-set)
              (ipv4-networks . usr:ipv4-networks)
              (super-domains . usr:super-domains)
              (cc-bins . usr:cc-bins)
              (strings . usr:strings)
              (collect-into . usr:collect-into)
              (discard . usr:discard)
              (set-important . usr:set-important)
              (set-note . usr:set-note))
            tree)))

(defmacro with-filter (var filter &body body)
  (let ((filter (translate-user-symbols filter)))
    `(let ((.filter.
             (filter::parse-filter 'test-filter ',filter)))
       (declare (type function .filter.))
       (flet ((,var (value)
                (funcall .filter. value)))
         ,@body))))

(defmacro config-test (test-name test-parameters &body body)
  `(test ,test-name ,test-parameters
     (let ((sink::*known-configurations* '())
           (filter::*filters* '()))
       ,@(loop with .usr. = (pastelyzer.config.package:user-package)
               for form in body
               collect (if (consp form)
                           (multiple-value-bind (symbol status)
                               (find-symbol (symbol-name (first form))
                                            .usr.)
                             (if (eq :external status)
                                 `(pastelyzer.config.loader::apply-directive
                                   ',symbol
                                   ',(translate-user-symbols (rest form)))
                                 form))
                           form)))))

(defclass test-job (ctx:configurable-job)
  ())

(defun filter-artefacts-by-class (artefacts types)
  (if types
      (let ((classes (mapcar #'find-class types)))
        (loop for item in artefacts
              when (find (class-of item) classes)
                collect item))
      artefacts))

;;; XXX: This is basically the same as PASTELYZER.TESTS::EXTRACT-ARTEFACTS,
;;; except here our TEST-JOB class is different.
(defun extract-artefacts (subject &rest types)
  (let* ((fragment (typecase subject
                     (string
                      (make-instance 'pastelyzer:string-fragment :body subject))
                     ((vector (unsigned-byte 8))
                      (make-instance 'pastelyzer:binary-fragment :body subject))
                     (otherwise
                      subject)))
         (job (make-instance 'test-job :subject fragment))
         (result (ctx:job-artefacts (pastelyzer:process job))))
    (values (filter-artefacts-by-class result types)
            job)))

(defclass test-sink-prototype (sink::prototype)
  ())

(defmethod sink:get-prototype ((name (eql 'test-sink)))
  (make-instance 'test-sink-prototype))

(defmethod sink:finish-sink ((proto test-sink-prototype) (sink sink:sink))
  ;; Nothing special to do.
  sink)

(defun collected-artefacts (job sink &rest types)
  (filter-artefacts-by-class (sink:sink-artefacts (ctx::get-sink job sink))
                                types))

(suite 'tests)

(test filter.= ()
  (with-filter f (= 42)
    (is (eq 't (f 42)))
    (is (eq 'nil (f 0)))))

(test filter.< ()
  (with-filter f (< 42)
    (is (eq 't (f 0)))
    (is (eq 'nil (f 100)))))

(test filter.> ()
  (with-filter f (> 42)
    (is (eq 't (f 100)))
    (is (eq 'nil (f 0)))))

(test filter.not ()
  (with-filter f (not (= 42))
    (is (eq 'nil (f 42)))
    (is (eq 't (f 0)))))

(test filter.and.0 ()
  (with-filter f (and)
    (is (eql 42 (f 42)))))

(test filter.and ()
  (with-filter f (and (> 40) (< 50))
    (is (eql 42 (f 42)))
    (is (eq 'nil (f 40)))
    (is (eq 'nil (f 50)))))

(test filter.or.0 ()
  (with-filter f (or)
    (is (eq 'nil (f 42)))))

(test filter.or ()
  (with-filter f (or (< 42) (= 42))
    (is (eq 't (f 0)))
    (is (eq 't (f 42)))
    (is (eq 'nil (f 50)))))

(test filter.type? ()
  (with-filter f (type? cl:string)
    (is (eq 't (f "foo")))
    (is (eq 'nil (f 42)))))

(test filter.exact-type? ()
  (with-filter f (exact-type? fixnum)
    (is (eq 't (f 42)))
    (is (eq 'nil (f (1+ most-positive-fixnum))))))

(test filter.length ()
  (with-filter f (length)
    (is (= 3 (f "foo")))
    (is (= 3 (f #(1 2 3))))))

(defun string-fragment (characters)
  (make-instance 'pastelyzer:string-fragment
                 :body (coerce characters 'string)))

(defun byte-array (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun binary-fragment (bytes)
  (make-instance 'pastelyzer:binary-fragment
                 :body (byte-array bytes)))

(defmacro test-string-filter (filter exp chars)
  `(progn
     (is (eq ,exp (,filter ',(coerce chars 'list))))
     (is (eq ,exp (,filter ,(coerce chars 'string))))
     (is (eq ,exp (,filter (string-fragment ,chars))))))

(defmacro test-binary-filter (filter exp bytes)
  `(progn
     (is (eq ,exp (,filter ',(coerce bytes 'list))))
     (is (eq ,exp (,filter ,(coerce bytes 'vector))))
     (is (eq ,exp (,filter (byte-array ,bytes))))
     (is (eq ,exp (,filter (binary-fragment ,bytes))))))

(test filter.starts-with? ()
  (with-filter f (starts-with? "foo")
    (test-string-filter f t "foo")
    (test-string-filter f t "foo bar")
    (test-string-filter f nil "")
    (test-string-filter f nil "fo")
    (test-string-filter f nil "bafoo")))

(test filter.starts-with?.binary ()
  (with-filter f (starts-with? #(#x4d #x5a))
    (test-binary-filter f t #(77 90 144))
    (test-binary-filter f nil #())
    (test-binary-filter f nil #(1 2 3))))

(test filter.ends-with? ()
  (with-filter f (ends-with? "bar")
    (test-string-filter f t "bar")
    (test-string-filter f t "fubar")
    (test-string-filter f nil "")
    (test-string-filter f nil "ar")
    (test-string-filter f nil "barfu")))

(test filter.ends-with?.binary ()
  (with-filter f (ends-with? #(#x5a #x90))
    (test-binary-filter f t #(77 90 144))
    (test-binary-filter f nil #())
    (test-binary-filter f nil #(1 2 3))))

(test filter.contains? ()
  (with-filter f (contains? "bar")
    (test-string-filter f t "bar")
    (test-string-filter f t "fubar")
    (test-string-filter f t "barfu")
    (test-string-filter f t "fubarz")
    (test-string-filter f nil "")
    (test-string-filter f nil "b ar")))

(test filter.mixed-case? ()
  (with-filter f (mixed-case?)
    (is (f "FooBar"))
    (is (not (f "foo-bar")))
    (is (not (f "FUBAR")))))

(test filter.-> ()
  (with-filter f (-> (length) (= 3))
    (is (eq 't (f "foo")))
    (is (eq 't (f '(1 2 3))))
    (is (eq 't (f #(1 2 3))))))

(test filter.composite.1 ()
  (with-filter f (and (type? cl:string)
                      (-> (length) (> 3)))
    (is (eq #1="fubar" (f #1#)))
    (is (eq 'nil (f "foo")))
    (is (eq 'nil (f (coerce "fubar" 'list))))))

(test filter.composite.2 ()
  (with-filter f (or (and (type? cl:string)
                          (starts-with? "fu"))
                     (and (type? cl:list)
                          (starts-with? #(#x4d #x5a))))
    (is (eq #1="fubar" (f #1#)))
    (is (eq 'nil (f "foo")))
    (is (eq #2='(77 90 144) (f #2#)))
    (is (eq 'nil (f 42)))))

(config-test ipv4-network-set.1 ()
  (define-set private-ip-addresses (ipv4-networks)
    :entries ("192.168.0.0/16" "172.16.0.0/12" "10.0.0.0/8"))

  (with-filter f (member? private-ip-addresses)
    (is (eq 't (f (ip:parse-address "10.0.42.1"))))
    (is (eq 'nil (f (ip:parse-address "127.0.0.1"))))))

(config-test super-domains.1 ()
  (define-set tlds (super-domains)
    :entries ("com" "org"))

  (with-filter f (member? tlds)
    (is (eq 't (f "example.com")))
    (is (eq 't (f "example.org")))
    (is (eq 'nil (f "example.lv")))))

(config-test super-domains.2 ()
  (define-set tlds (super-domains)
    :entries ("example.com"))

  (with-filter f (member? tlds)
    (is (eq 't (f "example.com")))
    (is (eq 't (f "test.example.com")))
    (is (eq 'nil (f "xexample.com")))
    (is (eq 'nil (f "ample.com")))))

(config-test super-domains.3 ()
  (define-set tlds (super-domains)
    :entries ("example.com"))

  (with-filter f (and (mixed-case?)
                      (not (member? tlds)))
    (is (f "XXX.com"))
    (is (not (f "example.com")))
    (is (not (f "Example.Com")))))

(config-test ipv4-networks.1 ()
  (define-set networks (ipv4-networks)
    :entries ("10.42.0.0/16" "10.42.42.0/24"))

  (with-filter f (member? networks)
    (is (eq 't (f (ip:parse-address "10.42.0.1"))))
    (is (eq 't (f (ip:parse-address "10.42.0.255"))))
    (is (eq 't (f (ip:parse-address "10.42.41.1"))))
    (is (eq 't (f (ip:parse-address "10.42.42.1"))))
    (is (eq 't (f (ip:parse-address "10.42.43.1"))))

    (is (eq nil (f (ip:parse-address "10.41.0.1"))))
    (is (eq nil (f (ip:parse-address "10.43.0.1"))))))

(config-test user-note.ip ()
  (define-set test-network (ipv4-networks)
    :entries (("10.42.0.0/16" "Test network")))

  (define-artefact-filter test
      (and (type? pastelyzer:ip-address)
           (not (member? test-network)))
    (discard))

  (let* ((content "10.42.10.42 is in test network; 10.0.0.1 is not")
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (let ((artefact (first artefacts)))
      (is (string= "10.42.10.42" (pastelyzer:artefact-source artefact)))
      (is (string= "Test network" (pastelyzer:artefact-note artefact))))))

(config-test user-note.domain ()
  (define-set test-domain (super-domains)
    :entries (("abc.test" "Test domain")))

  (define-artefact-filter test
      (and (type? pastelyzer:domain)
           (not (member? test-domain)))
    (discard))

  (let* ((content "Testing abc.test and xxx.test")
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (let ((artefact (first artefacts)))
      (is (string= "abc.test" (pastelyzer:artefact-source artefact)))
      (is (string= "Test domain" (pastelyzer:artefact-note artefact))))))

(config-test user-note.cc-bin ()
  (define-set test-bin (cc-bins)
    :entries (("4242xxxxxxxxxxxx" "Test BIN")))

  (define-artefact-filter test
      (and (type? pastelyzer:bank-card-number)
           (not (member? test-bin)))
    (discard))

  (let* ((content "To 1111222233334444 or to 4242424242424242?")
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (let ((artefact (first artefacts)))
      (is (string= "4242424242424242"
                   (pastelyzer:bank-card-number-digits artefact)))
      (is (string= "Test BIN" (pastelyzer:artefact-note artefact))))))

(config-test user-note.string ()
  (define-set watchlist (strings)
    :entries (("WriteProcessMemory" "Really important")))

  (define-artefact-filter test
      (and (type? pastelyzer:windows-internal)
           (not (member? watchlist)))
    (discard))

  (let* ((content "We care about WriteProcessMemory, not CreateRemoteThread.")
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (let ((artefact (first artefacts)))
      (is (string= "WriteProcessMemory" (pastelyzer:artefact-source artefact)))
      (is (string= "Really important" (pastelyzer:artefact-note artefact))))))

(config-test important.1 ()
  (define-set networks (ipv4-networks)
    :entries ("10.42.0.0/16"))
  (define-set domains (super-domains)
    :entries ("example.com"))

  (define-artefact-filter important-network
      (and (type? pastelyzer:ip-address)
           (member? networks))
    (set-important))

  (define-artefact-filter important-domain
      (and (type? pastelyzer:domain)
           (member? domains))
    (set-important))

  (let* ((content (format nil "~
          Assuming test.example.com domain resolves to two IP addresses: ~
          10.1.0.1 and 10.42.0.1, and another.domain.com also resolves to ~
          two IP addresses: 10.2.0.42 and 10.42.10.42, this string should ~
          contain 6 artefacts, 3 of which should be marked important."))
         (artefacts (extract-artefacts content)))
    (is (= 6 (length artefacts)))
    (is (= 3 (count-if #'pastelyzer:important-artefact-p artefacts)))))

(config-test discard.1 ()
  (define-sink fake-stuff (test-sink))

  ;; Collect before discarding.
  (define-artefact-filter fake-news-outlet
      (= "fake.news.test")
    (collect-into fake-stuff))

  (define-artefact-filter fake-news
      (starts-with? "fake")
    (discard "Fake news"))

  (let ((content "Discard fake.news.test, register only real.news.test"))
    (multiple-value-bind (artefacts job)
        (extract-artefacts content)
      (is (= 1 (length artefacts)))
      (is (string= "real.news.test"
                   (pastelyzer:artefact-source (first artefacts))))

      (let ((fake (collected-artefacts job 'fake-stuff)))
        (is (= 1 (length fake)))
        (is (string= "fake.news.test"
                     (pastelyzer:artefact-source (first fake))))))))

(config-test context.1 ()
  (define-artefact-filter ignore-inline-blob
      (and (type? pastelyzer:base64-blob)
           (-> (extract context-before)
               (ends-with? ";base64,")))
    (discard))

  (let* ((bytes (coerce (loop for i from 0 below 54 collect i)
                        '(vector (unsigned-byte 8))))
         (a (base64:usb8-array-to-base64-string bytes))
         (b (base64:usb8-array-to-base64-string (reverse bytes)))
         (content (format nil "data:image/jpeg;base64,~A and ~A" b a))
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (is (equalp bytes
                (pastelyzer:fragment-body
                 (pastelyzer:embedded-binary-bytes (first artefacts)))))))

(config-test context.2 ()
  (define-artefact-filter ignore-keybase-proof
      (and (type? pastelyzer:base64-blob)
           (-> (extract source-context)
               (starts-with? "### Keybase proof")))
    (set-note "keybase proof"))

  (let* ((bytes (coerce (loop for i from 0 below 48 collect i)
                        '(vector (unsigned-byte 8))))
         (content (format nil "### Keybase proof~%~@
                   I hereby claim whatever~%~@
                   -----BEGIN PGP MESSAGE-----~%~%~A~%~A"
                          (base64:usb8-array-to-base64-string bytes)
                          (base64:usb8-array-to-base64-string (reverse bytes))))
         (artefacts (extract-artefacts content)))
    (is (= 1 (length artefacts)))
    (is (string= "keybase proof"
                 (pastelyzer:artefact-note (first artefacts))))))

#+sbcl
(config-test cmd.1 ()
  (define-process-filter add-note
      (-> (extract stdout)
          (^ ^)
          (not (or (= "") (= "data"))))
    (set-note ^))

  (define-sink file (usr:cmd-sink)
    (:command "file" "-b" "-")
    (:stdin (extract bytes))
    (:stdout :collect-string)
    (:action add-note))

  (define-artefact-filter test
      (type? pastelyzer:embedded-binary)
    (collect-into file))

  (let* ((content (format nil "H4sIAAAAAAAAAytILC5JzamsSlUoycgsVlRQSM~
                               xLATJT8xSK83NTFXLzi1L1uACZ2lphJQAAAA=="))
         (artefacts (extract-artefacts content)))
    (is (some (lambda (artefact)
                (pastelyzer.util:starts-with-subseq
                 "gzip compressed data"
                 (pastelyzer:artefact-note artefact)))
              artefacts))))

#+sbcl
(config-test cmd.2 ()
  (define-process-filter add-note
      (-> (extract stdout)
          (^ ^)
          (not (or (= "") (= "data"))))
    (set-note ^))

  (define-sink file (usr:cmd-sink)
    (:command "file" "-b" (-> (extract bytes) (store-tmpfile)))
    (:stdout :collect-string)
    (:action add-note))

  (define-artefact-filter test
      (type? pastelyzer:embedded-binary)
    (collect-into file))

  (let* ((content (format nil "H4sIAAAAAAAAAytILC5JzamsSlUoycgsVlRQSM~
                               xLATJT8xSK83NTFXLzi1L1uACZ2lphJQAAAA=="))
         (artefacts (extract-artefacts content)))
    (is (some (lambda (artefact)
                (search "gzip compressed data"
                        (pastelyzer:artefact-note artefact)))
              artefacts))))

#+sbcl
(config-test cmd.3 ()
  (define-process-filter add-note
      (-> (extract stdout)
          (and (starts-with? "abcdef-")
               (^)))
    (set-note ^))

  (define-sink basename (usr:cmd-sink)
    (:command "basename" (store-tmpfile "abcdef-"))
    (:stdout :collect-string)
    (:action add-note))

  ;; XXX: A synthetic artefact to attach output from our command to.
  (define-artefact-filter origin-test
      (= "artefact.test")
    (collect-into basename))

  (let* ((string "aaa artefact.test zzz")
         (artefacts (extract-artefacts string)))
    (is (= 1 (length artefacts)))
    (is (pastelyzer.util:starts-with-subseq
         "abcdef-" (pastelyzer:artefact-note (first artefacts))))))

(defun string-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

#+sbcl
(config-test cmd.env.1 ()
  (define-process-filter add-note
      (-> (extract stdout)
          (^ environment))
    (set-note environment))

  (define-sink env (usr:cmd-sink)
    (:command "env")
    (:environment ("EXTRA_VAR_1" "value-1"))
    (:stdout :collect-string)
    (:action add-note))

  ;; XXX: A synthetic artefact to attach output from our command to.
  (define-artefact-filter origin-test
      (= "artefact.test")
    (collect-into env))

  (let* ((string "aaa artefact.test zzz")
         (artefacts (extract-artefacts string)))
    (is (= 1 (length artefacts)))
    (let* ((env (pastelyzer:artefact-note (first artefacts)))
           (lines (string-lines env)))
      (is (= 2 (length lines)))
      (is (some (lambda (line)
                  (pastelyzer.util:starts-with-subseq
                   "PASTELYZER_HOME=" line))
                lines))
      (is (some (lambda (line) (string= "EXTRA_VAR_1=value-1" line))
                lines)))))

(defclass template-test-prototype (test-sink-prototype)
  ())

(defmethod sink:get-prototype ((name (eql 'template-test-sink)))
  (make-instance 'template-test-prototype))

(defmethod sink:parse-sink-attribute
    ((proto template-test-prototype) (attr (eql :template)) &rest args)
  (list attr (cfg.util:parse-user-template args)))

(defclass fake-artefact (pastelyzer:string-artefact)
  ())

(defmethod sink:finish-sink ((proto template-test-prototype) (sink sink:sink))
  (let* ((document (sink:sink-document sink))
         (value (sink:attribute-value-in-context sink :template document))
         (fragment (make-instance 'pastelyzer:string-fragment :body value))
         (artefact (make-instance 'fake-artefact :source fragment)))
    (sink:add-artefact sink artefact)))

(defclass fake-paste (pastelyzer:paste)
  ())

(defun make-fake-paste (string &optional (id 0))
  (let* ((bytes (map '(vector (unsigned-byte 8)) #'char-code string))
         (content (make-instance 'pastelyzer:content :id id :body bytes)))
    (make-instance 'fake-paste
                   :id id
                   :provider "test"
                   :provider-id "a-paste"
                   :content content)))

(defmethod pastelyzer:paste-source ((paste fake-paste))
  (let ((id (pastelyzer:paste-id paste)))
    (values (format nil "~A:~A"
                    (pastelyzer:paste-provider paste)
                    (pastelyzer:paste-provider-id paste))
            (make-instance 'puri:uri
                           :scheme :test
                           :host "test.local"
                           :path (format nil "/~A" id))
            (make-instance 'puri:uri
                           :scheme :test
                           :host "test.local"
                           :path (format nil "/~A/raw" id)))))

(config-test template.1 ()
  (define-sink a-sink (template-test-sink)
    (:template (extract usr::origin)))

  ;; XXX: It would be nice if we could test template processing without
  ;; artificial artefacts.  But at least one artefact is needed for document
  ;; post-processing to kick in.
  (define-artefact-filter origin-test
      (= "artefact.test")
    (collect-into a-sink))

  (let ((paste (make-fake-paste "aaa artefact.test zzz")))
    (multiple-value-bind (artefacts job)
        (extract-artefacts paste)
      (declare (ignore artefacts))
      (let ((rendered (collected-artefacts job 'a-sink 'fake-artefact)))
        (is (= 1 (length rendered)))
        (is (string= "test:a-paste"
                     (pastelyzer:artefact-source (first rendered))))))))

(config-test template.2 ()
  (define-sink a-sink (template-test-sink)
    (:template (extract usr::remote-url)))

  ;; XXX: Same as above.
  (define-artefact-filter origin-test
      (= "artefact.test")
    (collect-into a-sink))

  (let ((paste (make-fake-paste "aaa artefact.test zzz" 42)))
    (multiple-value-bind (artefacts job)
        (extract-artefacts paste)
      (declare (ignore artefacts))
      (let ((rendered (collected-artefacts job 'a-sink 'fake-artefact)))
        (is (= 1 (length rendered)))
        (is (string= "test://test.local/42"
                     (pastelyzer:artefact-source (first rendered))))))))
