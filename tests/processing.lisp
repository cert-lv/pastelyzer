(in-package #:pastelyzer.tests)

(suite 'processing '(extractors domains))

(suite 'extractors)

(defclass test-job (job)
  ((artefacts
    :accessor test-job-artefacts
    :initform '())))

(defmethod register-artefact ((job test-job) (artefact t) (source t))
  (push artefact (test-job-artefacts job))
  artefact)

(defmethod resolve-domains-p ((job test-job))
  nil)

(defun extract-artefacts (subject &rest types)
  (let* ((fragment (typecase subject
                     (string
                      (make-instance 'string-fragment :body subject))
                     ((vector (unsigned-byte 8))
                      (make-instance 'binary-fragment :body subject))
                     (otherwise
                      subject)))
         (job (make-instance 'test-job :subject fragment))
         (result (process job)))
    (is (null (set-difference result (test-job-artefacts job))))
    (values (if types
                (let ((classes (mapcar #'find-class types)))
                  (loop for item in result
                        when (find (class-of item) classes)
                          collect item))
                result)
            job)))

(test extractors.credentials.1 ()
  (let* ((string "There's a user@service.com test email")
         (artefacts (extract-artefacts string 'email))
         (artefact (first artefacts)))
    (is (= 1 (length artefacts)))
    (is (string= "user@service.com"
                 (artefact-description artefact)))))

(test extractors.credentials.2 ()
  ;; Don't allow spaces in passwords until our extractor is smarter.
  (let* ((string "Great success: user@service.com:now with spaces!")
         (artefacts (extract-artefacts string 'credential))
         (artefact (first artefacts)))
    (is (= 1 (length artefacts)))
    (is (and (string= "user@service.com" (credential-username artefact))
             (string= "now" (credential-passphrase artefact))))))

(test extractors.credentials.3 ()
  (flet ((test (separator other)
           (let* ((exp-pass (format nil "pass~Cword" other))
                  (string (format nil "xxx~Cemail@company.com~C~A~Cxxx"
                                  separator separator exp-pass separator))
                  (artefacts (extract-artefacts string 'credential))
                  (artefact (first artefacts)))
             (is (= 1 (length artefacts)))
             (is (string= "email@company.com" (credential-username artefact)))
             (let ((pass (credential-passphrase artefact)))
               (is (string= exp-pass pass)
                   "With ~C -- expected '~A', got '~A'"
                   separator exp-pass pass)))))
    (test #\: #\;)
    (test #\; #\:)
    (test #\| #\:)
    (test #\tab #\:)))

(test extractors.credentials.4 ()
  (let* ((string "FooBar@Service.Com:whatever")
         (artefacts (extract-artefacts string 'credential))
         (artefact (first artefacts)))
    (is (= 1 (length artefacts)))
    (is (string= "FooBar@Service.Com" (credential-username artefact)))))

(test extractors.credentials.5 ()
  (dolist (string '("image-file@2x.png"
                    "for i in 0...@array.size"))
    (let ((artefacts (extract-artefacts string 'credential)))
      (is (null artefacts)))))

(test extractors.email.1 ()
  (let* ((string "XXX.person@test.local. nnThank you, ...")
         (emails (extract-artefacts string 'email)))
    (is (and (= 1 (length emails))
             (string= "XXX.person@test.local"
                      (artefact-source (first emails)))))))

(test extractors.bank-cards.1 ()
  (flet ((check (digits)
           (is (= 1 (length (extract-artefacts digits))))))
    ;; Another number good for testing: 1111-2222-3333-4444.
    (check "4242 4242 4242 4242")
    (check "4242  4242  4242  4242")
    (check "4242-4242-4242-4242")
    (check "4242424242424242")))

(test extractors.bank-cards.2 ()
  (flet ((check (digits)
           (is (endp (extract-artefacts digits)))))
    (check "4242_4242_4242_4242")
    (check "4242-4242_4242-4242")
    (check "4242424242424242.42")
    (check "42.4242424242424242")
    (check "42.4242424242424242.42")))

(test extractors.bank-cards.3 ()
  "No real bank would make a bank card with all the digits be ing the
same, right?  "
  (is (endp (extract-artefacts "1111111111111111"))))

(test extractors.bank-cards.4 ()
  "We would like to not fall for hexdumps, but nothing we can do while
we're using regular expressions..."
  (let ((string "4242 4242 4242 4242 4242 4242 4242 4242"))
    (is (= 2 (length (extract-artefacts string))))))

(test extractors.bank-cards.amex.1 ()
  (flet ((check (digits)
           (is (= 1 (length (extract-artefacts digits))))))
    (check "4242 424242 42424")
    (check "4242  424242  42424")
    (check "4242-424242-42424")
    (check "424242424242424")))

(test extractors.addresses.1 ()
  (let* ((artefacts (extract-artefacts "10.0.0.42 8.16.32.127 8.16.32.128"
                                       'ip-address)))
    (is (= 3 (length artefacts)))))

(test extractors.services.1 ()
  (let* ((string (format nil "a 1.2.3.4:1234 z"))
         (artefacts (extract-artefacts string 'ip-service)))
    (is (= 1 (length artefacts)))
    (= 1234 (ip-service-port (first artefacts)))))

(test extractors.embedded-binary.bin.1 ()
  (let* ((blob #(#xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D))
         (string (format nil "xxx ~{~8,'0B~} xxx" (coerce blob 'list)))
         (input (map '(vector (unsigned-byte 8) *) #'char-code string))
         (artefacts (extract-artefacts input 'binary-blob)))
    (is (= 1 (length artefacts)))
    (is (equalp blob
                (fragment-body (embedded-binary-bytes (first artefacts)))))))

(test extractors.embedded-binary.base64.1 ()
      (let* ((bytes #(#xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                      #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                      #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                      #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                      #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                      #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D))
             (blob (make-array (length bytes)
                               :element-type '(unsigned-byte 8)
                               :initial-contents bytes))
             (string
               (format nil "### Keybase proof xxx ~A xxx"
                       (base64:usb8-array-to-base64-string blob)))
             (input (map '(vector (unsigned-byte 8) *) #'char-code string))
             (artefacts (extract-artefacts input 'base64-blob)))
        (is (= 1 (length artefacts)))
        (let ((artefact (first artefacts)))
          (is (equalp blob
                      (fragment-body (embedded-binary-bytes artefact)))))))

(test extractors.embedded-binary.hex.1 ()
  (let* ((blob #(#xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                 #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                 #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D
                 #xDE #xAD #xBE #xEF #xFE #xED #xFA #xCE #xD0 #x0D))
         (string (format nil "xxx ~{~2,'0X~} xxx" (coerce blob 'list)))
         (input (map '(vector (unsigned-byte 8) *) #'char-code string))
         (artefacts (extract-artefacts input 'hex-blob)))
    (is (= 1 (length artefacts)))
    (is (equalp blob
                (fragment-body (embedded-binary-bytes (first artefacts)))))))

(test extractors.embedded-binary.base64.2 ()
  (let* ((string "Her b tresur: cHlyaXRlQGV4YW1wbGUuY29tOnlhcnJoYXJyeWFycmhhcnJ5YXJyaGFycnlhcnJoYXJyeWFycmhhcnJ5YXJyaGFycnlhcnJoYXJy")
         (bytes (map '(vector (unsigned-byte 8)) #'char-code string))
         (artefacts (extract-artefacts bytes)))
    (dolist (artefact artefacts)
      (typecase artefact
        (credential
         (is (string= "pyrite@example.com" (credential-username artefact)))
         (is (string= "yarrharryarrharryarrharryarrharryarrharryarrharryarrharr"
                      (credential-passphrase artefact))))
        (domain
         (is (string= "example.com" (artefact-description artefact))))))))

(test extractors.embedded-binary.0x.1 ()
  (let* ((string "
[Byte[]] $Shellcode32 = @(0xfc,0xe8,0x89,0x00,0x00,0x00,0x60,0x89,0xe5,
                          0x80,0xfb,0xe0,0x75,0x05,0xbb,0x47,0x13,0x72,
                          0x61,0x6c,0x63,0x00)")
         (artefacts (extract-artefacts string)))
    (is (= 1 (length artefacts)))
    (is (= 22 (length (fragment-body (embedded-binary-bytes (first artefacts))))))))

(test extractors.embedded-binary.0x.2 ()
  (let* ((string "
[Byte[]] $Shellcode32 = @(0xfc, 0xe8, 0x89, 0x00, 0x00, 0x00, 0x60, 0x89, 0xe5,
                          0x80, 0xfb, 0xe0, 0x75, 0x05, 0xbb, 0x47, 0x13, 0x72,
                          0x61, 0x6c, 0x63, 0x00)")
         (artefacts (extract-artefacts string)))
    (is (= 1 (length artefacts)))
    (is (= 22 (length (fragment-body (embedded-binary-bytes (first artefacts))))))))

(test extractors.uri.1 ()
  (let* ((string "xxx> http://example.local\\r\\n")
         (artefacts (extract-artefacts string 'uri)))
    (is (= 1 (length artefacts)))
    (is (string= "http://example.local"
                 (artefact-source (first artefacts))))))

(test extractors.uri.2 ()
  (let* ((string " http://user:pass@test.local:88/foo/bar/get?xxx=zzz#abc ")
         (artefacts (extract-artefacts string)))
    (is (and (<= 2 (length artefacts))
             (some (lambda (artefact) (typep artefact 'uri)) artefacts)
             (some (lambda (artefact) (typep artefact 'credential)) artefacts)))
    (dolist (artefact artefacts)
      (typecase artefact
        (credential
         (is (or (and (string= "user" (credential-username artefact))
                      (string= "pass" (credential-passphrase artefact)))
                 ;; XXX: Expected failure.
                 (string= "pass@test.local"
                          (credential-username artefact)))))
        (uri
         (is (string= "http://user:pass@test.local:88/foo/bar/get?xxx=zzz#abc"
                      (artefact-source artefact))))))))

(test extractors.uri.3 ()
  (let* ((string "<a href=http://www.\".$domains[1][0].\"/>")
         (artefacts (extract-artefacts string 'uri)))
    (is (endp artefacts))))

(test extractors.uri.4 ()
  (let* ((string "a https://2.xx.example.local/w/e.png z")
         (artefacts (extract-artefacts string 'uri)))
    (is (= 1 (length artefacts)))
    (is (string= "https://2.xx.example.local/w/e.png"
                 (artefact-source (first artefacts))))))

(test extractors.uri.5 ()
  (let* ((string "a hxxps://example.local/xyz z")
         (artefacts (extract-artefacts string 'uri)))
    (is (= 1 (length artefacts)))))

(test extractors.uri.numeric-host.ipv4.1 ()
  (let* ((string "#EXTM3U
http://91.109.113.38:8000/playlist.m3u8
#EXTINF:0,http://220.125.160.36:9981/playlist")
         (uris (extract-artefacts string 'uri)))
    (is (= 2 (length uris)))
    (dolist (artefact uris)
      (let ((uri (artefact-source artefact)))
        (is (or (string= "http://91.109.113.38:8000/playlist.m3u8" uri)
                (string= "http://220.125.160.36:9981/playlist" uri)))))))

(test extractors.uri.numeric-host.ipv4.2 ()
  (let* ((string "aaa http://2130706433/APH.hta zzz")
         (artefacts (extract-artefacts string 'uri)))
    (is (= 1 (length artefacts)))
    (is (string= "http://2130706433/APH.hta"
                 (artefact-source (first artefacts))))))

(test extractors.uri.numeric-host.ipv4.3 ()
  (let* ((string "aaa http://127.1/APH.hta zzz")
         (artefacts (extract-artefacts string 'uri)))
    (is (= 1 (length artefacts)))
    (is (string= "http://127.1/APH.hta"
                 (artefact-source (first artefacts))))))

(test extractors.uri.numeric-host.ipv6.1 ()
  ;; https://tools.ietf.org/html/rfc3513#section-2.2
  (let ((addresses '("FEDC:BA98:7654:3210:FEDC:BA98:7654:3210"
                     "1080:0:0:0:8:800:200C:417A"
                     "FF01:0:0:0:0:0:0:101"
                     "0:0:0:0:0:0:0:1"
                     "1080::8:800:200C:417A"
                     "FF01::101"
                     "::1"
                     "0:0:0:0:0:0:13.1.68.3"
                     "0:0:0:0:0:FFFF:129.144.52.38"
                     "::13.1.68.3"
                     "::FFFF:129.144.52.38")))
    (loop for addr in addresses
          for string = (format nil "a http://[~A]/xxx z" addr)
          do (let ((artefacts (extract-artefacts string 'uri)))
               (is (= 1 (length artefacts))
                   "Failed to extract URI from ~S" string)))))

(test extractors.uri.onion ()
  (let ((artefacts (extract-artefacts "xxx http://3g2upl4pq6kufc4m.onion/ zzz"
                                      'uri)))
    (is (= 1 (length artefacts)))
    (is (string= "http://3g2upl4pq6kufc4m.onion/"
                 (artefact-source (first artefacts))))))

(test extractors.domains.onion ()
  (let ((artefacts (extract-artefacts "xxx 3g2upl4pq6kufc4m.onion zzz" 'onion)))
    (is (= 1 (length artefacts)))
    (is (string= "3g2upl4pq6kufc4m.onion"
                 (artefact-source (first artefacts))))))

(test extractors.nested.1 ()
  (let* ((string (format nil "~@{~A~^~%~}"
                         "H4sICAQnv1wAA2Zha2UtcGFzdGUA8zrS"
                         "mJdZrOBydF9qUebRtqMLrRSyEoEieilV"
                         "QIG8Yofk1KISvZwyLgBMgLO1KQAAAA=="))
         (emails (extract-artefacts string 'email)))
    (is (= 1 (length emails)))
    (is (string= "janis.dzerins@cert.lv"
                 (artefact-source (first emails))))))

(test extractors.nested.from-paste ()
  (let* ((string (format nil "~@{~A~^~%~}"
                         "H4sICAQnv1wAA2Zha2UtcGFzdGUA8zrS"
                         "mJdZrOBydF9qUebRtqMLrRSyEoEieilV"
                         "QIG8Yofk1KISvZwyLgBMgLO1KQAAAA=="))
         (bytes (map '(vector (unsigned-byte 8)) #'char-code string))
         (content (make-instance 'content :id 0 :body bytes))
         (paste (make-instance 'circl-paste
                               :id 0
                               :provider "test"
                               :provider-id "from-paste"
                               :content content))
         (emails (extract-artefacts paste 'email)))
    (is (= 1 (length emails)))
    (is (string= "janis.dzerins@cert.lv"
                 (artefact-source (first emails)))))  )

(test extractors.broken-utf-8 ()
  (let* ((codes
           #(239 191 189 239 191 189 239 191 189 239 191 189 239 191 189 239
             191 189 239 191 189 239 191 189 239 191 189 239 191 189 239 191
             189 239 191 189 239 191 189 32  239 191 189 239 191 189 239 191
             189 239 191 189 239 191 189 239 191 189 239 191 189 239 191 189
             239 191 189 239 191 189 239 191 189 239 191 189 239 191 189 239
             191 189 32  239 191 189 239 191 189 239 191 189 239 191 189 58))
         (bytes (coerce codes '(vector (unsigned-byte 8))))
         (content (make-instance 'content :id 0 :body bytes))
         (paste (make-instance 'circl-paste
                               :id 0
                               :provider "test"
                               :provider-id "from-paste"
                               :content content)))
    (signals broken-utf-8 (extract-artefacts paste))))

(suite 'domains)

(defmacro is-viable-domain-p ((string &optional (start 0) end)
                              exp-value exp-reason)
  (check-type string string)
  (let ((end (or end (length string))))
    `(multiple-value-bind (.value. .reason.)
         (pastelyzer::viable-domain-p ,string ,start ,end)
       (is (and (eq ,exp-value .value.)
                (eq ,exp-reason .reason.))
           "Expected '~A' in '~A' to be (~S ~S), got (~S ~S)"
           (subseq ,string ,start ,end) ,string
           ,exp-value ,exp-reason
           .value. .reason.))))

(test viable-domains.fall-through ()
  (is-viable-domain-p ("www.xxx.com") t :undecided)
  (is-viable-domain-p ("123.xxx.com") t :undecided)
  (is-viable-domain-p ("www.xxx.xn--123xx") t :undecided))

(test viable-domains.tld ()
  (is-viable-domain-p ("www.xxx.c0m") nil :invalid-tld)
  (is-viable-domain-p ("www.xxx.c-m") nil :invalid-tld))

(test viable-domains.labels ()
  (is-viable-domain-p ("www.123.com") nil :invalid-labels)

  (is-viable-domain-p ("www.xxx.-xx") nil :invalid-labels)
  (is-viable-domain-p ("www.xxx.xx-") nil :invalid-labels)
  (is-viable-domain-p ("www.-xx.com") nil :invalid-labels)
  (is-viable-domain-p ("www.xx-.com") nil :invalid-labels)
  (is-viable-domain-p ("-ww.xxx.com") nil :invalid-labels)
  (is-viable-domain-p ("ww-.xxx.com") nil :invalid-labels)

  (is-viable-domain-p ("www.xxx.c_m") nil :invalid-labels)
  (is-viable-domain-p ("www.x_x.com") nil :invalid-labels)
  (is-viable-domain-p ("w_w.xxx.com") nil :invalid-labels))

(test viable-domains.url-like ()
  (is-viable-domain-p ("http://foo.xxx.com x" 7 18) t :url-like)
  (is-viable-domain-p ("http://foo.xxx.com/ x" 7 18) t :url-like)
  (is-viable-domain-p ("http://foo.xxx.com:80 x" 7 18) t :url-like)
  (is-viable-domain-p ("http://foo.xxx.java x" 7 19) t :url-like))

(test viable-domains.self ()
  (is-viable-domain-p ("self.whatever") nil :self)
  (is-viable-domain-p ("self.whatever=" 0 13) nil :self)
  (is-viable-domain-p ("self.whatever = " 0 13) nil :self))

(test viable-domains.file ()
  (is-viable-domain-p ("Third.png") nil :file-name))

(test viable-domains.path ()
  (is-viable-domain-p ("/xxx.com" 1) nil :path)
  (is-viable-domain-p ("\\xxx.com" 1) nil :path))

(test viable-domains.function-call ()
  (is-viable-domain-p ("xxx.whatever()" 0 12) nil :function-call))

(test viable-domains.assignment ()
  (is-viable-domain-p ("xxx.whatever=" 0 12) nil :expression)
  (is-viable-domain-p ("xxx.whatever = " 0 12) nil :expression))

(test domains.1 ()
  (let ((string "tw(rw,cfn(.3,.7,0)*ang(-math.pi/2,math.rad(-40),0),.1,'')"))
    (is (endp (extract-artefacts string)))))
