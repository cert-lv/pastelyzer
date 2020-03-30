(defpackage #:pastelyzer.tests.config.filter
  (:use #:common-lisp #:2am)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sink #:pastelyzer.config.sink)
                    (#:filter #:pastelyzer.config.filter))
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
              (member? . usr:member?)

              (define-set . usr:define-set)
              (ipv4-networks . usr:ipv4-networks)
              (super-domains . usr:super-domains)
              (cc-bins . usr:cc-bins)
              (strings . usr:strings))
            tree)))

(defmacro with-filter (var filter &body body)
  (let ((filter (translate-user-symbols filter)))
    `(let ((.filter.
             (pastelyzer.config.filter::parse-filter 'test-filter ',filter)))
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
    (is (eq 't (f 42)))))

(test filter.and ()
  (with-filter f (and (> 40) (< 50))
    (is (eq 't (f 42)))
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

(test filter.-> ()
  (with-filter f (-> (length) (= 3))
    (is (eq 't (f "foo")))
    (is (eq 't (f '(1 2 3))))
    (is (eq 't (f #(1 2 3))))))

(test filter.composite.1 ()
  (with-filter f (and (type? cl:string)
                      (-> (length) (> 3)))
    (is (eq 't (f "fubar")))
    (is (eq 'nil (f "foo")))
    (is (eq 'nil (f (coerce "fubar" 'list))))))

(test filter.composite.2 ()
  (with-filter f (or (and (type? cl:string)
                          (starts-with? "fu"))
                     (and (type? cl:list)
                          (starts-with? #(#x4d #x5a))))
    (is (eq 't (f "fubar")))
    (is (eq 'nil (f "foo")))
    (is (eq 't (f '(77 90 144))))
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
