(defpackage #:pastelyzer.tests.config.filter
  (:use #:common-lisp #:2am)
  (:local-nicknames (#:usr #:pastelyzer.config.user))
  (:export #:tests))

(in-package #:pastelyzer.tests.config.filter)

(suite 'tests)

(defmacro with-filter (var filter &body body)
  (flet ((translate-user-symbols (tree)
           (sublis '((or . usr:or)
                     (and . usr:and)
                     (not . usr:not)
                     (= . usr:=)
                     (> . usr:>)
                     (< . usr:<)
                     (type? . usr:type?)
                     (extract . usr:extract)
                     (-> . usr:->)
                     (length . usr:length)
                     (starts-with? . usr:starts-with?)
                     (ends-with? . usr:ends-with?)
                     (contains? . usr:contains?))
                   tree
                   :test #'equal)))
    (let ((filter (translate-user-symbols filter)))
      `(let ((.filter.
               (pastelyzer.config.filter::parse-filter 'test-filter ',filter)))
         (declare (type function .filter.))
         (flet ((,var (value)
                  (funcall .filter. value)))
           ,@body)))))

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
