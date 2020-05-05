(defpackage #:pastelyzer.tests.config.sets
  (:use #:common-lisp #:2am)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sets #:pastelyzer.config.sets))
  (:export #:tests))

(in-package #:pastelyzer.tests.config.sets)

(suite 'tests)

(test super-domain-set.1 ()
  (let ((ht (make-hash-table :test 'equal)))
    (sets::hashtree-add-path ht '("a") "set a")
    (sets::hashtree-add-path ht '("b" "c") "set bc")

    (loop for (value comment) in '((("a") "set a")
                                   (("a" "x") "set a")
                                   (("b" "c") "set bc")
                                   (("b" "c" "x") "set bc"))
          do (multiple-value-bind (found note)
                 (sets::hashtree-present-p ht value)
               (is found)
               (is (string= comment note))))
    (is (not (sets::hashtree-present-p ht '("b"))))))

(test super-domain-set.2 ()
  (let ((ht (make-hash-table :test 'equal)))
    (sets::hashtree-add-path ht '("a") ".a")

    (signals warning
      (sets::hashtree-add-path ht '("a" "b")))))
