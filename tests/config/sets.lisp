(defpackage #:pastelyzer.tests.config.sets
  (:use #:common-lisp #:2am)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sets #:pastelyzer.config.sets))
  (:export #:tests))

(in-package #:pastelyzer.tests.config.sets)

(suite 'tests)

(test super-domain-set.1 ()
  (let ((ht (make-hash-table :test 'equal)))
    (sets::hashtree-add-path ht '("a"))
    (sets::hashtree-add-path ht '("b" "c"))

    (is (sets::hashtree-present-p ht '("a")))
    (is (sets::hashtree-present-p ht '("b" "c")))
    (is (sets::hashtree-present-p ht '("a" "x")))
    (is (sets::hashtree-present-p ht '("b" "c" "x")))

    (is (not (sets::hashtree-present-p ht '("b"))))))

(test super-domain-set.2 ()
  (let ((ht (make-hash-table :test 'equal)))
    (sets::hashtree-add-path ht '("a"))

    (signals warning
      (sets::hashtree-add-path ht '("a" "b")))))
