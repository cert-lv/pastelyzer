(in-package #:pastelyzer.tests)

;;; This will not work nicely if any other loaded system uses T as the
;;; suite identifier -- if more than one such test suite is loaded in
;;; the same image the latest loaded suite will override all
;;; previously loaded ones.
;;;
;;; An alternative to this would be to use a name like 'all, and
;;; instead of:
;;;
;;;   (symbol-call '#:2am '#:run 't)
;;;
;;; in the system definition we'd have to do something like:
;;;
;;;   (symbol-call '#:2am '#:run (find-symbol* '#:all '#:pastelyzer.test))
;;;
;;; which does not look nice and feels like too much unnecessary code
;;; in a system definition.
;;;
;;; Also from within another system (e.g., pastelyzer) doing
;;;
;;;   (2am:run 't)
;;;
;;; is much more convenient to do than
;;;
;;;   (2am:run 'pastelyzer.test:all)
;;;
(suite 'config '(pastelyzer.tests.config.filter:tests
                 pastelyzer.tests.config.context:tests))

(suite 't '(addresses processing util circl-paste config))
