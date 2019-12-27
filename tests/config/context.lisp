(defpackage #:pastelyzer.tests.config.context
  (:use #:common-lisp #:2am)
  (:import-from #:pastelyzer
                #:applicable-extractors)
  (:import-from #:pastelyzer.config.context
                #:configurable-job
                #:get-sink)
  (:import-from #:pastelyzer.config.filter
                #:register-filter
                #:parse-filter)
  (:import-from #:pastelyzer.config.sink
                #:make-configuration
                #:collect-artefact
                #:sink-artefacts)
  (:import-from #:pastelyzer.config.package
                #:user-identifier)
  (:local-nicknames (#:usr #:pastelyzer.config.user))
  (:export #:tests))

(in-package #:pastelyzer.tests.config.context)

(suite 'tests)

(defclass test-job (configurable-job)
  ())

(defmethod pastelyzer::applicable-extractors
    ((target pastelyzer:string-fragment) (job test-job))
  (remove ':domain (call-next-method)))

(defvar *test-proto* nil)

(defclass test-sink-prototype (pastelyzer.config.sink::prototype)
  ())

(defmethod pastelyzer.config.sink:get-prototype
    ((name (eql (user-identifier "TEST-SINK"))))
  (if (null *test-proto*)
      (setq *test-proto* (make-instance 'test-sink-prototype))
      *test-proto*))

(defmacro with-temporary-filter ((name expression cfg) &body body)
  `(let ((pastelyzer.config.filter::*filters* '()))
     (register-filter ',name
                      (parse-filter ',name ',expression)
                      (list (lambda (artefact ctx)
                              (collect-artefact artefact ,cfg ctx))))
     ,@body))

(defun process (subject)
  (let* ((fragment
           (typecase subject
             (string
              (make-instance 'pastelyzer:string-fragment :body subject))
             ((vector (unsigned-byte 8))
              (make-instance 'pastelyzer:binary-fragment :body subject))
             (otherwise
              subject)))
         (job (make-instance 'test-job :subject fragment)))
    (values (pastelyzer:process job)
            job)))

(test config.1 ()
  (let ((lines (format nil "~
                | email      | test@example.com                 |~%~
                | url        | http://www.example.com/test.html |~%~
                | cc         | 4242-4242-4242-4242              |~%~
                | credential | user@example.com:hunter2         |~%"))
        (cfg (make-configuration (user-identifier "CFG")
                                 (user-identifier "TEST-SINK")
                                 '())))
    (with-temporary-filter (f-1
                            (usr:or (usr:type? pastelyzer:email)
                                    (usr:type? pastelyzer:credential))
                            cfg)
      (multiple-value-bind (artefacts job)
          (process lines)
        (let* ((sink (get-sink job cfg))
               (filtered (sink-artefacts sink)))
          (is (= 4 (length artefacts)))
          (is (= 2 (length filtered)))
          (is (subsetp filtered artefacts)))))))
