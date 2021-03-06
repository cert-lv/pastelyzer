(defpackage #:pastelyzer.config.filter
  (:use :common-lisp)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:pastelyzer
                #:starts-with-subseq
                #:ends-with-subseq)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:pastelyzer.config.package
                #:user-package)
  (:local-nicknames (#:usr #:pastelyzer.config.user)
                    (#:sink #:pastelyzer.config.sink)
                    (#:util #:pastelyzer.util))
  (:export #:filter
           #:add-filter
           #:get-filter
           #:apply-filter
           #:apply-filters
           #:collect-into
           #:extract
           #:generate-filter-function
           #:make-function))

(in-package #:pastelyzer.config.filter)

(defvar *filters* '()
  "An association list of defined filters.")

(defclass filter ()
  ((name
    :initarg :name
    :reader filter-name
    :type symbol)
   (function
    :initarg :function
    :reader filter-function
    :type (or symbol function))
   (actions
    :initarg :actions
    :reader filter-actions
    :type list)))

(defun get-filter (name)
  (or (cdr (assoc name *filters*))
      (error "Unknown filter: ~S." name)))

(defun register-filter (class name function actions)
  (check-type name symbol)
  (let ((filter (make-instance class
                               :name name
                               :function function
                               :actions actions)))
    (when (assoc name *filters*)
      (warn "Redefining filter ~A" name)
      (setq *filters* (remove name *filters* :key #'car)))
    (setq *filters* (append *filters* (list (cons name filter)))))
  name)

(defvar *variables* '())

(defun set-variable (name value)
  (setf *variables* (acons name value *variables*)))

(defun get-variable (name)
  (let ((cons (assoc name *variables*)))
    (if cons
        (cdr cons)
        (error "Variable ~S is not set." name))))

(defun apply-filter (filter value ctx)
  (handler-case
      (let ((*variables* '()))
        (when (funcall (filter-function filter) value)
          (dolist (action (filter-actions filter))
            (funcall action value ctx))))
    (serious-condition (condition)
      (msg :error "While applying filter ~S to ~S: ~A"
           (filter-name filter)
           value
           condition))))

(defun apply-filters (value ctx &key (class (find-class 'filter)))
  (loop for (nil . filter) in *filters*
        when (eq class (class-of filter))
        do (apply-filter filter value ctx))
  value)

(defgeneric parse-action (action &rest args)
  (:documentation "Parse a filter action form."))

(defmethod parse-action ((action (eql 'usr:collect-into)) &rest args)
  (check-type args (cons symbol null))
  (let ((cfg (sink:resolve-configuration (first args))))
    (lambda (artefact ctx)
      (sink:collect-artefact artefact cfg ctx))))

(defmethod parse-action ((action (eql 'usr:discard)) &rest args)
  (check-type args (or null (cons string null)))
  (let ((reason (first args)))
    (lambda (artefact ctx)
      (declare (ignore artefact ctx))
      (throw 'discard-artefact reason))))

(defmethod parse-action ((action (eql 'usr:set-important)) &rest args)
  (check-type args null)
  (lambda (artefact ctx)
    (declare (ignore ctx))
    (msg :debug "Marking ~S as important")
    (setf (pastelyzer:important-artefact-p artefact) t)))

(defmethod parse-action ((action (eql 'usr:set-note)) &rest args)
  (let ((datum (first args)))
    (etypecase datum
      (string
       (lambda (artefact ctx)
         (declare (ignore ctx))
         (msg :debug "Setting note of ~S to ~S" artefact datum)
         (setf (pastelyzer:artefact-note artefact) datum)))
      (symbol
       (lambda (artefact ctx)
         (declare (ignore ctx))
         (let ((note (get-variable datum)))
           (msg :debug "Setting note of ~S to value of ~A: ~S"
                artefact datum note)
           (setf (pastelyzer:artefact-note artefact) note)))))))

(defmacro make-function (op (&rest args) &body body)
  (declare (ignorable op))
  `(lambda (&rest .args.)
     ;; (format *trace-output* "~&(~S ~{~S~^ ~})~%" ',op .args.)
     (destructuring-bind ,args
         .args.
       ,@body)))

(defgeneric generate-filter-function (operator &rest body))

(defmethod generate-filter-function ((operator t) &rest body)
  (declare (ignore body))
  (error "Unknown operator: ~S" operator))

(defmethod generate-filter-function ((operator (eql 'usr:^)) &rest body)
  (check-type body (or null (cons symbol)))
  (let ((name (or (first body) 'usr:^)))
    (make-function ^ (value cont)
      (set-variable name value)
      (funcall cont value))))

(defmethod generate-filter-function ((operator (eql 'usr:and)) &rest body)
  (if (endp body)
      (make-function and (value cont)
        (declare (ignorable value))
        (funcall cont value))
      (let ((head (apply #'generate-filter-function (first body)))
            (tail (apply #'generate-filter-function operator (rest body))))
        (declare (type function head tail))
        (make-function and (value cont)
          (funcall head value
                   ;; XXX Allocates a closure at runtime.
                   (lambda (result)
                     (if result
                         (funcall tail value cont)
                         (funcall cont nil))))))))

(defmethod generate-filter-function ((operator (eql 'usr:or)) &rest body)
  (if (endp body)
      (make-function or (value cont)
        (declare (ignorable value))
        (funcall cont nil))
      (let ((head (apply #'generate-filter-function (first body)))
            (tail (apply #'generate-filter-function operator (rest body))))
        (declare (type function head tail))
        (make-function or (value cont)
          (funcall head value
                   ;; XXX Allocates a closure at runtime.
                   (lambda (result)
                     (if result
                         (funcall cont result)
                         (funcall tail value cont))))))))

(defmethod generate-filter-function ((operator (eql 'usr:not)) &rest body)
  (check-type body (cons list null))
  (let ((inner (apply #'generate-filter-function (first body))))
    (declare (type function inner))
    (make-function not (value cont)
      (funcall inner value
               ;; XXX Allocates a closure at runtime.
               (lambda (result)
                 (funcall cont (if result nil t)))))))

(defmethod equals ((left number) (right number))
  (= left right))

(defmethod lessp ((left number) (right number))
  (< left right))

(defmethod greaterp ((left number) (right number))
  (> left right))

(defmethod equals ((left string) (right string))
  (string= left right))

(defmethod equals ((left string) (right pastelyzer:string-artefact))
  (string= left (pastelyzer:artefact-source right)))

(defmethod lessp ((left string) (right string))
  (string< left right))

(defmethod lessp ((left string) (right pastelyzer:string-artefact))
  (string< left (pastelyzer:artefact-source right)))

(defmethod greaterp ((left string) (right string))
  (string> left right))

(defmethod greaterp ((left string) (right pastelyzer:string-artefact))
  (string> left (pastelyzer:artefact-source right)))

(defmethod generate-filter-function ((operator (eql 'usr:=)) &rest body)
  (check-type body (cons (or number string) null))
  (let ((datum (first body)))
    (make-function = (value cont)
      (funcall cont (equals datum value)))))

(defmethod generate-filter-function ((operator (eql 'usr:>)) &rest body)
  (check-type body (cons number null))
  (let ((number (first body)))
    (make-function > (value cont)
      (funcall cont (greaterp value number)))))

(defmethod generate-filter-function ((operator (eql 'usr:<)) &rest body)
  (check-type body (cons number null))
  (let ((number (first body)))
    (make-function < (value cont)
      (funcall cont (lessp value number)))))

(defmethod generate-filter-function ((operator (eql 'usr:type?)) &rest body)
  (check-type body (cons symbol null))
  (let ((type (first body)))
    (make-function type? (value cont)
      (funcall cont (typep value type)))))

(defmethod generate-filter-function ((operator (eql 'usr:exact-type?))
                                     &rest body)
  (check-type body (cons symbol null))
  (let ((type (first body)))
    (make-function type? (value cont)
      (funcall cont (eq type (class-name (class-of value)))))))

(defgeneric extract (field object)
  (:documentation "Used by EXTRACT filter expression."))

(defmethod extract ((field t) (object t))
  (error "Unknown extractor ~S for ~S" field object))

(defmethod generate-filter-function ((operator (eql 'usr:extract)) &rest body)
  (check-type body (cons symbol null))
  (let ((accessor (find-symbol (symbol-name (first body)) (user-package))))
    (make-function extract (value cont)
      (funcall cont (extract accessor value)))))

(defmethod generate-filter-function ((operator (eql 'usr:->)) &rest body)
  (if (endp body)
      (make-function -> (value cont)
        (funcall cont value))
      (let ((head (apply #'generate-filter-function (first body)))
            (tail (apply #'generate-filter-function operator (rest body))))
        (declare (type function head tail))
        ;; XXX Allocates a closure at runtime.
        (make-function -> (value cont)
          (funcall head value
                   (lambda (result)
                     (funcall tail result cont)))))))

(defmethod value-length ((value sequence))
  (length value))

(defmethod value-length ((value pastelyzer:fragment))
  (length (pastelyzer:fragment-body value)))

(defmethod value-length ((value pastelyzer:artefact))
  (multiple-value-bind (start end)
      (pastelyzer:artefact-source-seq-bounds value)
    (- end start)))

(defmethod generate-filter-function ((operator (eql 'usr:length)) &rest body)
  (check-type body null)
  (make-function length (value cont)
    (funcall cont (value-length value))))

(defmethod starts-with? ((prefix sequence) (value sequence))
  (starts-with-subseq prefix value))

(defmethod starts-with? ((prefix sequence) (value pastelyzer:fragment))
  (starts-with? prefix (pastelyzer:fragment-body value)))

(defmethod starts-with? ((prefix sequence) (value pastelyzer:artefact))
  (multiple-value-bind (start end)
      (pastelyzer:artefact-source-seq-bounds value)
    (starts-with-subseq prefix (pastelyzer:artefact-source-seq value)
                        :start2 start
                        :end2 end)))

(defmethod generate-filter-function ((operator (eql 'usr:starts-with?))
                                     &rest body)
  (check-type body (cons vector null))
  (let ((prefix (first body)))
    (make-function starts-with? (sequence cont)
      (funcall cont (starts-with? prefix sequence)))))

(defmethod ends-with? ((suffix sequence) (value sequence))
  (ends-with-subseq suffix value))

(defmethod ends-with? ((suffix sequence) (value pastelyzer:fragment))
  (ends-with? suffix (pastelyzer:fragment-body value)))

(defmethod ends-with? ((suffix sequence) (value pastelyzer:artefact))
  (multiple-value-bind (start end)
      (pastelyzer:artefact-source-seq-bounds value)
    (ends-with-subseq suffix (pastelyzer:artefact-source-seq value)
                      :start2 start
                      :end2 end)))

(defmethod generate-filter-function ((operator (eql 'usr:ends-with?))
                                     &rest body)
  (check-type body (cons vector null))
  (let ((suffix (first body)))
    (make-function ends-with? (sequence cont)
      (funcall cont (ends-with? suffix sequence)))))

(defmethod contains? ((needle sequence) (value sequence))
  (search needle value))

(defmethod contains? ((needle sequence) (value pastelyzer:fragment))
  (contains? needle (pastelyzer:fragment-body value)))

(defmethod contains? ((needle sequence) (value pastelyzer:artefact))
  (multiple-value-bind (start end)
      (pastelyzer:artefact-source-seq-bounds value)
    (search needle (pastelyzer:artefact-source-seq value)
            :start2 start
            :end2 end)))

(defmethod generate-filter-function ((operator (eql 'usr:contains?))
                                     &rest body)
  (check-type body (cons vector null))
  (let ((needle (first body)))
    (make-function contains? (sequence cont)
      (funcall cont (if (contains? needle sequence) t nil)))))

(defmethod mixed-case-p ((string string))
  (util:mixed-case-p string))

(defmethod mixed-case-p ((artefact pastelyzer:string-artefact))
  (util:mixed-case-p (pastelyzer:artefact-source-seq artefact)
                     (pastelyzer:artefact-source-seq-start artefact)
                     (pastelyzer:artefact-source-seq-end artefact)))

(defmethod generate-filter-function ((operator (eql 'usr:mixed-case?))
                                     &rest body)
  (check-type body null)
  (make-function mixed-case? (thing cont)
    (funcall cont (if (mixed-case-p thing) t nil))))

(defun parse-filter (name form)
  (msg :debug "Filter ~S: ~S" name form)
  (if form
      (let ((test (apply #'generate-filter-function form)))
        (lambda (value)
          (msg :debug "Applying filter ~S to ~S" name value)
          (funcall test value
                   (lambda (result)
                     (msg :debug "~S ~S => ~:[fail~;success~]"
                          name value result)
                     result))))
      #'identity))

(defun add-filter (class name code actions)
  (register-filter class name
                   (parse-filter name code)
                   (mapcar (lambda (action)
                             (apply #'parse-action
                                    (first action)
                                    (rest action)))
                           actions)))

;;; Extractors.

(defmethod extract ((field (eql 'usr::origin)) (object t))
  (pastelyzer:paste-source object))

(defmethod extract ((field (eql 'usr::source-url)) (object t))
  (warn "SOURCE-URL extractor is deprecated, use ORIGIN instead.")
  (extract 'usr::origin object))

(defmethod extract ((field (eql 'usr::local-url)) (object t))
  (pastelyzer::external-url-to object))

(defmethod extract ((field (eql 'usr::remote-url)) (object t))
  ;; Should cache this result.
  (multiple-value-bind (source url raw-url)
      (pastelyzer::paste-source object)
    (declare (ignore source raw-url))
    (when url
      (puri:render-uri url nil))))

(defmethod extract ((field (eql 'usr::remote-raw-url)) (object t))
  ;; Should use the cached result.
  (multiple-value-bind (source url raw-url)
      (pastelyzer::paste-source object)
    (declare (ignore source url))
    (when raw-url
      (puri:render-uri raw-url nil))))

(defmethod extract ((field (eql 'usr::artefact-descriptions)) (sink sink:sink))
  (with-output-to-string (out)
    (let ((groups (sink:group-artefacts sink)))
      (loop for group in groups
            for (class unique important duplicate-count) = group
            do (terpri out)
               (pastelyzer::summarize-artefact-group out group)
               (terpri out)
               (terpri out)
               (loop for bag being each hash-value in unique
                     for artefact = (first bag)
                     for string = (pastelyzer:artefact-description artefact)
                     do (write-string string out)
                        (terpri out))))))

(defmethod extract ((field (eql 'usr::artefact-summary-by-class))
                    (sink sink:sink))
  (let ((groups (sink:group-artefacts sink)))
    ;; TODO: Waiting for the artefact summarization refactoring.
    (pastelyzer::summarize-artefacts groups :text)))

(defmethod extract ((field (eql 'usr::digits))
                    (artefact pastelyzer:bank-card-number))
  (pastelyzer:bank-card-number-digits artefact))

(defmethod extract ((field (eql 'usr::note))
                    (artefact pastelyzer:artefact))
  (pastelyzer:artefact-note artefact))

(defmethod extract ((field (eql 'usr::important))
                    (artefact pastelyzer:artefact))
  (pastelyzer:important-artefact-p artefact))

(defmethod extract ((field (eql 'usr::source-string))
                    (artefact pastelyzer:artefact))
  (pastelyzer:artefact-source artefact))

(defmethod extract ((field (eql 'usr::source-context))
                    (artefact pastelyzer:artefact))
  (pastelyzer:artefact-source-seq artefact))

(defmethod extract ((field (eql 'usr::context-before))
                    (artefact pastelyzer:artefact))
  (pastelyzer::artefact-context-before artefact))

(defmethod extract ((field (eql 'usr::context-after))
                    (artefact pastelyzer:artefact))
  (pastelyzer::artefact-context-after artefact))

(defmethod extract ((field (eql 'usr::bytes))
                    (artefact pastelyzer:artefact))
  (pastelyzer:embedded-binary-bytes artefact))
