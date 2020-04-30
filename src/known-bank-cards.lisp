(in-package #:pastelyzer)

(defun read-bins (path)
  (let ((result '())
        (note nil))
    (labels ((get-bin-table (ndigits plength)
               (loop for (a b table) in result
                     when (and (= a ndigits)
                               (= b plength))
                       do (return-from get-bin-table table))
               (let ((table (make-hash-table :test 'equal)))
                 (push (list ndigits plength table) result)
                 table))
             (process-line (line)
               (with-simple-restart
                   (continue "Ignore invalid bin specification.")
                 (cond ((zerop (length line))
                        (setq note nil))
                       ((char= #\# (schar line 0))
                        (let ((start (position-if-not #'whitespace-char-p
                                                      line
                                                      :start 1)))
                          (setq note (if start
                                         (subseq line start)
                                         nil))))
                       (t
                        (let* ((string (remove-if #'whitespace-char-p line))
                               (length (length string))
                               (end (position-if-not #'digit-char-p string)))
                          (when (zerop end)
                            (error "Invalid bin: ~S" line))
                          (let* ((prefix (subseq string 0 end))
                                 (plength (length prefix))
                                 (table (get-bin-table length plength)))
                            (msg :debug "Important CC bin ~A/~D~@[: ~A~]"
                                 prefix length note)
                            (setf (gethash prefix table) note))))))))
      (msg :info "Reading important CC bins from ~A" path)
      (map-lines path #'process-line
                 :trim-space t
                 :ignore-comment-lines nil))
    (let ((count (loop for (nil nil table) in result
                       sum (hash-table-count table))))
      (msg :info "Read ~D bin~:P from ~A" count path))
    result))

(defvar *important-cc-bins* nil
  "The result of parsing bank card 'bin' file is stored here.  See
  READ-BINS and RECOGNISE-BANK-CARD-BIN functions.")

(defun recognise-bank-card-bin (digits &optional (bins *important-cc-bins*))
  (declare (type simple-string digits))
  (let ((length (length digits)))
    (loop for (ndigits plength table) in bins
          when (= ndigits length)
            do (multiple-value-bind (note foundp)
                   (gethash (subseq digits 0 plength) table)
                 (when foundp
                   (return-from recognise-bank-card-bin
                     (values 'bank-card-number
                             (list :important t :note note))))))))

(defun initialize-important-cc-bins (path)
  (setq *important-cc-bins* (read-bins path))
  (pushnew 'recognise-bank-card-bin *bank-card-extractors*))
