(in-package #:fmt)

(defun nbytes (stream n &optional colon-p at-sign-p)
  "Formats amount of N bytes in a human-readable fashion using powers
of 1024, or powers of 1000 if COLON-P is true."
  (declare (ignore at-sign-p)
           (type unsigned-byte n))
  (cond ((zerop n)
         (write-string "0B" stream))
        (t
         (multiple-value-bind (base units)
             (if colon-p
                 (values 1000.0d0 "BkMGTPEZY")
                 (values 1024.0d0 "BKMGTPEZY"))
           (loop for i fixnum from 0 below (1- (length units))
                 for f double-float = (coerce n 'double-float) then (/ f base)
                 until (< f base)
                 finally (let ((unit (schar units i)))
                           (if (and (< f 10) (plusp i))
                               (format stream "~,1F~A" f unit)
                               (format stream "~D~A" (round f) unit))))))))

(defun bytes (stream bytes &optional colon-p at-sign-p)
  "Formats a sequence of BYTES as hex-digit pairs."
  (declare (ignore colon-p at-sign-p))
  (etypecase bytes
    (vector
     (loop for byte of-type (unsigned-byte 8) across bytes
           do (when (< byte #x10)
                (write-char #\0 stream))
              (write byte :stream stream
                          :base 16
                          :radix nil
                          :readably nil
                          :escape nil)))
    (list
     (format stream "~{~2,'0x~}" bytes))))
