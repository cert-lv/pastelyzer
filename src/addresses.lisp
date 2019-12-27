(in-package #:pastelyzer)

(defun read-networks (path)
  (let ((result '()))
    (map-lines path
               (lambda (string)
                 (with-simple-restart
                     (continue "Ignore the invalid network.")
                   (push (ip:parse-address string :network) result)))
               :trim-space t
               :ignore-comment-lines t)
    (nreverse result)))

;;; XXX: Does this belong in IP package?
(defun local-address-p (address)
  ;; https://en.wikipedia.org/wiki/List_of_assigned_/8_IPv4_address_blocks
  ;; https://en.wikipedia.org/wiki/Private_network
  (let ((local-networks (load-time-value
                         (mapcar #'ip:parse-address
                                 '("127.0.0.0/8"
                                   "10.0.0.0/8"
                                   "172.16.0.0/12"
                                   "192.168.0.0/16")))))
    (dolist (network local-networks nil)
      (when (ip:address-in-network-p address network)
        (return t)))))

(defun interesting-ip-address-p (address)
  (and (not (local-address-p address))
       (dolist (network *interesting-networks* nil)
         (when (ip:address-in-network-p address network)
           (return (values t network))))))
