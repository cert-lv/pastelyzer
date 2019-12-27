(in-package #:pastelyzer.tests)

(suite 'addresses)

(test addresses.local.1 ()
  (dolist (string '("192.168.42.42" "10.0.42.42"))
    (is (pastelyzer::local-address-p (ip:parse-address string)))))

(test addresses.interesting.1 ()
  (let ((pastelyzer::*interesting-networks*
          (list (ip:parse-address "42.42.0.0/16" :network))))
    (is (pastelyzer::interesting-ip-address-p (ip:parse-address "42.42.42.42")))
    (loop for string in '("42.43.43.43" "42.41.41.41")
          for address = (ip:parse-address string)
          do (is (not (pastelyzer::interesting-ip-address-p address))))))
