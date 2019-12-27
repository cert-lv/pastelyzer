(in-package #:pastelyzer.tests)

(suite 'circl-paste)

(defmacro test-circl-paste ((site id) exp-name exp-url &optional exp-raw-url)
  (let ((provider-id (format nil "archive/~A/2001/01/01/~A.gz" site id)))
    `(let* ((paste (make-instance 'pastelyzer::circl-paste
                                 :id nil
                                 :content nil
                                 :provider "circl"
                                 :provider-id ,provider-id)))
       (multiple-value-bind (name url raw-url)
           (pastelyzer::paste-source paste)
         (setq url (puri:render-uri url nil))
         (when raw-url
           (setq raw-url (puri:render-uri raw-url nil)))
         (is (string= ,exp-name name)
             "Expected name to be '~A', got '~A'" ,exp-name name)
         (is (string= ,exp-url url)
             "Expected URL to be '~A', got '~A'" ,exp-url url)
         (if ,exp-raw-url
             (is (string= ,exp-raw-url raw-url)
                 "Expected raw URL to be '~A', got '~A'" ,exp-raw-url raw-url)
             (is (null raw-url)
                 "Expected raw URL to be NIL, got '~A'" raw-url))))))

(test circl-paste.justpaste.it ()
  (test-circl-paste ("justpaste.it" "19p4m")
                    "justpaste.it"
                    "https://justpaste.it/19p4m"))

(test circl-paste.pastebin.fr ()
  (test-circl-paste ("pastebin.fr" "50512")
                    "pastebin.fr"
                    "http://pastebin.fr/50512"))

(test circl-paste.codepad.org ()
  (test-circl-paste ("codepad.org" "B4d6aOXk")
                    "codepad.org"
                    "http://codepad.org/B4d6aOXk"))

(test circl-paste.paste.kde.org ()
  (test-circl-paste ("paste.kde.org" "py1tgdbtj")
                    "paste.kde.org"
                    "https://paste.kde.org/py1tgdbtj"))

(test circl-paste.paste.frubar.net ()
  (test-circl-paste ("paste.frubar.net" "27699")
                    "paste.frubar.net"
                    "http://paste.frubar.net/27699"))

(test circl-paste.paste.org.ru ()
  (test-circl-paste ("paste.org.ru" "3fjc56")
                    "paste.org.ru"
                    "http://paste.org.ru/?3fjc56"))

(test circl-paste.slexy.org ()
  (test-circl-paste ("slexy.org" "s2uWkaBEHk")
                    "slexy.org"
                    "https://slexy.org/view/s2uWkaBEHk"))

(test circl-paste.snipplr.com ()
  (test-circl-paste ("snipplr.com" "311072_oracle-asciistr-function_")
                    "snipplr.com"
                    "http://snipplr.com/view/311072/oracle-asciistr-function/"))

(test circl-paste.gist.github.com ()
  (test-circl-paste ("gist.github.com" "micmn_dfb1ea9c9b0ccf37f6112b5f1e21a489")
                    "gist.github.com"
                    "https://gist.github.com/micmn/dfb1ea9c9b0ccf37f6112b5f1e21a489"))

(test circl-paste.pastebin.com_pro ()
  (test-circl-paste ("pastebin.com_pro" "tE9RvHDy")
                    "pastebin.com"
                    "https://pastebin.com/tE9RvHDy"
                    "https://pastebin.com/raw/tE9RvHDy"))

(test circl-paste.lpaste.net ()
  (test-circl-paste ("lpaste.net" "357379")
                    "lpaste.net"
                    "http://lpaste.net/357379"
                    "http://lpaste.net/raw/357379"))

(test circl-paste.paste.debian.net ()
  (test-circl-paste ("paste.debian.net" "979832")
                    "paste.debian.net"
                    "http://paste.debian.net/979832"
                    "http://paste.debian.net/plain/979832"))

(test circl-paste.ideone.com ()
  (test-circl-paste ("ideone.com" "8Z8qre")
                    "ideone.com"
                    "http://ideone.com/8Z8qre"
                    "http://ideone.com/plain/8Z8qre"))

(test circl-paste.paste.opensuse.org ()
  (test-circl-paste ("paste.opensuse.org" "99489842")
                    "paste.opensuse.org"
                    "http://paste.opensuse.org/99489842"
                    "http://paste.opensuse.org/view/raw/99489842"))

(test circl-paste.kpaste.net ()
  (test-circl-paste ("kpaste.net" "432b8")
                    "kpaste.net"
                    "http://kpaste.net/432b8"
                    "http://kpaste.net/432b8?raw"))

(test circl-paste.pastebin.ru ()
  (test-circl-paste ("pastebin.ru" "dPydEwzw")
                    "pastebin.ru"
                    "http://pastebin.ru/dPydEwzw"
                    "http://pastebin.ru/dPydEwzw/d/"))
