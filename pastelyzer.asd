(defsystem "pastelyzer"
  :description "Leak analyser"
  :author "Jānis Džeriņš <janis.dzerins@cert.lv>"
  :version (:read-file-form "src/vars.lisp" :at (1 2))
  :license "n/a"
  :long-description "Monitor data posted on the interwebs -- mainly
  (but not only) on paste sites."
  :build-operation program-op
  :build-pathname "bin/pastelyzer"
  :entry-point "pastelyzer:run-standalone"
  :depends-on ("alexandria"
               "cl-base64"
               "cl-log"
               "cl-postgres"
               "cl-postgres+local-time"
               "cl-ppcre"
               "cl-smtp"
               "cl-speedy-queue"
               "cl-who"
               "drakma"
               "flexi-streams"
               "hunchentoot"
               "hunchensocket"
               "ip"
               "jsown"
               "postmodern"
               "pzmq"
               "split-sequence"
               "string-case"
               (:feature (:not :swank) "swank")
               (:feature (:not (:or :sbcl :ccl)) "trivial-utf-8")
               (:feature :sbcl (:require "sb-concurrency"))
               (:feature :sbcl "chipz")
               (:feature (:not :sbcl) "chanl"))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "vars")
                             (:file "sys")
                             (:file "compat")
                             (:file "util")
                             (:file "fmt")
                             (:file "log")
                             (:file "deflate"
                              :if-feature (:and (:not :sbcl) (:not :quicklisp)))
                             (:file "db")
                             (:file "circl-zmq")
                             (:file "circl-paste")
                             (:file "job")
                             (:file "processing")
                             (:module "config"
                              :serial t
                              :components ((:file "package")
                                           (:file "sink")
                                           (:file "filter")
                                           (:file "context")
                                           (:file "loader")
                                           (:file "util")
                                           (:file "sets")
                                           (:file "smtp")
                                           (:file "cmd")))
                             (:file "server")
                             (:file "json-api")
                             (:file "rest")
                             (:module "modules"
                              :components ((:file "misp")))
                             (:file "cli")
                             (:file "pastelyzer"))))
  :in-order-to ((test-op (test-op "pastelyzer/tests"))))

(defsystem "pastelyzer/tests"
  :depends-on ("alexandria"
               "2am"
               "pastelyzer")
  :pathname "tests/"
  :components ((:file "package")
               (:file "processing" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "circl-paste" :depends-on ("package"))
               (:module "config"
                :depends-on ("package")
                :components ((:file "filter")
                             (:file "sets")))
               (:file "suites" :depends-on ("package" "config")))
  :perform (test-op (o c)
                    (symbol-call '#:2am '#:run 't)))
