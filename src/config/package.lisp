(defpackage #:pastelyzer.config.user
  (:use)
  (:import-from #:pastelyzer
                #:artefact
                #:binary-artefact
                #:compressed-blob
                #:encoded-string
                #:string-artefact

                #:bank-card-number
                #:credential
                #:domain
                #:onion
                #:email
                #:embedded-binary
                #:base64-blob
                #:binary-blob
                #:hex-blob
                #:ip-address
                #:ip-service
                #:resolved-ip-address
                #:m3u-entry
                #:uri
                #:windows-internal

                #:embedded-binary-bytes)
  (:export #:define-sink
           #:define-artefact-filter
           #:collect-into
           #:discard
           #:set-important
           #:fmt
           #:env
           #:file-contents
           #:yes
           #:no
           #:true
           #:false
           #:note

           #:or
           #:and
           #:not
           #:=
           #:>
           #:<
           #:type?
           #:exact-type?
           #:extract
           #:->
           #:length
           #:starts-with?
           #:ends-with?
           #:contains?

           ;; Sets.
           #:define-set
           #:strings
           #:cc-bins
           #:super-domains
           #:ipv4-networks
           #:member?
           ))

(defpackage #:pastelyzer.config.package
  (:use :common-lisp)
  (:export #:user-package
           #:user-identifier
           #:user-identifier-p))

(in-package #:pastelyzer.config.package)

(defun user-package ()
  (find-package "PASTELYZER.CONFIG.USER"))

(defun user-identifier (name)
  (let* ((package (user-package))
         (symbol (etypecase name
                   (symbol
                    (import name package)
                    name)
                   (string
                    (intern name package)))))
    (export symbol package)
    symbol))

(defun user-identifier-p (thing)
  (and (symbolp thing)
       (eq thing (find-symbol (symbol-name thing) (user-package)))))
