(defpackage #:pastelyzer.log
  (:use #:common-lisp)
  (:local-nicknames (#:cl-log #:com.ravenbrook.common-lisp-log))
  (:import-from #:alexandria
                #:when-let)
  (:export #:msg
           #:maybe-log
           #:known-log-category-p
           #:setup-logging
           #:with-logged-warnings
           #:plain-message
           #:timestamped-message))

(defpackage #:pastelyzer.db
  (:use #:common-lisp)
  (:import-from #:pastelyzer.log
                #:msg)
  (:import-from #:postmodern
                #:with-transaction)
  (:local-nicknames (#:pg #:cl-postgres))
  (:export #:*db-params*
           #:*current-version-id*
           #:initialize
           #:with-connection
           #:with-auto-reconnect
           #:call-with-auto-reconnect
           #:with-transaction
           #:insert-content-fix
           #:register-broken-content
           #:insert-content
           #:store-content
           #:content-body
           #:insert-paste
           #:store-paste
           #:get-paste
           #:paste-with-content
           #:initiate-analysis
           #:finish-analysis
           #:flush-content-artefacts
           #:register-artefact
           #:map-unprocessed-content-ids
           #:content-artefacts
           #:content-sources
           #:search-artefacts
           #:search-artefacts-multi

           #:congested-content
           #:congested-content-id))

(defpackage #:pastelyzer.util
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:array-index
                #:array-length
                #:when-let)
  (:export #:alphabet
           #:sub-alphabet-p
           #:histogram
           #:show-histogram
           #:entropy
           #:group
           #:partition
           #:trim-space
           #:whitespace-char-p
           #:one-line
           #:string-context-before
           #:string-context-after
           #:dsubseq
           #:starts-with-subseq
           #:ends-with-subseq
           #:mixed-case-p
           #:summarize-numbers))

(defpackage #:pastelyzer
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:array-index
                #:array-length
                #:compose
                #:define-constant
                #:delete-from-plist
                #:if-let
                #:hash-table-values
                #:read-file-into-byte-vector
                #:read-stream-content-into-byte-vector
                #:read-stream-content-into-string
                #:when-let
                #:when-let*
                #:with-gensyms)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:string-case
                #:string-case)
  (:import-from #:pastelyzer.log
                #:msg
                #:maybe-log
                #:setup-logging
                #:with-logged-warnings)
  (:import-from #:pastelyzer.util
                #:alphabet
                #:sub-alphabet-p
                #:group
                #:whitespace-char-p
                #:one-line
                #:string-context-after
                #:string-context-before
                #:dsubseq
                #:starts-with-subseq
                #:ends-with-subseq
                #:summarize-numbers)
  #+sbcl
  (:import-from #:sb-concurrency
                #:make-mailbox
                #:mailbox-empty-p
                #:receive-message
                #:send-message)
  (:local-nicknames (#:pg #:cl-postgres)
                    (#:ht #:hunchentoot)
                    (#:hs #:hunchensocket)
                    (#:sq #:cl-speedy-queue)
                    (#:db #:pastelyzer.db))
  (:export #:run
           #:run-standalone
           #:process

           #:binary-fragment
           #:fragment
           #:string-fragment
           #:fragment-body

           #:artefact
           #:artefact-source
           #:artefact-source-seq
           #:artefact-source-seq-start
           #:artefact-source-seq-end
           #:artefact-source-seq-bounds
           #:artefact-description
           #:artefact-context-before
           #:artefact-context-after
           #:artefact-key
           #:artefact-note
           #:important-artefact-p
           #:string-artefact
           #:m3u-entry
           #:windows-internal
           #:email
           #:credential
           #:credential-username
           #:credential-passphrase
           #:ip-address
           #:ip-service
           #:ip-service-address
           #:ip-service-port
           #:resolved-ip-address
           #:domain
           #:onion
           #:uri
           #:bank-card-number
           #:bank-card-number-digits

           #:binary-artefact
           #:compressed-blob
           #:compressed-blob-bytes
           #:compressed-blob-method
           #:encoded-string
           #:encoded-string-encoding

           #:broken-fragment
           #:broken-fragment-datum
           #:broken-fragment-locations
           #:broken-utf-8
           #:artefact-discarded

           #:embedded-binary
           #:embedded-binary-bytes
           #:binary-blob
           #:hex-blob
           #:base64-blob

           #:job
           #:job-subject
           #:job-artefacts
           #:register-artefact
           #:finish-job
           #:resolve-domains-p

           #:content
           #:content-id
           #:content-body

           #:paste
           #:paste-id
           #:paste-provider
           #:paste-provider-id
           #:circl-paste
           #:web-paste
           #:paste-source
           #:paste-origins
           #:remote-content
           #:remote-content-location
           #:http-content
           #:link-only-http-content

           #:async
           #:*announcers*))

(defpackage #:fmt
  (:use #:common-lisp)
  (:export #:bytes
           #:nbytes))
