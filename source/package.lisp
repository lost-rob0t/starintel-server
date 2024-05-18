;; [[file:../source.org::*Namespace setup][Namespace setup:2]]
(uiop:define-package   :starintel-gserver
  (:nicknames :star)
  (:use       :cl)
  (:export
   #:init-db
   #:*rabbit-password*
   #:*rabbit-user*
   #:*rabbit-port*
   #:*rabbit-address*
   #:*http-scheme*
   #:*http-key-file*
   #:*http-cert-file*
   #:*http-api-base-path*
   #:*http-api-port*
   #:*http-api-address*
   #:*couchdb-default-database*
   #:*couchdb-host*
   #:*couchdb-port*
   #:*couchdb-user*
   #:*couchdb-password*
   #:*couchdb-scheme*))
;; Namespace setup:2 ends here

;; [[file:../source.org::*Namespace setup][Namespace setup:3]]
(uiop:define-package   :star.rabbit
  (:use       :cl :star.consumers)
  (:documentation "Rabitmq namespace")
  (:export
   #:start-rabbit-document-thread
   #:with-rabbit-send
   #:with-rabbit-recv
   #:emit-document
   #:+injest-queue+
   #:+updates-queue+
   #:+injest-key+
   #:+update-key+
   #:+targets-key+
   #:rabbit-queue-stream
   #:open-stream
   #:close-stream
   #:stream-read
   #:rabbit-consumer
   #:transient-p
   #:test-make-doc
   #:test-send))
;; Namespace setup:3 ends here

;; [[file:../source.org::*Namespace setup][Namespace setup:4]]
(uiop:define-package   :starintel-gserver-http-api
  (:nicknames :star.frontends.http-api)
  (:use       :cl)
  (:documentation "doc"))
;; Namespace setup:4 ends here
