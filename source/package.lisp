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
   #:*http-api-address*))
;; Namespace setup:2 ends here

;; [[file:../source.org::*Namespace setup][Namespace setup:3]]
(uiop:define-package   :starintel-gserver.rabbit
  (:nicknames :star.rabbit)
  (:use       :cl)
  (:documentation "Rabitmq namespace")
  (:export
   #:start-rabbit-document-thread
   #:with-rabbit-send
   #:with-rabbit-recv
   #:emit-document))
;; Namespace setup:3 ends here

;; [[file:../source.org::*Namespace setup][Namespace setup:4]]
(uiop:define-package   :starintel-gserver-http-api
  (:nicknames :star.frontends.http-api)
  (:use       :cl)
  (:documentation "doc"))
;; Namespace setup:4 ends here
