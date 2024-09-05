(uiop:define-package   :star.producers
  (:use       :cl)
  (:documentation "doc")
  (:export
   #:producer
   #:producer-queue
   #:producer-max-size
   #:producer-state
   #:producer-stream
   #:producer-lock
   #:producer-update-state
   #:producer-cleanup
   #:with-producer-lock
   #:make-producer
   #:producer-name
   #:producer-exchange
   #:producer-exchange-type
   #:exchange-durable-p
   #:producer-conn
   #:producer-user
   #:producer-password
   #:producer-vhost
   #:producer-port
   #:producer-host
   #:producer-open-p
   #:destroy
   #:producer-connect
   #:publish))
