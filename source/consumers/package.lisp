(uiop:define-package   :star.consumers
  (:use       :cl #:LPARALLEL)
  (:documentation "doc")
  (:export
   #:consumer
   #:consumer-name
   #:consumer-filter
   #:consumer-fn
   #:consumer-take
   #:consumer-queue
   #:consumer-channel
   #:consumer-max-size
   #:consumer-state
   #:consumer-stream
   #:consumer-lock
   #:consumer-update-state
   #:consumer-cleanup
   #:with-consumer-lock
   #:consumer-update
   #:consumer-read
   #:consume
   #:start-consumer
   #:make-consumer))
