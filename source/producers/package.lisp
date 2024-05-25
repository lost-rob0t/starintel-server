(uiop:define-package   :star.producers
  (:use       :cl #:LPARALLEL)
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
   #:producer-publish
   #:make-producer))
