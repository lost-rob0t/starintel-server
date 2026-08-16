(uiop:define-package :star.documents
  (:use :cl)
  (:export
   #:object-has-key-p
   #:object-value
   #:object-keys
   #:parse-document-object
   #:clone-document-object
   #:canonical-dtype
   #:document-id
   #:document-data
   #:document-value
   #:document-dtype
   #:document-dataset
   #:document-date-added
   #:document-date-updated
   #:document-transient-p
   #:ensure-document
   #:document-json
   #:utc-now))
