(uiop:define-package :star.documents
  (:use :cl)
  (:export
   #:document-schema-validation-error
   #:document-schema-validation-category
   #:document-schema-validation-reason
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
   #:validate-v09-document
   #:ensure-document
   #:document-json
   #:utc-now))
