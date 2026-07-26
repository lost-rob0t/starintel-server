(uiop:define-package :star.documents
  (:nicknames :star.documents.v09)
  (:use :cl)
  (:export
   #:document-schema-profile
   #:v08-schema-profile
   #:v09-schema-profile
   #:schema-profile-version
   #:schema-profile-writable-p
   #:find-schema-profile
   #:schema-profile-for-document
   #:writable-schema-profile-for-document
   #:profile-data-slot-map
   #:normalize-document-for-index
   #:unsupported-document-schema
   #:read-only-document-schema
   #:document-value
   #:document-data
   #:document-dtype
   #:document-date-added
   #:document-transient-p
   #:ensure-v09-document
   #:v09-document-p
   #:v09-document-json
   #:v09-document-error))
