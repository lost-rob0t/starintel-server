(in-package :star.databases.couchdb)

(export
 '(document-update-outcome
   document-update-outcome-status
   document-update-outcome-document
   document-update-outcome-attempts
   document-update-outcome-reason
   document-update-validation-error
   document-update-validation-reason
   document-update-store-conflict
   merge-document-update
   prepare-document-insert
   upsert-document-update
   couchdb-upsert-document-update
   document-update-outcome-json)
 :star.databases.couchdb)
