(in-package :star.storage)

(defun document-storage-metadata-present-p (document)
  "True when DOCUMENT carries an explicit server-owned placement record."
  (let* ((object (parse-json-document document))
         (extensions (object-slot-object object "extensions")))
    (not (null
          (object-slot-object extensions +document-storage-extension-key+)))))

(defun storage-aware-upsert-document (client database document-id patch
                                      &key (max-attempts 8))
  "Optimistic document update preserving explicit lifecycle placement.

Legacy/unplaced inserts adopt the tenant default. Once a document has an
explicit placement record, including explicit HOT placement with no object key,
later updates preserve that tier until a lifecycle transition changes it."
  (star.databases.couchdb:upsert-document-update
   (lambda (id)
     (handler-case
         (load-document client database id)
       (dex:http-request-not-found () nil)))
   (lambda (candidate)
     (handler-case
         (let* ((tier
                  (if (document-storage-metadata-present-p candidate)
                      (document-storage-tier candidate)
                      (tenant-default-storage-tier
                       (document-tenant candidate))))
                (stored
                  (save-document-placement client database candidate tier)))
           (if (offloaded-tier-p tier)
               (load-document client database document-id)
               stored))
       (dex:http-request-conflict ()
         (error 'star.databases.couchdb:document-update-store-conflict))))
   document-id
   patch
   :max-attempts max-attempts))
