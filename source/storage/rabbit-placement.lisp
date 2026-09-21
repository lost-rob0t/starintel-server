(in-package :star.rabbit)

(defun raw-stored-document (client database document-id)
  "Return the CouchDB representation for DOCUMENT-ID without external hydration."
  (handler-case
      (star.documents:parse-document-object
       (cl-couch:get-document client database document-id))
    (dex:http-request-not-found () nil)))

(defun persisted-document-tier-before-mutation (client database document)
  "Return the committed tier when updating an existing document, or NIL."
  (let ((id (star.documents:document-id document)))
    (and id
         (let ((stored (raw-stored-document client database id)))
           (and stored (star.storage:document-storage-tier stored))))))

(defun desired-mutation-storage-tier (client database document operation)
  "Choose placement from committed state for updates and tenant policy for creates."
  (or (and (eq operation :updated)
           (persisted-document-tier-before-mutation client database document))
      (and (string= (or (star.documents:document-dtype document) "") "file")
           "hot")
      (star.storage:tenant-default-storage-tier
       (or (star.documents:document-value document "tenant_id" nil)
           (star.documents:document-value document "tenant" nil)
           "default"))))

(defun public-incoming-storage-document (document)
  "Clone DOCUMENT, discard actor-owned placement, and materialize file artifacts."
  (let ((copy (star.storage::clone-json document)))
    (star.storage::remove-storage-metadata! copy)
    (handler-case
        (star.storage:prepare-file-artifact-document copy)
      (star.storage:file-artifact-validation-error (condition)
        (error 'star.consumers:schema-invalid-delivery-error
               :cause condition
               :reason (princ-to-string condition))))))

(defun save-rabbit-state-with-placement (client database tier state)
  "Save one outbox state without ever committing public content ahead of placement.

A state already carrying server placement metadata is an outbox-bookkeeping
revision loaded from CouchDB; it can be written directly because its public
payload has not changed. A state without placement metadata is a new public
mutation and must stage its external payload before CouchDB becomes the commit
point."
  (handler-case
      (if (star.storage::document-storage-metadata-present-p state)
          (star.databases.couchdb::couchdb-save-outbox-document
           client database state)
          (let* ((document-id (star.documents:document-id state))
                 (previous (raw-stored-document client database document-id))
                 (old-metadata
                   (and previous
                        (star.storage:document-storage-metadata previous)))
                 (saved
                   (star.storage::save-document-placement
                    client database state tier)))
            (when old-metadata
              (star.storage::cleanup-old-object-copy old-metadata saved))
            saved))
    (dex:http-request-conflict ()
      (error 'star.databases.couchdb::outbox-store-conflict))))

(defun persist-rabbit-document-mutation (document operation)
  "Persist an outbox mutation atomically with its server-owned storage tier.

New documents use the tenant default tier. Updates preserve the tier visible in
the committed CouchDB representation. External content is staged before the
CouchDB compare-and-swap, so an S3 failure never leaves a cold/archive update
committed as hot. Outbox publication markers remain CouchDB-only server state.
Rabbit delivery is acknowledged only after this durable sequence completes."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (let* ((database star:*couchdb-default-database*)
           (incoming (public-incoming-storage-document document))
           (tier (desired-mutation-storage-tier
                  client database incoming operation)))
      (star.databases.couchdb::process-outbox-mutation
       (lambda (document-id)
         (raw-stored-document client database document-id))
       (lambda (state)
         (save-rabbit-state-with-placement client database tier state))
       #'publish-outbox-event
       incoming
       operation))))
