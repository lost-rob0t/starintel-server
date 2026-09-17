(in-package :star.rabbit)

(defun persisted-document-tier-before-mutation (client database document)
  "Return the current tier when updating an existing document, or NIL."
  (let ((id (star.documents:document-id document)))
    (and id
         (handler-case
             (star.storage:document-storage-tier
              (star.documents:parse-document-object
               (cl-couch:get-document client database id)))
           (dex:http-request-not-found () nil)))))

(defun desired-mutation-storage-tier (client database document operation)
  (or (and (eq operation :updated)
           (persisted-document-tier-before-mutation client database document))
      (star.storage:tenant-default-storage-tier
       (or (star.documents:document-value document "tenant_id" nil)
           (star.documents:document-value document "tenant" nil)
           "default"))))

(defun place-mutated-document (client database document tier)
  (let ((id (star.documents:document-id document)))
    (when (and id (not (string= tier "hot")))
      (star.storage:transition-document client database id tier))))

(defun persist-rabbit-document-mutation (document operation)
  "Persist an outbox mutation, then enforce server-owned document placement.

New documents use the tenant default tier. Updates preserve the pre-existing
tier, preventing an update event from accidentally rehydrating a cold/archive
document into permanent hot storage. The Rabbit delivery is not acknowledged
until object placement succeeds."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (let* ((database star:*couchdb-default-database*)
           (tier (desired-mutation-storage-tier
                  client database document operation))
           (result
             (star.databases.couchdb:couchdb-process-outbox-mutation
              client
              database
              #'publish-outbox-event
              document
              operation)))
      (place-mutated-document client database document tier)
      result)))
