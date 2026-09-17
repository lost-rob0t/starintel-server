(in-package :star.storage)

(defun hmac-sha256 (key data)
  "Return HMAC-SHA256(DATA, KEY) using Ironclad's generic MAC interface."
  (let ((mac (ironclad:make-mac :hmac key :sha256)))
    (ironclad:update-mac mac data)
    (ironclad:produce-mac mac)))

(defun document-storage-metadata-present-p (document)
  "True when DOCUMENT carries an explicit server-owned placement record."
  (let* ((object (parse-json-document document))
         (extensions (object-slot-object object "extensions")))
    (not (null
          (object-slot-object extensions +document-storage-extension-key+)))))

(defun storage-object-key (document &optional content-sha256)
  "Return a tenant/dataset scoped object key.

When CONTENT-SHA256 is supplied the key is immutable/content-addressed. Storage
commits always use that form so an optimistic CouchDB conflict can leave only an
unreferenced staged object; it cannot overwrite the payload referenced by the
currently committed CouchDB revision."
  (let ((tenant (document-tenant document))
        (dataset (or (star.documents:document-dataset document) "default"))
        (id (or (star.documents:document-id document)
                (error "Document storage requires _id"))))
    (format nil "tenants/~a/datasets/~a/documents/~a~@[.~a~].json"
            (safe-key-component tenant)
            (safe-key-component dataset)
            (safe-key-component id)
            (and content-sha256
                 (safe-key-component content-sha256)))))

(defun save-document-placement (client database document tier)
  "Persist DOCUMENT in TIER using an immutable external object commit.

External payload is staged first under its SHA-256 key. CouchDB CAS is the
commit point. The returned value is reloaded after the CouchDB write so callers
observe the committed revision rather than the pre-write candidate."
  (let* ((normalized-tier (normalize-storage-tier tier))
         (backend-name (tier-backend-name normalized-tier))
         (external-p (and (external-tier-p normalized-tier)
                          (not (string= backend-name "couchdb"))))
         (payload (and external-p (canonical-storage-payload document)))
         (content-sha256 (and payload (sha256-hex payload)))
         (object-key
           (and external-p
                (storage-object-key document content-sha256)))
         (metadata
           (storage-metadata
            normalized-tier
            backend-name
            :object-key object-key
            :state (cond
                     ((string= normalized-tier "warm") "mirrored")
                     ((offloaded-tier-p normalized-tier) "offloaded")
                     (t "resident"))
            :content-sha256 content-sha256))
         (candidate
           (if (offloaded-tier-p normalized-tier)
               (storage-document-stub document metadata)
               (set-storage-metadata! (clone-json document) metadata))))
    (when external-p
      (storage-put
       (resolve-storage-backend backend-name)
       object-key
       payload
       :content-type "application/json"
       :metadata
       (list :tenant (document-tenant document)
             :dataset (star.documents:document-dataset document)
             :document-id (star.documents:document-id document)
             :content-sha256 content-sha256)))
    (couchdb-save-document client database candidate)
    (load-document
     client
     database
     (star.documents:document-id document))))

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
         (let ((tier
                 (if (document-storage-metadata-present-p candidate)
                     (document-storage-tier candidate)
                     (tenant-default-storage-tier
                      (document-tenant candidate)))))
           (save-document-placement client database candidate tier))
       (dex:http-request-conflict ()
         (error 'star.databases.couchdb:document-update-store-conflict))))
   document-id
   patch
   :max-attempts max-attempts))
