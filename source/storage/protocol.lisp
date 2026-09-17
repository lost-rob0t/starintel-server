(in-package :star.storage)

(defparameter +document-storage-extension-key+ "_server_storage"
  "Server-owned document extension describing authoritative storage placement.")

(defparameter +document-storage-tiers+ '("hot" "warm" "cold" "archive")
  "Supported document placement tiers ordered from online to archival.")

(defparameter +storage-stub-keys+
  '("_id" "_rev" "dataset" "dtype" "schema_version" "schemaVersion"
    "tenant_id" "tenant" "date_added" "dateAdded" "date_updated"
    "dateUpdated" "createdAt" "updatedAt" "collectedAt" "observedAt"
    "contentHash" "hashAlgorithm" "sizeBytes" "deleted" "tombstoneReason"
    "extensions")
  "Fields retained in CouchDB when the canonical payload is offloaded.")

(define-condition storage-backend-error (error)
  ((backend :initarg :backend :reader storage-backend-error-backend)
   (operation :initarg :operation :reader storage-backend-error-operation)
   (key :initarg :key :reader storage-backend-error-key)
   (reason :initarg :reason :reader storage-backend-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Storage backend ~a failed ~a for ~a: ~a"
             (storage-backend-error-backend condition)
             (storage-backend-error-operation condition)
             (storage-backend-error-key condition)
             (storage-backend-error-reason condition)))))

(defclass storage-backend ()
  ((name :initarg :name :reader storage-backend-name))
  (:documentation "Pluggable external document payload backend."))

(defgeneric storage-put (backend key content &key content-type metadata)
  (:documentation "Persist CONTENT under KEY and return backend metadata."))

(defgeneric storage-get (backend key)
  (:documentation "Return the complete object stored under KEY."))

(defgeneric storage-delete (backend key)
  (:documentation "Delete KEY. Deleting a missing object should be idempotent."))

(defgeneric storage-head (backend key)
  (:documentation "Return metadata for KEY without fetching its body."))

(defclass memory-storage-backend (storage-backend)
  ((objects :initform (make-hash-table :test #'equal)
            :reader memory-storage-objects)
   (lock :initform (bt:make-lock "memory-document-storage")
         :reader memory-storage-lock)))

(defun make-memory-storage-backend (&key (name "memory"))
  (make-instance 'memory-storage-backend :name name))

(defmethod storage-put ((backend memory-storage-backend) key content
                        &key (content-type "application/json") metadata)
  (declare (ignore metadata))
  (bt:with-lock-held ((memory-storage-lock backend))
    (setf (gethash key (memory-storage-objects backend)) content))
  (list :key key
        :content-type content-type
        :size (length content)))

(defmethod storage-get ((backend memory-storage-backend) key)
  (bt:with-lock-held ((memory-storage-lock backend))
    (multiple-value-bind (value present-p)
        (gethash key (memory-storage-objects backend))
      (unless present-p
        (error 'storage-backend-error
               :backend (storage-backend-name backend)
               :operation :get
               :key key
               :reason "object not found"))
      value)))

(defmethod storage-delete ((backend memory-storage-backend) key)
  (bt:with-lock-held ((memory-storage-lock backend))
    (remhash key (memory-storage-objects backend)))
  t)

(defmethod storage-head ((backend memory-storage-backend) key)
  (bt:with-lock-held ((memory-storage-lock backend))
    (multiple-value-bind (value present-p)
        (gethash key (memory-storage-objects backend))
      (and present-p
           (list :key key :size (length value))))))

(defvar *storage-backends* (make-hash-table :test #'equal))
(defvar *storage-backends-lock* (bt:make-lock "document-storage-backends"))

(defun normalize-storage-token (value)
  (and value
       (string-downcase
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (string value)))))

(defun register-storage-backend (backend &key name)
  "Register BACKEND under NAME (or its own name), replacing an older instance."
  (let ((key (normalize-storage-token (or name (storage-backend-name backend)))))
    (unless (and key (plusp (length key)))
      (error "Storage backend requires a non-empty name"))
    (bt:with-lock-held (*storage-backends-lock*)
      (setf (gethash key *storage-backends*) backend))
    backend))

(defun resolve-storage-backend (name &key (errorp t))
  (let* ((key (normalize-storage-token name))
         (backend
           (and key
                (bt:with-lock-held (*storage-backends-lock*)
                  (gethash key *storage-backends*)))))
    (cond
      (backend backend)
      (errorp
       (error 'storage-backend-error
              :backend (or key "<unset>")
              :operation :resolve
              :key ""
              :reason "backend is not configured"))
      (t nil))))

(defun valid-storage-tier-p (tier)
  (member (normalize-storage-token tier)
          +document-storage-tiers+
          :test #'string=))

(defun normalize-storage-tier (tier)
  (let ((value (normalize-storage-token tier)))
    (unless (valid-storage-tier-p value)
      (error "Unsupported document storage tier ~s; expected one of ~{~a~^, ~}"
             tier +document-storage-tiers+))
    value))

(defun parse-name-map (value)
  "Parse comma-separated NAME=VALUE configuration into a normalized alist."
  (loop for entry in (star::split-comma-setting value)
        for separator = (position #\= entry)
        when (and separator (plusp separator) (< separator (1- (length entry))))
          collect
          (cons (normalize-storage-token (subseq entry 0 separator))
                (normalize-storage-token (subseq entry (1+ separator))))))

(defun configured-map-value (raw key &optional default)
  (or (cdr (assoc (normalize-storage-token key)
                  (parse-name-map raw)
                  :test #'string=))
      default))

(defun tier-backend-name (tier)
  "Resolve the server-owned backend mapped to TIER."
  (let* ((normalized (normalize-storage-tier tier))
         (fallback (if (string= normalized "hot") "couchdb" "s3")))
    (configured-map-value star::*document-storage-tier-backends*
                          normalized
                          fallback)))

(defun tenant-default-storage-tier (tenant)
  "Resolve TENANT's default tier, falling back to the global default."
  (normalize-storage-tier
   (configured-map-value
    star::*document-storage-tenant-tiers*
    (or tenant "default")
    star::*document-storage-default-tier*)))

(defun configure-storage-backends ()
  "Install built-in test storage and the configured S3 backend when available."
  (register-storage-backend (make-memory-storage-backend))
  (let ((s3 (make-s3-storage-backend-from-settings :errorp nil)))
    (when s3
      (register-storage-backend s3)))
  t)

(defun json-object-p (value)
  (and (consp value) (eq (first value) :obj)))

(defun clone-json (value)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json value))))

(defun parse-json-document (document)
  (etypecase document
    (string
     (jsown:with-injective-reader
       (jsown:parse document)))
    (list document)))

(defun document-tenant (document)
  (or (star.documents:document-value document "tenant_id" nil)
      (star.documents:document-value document "tenant" nil)
      "default"))

(defun object-slot-object (object key)
  (let ((value (and object (jsown:val-safe object key))))
    (and (json-object-p value) value)))

(defun default-storage-metadata ()
  (jsown:new-js
    ("version" 1)
    ("tier" "hot")
    ("backend" "couchdb")
    ("state" "resident")
    ("object_key" :null)))

(defun document-storage-metadata (document)
  "Return server placement metadata or the implicit hot/CouchDB default."
  (let* ((object (parse-json-document document))
         (extensions (object-slot-object object "extensions"))
         (metadata (object-slot-object extensions +document-storage-extension-key+)))
    (if metadata (clone-json metadata) (default-storage-metadata))))

(defun document-storage-tier (document)
  (normalize-storage-tier
   (or (jsown:val-safe (document-storage-metadata document) "tier") "hot")))

(defun document-storage-backend-name (document)
  (normalize-storage-token
   (or (jsown:val-safe (document-storage-metadata document) "backend")
       (tier-backend-name (document-storage-tier document)))))

(defun document-storage-object-key (document)
  (let ((value (jsown:val-safe (document-storage-metadata document) "object_key")))
    (and (stringp value) value)))

(defun ensure-extensions! (document)
  (let ((extensions (object-slot-object document "extensions")))
    (unless extensions
      (setf extensions (jsown:empty-object)
            (jsown:val document "extensions") extensions))
    extensions))

(defun set-storage-metadata! (document metadata)
  (setf (jsown:val (ensure-extensions! document)
                   +document-storage-extension-key+)
        metadata)
  document)

(defun remove-storage-metadata! (document)
  (let ((extensions (object-slot-object document "extensions")))
    (when extensions
      (ignore-errors
        (jsown:remkey extensions +document-storage-extension-key+))))
  document)

(defun safe-key-component (value)
  "Encode a logical key segment without path separators or control bytes."
  (with-output-to-string (stream)
    (loop for character across (princ-to-string value)
          if (or (alphanumericp character)
                 (member character '(#\- #\_ #\. #\~)))
            do (write-char character stream)
          else
            do (format stream "_~X_" (char-code character)))))

(defun storage-object-key (document)
  "Return the deterministic tenant/dataset/document object key."
  (let ((tenant (document-tenant document))
        (dataset (or (star.documents:document-dataset document) "default"))
        (id (or (star.documents:document-id document)
                (error "Document storage requires _id"))))
    (format nil "tenants/~a/datasets/~a/documents/~a.json"
            (safe-key-component tenant)
            (safe-key-component dataset)
            (safe-key-component id))))

(defun sha256-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets string :encoding :utf-8))))

(defun storage-metadata (tier backend &key object-key state content-sha256)
  (let ((metadata
          (jsown:new-js
            ("version" 1)
            ("tier" tier)
            ("backend" backend)
            ("state" state)
            ("object_key" (or object-key :null))
            ("updated_at" (star.documents:utc-now)))))
    (when content-sha256
      (setf (jsown:val metadata "content_sha256") content-sha256))
    metadata))

(defun copy-selected-document-fields (document)
  (let ((result (jsown:empty-object)))
    (jsown:do-json-keys (key value) document
      (when (member key +storage-stub-keys+ :test #'string=)
        (setf (jsown:val result key) (clone-json value))))
    result))

(defun storage-document-stub (document metadata)
  "Build the CouchDB auth/index stub used by cold and archive tiers."
  (let ((stub (copy-selected-document-fields document)))
    (set-storage-metadata! stub metadata)
    stub))

(defun external-tier-p (tier)
  (member tier '("warm" "cold" "archive") :test #'string=))

(defun offloaded-tier-p (tier)
  (member tier '("cold" "archive") :test #'string=))

(defun canonical-storage-payload (document)
  "Serialize a backend-neutral document without CouchDB revision or placement metadata."
  (let ((copy (clone-json document)))
    (ignore-errors (jsown:remkey copy "_rev"))
    (remove-storage-metadata! copy)
    (jsown:to-json copy)))

(defun couchdb-save-document (client database document)
  (cl-couch:create-document client database (jsown:to-json document)))

(defun hydrate-from-storage (stored-document)
  (let* ((metadata (document-storage-metadata stored-document))
         (tier (normalize-storage-tier (jsown:val-safe metadata "tier")))
         (backend-name (normalize-storage-token
                        (jsown:val-safe metadata "backend")))
         (key (jsown:val-safe metadata "object_key")))
    (if (and (offloaded-tier-p tier)
             (stringp backend-name)
             (not (string= backend-name "couchdb"))
             (stringp key))
        (let* ((backend (resolve-storage-backend backend-name))
               (payload (storage-get backend key))
               (document (parse-json-document payload))
               (revision (jsown:val-safe stored-document "_rev")))
          (when revision
            (setf (jsown:val document "_rev") revision))
          (set-storage-metadata! document metadata)
          document)
        stored-document)))

(defun load-document (client database document-id
                       &key (get-fn #'cl-couch:get-document))
  "Load DOCUMENT-ID and hydrate object-backed payloads transparently."
  (hydrate-from-storage
   (parse-json-document (funcall get-fn client database document-id))))

(defun save-document-placement (client database document tier)
  "Persist DOCUMENT in TIER, writing external content before the CouchDB pointer."
  (let* ((normalized-tier (normalize-storage-tier tier))
         (backend-name (tier-backend-name normalized-tier))
         (external-p (and (external-tier-p normalized-tier)
                          (not (string= backend-name "couchdb"))))
         (payload (and external-p (canonical-storage-payload document)))
         (object-key (and external-p (storage-object-key document)))
         (content-sha256 (and payload (sha256-hex payload)))
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
         (candidate nil))
    (when external-p
      (storage-put (resolve-storage-backend backend-name)
                   object-key
                   payload
                   :content-type "application/json"
                   :metadata
                   (list :tenant (document-tenant document)
                         :dataset (star.documents:document-dataset document)
                         :document-id (star.documents:document-id document))))
    (setf candidate
          (if (offloaded-tier-p normalized-tier)
              (storage-document-stub document metadata)
              (set-storage-metadata! (clone-json document) metadata)))
    (couchdb-save-document client database candidate)
    candidate))

(defun cleanup-old-object-copy (old-metadata new-document)
  (let* ((old-backend (normalize-storage-token
                       (jsown:val-safe old-metadata "backend")))
         (old-key (jsown:val-safe old-metadata "object_key"))
         (new-backend (document-storage-backend-name new-document))
         (new-key (document-storage-object-key new-document)))
    (when (and (stringp old-backend)
               (not (string= old-backend "couchdb"))
               (stringp old-key)
               (or (not (string= old-backend new-backend))
                   (not (and new-key (string= old-key new-key)))))
      (storage-delete (resolve-storage-backend old-backend) old-key))))

(defun transition-document (client database document-id target-tier)
  "Move DOCUMENT-ID to TARGET-TIER without exposing backend selection to clients.

The external object is written first. CouchDB is updated second. Any cleanup of
the prior external copy happens only after CouchDB points at the new placement,
so a failed transition can leak an orphan but cannot lose the canonical payload."
  (let* ((document (load-document client database document-id))
         (old-metadata (document-storage-metadata document))
         (old-tier (document-storage-tier document))
         (new-tier (normalize-storage-tier target-tier))
         (new-backend (tier-backend-name new-tier)))
    (if (and (string= old-tier new-tier)
             (string= (document-storage-backend-name document) new-backend))
        document
        (let ((stored (save-document-placement client database document new-tier)))
          (cleanup-old-object-copy old-metadata stored)
          stored))))

(defun storage-aware-upsert-document (client database document-id patch
                                      &key (max-attempts 8))
  "Optimistic document update preserving the document's storage tier."
  (star.databases.couchdb:upsert-document-update
   (lambda (id)
     (handler-case
         (load-document client database id)
       (dex:http-request-not-found () nil)))
   (lambda (candidate)
     (handler-case
         (let* ((existing-tier
                  (if (document-storage-object-key candidate)
                      (document-storage-tier candidate)
                      (tenant-default-storage-tier (document-tenant candidate))))
                (stored (save-document-placement
                         client database candidate existing-tier)))
           ;; Return the full canonical document even when CouchDB now holds a stub.
           (if (offloaded-tier-p existing-tier)
               (load-document client database document-id)
               stored))
       (dex:http-request-conflict ()
         (error 'star.databases.couchdb:document-update-store-conflict))))
   document-id
   patch
   :max-attempts max-attempts))

(defun delete-document-with-storage (client database document-id revision
                                     &key (delete-fn #'cl-couch:delete-document))
  "Delete CouchDB state first, then best-effort delete any external object copy."
  (let* ((stored (parse-json-document
                  (cl-couch:get-document client database document-id)))
         (metadata (document-storage-metadata stored))
         (backend-name (normalize-storage-token
                        (jsown:val-safe metadata "backend")))
         (object-key (jsown:val-safe metadata "object_key"))
         (result (funcall delete-fn client database document-id revision)))
    (when (and (stringp backend-name)
               (not (string= backend-name "couchdb"))
               (stringp object-key))
      (handler-case
          (storage-delete (resolve-storage-backend backend-name) object-key)
        (error (condition)
          ;; The document is already gone; leave an orphan for later GC rather
          ;; than turning a successful user delete into a misleading failure.
          (log:warn "External storage cleanup failed for ~a: ~a"
                    document-id condition))))
    result))

(defun apply-document-storage-policy (client database document-id)
  "Move DOCUMENT-ID to its server-configured tenant default tier when needed."
  (let* ((document (load-document client database document-id))
         (target-tier (tenant-default-storage-tier (document-tenant document))))
    (if (string= (document-storage-tier document) target-tier)
        document
        (transition-document client database document-id target-tier))))

(defun document-storage-lifecycle-json (document)
  "Return a client-safe lifecycle envelope for DOCUMENT."
  (let* ((metadata (document-storage-metadata document))
         (tier (document-storage-tier document)))
    (jsown:new-js
      ("document_id" (star.documents:document-id document))
      ("tenant" (document-tenant document))
      ("dataset" (or (star.documents:document-dataset document) :null))
      ("tier" tier)
      ("backend" (document-storage-backend-name document))
      ("state" (or (jsown:val-safe metadata "state") "resident"))
      ("object_key" (or (document-storage-object-key document) :null))
      ("content_sha256" (or (jsown:val-safe metadata "content_sha256") :null))
      ("updated_at" (or (jsown:val-safe metadata "updated_at") :null)))))
