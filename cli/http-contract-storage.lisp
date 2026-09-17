(in-package :star.http.contract)

(defparameter +document-storage-tier-schema+
  (string-schema
   :description "Logical document data tier: hot, warm, cold, or archive."))

(defparameter +document-lifecycle-request-schema+
  (object-schema
   (list (cons "tier" +document-storage-tier-schema+))
   :required '("tier")
   :additional-properties nil
   :description
   "Move a document to a logical tier. Physical backend selection is server-owned."))

(defparameter +document-lifecycle-response-schema+
  (object-schema
   (list
    (cons "document_id" (string-schema :min-length 1))
    (cons "tenant" (string-schema :min-length 1))
    (cons "dataset" (string-schema))
    (cons "tier" +document-storage-tier-schema+)
    (cons "backend" (string-schema :min-length 1))
    (cons "state" (string-schema :min-length 1))
    (cons "object_key" (string-schema))
    (cons "content_sha256" (string-schema))
    (cons "updated_at" (string-schema)))
   :required '("document_id" "tenant" "tier" "backend" "state")
   :additional-properties t
   :description "Server-owned document placement and lifecycle state."))

(upsert-http-operation
 (make-http-operation
  :id "documents.lifecycle.get"
  :client-name "document-lifecycle-get"
  :method :get
  :path "/api/v1/documents/:id/lifecycle"
  :summary "Read one tenant document's storage lifecycle state"
  :tags '("documents" "storage")
  :authority :authenticated
  :scopes '("documents:read")
  :path-parameters '("id")
  :responses
  (append
   (list
    (response 200 "Document lifecycle state."
              +document-lifecycle-response-schema+))
   (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.lifecycle.update"
  :client-name "document-lifecycle-update"
  :method :put
  :path "/api/v1/documents/:id/lifecycle"
  :summary "Move one tenant document between logical data tiers"
  :tags '("documents" "storage")
  :authority :authenticated
  :scopes '("documents:write")
  :path-parameters '("id")
  :request-schema +document-lifecycle-request-schema+
  :responses
  (append
   (list
    (response 200 "Updated document lifecycle state."
              +document-lifecycle-response-schema+))
   (standard-errors))))

(dolist (operation *http-operations*)
  (normalize-schema-json-values (http-operation-request-schema operation))
  (dolist (response (http-operation-responses operation))
    (normalize-schema-json-values (getf response :schema))))
