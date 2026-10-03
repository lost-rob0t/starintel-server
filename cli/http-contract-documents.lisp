(in-package :star.http.contract)

;; Versioned, canonical StarIntel document operations shared by the server
;; and the generated client. This file is loaded last so the operations and
;; their schemas can rely on every helper of the base contract and the final
;; normalization layer.

(defparameter +document-request-schema+
  (object-schema
   (list
    (cons "_id" (string-schema :min-length 1
                               :description "Stable StarIntel document identifier."))
    (cons "dataset" (string-schema :min-length 1))
    (cons "dtype" (string-schema :min-length 1))
    (cons "schema_version" (string-schema :min-length 1))
    (cons "version" (integer-schema :minimum 0))
    (cons "date_added" (string-schema))
    (cons "date_updated" (string-schema))
    (cons "sources" (array-schema (generic-object-schema)))
    (cons "evidence" (array-schema (generic-object-schema)))
    (cons "data" (generic-object-schema))
    (cons "extensions" (generic-object-schema)))
   :required '("_id" "dataset" "dtype" "schema_version")
   :additional-properties t
   :description "StarIntel v0.9 document accepted by the canonical boundary."))

(defparameter +document-bulk-request-schema+
  (array-schema
   +document-request-schema+
   :description "Bounded batch of StarIntel v0.9 documents."))

(defparameter +document-update-request-schema+
  (generic-object-schema
   "Deep-merge patch onto the current document. The HTTP boundary deep-merges
the patch, validates the merged candidate against the StarIntel v0.9 schema,
and persists it only when valid."))

(defparameter +document-update-outcome-schema+
  (object-schema
   (list
    (cons "status" (string-schema
                    :description "One of created, updated, or duplicate."))
    (cons "attempts" (integer-schema :minimum 0))
    (cons "code" (string-schema))
    (cons "reason" (string-schema))
    (cons "document" (generic-object-schema)))
   :required '("status" "attempts" "document")
   :additional-properties t
   :description "Result of one optimistic deep-merge document update."))

(defparameter +document-bulk-inline-response-schema+
  (object-schema
   (list
    (cons "total" (integer-schema :minimum 0))
    (cons "succeeded" (integer-schema :minimum 0))
    (cons "failed" (integer-schema :minimum 0))
    (cons "correlation_id" (string-schema)))
   :required '("total" "succeeded" "failed" "correlation_id")
   :additional-properties nil
   :description "Synchronous result of a bounded inline bulk request."))

(defparameter +document-bulk-accepted-response-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "job_id" (string-schema :min-length 1))
    (cons "total" (integer-schema :minimum 0))
    (cons "status_url" (string-schema :min-length 1))
    (cons "correlation_id" (string-schema)))
   :required '("status" "job_id" "total" "status_url" "correlation_id")
   :additional-properties nil
   :description "Asynchronous bulk ingest job acceptance."))

(defparameter +status-envelope-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "msg" (string-schema))
    (cons "correlation_id" (string-schema)))
   :required '("status" "msg" "correlation_id")
   :additional-properties t
   :description "Client-safe status envelope."))

(defparameter +document-search-query-parameters+
  (list
   (list :name "q" :required t
         :schema (string-schema :min-length 1
                                :description "Full-text search query."))
   (list :name "limit"
         :schema (integer-schema :minimum 1
                                 :description "Maximum number of hits."))
   (list :name "bookmark"
         :schema (string-schema :description "Search pagination bookmark."))
   (list :name "sort")
   (list :name "dataset")
   (list :name "tenant")))

(upsert-http-operation
 (make-http-operation
  :id "documents.create"
  :client-name "document-create"
  :method :post
  :path "/api/v1/documents"
  :summary "Ingest one StarIntel document through the canonical boundary"
  :tags '("documents")
  :authority :authenticated
  :scopes '("documents:write")
  :request-schema +document-request-schema+
  :responses (append
              (list
               (response 200 "Document accepted for asynchronous ingestion."
                         +document-request-schema+))
              (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.bulk.create"
  :client-name "document-bulk-create"
  :method :post
  :path "/api/v1/documents/bulk"
  :summary "Ingest a bounded batch of StarIntel documents"
  :tags '("documents")
  :authority :authenticated
  :scopes '("documents:bulk")
  :request-schema +document-bulk-request-schema+
  :responses (append
              (list
               (response 200 "Inline bulk result."
                         +document-bulk-inline-response-schema+)
               (response 202 "Bulk ingest job accepted."
                         +document-bulk-accepted-response-schema+))
              (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.get"
  :client-name "document-get"
  :method :get
  :path "/api/v1/documents/:id"
  :summary "Read one StarIntel document by id"
  :tags '("documents")
  :authority :authenticated
  :scopes '("documents:read")
  :path-parameters '("id")
  :responses (append
              (list
               (response 200 "The stored StarIntel document."
                         (generic-object-schema)))
              (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.update"
  :client-name "document-update"
  :method :put
  :path "/api/v1/documents/:id"
  :summary "Deep-merge a patch into one StarIntel document"
  :tags '("documents")
  :authority :authenticated
  :scopes '("documents:write")
  :path-parameters '("id")
  :request-schema +document-update-request-schema+
  :responses (append
              (list
               (response 200 "Validated deep-merge update outcome."
                         +document-update-outcome-schema+))
              (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.delete"
  :client-name "document-delete"
  :method :delete
  :path "/api/v1/documents/:id"
  :summary "Delete one StarIntel document by id"
  :tags '("documents")
  :authority :authenticated
  :scopes '("documents:delete")
  :path-parameters '("id")
  :responses (append
              (list
               (response 200 "Deletion receipt." +status-envelope-schema+))
              (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "documents.search"
  :client-name "document-search"
  :method :get
  :path "/api/v1/documents/search"
  :summary "Full-text search scoped to the caller's datasets and tenants"
  :tags '("documents" "search")
  :authority :authenticated
  :scopes '("search:read")
  :query-parameters +document-search-query-parameters+
  :responses (append
              (list
               (response 200 "Full-text search hits."
                         (generic-object-schema)))
              (standard-errors))))

;; Re-run the schema literal normalization over the whole contract so the
;; operations registered by this file (and by files loaded before the final
;; normalization pass) emit JSON boolean literals instead of bare lists.
(dolist (operation *http-operations*)
  (normalize-schema-json-values (http-operation-request-schema operation))
  (dolist (response (http-operation-responses operation))
    (normalize-schema-json-values (getf response :schema))))
