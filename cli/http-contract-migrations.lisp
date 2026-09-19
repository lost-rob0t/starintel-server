(in-package :star.http.contract)

(defun migration-limit-schema ()
  "Return the migration batch-limit schema without widening the shared helper API."
  (let ((schema
          (integer-schema
           :minimum 1
           :description "Maximum migration candidates returned; runtime maximum is 100.")))
    (setf (jsown:val schema "maximum") 100)
    schema))

(defparameter +migration-preview-query-parameters+
  (list
   (list :name "tenant"
         :required t
         :schema
         (string-schema
          :min-length 1
          :description "Tenant whose documents may be previewed."))
   (list :name "dataset"
         :required t
         :schema
         (string-schema
          :min-length 1
          :description "Dataset whose documents may be previewed."))
   (list :name "from_schema"
         :schema
         (string-schema
          :min-length 1
          :description "Optional source-schema filter."))
   (list :name "to_schema"
         :schema
         (string-schema
          :min-length 1
          :description
          "Optional target schema; must equal the runtime current schema."))
   (list :name "limit"
         :schema (migration-limit-schema))))

(defparameter +migration-apply-request-schema+
  (object-schema
   (list
    (cons "tenant" (string-schema :min-length 1))
    (cons "dataset" (string-schema :min-length 1))
    (cons "from_schema" (string-schema :min-length 1))
    (cons "to_schema" (string-schema :min-length 1))
    (cons "limit" (migration-limit-schema))
    (cons "dry_run" (boolean-schema)))
   :required '("tenant" "dataset")
   :additional-properties nil
   :description
   "Bounded tenant-scoped request to migrate documents to the current schema."))

(defparameter +migration-response-schema+
  (generic-object-schema
   "Migration preview or apply result. Migrated document values use the canonical StarIntel document envelope."))

(upsert-http-operation
 (make-http-operation
  :id "migrations.preview"
  :client-name "migration-preview"
  :method :get
  :path "/api/v1/migrations/preview"
  :summary "Preview Prolog-derived document schema migrations"
  :tags '("migrations" "documents")
  :authority :authenticated
  :scopes '("views:read")
  :query-parameters +migration-preview-query-parameters+
  :responses
  (append
   (list
    (response
     200
     "Tenant-scoped migrated JSON candidates without writes."
     +migration-response-schema+))
   (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "migrations.apply"
  :client-name "migration-apply"
  :method :post
  :path "/api/v1/migrations/apply"
  :summary "Apply Prolog-derived schema migrations through optimistic CAS"
  :tags '("migrations" "documents")
  :authority :authenticated
  :scopes '("documents:write")
  :request-schema +migration-apply-request-schema+
  :responses
  (append
   (list
    (response
     200
     "Bounded migration results, including dry-run, stale, and rejected outcomes."
     +migration-response-schema+))
   (standard-errors))))

(dolist (operation *http-operations*)
  (normalize-schema-json-values
   (http-operation-request-schema operation))
  (dolist (response (http-operation-responses operation))
    (normalize-schema-json-values
     (getf response :schema))))
