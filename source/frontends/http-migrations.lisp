(in-package :star.frontends.http-api)

(defparameter +migration-preview-path+ "/api/v1/migrations/preview"
  "Tenant-scoped read-only schema migration preview endpoint.")

(defparameter +migration-apply-path+ "/api/v1/migrations/apply"
  "Tenant-scoped schema migration apply endpoint.")

(defun migration-resource (tenant dataset)
  (star.authorization:make-authorization-resource
   :tenant-id tenant
   :dataset-id dataset))

(defun authorize-migration-scope! (action tenant dataset method path)
  (star.authorization:authorize!
   action
   :principal (current-policy-principal)
   :resource (migration-resource tenant dataset)
   :metadata (route-policy-metadata path method)))

(defun migration-request-target-schema (requested)
  (let ((current
          (star.migrations:migration-current-schema-version)))
    (when (and requested
               (not (string= requested current)))
      (signal-http-input-error
       422
       "unsupported_migration_target"
       "Only the runtime's current immutable document schema can be targeted"
       (jsown:new-js
         ("expected" current)
         ("received" requested))))
    current))

(defun migration-candidate-source-schema (candidate)
  (star.migrations:migration-source-schema candidate))

(defun migration-candidate-matches-filter-p
    (candidate from-schema to-schema)
  (and
   (or (null from-schema)
       (let ((source
               (migration-candidate-source-schema candidate)))
         (and (stringp source)
              (string= source from-schema))))
   (let ((target (jsown:val-safe candidate "schema_version")))
     (and (stringp target)
          (string= target to-schema)))))

(defun migration-document-scope-p (document tenant dataset)
  (and
   (string=
    (star.migrations:migration-effective-tenant document)
    tenant)
   (let ((document-dataset
           (star.documents:document-value
            document "dataset" nil)))
     (and (stringp document-dataset)
          (string= document-dataset dataset)))))

(defun require-migration-document-scope (document tenant dataset)
  (unless (migration-document-scope-p document tenant dataset)
    (error
     "Migration view scope mismatch for document ~s"
     (star.documents:document-value document "_id" nil)))
  document)

(defun public-migration-document (document)
  (let ((copy
          (jsown:with-injective-reader
            (jsown:parse (jsown:to-json document)))))
    (when (jsown:keyp copy "tenant_id")
      (jsown:remkey copy "tenant_id"))
    (when (jsown:keyp copy "tenant")
      (jsown:remkey copy "tenant"))
    (when (jsown:keyp copy "extensions")
      (strip-outbox-payload-tenants
       (jsown:val copy "extensions")))
    copy))

(defun migration-preview-candidates
    (client tenant dataset limit from-schema to-schema)
  (let ((candidates
          (couchdb-migration-candidates
           client
           star:*couchdb-default-database*
           tenant
           dataset
           :limit limit
           :update t)))
    (loop for candidate in candidates
          do (require-migration-document-scope
              candidate tenant dataset)
          when (migration-candidate-matches-filter-p
                candidate from-schema to-schema)
            collect candidate)))

(defun migration-preview-json
    (tenant dataset from-schema to-schema candidates)
  (declare (ignore tenant))
  (jsown:new-js
    ("status" "ok")
    ("dataset" dataset)
    ("from_schema" (or from-schema :null))
    ("to_schema" to-schema)
    ("count" (length candidates))
    ("documents"
     (mapcar #'public-migration-document candidates))
    ("correlation_id" (current-correlation-id))))

(defun handle-migration-preview-route (params)
  (with-http-boundary ()
    (let* ((tenant (require-query-string params "tenant"))
           (dataset (require-query-string params "dataset"))
           (from-schema (query-value params "from_schema"))
           (to-schema
             (migration-request-target-schema
              (query-value params "to_schema")))
           (limit
             (bounded-query-integer
              params "limit"
              :default 50
              :minimum 1
              :maximum 100)))
      (authorize-migration-scope!
       "views:read"
       tenant dataset
       "GET"
       +migration-preview-path+)
      (couchdb-handler (client *couchdb-pool*)
        (jsown:to-json
         (migration-preview-json
          tenant dataset from-schema to-schema
          (migration-preview-candidates
           client tenant dataset limit from-schema to-schema)))))))

(defun require-body-string (body key)
  (let ((value (jsown:val-safe body key)))
    (unless (and (stringp value) (plusp (length value)))
      (signal-http-input-error
       400
       "invalid_migration_request"
       (format nil "~a must be a non-empty string" key)
       (jsown:new-js ("field" key))))
    value))

(defun optional-body-string (body key)
  (let ((value (jsown:val-safe body key)))
    (cond
      ((null value) nil)
      ((and (stringp value) (plusp (length value))) value)
      (t
       (signal-http-input-error
        400
        "invalid_migration_request"
        (format nil "~a must be a non-empty string when supplied" key)
        (jsown:new-js ("field" key)))))))

(defun body-migration-limit (body)
  (let ((value (jsown:val-safe body "limit")))
    (cond
      ((null value) 50)
      ((and (integerp value) (<= 1 value 100)) value)
      (t
       (signal-http-input-error
        400
        "invalid_migration_request"
        "limit must be an integer between 1 and 100"
        (jsown:new-js ("field" "limit")))))))

(defun body-dry-run-p (body)
  (let ((value (jsown:val-safe body "dry_run")))
    (cond
      ((null value) nil)
      ((or (eq value t) (eq value :true)) t)
      ((eq value :false) nil)
      (t
       (signal-http-input-error
        400
        "invalid_migration_request"
        "dry_run must be a JSON boolean"
        (jsown:new-js ("field" "dry_run")))))))

(defun authorize-current-migration-document
    (document tenant dataset)
  (require-migration-document-scope document tenant dataset)
  (star.authorization:authorize-document!
   "documents:write"
   document
   :principal (current-policy-principal)
   :metadata
   (route-policy-metadata
    +migration-apply-path+
    "POST")))

(defun migration-status-count (status outcomes)
  (count status outcomes
         :key #'migration-outcome-status
         :test #'eq))

(defun migration-apply-response
    (tenant dataset from-schema to-schema dry-run outcomes)
  (declare (ignore tenant))
  (jsown:new-js
    ("status" "ok")
    ("dataset" dataset)
    ("from_schema" (or from-schema :null))
    ("to_schema" to-schema)
    ("dry_run" (if dry-run :true :false))
    ("total" (length outcomes))
    ("updated" (migration-status-count :updated outcomes))
    ("ready" (migration-status-count :ready outcomes))
    ("already_current"
     (migration-status-count :already-current outcomes))
    ("stale" (migration-status-count :stale outcomes))
    ("missing" (migration-status-count :missing outcomes))
    ("rejected" (migration-status-count :rejected outcomes))
    ("results"
     (mapcar
      (lambda (outcome)
        (migration-outcome-json
         outcome
         :public-document-fn #'public-migration-document))
      outcomes))
    ("correlation_id" (current-correlation-id))))

(defun handle-migration-apply-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((body (require-json-object (parse-json-request)))
           (tenant (require-body-string body "tenant"))
           (dataset (require-body-string body "dataset"))
           (from-schema (optional-body-string body "from_schema"))
           (to-schema
             (migration-request-target-schema
              (optional-body-string body "to_schema")))
           (limit (body-migration-limit body))
           (dry-run (body-dry-run-p body)))
      (authorize-migration-scope!
       "documents:write"
       tenant dataset
       "POST"
       +migration-apply-path+)
      (couchdb-handler (client *couchdb-pool*)
        (let* ((candidates
                 (migration-preview-candidates
                  client tenant dataset limit from-schema to-schema))
               (outcomes
                 (loop for candidate in candidates
                       collect
                       (couchdb-apply-migration-candidate
                        client
                        star:*couchdb-default-database*
                        candidate
                        :write-p (not dry-run)
                        :authorize-fn
                        (lambda (current)
                          (authorize-current-migration-document
                           current tenant dataset))))))
          (jsown:to-json
           (migration-apply-response
            tenant dataset from-schema to-schema dry-run outcomes)))))))

(setf (ningle:route *app* +migration-preview-path+ :method :get)
      #'handle-migration-preview-route)

(setf (ningle:route *app* +migration-apply-path+ :method :post)
      #'handle-migration-apply-route)
