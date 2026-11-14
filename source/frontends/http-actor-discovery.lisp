(in-package :star.frontends.http-api)

(defun discover-stored-actor-manifests (params)
  "Read only the authorized tenant/dataset slice using the existing scoped view.

An actors:read-only principal retains the native catalog without storage I/O.
Document publication grants no global catalog or runtime registration authority."
  (let* ((tenant (or (query-value params "tenant") "default"))
         (dataset (or (query-value params "dataset") "starintel-system"))
         (principal (current-policy-principal))
         (metadata (route-policy-metadata "/v1/actors" "GET"))
         (resource-scope-valid
           (and (non-empty-string-p tenant) (<= (length tenant) 512)
                (non-empty-string-p dataset) (<= (length dataset) 512)))
         (decision
           (progn
             (unless resource-scope-valid
               (signal-http-input-error 400 "invalid_actor_manifest_scope"
                                        "Tenant and dataset must be bounded non-empty strings"))
           (star.authorization:authorize
            "documents:read" :principal principal :metadata metadata
            :resource (star.authorization:make-authorization-resource
                       :tenant-id tenant :dataset-id dataset)))))
    (unless (star.authorization:authorization-decision-allowed-p decision)
      (return-from discover-stored-actor-manifests nil))
    (couchdb-handler (client *couchdb-pool*)
      (let* ((response
               (query-view client star:*couchdb-default-database*
                           "data" "count_by_tenant_dataset_dtype"
                           :key (list tenant dataset "actor-manifest")
                           :reduce nil :include-docs t :limit 129))
             (rows (jsown:val response "rows")))
        (when (> (length rows) 128)
          (signal-http-input-error 413 "actor_manifest_limit"
                                   "Actor manifest scope exceeds the bounded catalog size"))
        (loop for row across (coerce rows 'vector)
              for document = (jsown:val row "doc")
              collect (progn
                        (star.authorization:authorize-document!
                         "documents:read" document :principal principal :metadata metadata)
                        (unless (and (equal tenant (or (star.documents:document-value document "tenant_id")
                                                       (star.documents:document-value document "tenant")))
                                     (equal dataset (star.documents:document-dataset document))
                                     (equal "actor-manifest" (star.documents:document-dtype document)))
                          (signal-http-input-error 500 "actor_manifest_scope_mismatch"
                                                   "Stored actor manifest view returned an out-of-scope record"))
                        document))))))

(defun handle-actor-discovery-route (params)
  (with-http-boundary ()
    (set-cache-control "no-store")
    (jsown:to-json
     (star.actors:actor-registry-document
      :documents (discover-stored-actor-manifests params)))))

(mount-http-operation "actors.list" #'handle-actor-discovery-route)
