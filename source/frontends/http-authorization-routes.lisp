(in-package :star.frontends.http-api)

(defun route-policy-metadata (route method)
  (list :route route
        :method method
        :correlation-id (current-correlation-id)))

(defun current-policy-principal ()
  (star.auth:current-request-principal))

(defun require-path-string (params name)
  (let ((value (query-value params name)))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       400
       "missing_path_parameter"
       (format nil "Route parameter ~a is required" name)))
    value))

(defun handle-authorized-new-document-route (params)
  (with-http-boundary ()
    (let* ((path-dtype (require-path-string params "dtype"))
           (document (require-json-object (parse-json-request))))
      (validate-document-input document :path-dtype path-dtype)
      (publish-document document)
      (jsown:to-json document))))

(defun handle-authorized-new-target-route (params)
  (with-http-boundary ()
    (let* ((actor (require-path-string params "actor"))
           (document (require-json-object (parse-json-request))))
      (setf (jsown:val document "dtype") "target"
            (jsown:val document "actor") actor)
      ;; Keep the historical target envelope as an explicit narrow exception.
      (validate-document-input
       document
       :path-dtype "target"
       :strict-schema-p nil)
      (star.authorization:authorized-publish-document
       document
       #'publish-target-document-unchecked
       :principal (current-publish-service-context)
       :actor-name actor
       :action "targets:dispatch"
       :metadata (route-policy-metadata
                  "/new/target/:actor" "POST"))
      (jsown:to-json document))))

(defun handle-authorized-bulk-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((documents (require-json-array (parse-json-request)))
           (document-count (length documents))
           (metadata (route-policy-metadata "/documents/bulk" "POST")))
      (when (> document-count star:*bulk-max-documents*)
        (signal-http-input-error
         413
         "bulk_document_limit_exceeded"
         "Bulk request exceeds the configured document limit"
         (jsown:new-js ("requested" document-count)
                       ("maximum" star:*bulk-max-documents*))))
      (loop for document in documents
            for index from 0
            do (validate-document-input document :index index))
      (star.authorization:authorize-bulk-documents!
       documents
       :principal (current-policy-principal)
       :metadata metadata)
      (if (eq :inline (bulk-request-mode document-count))
          (process-inline-bulk documents)
          (let ((job
                  (submit-bulk-ingest-job
                   documents
                   (request-principal))))
            (setf (lack.response:response-status *response*) 202)
            (jsown:to-json
             (jsown:new-js
               ("status" "accepted")
               ("job_id" (bulk-ingest-job-id job))
               ("total" document-count)
               ("status_url"
                (format nil "/documents/bulk/~a"
                        (bulk-ingest-job-id job)))
               ("correlation_id" (current-correlation-id)))))))))

(defun handle-authorized-search-route (params)
  (with-http-boundary ()
    (let* ((q (require-query-string params "q"))
           (limit (bounded-query-integer
                   params "limit" :default 25 :minimum 1))
           (dataset (query-value params "dataset"))
           (tenant (or (query-value params "tenant") "default"))
           (scoped-query
             (star.authorization:authorized-search-query
              q
              :principal (current-policy-principal)
              :requested-dataset dataset
              :requested-tenant tenant
              :metadata (route-policy-metadata "/search" "GET"))))
      (couchdb-handler (client *couchdb-pool*)
        (let* ((db star:*couchdb-default-database*)
               (bookmark (query-value params "bookmark"))
               (sort (query-value params "sort"))
               (query (jsown:new-js
                        ("q" scoped-query)
                        ("limit" limit)
                        ("include_docs" t))))
          (when sort
            (setf (jsown:val query "sort") sort))
          (when bookmark
            (setf (jsown:val query "bookmark") bookmark))
          (cl-couch:fts-search
           client
           (jsown:to-json query)
           db
           "search"
           "fts"))))))

(defun handle-authorized-document-get-route (params)
  (with-http-boundary ()
    (let ((document-id (require-path-string params "id")))
      (couchdb-handler (client *couchdb-pool*)
        (star.authorization:authorized-fetch-document
         document-id
         (lambda (id)
           (cl-couch:get-document
            client star:*couchdb-default-database* id))
         :principal (current-policy-principal)
         :metadata
         (route-policy-metadata "/document/:id" "GET"))))))

(defun handle-authorized-document-delete-route (params)
  (with-http-boundary ()
    (let ((document-id (require-path-string params "id")))
      (couchdb-handler (client *couchdb-pool*)
        (progn
          (star.authorization:authorized-delete-document
           document-id
           (lambda (id)
             (cl-couch:get-document
              client star:*couchdb-default-database* id))
           (lambda (id revision)
             (cl-couch:delete-document
              client star:*couchdb-default-database* id revision))
           :principal (current-policy-principal)
           :metadata
           (route-policy-metadata "/document/:id" "DELETE"))
          (status-msg
           (format nil "Document ~a deleted" document-id)
            'success))))))

(defun handle-authorized-document-update-route (params)
  (with-http-boundary ()
    (let* ((document-id (require-path-string params "id"))
           (patch (request-json-body)))
      (couchdb-handler (client *couchdb-pool*)
        (star.authorization:authorized-update-document
         document-id
         patch
         (lambda (id)
           (handler-case
               (cl-couch:get-document
                client star:*couchdb-default-database* id)
             (dex:http-request-not-found () nil)))
         (lambda (candidate)
           (document-update-response
            (star.databases.couchdb:couchdb-upsert-document-update
             client
             star:*couchdb-default-database*
             document-id
             candidate)))
         :principal (current-policy-principal)
         :metadata
         (route-policy-metadata "/document/:id" "PUT"))))))

(defun handle-authorized-targets-route (params)
  (with-http-boundary ()
    (let* ((actor (require-path-string params "actor"))
           (metadata (route-policy-metadata "/targets/:actor" "GET")))
      (star.authorization:authorize!
       "targets:read"
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :tenant-id "default"
        :actor-name actor)
       :metadata metadata)
      (couchdb-handler (client *couchdb-pool*)
        (let* ((view
                 (query-view
                  client
                  star:*couchdb-default-database*
                  "targets"
                  "by_actor"
                  :include-docs t
                  :key actor
                  :reduce nil))
               (rows (or (jsown:val-safe view "rows") nil))
               (documents
                 (loop for row in rows
                       for document = (jsown:val-safe row "doc")
                       when document collect document)))
          (jsown:to-json
           (star.authorization:authorized-target-documents
            documents actor "targets:read"
            :principal (current-policy-principal)
            :metadata metadata)))))))

(defun safe-view-name-p (value)
  (and (non-empty-string-p value)
       (<= (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character '(#\- #\_))))
              value)))

(defun handle-authorized-scoped-view-route (params)
  (with-http-boundary ()
    (let* ((design (require-path-string params "design"))
           (view-name (require-path-string params "view"))
           (dataset (require-query-string params "dataset"))
           (tenant (or (query-value params "tenant") "default"))
           (limit (bounded-query-integer
                   params "limit" :default 50 :minimum 1))
           (metadata (route-policy-metadata
                      "/views/:design/:view" "GET")))
      (unless (and (safe-view-name-p design)
                   (safe-view-name-p view-name))
        (signal-http-input-error
         400
         "invalid_view"
         "View identifiers contain unsupported characters"))
      (couchdb-handler (client *couchdb-pool*)
        (let ((response
                (query-view
                 client
                 star:*couchdb-default-database*
                 design
                 view-name
                 :include-docs t
                 :limit limit
                 :reduce nil)))
          (jsown:to-json
           (star.authorization:authorized-view-response
            response
            :principal (current-policy-principal)
            :requested-dataset dataset
            :requested-tenant tenant
            :metadata metadata)))))))

(defun handle-authorized-dataset-size-route (params)
  (with-http-boundary ()
    (let* ((dataset (require-query-string params "dataset"))
           (tenant (or (query-value params "tenant") "default"))
           (metadata (route-policy-metadata "/dataset-size" "GET")))
      (star.authorization:authorize!
       "views:read"
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :tenant-id tenant
        :dataset-id dataset)
       :metadata metadata)
      (couchdb-handler (client *couchdb-pool*)
        (jsown:to-json
         (dataset-size
          client
          star:*couchdb-default-database*
          :key dataset
          :include-docs nil
          :reduce t))))))

(defun unavailable-lease-response (action actor target-id params)
  (with-http-boundary ()
    (let ((tenant (or (query-value params "tenant") "default"))
          (dataset (query-value params "dataset"))
          (namespace (query-value params "namespace"))
          (program (query-value params "program")))
      (star.authorization:authorize!
       action
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :tenant-id tenant
        :dataset-id dataset
        :actor-name actor
        :target-id target-id
        :target-namespace namespace
        :program-id program)
       :metadata
       (route-policy-metadata
        (if (string= action "targets:force-release")
            "/targets/:actor/:target-id/force-release"
            "/targets/:actor/:target-id/lease")
        "POST"))
      (setf (lack.response:response-status *response*) 501)
      (status-msg "Target lease backend is not implemented"
                  'error
                  :code "lease_backend_unavailable"))))

(defun handle-authorized-target-lease-route (params)
  (unavailable-lease-response
   "targets:lease"
   (require-path-string params "actor")
   (require-path-string params "target-id")
   params))

(defun handle-authorized-target-force-release-route (params)
  (unavailable-lease-response
   "targets:force-release"
   (require-path-string params "actor")
   (require-path-string params "target-id")
   params))

(defun handle-authorized-event-replay-route (params)
  (with-http-boundary ()
    (let ((event-id (require-path-string params "event-id")))
      (star.authorization:authorize!
       "events:replay"
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :resource-id event-id)
       :metadata
       (route-policy-metadata "/events/:event-id/replay" "POST"))
      (setf (lack.response:response-status *response*) 501)
      (status-msg "Event replay backend is not implemented"
                  'error
                  :code "replay_backend_unavailable"))))

(defvar *credential-administration-action* nil)

(defun require-administrator-context ()
  (star.authorization:authorize!
   (or *credential-administration-action* "principals:manage")
   :principal (current-policy-principal)
   :metadata (route-policy-metadata "credential-administration" "INTERNAL"))
  star.auth:*request-security-context*)

(defun credential-action-handler (action handler)
  (lambda (params)
    (let ((*credential-administration-action* action))
      (funcall handler params))))

(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'handle-authorized-new-document-route)
(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'handle-authorized-new-target-route)
(setf (ningle:route *app* "/documents/bulk" :method :post)
      #'handle-authorized-bulk-route)
(setf (ningle:route *app* "/search" :method :get)
      #'handle-authorized-search-route)
(setf (ningle:route *app* "/document/:id" :method :get)
       #'handle-authorized-document-get-route)
(setf (ningle:route *app* "/document/:id" :method :put)
      #'handle-authorized-document-update-route)
(setf (ningle:route *app* "/document/:id" :method :delete)
      #'handle-authorized-document-delete-route)
(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'handle-authorized-targets-route)
(setf (ningle:route *app* "/views/:design/:view" :method :get)
      #'handle-authorized-scoped-view-route)
(setf (ningle:route *app* "/dataset-size" :method :get)
      #'handle-authorized-dataset-size-route)
(setf (ningle:route *app* "/targets/:actor/:target-id/lease" :method :post)
      #'handle-authorized-target-lease-route)
(setf (ningle:route *app* "/targets/:actor/:target-id/force-release" :method :post)
      #'handle-authorized-target-force-release-route)
(setf (ningle:route *app* "/events/:event-id/replay" :method :post)
      #'handle-authorized-event-replay-route)

(setf (ningle:route *app* "/auth/credentials" :method :get)
      (credential-action-handler
       "credentials:read" #'handle-auth-list-route))
(setf (ningle:route *app* "/auth/credentials" :method :post)
      (credential-action-handler
       "credentials:create" #'handle-auth-create-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/rotate" :method :post)
      (credential-action-handler
       "credentials:rotate" #'handle-auth-rotate-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/revoke" :method :post)
      (credential-action-handler
       "credentials:revoke" #'handle-auth-revoke-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/disable" :method :post)
      (credential-action-handler
       "credentials:disable" #'handle-auth-disable-route))
