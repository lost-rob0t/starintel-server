(in-package :star-server-tests)

(in-suite http-boundary-tests)

(defun capability-data-value (document key)
  (jsown:val (jsown:val document "data") key))

(defun capability-feature-value (document key)
  (jsown:val (capability-data-value document "features") key))

(defun capability-endpoint-by-id (document id)
  (find id
        (capability-data-value document "endpoints")
        :key (lambda (endpoint)
               (jsown:val endpoint "id"))
        :test #'string=))

(test api-v1-capabilities-has-stable-envelope
  (let* ((document
           (star.frontends.http-api::capabilities-document))
         (data (jsown:val document "data"))
         (schemas (jsown:val data "schema_revisions"))
         (build (jsown:val data "build")))
    (is (string= "ok" (jsown:val document "status")))
    (is (string= "v1" (jsown:val schemas "api")))
    (is (string= starintel:+starintel-doc-version+
                 (jsown:val schemas "document")))
    (is (string= "starintel-gserver"
                 (jsown:val build "service")))
    (is (string= star:*star-server-version*
                 (jsown:val build "version")))))

(test api-v1-capabilities-advertises-only-current-features
  (let ((document
          (star.frontends.http-api::capabilities-document)))
    (is (eq :true
            (capability-feature-value document "documents")))
    (is (eq :true
            (capability-feature-value document "search")))
    (is (eq :true
            (capability-feature-value document "targets")))
    (is (eq :true
            (capability-feature-value document "stats")))
    (is (eq :false
            (capability-feature-value document "target_leases")))
    (is (eq :false
            (capability-feature-value document "streams")))
    (is (eq :true
            (capability-feature-value document "openapi")))))

(test api-v1-capabilities-identifies-versioned-and-legacy-routes
  (let* ((document
           (star.frontends.http-api::capabilities-document))
         (capabilities
           (capability-endpoint-by-id document "capabilities"))
         (public-search
           (capability-endpoint-by-id document "public_search"))
         (stats
           (capability-endpoint-by-id document "stats"))
         (legacy-document
           (capability-endpoint-by-id document "document_create")))
    (is (not (null capabilities)))
    (is (not (null public-search)))
    (is (not (null stats)))
    (is (not (null legacy-document)))
    (is (string= "GET" (jsown:val capabilities "method")))
    (is (string= "/api/v1/capabilities"
                 (jsown:val capabilities "path")))
    (is (eq :false (jsown:val capabilities "legacy")))
    (is (eq :false (jsown:val public-search "legacy")))
    (is (eq :false (jsown:val stats "legacy")))
    (is (eq :true (jsown:val legacy-document "legacy")))
    (is (string= "public" (jsown:val public-search "authority")))
    (is (string= "public" (jsown:val stats "authority")))
    (is (string= "authenticated" (jsown:val legacy-document "authority")))))

(test api-v1-public-read-surface-is-public
  (dolist (path '("/api/v1/capabilities"
                  "/api/v1/search"
                  "/api/v1/stats"))
    (is (member path star:*auth-public-paths* :test #'string=)))
  (dolist (operation-id '("public.search.get" "stats.get"))
    (let ((operation (star.http.contract:find-http-operation operation-id)))
      (is (eq :public
              (star.http.contract:http-operation-authority operation))))))

(test api-v1-mutation-routes-remain-authenticated
  (dolist (path '("/new/document/message"
                  "/new/target/nmap"
                  "/documents/bulk"))
    (is (null (member path star:*auth-public-paths* :test #'string=))))
  (let ((document-create
          (capability-endpoint-by-id
           (star.frontends.http-api::capabilities-document)
           "document_create"))
        (target-create
          (capability-endpoint-by-id
           (star.frontends.http-api::capabilities-document)
           "target_create")))
    (is (string= "authenticated" (jsown:val document-create "authority")))
    (is (equal '("documents:write")
               (jsown:val document-create "scopes")))
    (is (string= "authenticated" (jsown:val target-create "authority")))
    (is (equal '("targets:dispatch")
               (jsown:val target-create "scopes")))))

(test api-v1-public-search-uses-server-owned-scope
  (let ((star.frontends.http-api::*public-search-datasets*
          '("public-a" "public-b")))
    (let ((query
            (star.frontends.http-api::public-search-authorized-query
             "alice")))
      (is (search "tenant_id:\"public\"" query))
      (is (search "dataset:\"public-a\"" query))
      (is (search "dataset:\"public-b\"" query))
      (is (null (search "private-dataset" query))))))

(test api-v1-public-search-fails-closed-without-explicit-datasets
  (dolist (datasets '(nil ("*")))
    (let ((star.frontends.http-api::*public-search-datasets* datasets))
      (signals error
        (star.frontends.http-api::public-search-authorized-query
         "alice")))))

(test api-v1-public-search-keeps-caller-query-inside-scope
  (let ((star.frontends.http-api::*public-search-datasets* '("public")))
    (let ((query
            (star.frontends.http-api::public-search-authorized-query
             "alice) OR (tenant_id:private")))
      (is (search "alice\\) OR \\(tenant_id\\:private" query))
      (is (search "tenant_id:\"public\"" query))
      (is (null (search "(alice) OR" query))))))

(test api-v1-public-stats-are-aggregate-only
  (let* ((star.frontends.http-api::*public-search-datasets* '("public-a"))
         (response
           (jsown:new-js
             ("rows"
              (list
               (jsown:new-js ("key" '("public" "public-a" "person")) ("value" 10))
               (jsown:new-js ("key" '("public" "public-a" "relation")) ("value" 20))
               (jsown:new-js ("key" '("public" "public-a" "target")) ("value" 3))
               (jsown:new-js
                 ("key" '("public" "public-a" "investigation-target")) ("value" 2))))))
         (document
           (star.frontends.http-api::public-stats-document-from-view-responses
            (list response)))
         (data (jsown:val document "data"))
         (documents (jsown:val data "documents"))
         (by-dtype (jsown:val documents "by_dtype"))
         (targets (jsown:val data "targets")))
    (is (string= "ok" (jsown:val document "status")))
    (is (= 35 (jsown:val documents "total")))
    (is (= 10 (jsown:val by-dtype "person")))
    (is (= 20 (jsown:val by-dtype "relation")))
    (is (= 5 (jsown:val targets "total")))
    (is (null (jsown:val-safe data "documents_raw")))
    (is (null (search "_id" (jsown:to-json document))))))

(test api-v1-public-stats-rejects-cross-tenant-view-row
  (let ((star.frontends.http-api::*public-search-datasets* '("public-a")))
    (signals error
      (star.frontends.http-api::public-stats-document-from-view-responses
       (list (jsown:new-js
              ("rows" (list (jsown:new-js
                              ("key" '("private" "public-a" "person"))
                              ("value" 1))))))))))

(test api-v1-public-stats-does-not-wait-for-index-catch-up
  (let* ((datasets '("stats-public-a" "stats-public-b"))
         (requests nil)
         (star.frontends.http-api::*public-search-datasets* datasets)
         (star.frontends.http-api::*response* (lack.response:make-response 200))
         (star.frontends.http-api::*couchdb-pool*
           (anypool:make-pool
            :name "stats-request-test"
            :connector #'test-view-client
            :disconnector (lambda (client) (declare (ignore client)))
            :max-open-count 1 :max-idle-count 1))
         (star.databases.couchdb:*couchdb-view-transport*
           (lambda (client request)
             (declare (ignore client))
             (push request requests)
             (let* ((uri (quri:uri (star.databases.couchdb:couchdb-view-request-uri request)))
                    (parameters (quri:url-decode-params (quri:uri-query uri)))
                    (start (jsown:parse (cdr (assoc "startkey" parameters :test #'string=))))
                    (dataset (second start)))
               (jsown:to-json
                (jsown:new-js
                  ("rows" (list (jsown:new-js
                                  ("key" (list "public" dataset "target"))
                                  ("value" (if (string= dataset (first datasets)) 3 7))))))))))
         (body (star.frontends.http-api::handle-public-stats-route nil)))
    (is (= 200 (lack.response:response-status star.frontends.http-api::*response*)))
    (is (= 2 (length requests)))
    (loop for request in (reverse requests)
          for dataset in datasets do
      (let* ((uri (quri:uri (star.databases.couchdb:couchdb-view-request-uri request)))
             (parameters (quri:url-decode-params (quri:uri-query uri))))
        (is (eq :get (star.databases.couchdb:couchdb-view-request-method request)))
        (is (search "/_design/data/_view/count_by_tenant_dataset_dtype" (quri:uri-path uri)))
        (is (string= "lazy" (cdr (assoc "update" parameters :test #'string=))))
        (is (string= (jsown:to-json (list "public" dataset))
                     (cdr (assoc "startkey" parameters :test #'string=))))
        (is (string= (jsown:to-json (list "public" dataset (jsown:new-js)))
                     (cdr (assoc "endkey" parameters :test #'string=))))
        (dolist (expected '(("limit" . "1000") ("include_docs" . "false")
                            ("reduce" . "true") ("group" . "true")))
          (is (string= (cdr expected)
                       (cdr (assoc (car expected) parameters :test #'string=)))))))
    (let* ((document (jsown:parse body))
           (data (jsown:val document "data")))
      (is (string= "ok" (jsown:val document "status")))
      (is (= 10 (jsown:val (jsown:val data "documents") "total")))
      (is (= 10 (jsown:val (jsown:val data "targets") "total")))
      (is (null (search "stats-public-" body)))
      (is (null (search "_id" body))))))

(test api-v1-public-stats-rejects-unconfigured-scope-before-transport
  (dolist (datasets '(nil ("*")))
    (let* ((calls 0)
           (star.frontends.http-api::*public-search-datasets* datasets)
           (star.frontends.http-api::*response* (lack.response:make-response 200))
           (star.databases.couchdb:*couchdb-view-transport*
             (lambda (client request)
               (declare (ignore client request))
               (incf calls)
               (error "Unconfigured scope must not reach CouchDB")))
           (body (star.frontends.http-api::handle-public-stats-route nil)))
      (is (= 503 (lack.response:response-status star.frontends.http-api::*response*)))
      (is (search "public_scope_unconfigured" body))
      (is (zerop calls)))))

(test api-v1-capabilities-is-public-discovery
  (is (member "/api/v1/capabilities"
              star:*auth-public-paths*
              :test #'string=)))

(test api-v1-capabilities-does-not-leak-secrets
  (let ((star:*couchdb-password* "capabilities-db-secret")
        (star:*rabbit-password* "capabilities-rabbit-secret")
        (star:*auth-pepper* "capabilities-pepper-secret")
        (star:*auth-bootstrap-secret* "capabilities-bootstrap-secret"))
    (let ((json (star.frontends.http-api::capabilities-json)))
      (is (null (search "capabilities-db-secret" json)))
      (is (null (search "capabilities-rabbit-secret" json)))
      (is (null (search "capabilities-pepper-secret" json)))
      (is (null (search "capabilities-bootstrap-secret" json))))))

(test api-v1-capabilities-reports-auth-mode-without-auth-material
  (let ((star:*auth-mode* "api-key"))
    (let* ((document
             (star.frontends.http-api::capabilities-document))
           (authentication
             (capability-data-value document "authentication")))
      (is (equal '("api-key")
                 (jsown:val authentication "modes")))
      (is (eq :false
              (jsown:val authentication
                         "capabilities_endpoint_requires_auth"))))))
