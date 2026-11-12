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

(defun public-search-test-document (id tenant dataset &optional (title "CURRENT-TEXT"))
  (jsown:new-js ("_id" id) ("id" id) ("dtype" "message")
                ("tenant_id" tenant) ("dataset" dataset) ("title" title)))

(defun public-search-test-row (id)
  (jsown:new-js ("id" id)
                ("doc" (public-search-test-document id "public" "public-a" "INDEX-SECRET"))
                ("fields" (jsown:new-js ("title" "INDEX-SECRET")))))

(defun call-public-search-test (rows fetch-document &key (limit 5))
  (let ((old-search (symbol-function 'cl-couch:fts-search))
        (old-get (symbol-function 'cl-couch:get-document))
        (query nil)
        (fetched nil)
        (star:*couchdb-default-database* "canonical-test-database")
        (star.frontends.http-api::*public-search-datasets* '("public-a"))
        (star.frontends.http-api::*response* (lack.response:make-response 200))
        (star.frontends.http-api::*couchdb-pool*
          (star.databases.couchdb::make-session-aware-couchdb-pool
           :name "public-search-test" :connector (constantly :fake-client)
           :authenticator (constantly t) :session-valid-p (constantly t)
           :disconnector (constantly nil) :max-open-count 1 :max-idle-count 1)))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-couch:fts-search)
                 (lambda (client body database design index)
                   (declare (ignore client))
                   (is (string= "canonical-test-database" database))
                   (is (string= "search" design))
                   (is (string= "fts" index))
                   (setf query (jsown:parse body))
                   (jsown:to-json
                    (jsown:new-js ("rows" rows) ("total_rows" 999)
                                  ("bookmark" "next-cursor"))))
                 (symbol-function 'cl-couch:get-document)
                 (lambda (client database id &rest options)
                   (declare (ignore client options))
                   (is (string= "canonical-test-database" database))
                   (push id fetched)
                   (funcall fetch-document id)))
           (let ((body
                   (star.frontends.http-api::handle-public-search-route
                    `(("q" . "alice") ("limit" . ,(write-to-string limit))
                      ("bookmark" . "input-cursor")))))
             (values (if (stringp body) (jsown:parse body) body)
                     query (nreverse fetched)
                     (lack.response:response-status star.frontends.http-api::*response*))))
      (setf (symbol-function 'cl-couch:fts-search) old-search
            (symbol-function 'cl-couch:get-document) old-get))))

(test public-search-does-not-wait-for-index-catchup
  (multiple-value-bind (body query fetched status)
      (call-public-search-test
       (list (public-search-test-row "one"))
       (lambda (id) (public-search-test-document id "public" "public-a")))
    (is (= 200 status))
    (is (equal "ok" (jsown:val-safe query "stale")))
    (is (null (jsown:val-safe query "include_docs")))
    (is (= 5 (jsown:val query "limit")))
    (is (string= "input-cursor" (jsown:val query "bookmark")))
    (is (search "tenant_id:\"public\"" (jsown:val query "q")))
    (is (search "dataset:\"public-a\"" (jsown:val query "q")))
    (is (equal '("one") fetched))
    (is (string= "next-cursor" (jsown:val body "bookmark")))
    (is (= 1 (jsown:val body "total_rows")))
    (is (search "CURRENT-TEXT" (jsown:to-json body)))
    (is (null (search "INDEX-SECRET" (jsown:to-json body))))
    (is (null (search "tenant_id" (jsown:to-json body))))))

(test public-search-rechecks-canonical-scope-before-disclosure
  (multiple-value-bind (body query fetched status)
      (call-public-search-test
       (mapcar #'public-search-test-row '("good" "private" "moved" "missing" "deleted"))
       (lambda (id)
         (cond
           ((string= id "private") (public-search-test-document id "private" "public-a"))
           ((string= id "moved") (public-search-test-document id "public" "private-dataset"))
           ((string= id "missing") nil)
           ((string= id "deleted") (jsown:new-js ("_id" id) ("_deleted" :true)))
           (t (public-search-test-document id "public" "public-a")))))
    (declare (ignore query))
    (is (= 200 status))
    (is (= 5 (length fetched)))
    (is (equal '("good") (mapcar (lambda (row) (jsown:val row "id")) (jsown:val body "rows"))))
    (is (null (search "INDEX-SECRET" (jsown:to-json body))))))

(test public-search-canonical-failure-is-not-an-empty-success
  (multiple-value-bind (body query fetched status)
      (call-public-search-test (list (public-search-test-row "one"))
                               (lambda (id) (declare (ignore id)) (error "Backend unavailable")))
    (declare (ignore body query))
    (is (= 500 status))
    (is (equal '("one") fetched))))

(test public-search-rejects-malformed-or-substituted-current-documents
  (dolist (current (list "not-json" '("not-an-object")
                         (public-search-test-document "other-id" "public" "public-a")))
    (multiple-value-bind (body query fetched status)
        (call-public-search-test (list (public-search-test-row "one"))
                                 (lambda (id) (declare (ignore id)) current))
      (declare (ignore query fetched))
      (is (= 500 status))
      (is (null (search "INDEX-SECRET" (jsown:to-json body)))))))

(test public-search-bounds-canonical-work-and-rejects-malformed-candidates
  (dolist (rows (list (list (public-search-test-row "one") (public-search-test-row "two"))
                      (list (jsown:new-js ("id" 42)))))
    (multiple-value-bind (body query fetched status)
        (call-public-search-test rows
                                 (lambda (id) (public-search-test-document id "public" "public-a"))
                                 :limit 1)
      (declare (ignore body query))
      (is (= 500 status))
      (is (null fetched)))))

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
