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
    (is (eq :false
            (capability-feature-value document "target_leases")))
    (is (eq :false
            (capability-feature-value document "streams")))
    (is (eq :false
            (capability-feature-value document "openapi")))))

(test api-v1-capabilities-identifies-versioned-and-legacy-routes
  (let* ((document
           (star.frontends.http-api::capabilities-document))
         (capabilities
           (capability-endpoint-by-id document "capabilities"))
         (legacy-document
           (capability-endpoint-by-id document "document_create")))
    (is capabilities)
    (is legacy-document)
    (is (string= "GET" (jsown:val capabilities "method")))
    (is (string= "/api/v1/capabilities"
                 (jsown:val capabilities "path")))
    (is (eq :false (jsown:val capabilities "legacy")))
    (is (eq :true (jsown:val legacy-document "legacy")))))

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
