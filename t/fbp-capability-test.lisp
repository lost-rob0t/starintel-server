(in-package :star-server-tests)

(def-suite fbp-capability-tests)
(in-suite fbp-capability-tests)

(defun json-array-values (value)
  (if (and (consp value) (eq (first value) :array)) (rest value) value))

(test fbp-descriptors-are-a-bijection-over-http-contract
  (let* ((operations (star.http.contract:all-http-operations))
         (catalog (star.http.contract:fbp-node-catalog-document))
         (nodes (json-array-values (jsown:val catalog "nodes"))))
    (is (= (length operations) (length nodes)))
    (is (equal (mapcar #'star.http.contract:http-operation-id operations)
               (mapcar (lambda (node) (jsown:val node "operation_id")) nodes)))))

(test target-node-preserves-authoritative-contract
  (let* ((operation (star.http.contract:find-http-operation "targets.create"))
         (node (star.http.contract:operation-fbp-node-object operation)))
    (is (string= "starintel.operation/targets.create" (jsown:val node "id")))
    (is (string= "targets.create" (jsown:val node "operation_id")))
    (is (string= (string-downcase
                  (symbol-name (star.http.contract:http-operation-method operation)))
                 (jsown:val node "method")))
    (is (string= (star.http.contract:http-operation-path operation)
                 (jsown:val node "path")))
    (is (string= (star.http.contract:openapi-path operation)
                 (jsown:val node "openapi_path")))
    (is (equal (star.http.contract:http-operation-path-parameters operation)
               (jsown:val node "path_parameters")))
    (is (equal (star.http.contract:http-operation-request-schema operation)
               (jsown:val node "request_schema")))
    (is (= (length (star.http.contract:http-operation-responses operation))
           (length (json-array-values (jsown:val node "responses")))))
    (is (string= (string-downcase
                  (symbol-name (star.http.contract:http-operation-authority operation)))
                 (jsown:val node "authority")))
    (is (member "targets:dispatch" (jsown:val node "scopes") :test #'string=))
    (is (equal (star.http.contract:http-operation-idempotency operation)
               (jsown:val node "idempotency")))))

(test descriptor-preserves-parameter-routing-metadata
  (let* ((search-operation
           (star.http.contract:find-http-operation "documents.search"))
         (search-node
           (star.http.contract:operation-fbp-node-object search-operation))
         (query-parameters
           (json-array-values (jsown:val search-node "query_parameters")))
         (get-operation
           (star.http.contract:find-http-operation "documents.get"))
         (get-node
           (star.http.contract:operation-fbp-node-object get-operation)))
    (is (= (length (star.http.contract:http-operation-query-parameters
                    search-operation))
           (length query-parameters)))
    (is (every (lambda (parameter)
                 (and (stringp (jsown:val parameter "name"))
                      (listp (jsown:val parameter "schema"))))
               query-parameters))
    (is (equal (star.http.contract:http-operation-path-parameters get-operation)
               (jsown:val get-node "path_parameters")))))

(test zero-required-input-operation-has-required-trigger
  (let* ((operation (star.http.contract:find-http-operation "health.get"))
         (node (star.http.contract:operation-fbp-node-object operation))
         (inputs (json-array-values (jsown:val node "inputs")))
         (trigger (find "trigger" inputs
                        :key (lambda (input) (jsown:val input "name"))
                        :test #'string=)))
    (is (not (null trigger)))
    (is (eq t (jsown:val trigger "required")))
    (is (string= "control" (jsown:val trigger "source")))))

(test descriptor-requires-operation-and-credential-reference
  (let* ((operation (star.http.contract:find-http-operation "targets.create"))
         (node (star.http.contract:operation-fbp-node-object operation))
         (config-schema (jsown:val node "config_schema"))
         (required (json-array-values (jsown:val config-schema "required"))))
    (is (member "operation" required :test #'string=))
    (is (member "credentialReference" required :test #'string=))
    (is-false (jsown:val config-schema "additionalProperties"))))

(test manifest-preserves-summary-tags-query-schemas-and-secret-annotations
  (let* ((manifest (star.http.contract:client-manifest-document))
         (operations (json-array-values (jsown:val manifest "operations")))
         (login (find "auth.login" operations
                      :key (lambda (item) (jsown:val item "operation_id"))
                      :test #'string=))
         (password (jsown:val (jsown:val (jsown:val login "request_schema") "properties")
                              "password")))
    (is (stringp (jsown:val login "summary")))
    (is (listp (jsown:val login "tags")))
    (is (eq t (jsown:val password "x-starintel-secret")))
    (is (eq t (jsown:val password "writeOnly")))))

(test descriptors-contain-references-never-secret-values
  (let ((json (star.http.contract:fbp-node-catalog-json)))
    (is (search "credentialReference" json))
    (is-false (search "star_sk_v1_" json))
    (is-false (search "rabbit_password" (string-downcase json)))
    (is-false (search "couchdb_password" (string-downcase json)))))
