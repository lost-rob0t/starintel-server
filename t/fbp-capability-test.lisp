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
    (is (string= (string-downcase
                  (symbol-name (star.http.contract:http-operation-authority operation)))
                 (jsown:val node "authority")))
    (is (member "targets:dispatch" (jsown:val node "scopes") :test #'string=))
    (is (equal (star.http.contract:http-operation-idempotency operation)
               (jsown:val node "idempotency")))))

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
