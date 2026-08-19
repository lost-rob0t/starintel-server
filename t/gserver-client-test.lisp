(in-package :star-server-tests)

(def-suite gserver-client-tests
  :description "Machine-readable HTTP contract, generated client runtime, and local admin boundary")

(in-suite gserver-client-tests)

(defun fake-json-response (status body &key (correlation-id "corr-test"))
  (star.api.client::make-client-response
   :status status
   :headers (list (cons "content-type" "application/json")
                  (cons "x-correlation-id" correlation-id))
   :body body
   :uri "http://example.test"
   :correlation-id correlation-id
   :content-type "application/json"))

(test machine-readable-contract-covers-auth-management-surface
  (let ((operations (star.http.contract:all-http-operations)))
    (is (>= (length operations) 16))
    (is (eq :post
            (star.http.contract:http-operation-method
             (star.http.contract:find-http-operation "auth.login"))))
    (is (string= "/auth/users/:username/password"
                 (star.http.contract:http-operation-path
                  (star.http.contract:find-http-operation
                   "auth.users.password.reset"))))
    (is (member "/openapi.json" star:*auth-public-paths* :test #'string=))
    (is (member "/client-manifest.json" star:*auth-public-paths* :test #'string=)))
  (let* ((document (jsown:parse (star.http.contract:openapi-json)))
         (paths (jsown:val document "paths")))
    (is (string= "3.1.2" (jsown:val document "openapi")))
    (is (jsown:val-safe paths "/auth/login"))
    (is (jsown:val-safe paths "/auth/users/{username}/password"))))

(test generated-operation-functions-exist
  (dolist (name '("REQUEST-AUTH-LOGIN"
                  "REQUEST-AUTH-CREATE-USER"
                  "REQUEST-AUTH-ROTATE-CREDENTIAL"
                  "REQUEST-OPENAPI-DOCUMENT"))
    (multiple-value-bind (symbol status)
        (find-symbol name :star.api.client)
      (is (eq :external status))
      (is-true (fboundp symbol)))))

(test login-is-structured-and-does-not-mutate-client-authentication
  (let ((captured nil))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (setf captured request)
                (fake-json-response
                 200
                 "{\"api_key\":\"star_sk_v1_test_deadbeef\",\"credential\":{\"credential_id\":\"cred-1\"},\"user\":{\"username\":\"alice\"},\"correlation_id\":\"corr-login\"}"
                 :correlation-id "corr-login"))))
           (client (star.api.client:make-star-client
                    :base-url "http://example.test"
                    :transport transport))
           (result (star.api.client:login client "alice" "secret-password")))
      (is (typep result 'star.api.client:login-result))
      (is (string= "star_sk_v1_test_deadbeef"
                   (star.api.client:login-result-api-key result)))
      (is (string= "alice"
                   (jsown:val (star.api.client:login-result-user result)
                              "username")))
      (is (eq :post (star.api.client:client-request-method captured)))
      (is (search "/auth/login" (star.api.client:client-request-uri captured)))
      (is (null (assoc "Authorization"
                       (star.api.client:client-request-headers captured)
                       :test #'string-equal)))
      (is (search "REDACTED" (prin1-to-string result)))
      (is (null (search "star_sk_v1_test_deadbeef"
                        (prin1-to-string result)))))))

(test authenticated-derived-client-adds-bearer-without-mutating-base-client
  (let ((requests nil))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (push request requests)
                (fake-json-response
                 200
                 "{\"principal_id\":\"alice\",\"correlation_id\":\"corr-auth\"}"))))
           (base (star.api.client:make-star-client
                  :base-url "http://example.test"
                  :transport transport))
           (authenticated
             (star.api.client:client-with-api-key base "star_sk_v1_demo_secret")))
      (star.api.client:auth-context authenticated)
      (let ((request (first requests)))
        (is (string= "Bearer star_sk_v1_demo_secret"
                     (cdr (assoc "Authorization"
                                 (star.api.client:client-request-headers request)
                                 :test #'string-equal)))))
      (is (null (search "star_sk_v1_demo_secret" (prin1-to-string authenticated))))
      (is (null (search "star_sk_v1_demo_secret" (prin1-to-string base)))))))

(test client-normalizes-http-errors-and-keeps-correlation
  (let* ((transport
           (star.api.client:make-function-transport
            (lambda (request)
              (declare (ignore request))
              (fake-json-response
               403
               "{\"status\":\"error\",\"msg\":\"Denied\",\"code\":\"missing_scope\",\"correlation_id\":\"corr-denied\"}"
               :correlation-id "corr-denied"))))
         (client (star.api.client:make-star-client
                  :base-url "http://example.test"
                  :transport transport)))
    (handler-case
        (progn
          (star.api.client:auth-context client)
          (fail "Expected authorization error"))
      (star.api.client:client-authorization-error (condition)
        (is (= 403 (star.api.client:client-http-error-status condition)))
        (is (string= "missing_scope"
                     (star.api.client:client-http-error-code condition)))
        (is (string= "corr-denied"
                     (star.api.client:client-http-error-correlation-id condition)))))))

(test local-admin-user-helpers-reuse-auth-domain-services
  (let* ((star:*auth-pepper* "admin-test-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (store (star.auth:make-memory-credential-store)))
    (let ((record
            (star::admin-create-user*
             "root-admin"
             "temporary-password-123"
             nil
             :administrator t
             :store store)))
      (is (string= "administrator"
                   (star.auth:user-record-principal-type record)))
      (is (equal '("admin") (star.auth:user-record-scopes record))))
    (let ((record
            (star::admin-create-user*
             "analyst"
             "analyst-password-123"
             '("documents:read" "search:read")
             :store store)))
      (is (string= "analyst" (star.auth:user-record-username record))))
    (star::admin-reset-user-password*
     "analyst" "replacement-password-456" :store store)
    (is-true
     (star.auth:authenticate-user-password
      "analyst" "replacement-password-456" :store store))))

(test local-admin-credential-helpers-reuse-auth-domain-services
  (let* ((star:*auth-pepper* "admin-test-pepper")
         (store (star.auth:make-memory-credential-store)))
    (multiple-value-bind (record raw-key)
        (star::admin-create-credential*
         "robot" "service" '("documents:read") :store store)
      (is (search "star_sk_v1_" raw-key))
      (multiple-value-bind (replacement replacement-key)
          (star::admin-rotate-credential*
           (star.auth:api-key-record-id record) 0 :store store)
        (is (search "star_sk_v1_" replacement-key))
        (is (not (string= (star.auth:api-key-record-id record)
                          (star.auth:api-key-record-id replacement))))))))

(test admin-password-source-is-explicit
  (multiple-value-bind (password generated-p)
      (star::resolve-admin-password :random-password t)
    (is (= 32 (length password)))
    (is-true generated-p))
  (with-input-from-string (*standard-input* "stdin-password-123\n")
    (multiple-value-bind (password generated-p)
        (star::resolve-admin-password :password-stdin t)
      (is (string= "stdin-password-123" password))
      (is-false generated-p)))
  (signals error
    (star::resolve-admin-password :password "one" :random-password t)))
