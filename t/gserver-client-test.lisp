(in-package :star-server-tests)

(def-suite gserver-client-tests
  :description "Machine-readable HTTP contract, generated client runtime, and administration boundaries")

(in-suite gserver-client-tests)

(defun fake-json-response (status body &key
                                       (correlation-id "corr-test")
                                       (content-type "application/json"))
  (star.api.client::make-client-response
   :status status
   :headers (append
             (list (cons "content-type" content-type))
             (when correlation-id
               (list (cons "x-correlation-id" correlation-id))))
   :body body
   :uri "http://example.test"
   :correlation-id correlation-id
   :content-type content-type))

(defun sub-command-names (command)
  (mapcar #'clingon:command-name (clingon:command-sub-commands command)))

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

(test client-base-url-and-wire-values-are-normalized-and-encoded
  (let ((captured nil))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (setf captured request)
                (fake-json-response
                 200
                 "{\"status\":\"ok\",\"user\":{},\"correlation_id\":\"corr\"}"))))
           (client (star.api.client:make-star-client
                    :base-url "http://example.test///"
                    :transport transport)))
      (is (string= "http://example.test" (star.api.client:base-url client)))
      (star.api.client:call-operation
       client
       "auth.users.password.reset"
       :path-parameters (list (cons "username" "a/b c"))
       :query-parameters (list (cons "q" "a b&c"))
       :body (jsown:new-js ("password" "replacement-password-123")))
      (let ((uri (star.api.client:client-request-uri captured)))
        (is (null (search "a/b c" uri)))
        (is (search "%2F" uri))
        (is (search "%26" uri))))))

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

(test correlation-is-preserved-from-success-body
  (let* ((transport
           (star.api.client:make-function-transport
            (lambda (request)
              (declare (ignore request))
              (fake-json-response
               200
               "{\"principal_id\":\"alice\",\"correlation_id\":\"body-corr\"}"
               :correlation-id nil))))
         (client (star.api.client:make-star-client
                  :base-url "http://example.test"
                  :transport transport)))
    (multiple-value-bind (value response)
        (star.api.client:auth-context client)
      (declare (ignore value))
      (is (string= "body-corr"
                   (star.api.client:client-response-correlation-id response))))))

(test client-normalizes-legacy-and-problem-errors
  (let* ((legacy-transport
           (star.api.client:make-function-transport
            (lambda (request)
              (declare (ignore request))
              (fake-json-response
               403
               "{\"status\":\"error\",\"msg\":\"Denied\",\"code\":\"missing_scope\",\"correlation_id\":\"corr-denied\"}"
               :correlation-id "corr-denied"))))
         (legacy-client (star.api.client:make-star-client
                         :base-url "http://example.test"
                         :transport legacy-transport)))
    (handler-case
        (progn
          (star.api.client:auth-context legacy-client)
          (fail "Expected authorization error"))
      (star.api.client:client-authorization-error (condition)
        (is (= 403 (star.api.client:client-http-error-status condition)))
        (is (string= "missing_scope"
                     (star.api.client:client-http-error-code condition)))
        (is (string= "corr-denied"
                     (star.api.client:client-http-error-correlation-id condition))))))
  (let* ((problem-transport
           (star.api.client:make-function-transport
            (lambda (request)
              (declare (ignore request))
              (fake-json-response
               422
               "{\"type\":\"https://starintel.actor/problems/invalid-user\",\"title\":\"Invalid user\",\"detail\":\"Username is invalid\"}"
               :content-type "application/problem+json"))))
         (problem-client (star.api.client:make-star-client
                          :base-url "http://example.test"
                          :transport problem-transport)))
    (handler-case
        (progn
          (star.api.client:auth-context problem-client)
          (fail "Expected validation error"))
      (star.api.client:client-validation-error (condition)
        (is (string= "https://starintel.actor/problems/invalid-user"
                     (star.api.client:client-http-error-code condition)))
        (is (string= "Username is invalid"
                     (star.api.client:client-http-error-message condition)))))))

(test important-http-statuses-map-to-stable-condition-families
  (dolist (case '((401 star.api.client:client-authentication-error)
                  (403 star.api.client:client-authorization-error)
                  (404 star.api.client:client-not-found-error)
                  (409 star.api.client:client-conflict-error)
                  (422 star.api.client:client-validation-error)
                  (429 star.api.client:client-rate-limit-error)
                  (503 star.api.client:client-server-unavailable-error)))
    (destructuring-bind (status expected-class) case
      (let* ((transport
               (star.api.client:make-function-transport
                (lambda (request)
                  (declare (ignore request))
                  (fake-json-response
                   status
                   "{\"status\":\"error\",\"msg\":\"expected\",\"code\":\"expected\"}"))))
             (client (star.api.client:make-star-client
                      :base-url "http://example.test"
                      :transport transport))
             (caught nil))
        (handler-case
            (star.api.client:auth-context client)
          (star.api.client:client-http-error (condition)
            (setf caught condition)))
        (is-true caught)
        (is (typep caught expected-class))))))

(test malformed-json-becomes-protocol-error
  (let* ((transport
           (star.api.client:make-function-transport
            (lambda (request)
              (declare (ignore request))
              (fake-json-response 200 "{"))))
         (client (star.api.client:make-star-client
                  :base-url "http://example.test"
                  :transport transport)))
    (signals star.api.client:malformed-server-response
      (star.api.client:auth-context client))))

(test expired-deadline-never-calls-transport
  (let ((calls 0))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (declare (ignore request))
                (incf calls)
                (fake-json-response 200 "{}"))))
           (client (star.api.client:make-star-client
                    :base-url "http://example.test"
                    :transport transport))
           (options (star.api.client:make-request-options :timeout-ms 0)))
      (signals star.api.client:client-timeout-error
        (star.api.client:auth-context client :request-options options))
      (is (= 0 calls)))))

(test mutating-operation-is-not-retried-by-default
  (let ((calls 0))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (declare (ignore request))
                (incf calls)
                (fake-json-response
                 503
                 "{\"status\":\"error\",\"msg\":\"unavailable\",\"code\":\"unavailable\"}"))))
           (client (star.api.client:make-star-client
                    :base-url "http://example.test"
                    :transport transport)))
      (signals star.api.client:client-server-unavailable-error
        (star.api.client:create-user
         client "alice" "long-enough-password" '("documents:read")))
      (is (= 1 calls)))))

(test remote-and-local-management-command-trees-are-exposed
  (let ((remote (star-cli::main/command))
        (local (star::main/command)))
    (is (member "auth" (sub-command-names remote) :test #'string=))
    (is (member "admin" (sub-command-names remote) :test #'string=))
    (let* ((auth (find "auth" (clingon:command-sub-commands remote)
                       :key #'clingon:command-name :test #'string=))
           (auth-commands (sub-command-names auth))
           (profiles (find "profile" (clingon:command-sub-commands auth)
                           :key #'clingon:command-name :test #'string=)))
      (is (member "profile" auth-commands :test #'string=))
      (is (member "token" auth-commands :test #'string=))
      (is (equal '("path" "list" "add" "use")
                 (sub-command-names profiles))))
    (is (member "start" (sub-command-names local) :test #'string=))
    (is (member "admin" (sub-command-names local) :test #'string=))
    (let ((local-admin
            (find "admin" (clingon:command-sub-commands local)
                  :key #'clingon:command-name :test #'string=)))
      (is (member "user" (sub-command-names local-admin) :test #'string=))
      (is (member "credential" (sub-command-names local-admin) :test #'string=)))))

(test remote-management-source-does-not-own-http-wire-details
  (let* ((directory (asdf:system-source-directory :star-cli))
         (source (uiop:read-file-string
                  (merge-pathnames "star-cli-management.lisp" directory))))
    (is (null (search "dexador:" source :test #'char-equal)))
    (is (null (search "\"/auth/" source :test #'char-equal)))))

(test local-admin-source-does-not-start-runtime-or-write-couchdb-directly
  (let* ((directory (asdf:system-source-directory :starintel-gserver))
         (source (uiop:read-file-string
                  (merge-pathnames "admin.lisp" directory))))
    (is (null (search "star.runtime:start-runtime" source :test #'char-equal)))
    (is (null (search "connect-rabbitmq" source :test #'char-equal)))
    (is (null (search "cl-couch:" source :test #'char-equal)))))

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

(test credential-secret-output-does-not-disclose-api-key
  (let* ((secret "star_sk_v1_wallet-redaction-test")
         (result
           (star.api.client::make-credential-secret-result
            :api-key secret
            :credential (jsown:new-js ("id" "cred-test")
                                     ("owner" "wallet-test")
                                     ("principal_type" "service")
                                     ("scopes" '("documents:read"))
                                     ("status" "active"))
            :correlation-id "corr-test"))
         (stored nil)
         (star-cli::*secret-service-runner*
           (lambda (arguments input)
             (if (string= (first arguments) "store")
                 (setf stored input)
                 (error "unexpected wallet operation"))
             ""))
         (output (with-output-to-string (stream)
                   (let ((*standard-output* stream))
                       (star-cli::auth/store-secret "wallet-profile" secret)
                       (star-cli::remote-admin/print-credential-summary
                        result "wallet-profile")))))
    (is (string= (format nil "~a~%" secret) stored))
    (is (null (search secret output)))
    (is (search "cred-test" output))
    (is (search "wallet-profile" output))))

(test login-summary-stores-key-and-never-prints-it
  (let* ((secret "star_sk_v1_login-summary-test")
         (stored nil)
         (star-cli::*secret-service-runner*
           (lambda (arguments input)
             (declare (ignore arguments))
             (setf stored input)
             ""))
         (result
           (star.api.client::make-login-result
            :api-key secret
            :credential (jsown:new-js ("id" "login-credential"))
             :user (jsown:new-js ("username" "operator") ("api_key" secret))
            :correlation-id "corr-login"))
         (output (with-output-to-string (stream)
                   (let ((*standard-output* stream))
                     (star-cli::auth/store-login-key result "prod")
                     (star-cli::auth/print-login-summary result "prod")))))
    (is (string= (format nil "~a~%" secret) stored))
    (is (null (search secret output)))
    (is (search "login-credential" output))
    (is (search "prod" output))))

(test secret-service-receives-secrets-only-through-stdin
  (let* ((stored nil)
         (calls nil)
         (secret "star_sk_v1_secret-service-test")
         (star-cli::*secret-service-runner*
           (lambda (arguments input)
             (push (list arguments input) calls)
             (cond
               ((string= (first arguments) "store")
                (setf stored input)
                "")
               ((string= (first arguments) "lookup") stored)
               (t (error "unexpected wallet operation"))))))
    (star-cli::auth/store-secret "prod" secret)
    (is (equal secret (star-cli::auth/lookup-secret "prod")))
    (is (= 2 (length calls)))
    (let* ((store-call
             (find "store" calls :key (lambda (call) (first (first call)))
                                      :test #'string=))
           (arguments (first store-call)))
      (is-true store-call)
      (is (null (search secret (prin1-to-string arguments))))
      (is (search secret (second store-call))))))

(test secret-service-real-subprocess-stdin-boundary
  ;; Exercise UIOP itself: the higher-level injected runner cannot catch a
  ;; string being interpreted as a pathname instead of credential contents.
  (require :sb-posix)
  (let* ((directory (uiop:ensure-directory-pathname
                     (string-trim '(#\Newline)
                                  (uiop:run-program '("mktemp" "-d") :output :string))))
         (program (merge-pathnames "secret-tool" directory))
         (old-path (uiop:getenv "PATH"))
         (secret "star_sk_v1_subprocess-only-test")
         (star-cli::*secret-service-runner* #'star-cli::auth/secret-tool-run))
    (unwind-protect
         (progn
           (with-open-file (stream program :direction :output :if-exists :error)
             (write-string "#!/bin/sh
set -eu
case \"$1\" in
  store)
    IFS= read -r value
    test \"$value\" = star_sk_v1_subprocess-only-test
    test -z \"$(cat)\"
    printf '%s\\n' \"$@\"
    ;;
  lookup|clear)
    if IFS= read -r value; then exit 8; fi
    printf '%s\\n' \"$1\"
    ;;
  fail)
    cat >&2
    exit 9
    ;;
esac
" stream))
           (uiop:run-program (list "chmod" "700" (namestring program)))
           (uiop:symbol-call :sb-posix :setenv "PATH"
                             (format nil "~a:~a" (namestring directory) old-path) 1)
           ;; Prove the fixture works independently of the production runner.
           (is (string= (format nil "lookup~%")
                        (uiop:run-program (list (namestring program) "lookup")
                                          :input nil :output :string)))
           (let ((output (star-cli::auth/secret-tool-run
                          '("store" "application" "star-cli" "profile" "test")
                          (format nil "~a~%" secret))))
             (is (string= (format nil "store~%application~%star-cli~%profile~%test~%")
                          output))
             (is (null (search secret output))))
           (is (string= "lookup" (star-cli::auth/lookup-secret "test")))
           (is-true (star-cli::auth/clear-secret "test"))
           (let ((message
                   (handler-case
                       (progn (star-cli::auth/secret-service-call '("fail") secret)
                              "no error")
                     (error (condition) (princ-to-string condition)))))
             (is (search "Secret Service unavailable" message))
             (is (null (search secret message)))))
      (if old-path
          (uiop:symbol-call :sb-posix :setenv "PATH" old-path 1)
          (uiop:symbol-call :sb-posix :unsetenv "PATH"))
      (uiop:delete-directory-tree directory :validate t))))

(test secret-service-failure-does-not-echo-secret
  (let* ((secret "star_sk_v1_wallet-error-test")
         (star-cli::*secret-service-runner*
           (lambda (arguments input)
             (declare (ignore arguments input))
             (error "backend included ~a" secret)))
         (message
           (handler-case
               (progn (star-cli::auth/store-secret "prod" secret) "no error")
             (error (condition) (princ-to-string condition)))))
    (is (null (search secret message)))
    (is (search "Secret Service unavailable" message))))

(test unrestricted-admin-credential-needs-explicit-confirmation
  (let ((command
          (clingon:make-command
           :name "credential-create"
           :options
           (list (clingon:make-option :boolean
                                      :description "Confirm admin scope"
                                      :long-name "confirm-admin"
                                      :initial-value nil
                                      :key :confirm-admin)))))
    (signals error
      (star-cli::remote-admin/confirm-admin-scope command '("admin")))
    (is (null (star-cli::remote-admin/confirm-admin-scope
               command '("documents:read"))))))

(test credential-metadata-view-never-echoes-extra-secret-fields
  (let* ((secret "star_sk_v1_metadata-list-test")
         (metadata (jsown:parse
                    (format nil
                            "{\"id\":\"cred-safe\",\"owner\":\"operator\",\"status\":\"active\",\"api_key\":\"~a\"}"
                            secret)))
         (output (jsown:to-json
                  (star-cli::auth/safe-credential-metadata metadata))))
    (is (search "cred-safe" output))
    (is (null (search secret output)))
    (is (null (search "api_key" output)))))

(test credential-wallet-failure-attempts-server-revocation
  (let* ((captured nil)
         (star-cli::*secret-service-runner*
           (lambda (arguments input)
             (declare (ignore arguments input))
             (error "wallet is locked")))
         (client
           (star.api.client:make-star-client
            :base-url "http://example.test"
            :transport
            (star.api.client:make-function-transport
             (lambda (request)
               (setf captured request)
               (fake-json-response
                200
                "{\"status\":\"revoked\",\"msg\":\"ok\",\"credential\":{\"id\":\"cred-test\",\"owner\":\"operator\",\"principal_type\":\"service\",\"scopes\":[],\"status\":\"revoked\"},\"correlation_id\":\"corr-test\"}")))))
         (result
           (star.api.client::make-credential-secret-result
            :api-key "star_sk_v1_revocation-test"
            :credential (jsown:new-js ("id" "cred-test"))
            :correlation-id "corr-test")))
    (signals error
      (star-cli::remote-admin/store-issued-credential client result "prod"))
    (is-true captured)
    (is (search "/auth/credentials/cred-test/revoke"
                (star.api.client:client-request-uri captured)))))

(test auth-config-selects-multiple-server-and-general-profiles
  (let* ((config
           (jsown:parse
            "{\"version\":1,\"default_profile\":\"prod\",\"profiles\":[{\"name\":\"prod\",\"server_url\":\"https://starintel.example\"},{\"name\":\"general\",\"server_url\":\"https://api.example\"}]}"))
         (prod (star-cli::auth/profile-entry config "prod"))
         (general (star-cli::auth/profile-entry config "general")))
    (is (string= "https://starintel.example"
                 (jsown:val prod "server_url")))
    (is (string= "https://api.example"
                 (jsown:val general "server_url")))
    (is (null (star-cli::auth/profile-entry config "missing")))))

(test auth-profile-url-rejects-embedded-credentials-and-query-secrets
  (is (star-cli::auth/safe-server-url-p "https://server.example"))
  (is (star-cli::auth/safe-server-url-p "http://127.0.0.1:5000"))
  (is (null (star-cli::auth/safe-server-url-p "https://user:pass@server.example")))
  (is (null (star-cli::auth/safe-server-url-p "https://server.example?api_key=x")))
  (is (null (star-cli::auth/safe-server-url-p "file:///tmp/api"))))

(test auth-config-write-roundtrips-profile-metadata
  (let ((path (merge-pathnames "star-cli-auth-roundtrip-test.json"
                               (uiop:temporary-directory))))
    (unwind-protect
         (let ((config (star-cli::auth/empty-config)))
           (setf (jsown:val config "profiles")
                 (list (jsown:new-js ("name" "prod")
                                     ("server_url" "https://starintel.example"))))
           (setf (jsown:val config "default_profile") "prod")
           (star-cli::auth/write-config path config)
           (let* ((roundtrip (star-cli::auth/read-config path))
                  (profile (star-cli::auth/profile-entry roundtrip "prod")))
             (is (string= "prod" (jsown:val roundtrip "default_profile")))
             (is (string= "https://starintel.example"
                          (jsown:val profile "server_url"))))
           (is (probe-file path))
       (when (probe-file path) (delete-file path))))))

(test auth-config-rejects-plaintext-secret-fields
  (let ((path (merge-pathnames "star-cli-auth-plaintext-test.json"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (write-string
              "{\"version\":1,\"profiles\":[{\"name\":\"prod\",\"api_key\":\"not-allowed\"}]}"
              stream))
           (signals error (star-cli::auth/read-config path)))
      (when (probe-file path) (delete-file path)))))
