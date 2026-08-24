(in-package :star-server-tests)

(in-suite auth-users-tests)

(test first-run-without-configured-password-skips-human-administrator
  (let* ((star:*auth-pepper* "unit-test-security-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-initial-username* "star")
         (star:*auth-initial-password* nil)
         (store (star.auth:make-memory-credential-store)))
    (is (null (star.auth:ensure-initial-user :store store)))
    (is (= 0 (star.auth:user-store-count store)))))

(in-suite http-auth-tests)

(defun security-test-principal (scopes &optional (type "api_client"))
  (star.auth::%make-request-principal
   :id "security-test-principal"
   :type type
   :scopes scopes
   :credential-id "security-test-credential"))

(test administrator-authority-requires-admin-scope
  (let ((type-only
          (security-test-principal '("documents:read") "administrator"))
        (scope-admin
          (security-test-principal '("admin") "api_client")))
    (is-false (star.auth:administrator-principal-p type-only))
    (is-true (star.auth:administrator-principal-p scope-admin))))

(test every-http-response-has-browser-hardening-headers
  (let ((headers star.frontends.http-api::*security-response-headers*))
    (is (string= "nosniff" (getf headers :x-content-type-options)))
    (is (string= "DENY" (getf headers :x-frame-options)))
    (is (search "frame-ancestors 'none'"
                (getf headers :content-security-policy)))
    (is (string= "no-referrer" (getf headers :referrer-policy)))
    (is (search "camera=()" (getf headers :permissions-policy)))))

(test human-user-routes-are-explicitly-authorized
  (is (string= "principals:manage"
               (star.frontends.http-api::route-action
                :get "/auth/users")))
  (is (string= "principals:manage"
               (star.frontends.http-api::route-action
                :post "/auth/users")))
  (is (string= "principals:manage"
               (star.frontends.http-api::route-action
                :post "/auth/users/alice/password")))
  (is (eq :authenticated
          (star.frontends.http-api::route-action
           :post "/auth/password")))
  (is (null
       (star.frontends.http-api::route-action
        :delete "/auth/users"))))

(test delegated-credential-identity-cannot-impersonate-another-principal
  (let ((delegator
          (security-test-principal '("credentials:create")))
        (administrator
          (security-test-principal '("admin"))))
    (is-true
     (star.frontends.http-api::credential-identity-delegable-p
      "security-test-principal" "api_client" delegator))
    (is-false
     (star.frontends.http-api::credential-identity-delegable-p
      "other-principal" "api_client" delegator))
    (is-false
     (star.frontends.http-api::credential-identity-delegable-p
      "security-test-principal" "actor_component" delegator))
    (is-true
     (star.frontends.http-api::credential-identity-delegable-p
      "other-principal" "administrator" administrator))))

(test delegated-credential-creation-cannot-escalate-authority
  (let ((delegator
          (security-test-principal
           '("credentials:create"
             "documents:read"
             "dataset:dataset-a")))
        (wildcard-delegator
          (security-test-principal
           '("credentials:create"
             "documents:read"
             "dataset:*")))
        (administrator
          (security-test-principal '("admin"))))
    (is-true
     (star.frontends.http-api::credential-grant-delegable-p
      "api_client"
      '("documents:read" "dataset:dataset-a")
      delegator))
    (is-true
     (star.frontends.http-api::credential-grant-delegable-p
      "api_client"
      '("documents:read" "dataset:dataset-b")
      wildcard-delegator))
    (is-false
     (star.frontends.http-api::credential-grant-delegable-p
      "api_client"
      '("documents:write" "dataset:dataset-a")
      delegator))
    (is-false
     (star.frontends.http-api::credential-grant-delegable-p
      "api_client"
      '("admin")
      delegator))
    (is-false
     (star.frontends.http-api::credential-grant-delegable-p
      "administrator"
      '("documents:read" "dataset:dataset-a")
      delegator))
    (is-true
     (star.frontends.http-api::credential-grant-delegable-p
      "administrator"
      '("admin")
      administrator))))

(test delegated-credential-lifecycle-cannot-cross-identity-or-authority
  (let* ((delegator
           (security-test-principal
            '("credentials:rotate"
              "credentials:revoke"
              "documents:read"
              "dataset:dataset-a")))
         (administrator
           (security-test-principal '("admin")))
         (read-record
           (star.auth::make-api-key-record
            :owner "security-test-principal"
            :principal-type "api_client"
            :scopes '("documents:read" "dataset:dataset-a")))
         (foreign-read-record
           (star.auth::make-api-key-record
            :owner "other-principal"
            :principal-type "api_client"
            :scopes '("documents:read" "dataset:dataset-a")))
         (admin-record
           (star.auth::make-api-key-record
            :owner "security-administrator"
            :principal-type "administrator"
            :scopes '("admin"))))
    (is-true
     (star.frontends.http-api::credential-record-delegable-p
      read-record delegator))
    (is-false
     (star.frontends.http-api::credential-record-delegable-p
      foreign-read-record delegator))
    (is-false
     (star.frontends.http-api::credential-record-delegable-p
      admin-record delegator))
    (is-true
     (star.frontends.http-api::credential-record-delegable-p
      admin-record administrator))))
