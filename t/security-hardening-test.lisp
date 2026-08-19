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

(defun security-test-principal (scopes)
  (star.auth::%make-request-principal
   :id "security-test-principal"
   :type "api_client"
   :scopes scopes
   :credential-id "security-test-credential"))

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
