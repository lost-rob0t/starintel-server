(in-package :star-server-tests)

(def-suite actor-credentials-tests)
(in-suite actor-credentials-tests)

(test account-bindings-are-explicit-and-provider-errors-are-redacted
  (let ((star.actor-credentials::*accounts* (make-hash-table :test #'equal)))
    (signals error
      (star.actor-credentials:register-credential "discord-watch" "../other"
                                                  "bot-token" (lambda () "secret")))
    (star.actor-credentials:register-credential
     "discord-watch" "bot-east" "bot-token" (lambda () "fixture-secret"))
    (is (string= "fixture-secret"
                 (star.actor-credentials:resolve-credential
                  "discord-watch" "bot-east" "bot-token")))
    (signals error
      (star.actor-credentials:resolve-credential "discord-watch" "bot-west" "bot-token"))
    (star.actor-credentials:register-credential
     "discord-watch" "bot-west" "bot-token"
     (lambda () (error "provider leaked fixture-secret")))
    (handler-case
        (star.actor-credentials:resolve-credential "discord-watch" "bot-west" "bot-token")
      (error (condition)
        (is (null (search "fixture-secret" (princ-to-string condition))))))
    (star.actor-credentials:register-credential
     "discord-watch" "bot-east" "oauth"
     (lambda () (jsown:new-js ("access_token" "fixture-access")
                              ("refresh_token" "fixture-refresh"))))
    (is (string= "fixture-access"
                 (jsown:val (star.actor-credentials:resolve-credential
                             "discord-watch" "bot-east" "oauth")
                            "access_token")))))

(test credential-grants-require-actor-account-and-type
  (let ((resource (star.authorization:make-authorization-resource
                   :actor-name "discord-watch" :account-id "bot-east"
                   :credential-type "bot-token")))
    (flet ((principal (scopes)
             (star.auth::%make-request-principal
              :id "worker" :type "service" :scopes scopes)))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         "actor-credentials:read"
         :principal (principal '("actor-credentials:read" "actor:discord-watch"))
         :resource resource))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         "actor-credentials:read"
         :principal (principal '("actor-credentials:read" "account:bot-east"))
         :resource resource))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         "actor-credentials:read"
         :principal (principal '("actor-credentials:read" "actor:discord-watch"
                                 "account:bot-east"))
         :resource resource))
      (is-true
       (star.authorization:authorize!
        "actor-credentials:read"
        :principal (principal '("actor-credentials:read"
                                "actor:discord-watch" "account:bot-east"
                                "credential-type:bot-token"))
        :resource resource)))))

(test credential-operation-is-discoverable-and-scoped
  (star:load-addon :starintel-actor-credentials)
  (let ((operation
          (star.http.contract:find-http-operation
           "actor-credentials.resolve")))
    (is (string= "/api/v1/actor-credentials/resolve"
                 (star.http.contract:http-operation-path operation)))
    (is (eq :post (star.http.contract:http-operation-method operation)))
    (is (string= "actor-credentials:read"
                 (first (star.http.contract:http-operation-scopes operation)))))
  (star:unload-addon :starintel-actor-credentials))
