(in-package :star-server-tests)

(in-suite oauth-authorization-code-tests)

(defun provider-params (&rest pairs)
  (loop for (key value) on pairs by #'cddr
        collect (cons key value)))

(test oauth-provider-routes-are-public-but-protocol-validates-clients
  (is (member "/oauth/authorize" star:*auth-public-paths* :test #'string=))
  (is (member "/oauth/token" star:*auth-public-paths* :test #'string=)))

(test oauth-provider-validates-authorization-request-before-login
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client
         (list redirect)
         '("documents:read" "search:read")
         :store store)
      (declare (ignore secret))
      (let* ((client-id (star.auth:oauth-client-record-id client))
             (request
               (star.frontends.http-api::oauth-provider-authorization-request
                (provider-params
                 "response_type" "code"
                 "client_id" client-id
                 "redirect_uri" redirect
                 "scope" "documents:read search:read"
                 "state" "state value"
                 "code_challenge" challenge
                 "code_challenge_method" "S256")
                :store store)))
        (is (string= client-id (getf request :client-id)))
        (is (string= redirect (getf request :redirect-uri)))
        (is (equal '("documents:read" "search:read") (getf request :scopes)))
        (is (string= "state value" (getf request :state)))))))

(test oauth-provider-rejects-open-redirect-and-pkce-downgrade-before-user-auth
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (declare (ignore secret))
      (let ((client-id (star.auth:oauth-client-record-id client)))
        (is (string= "invalid_redirect_uri"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-authorization-request
                         (provider-params
                          "response_type" "code"
                          "client_id" client-id
                          "redirect_uri" "https://evil.example/cb"
                          "scope" "documents:read"
                          "state" "opaque"
                          "code_challenge" challenge
                          "code_challenge_method" "S256")
                         :store store)))))
        (is (string= "invalid_request"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-authorization-request
                         (provider-params
                          "response_type" "code"
                          "client_id" client-id
                          "redirect_uri" redirect
                          "scope" "documents:read"
                          "state" "opaque"
                          "code_challenge" challenge
                          "code_challenge_method" "plain")
                         :store store)))))))))

(test oauth-provider-rejects-response-type-and-scope-escalation-before-login
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (declare (ignore secret))
      (let ((client-id (star.auth:oauth-client-record-id client)))
        (is (string= "unsupported_response_type"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-authorization-request
                         (provider-params
                          "response_type" "token"
                          "client_id" client-id
                          "redirect_uri" redirect
                          "scope" "documents:read"
                          "state" "opaque"
                          "code_challenge" challenge
                          "code_challenge_method" "S256")
                         :store store)))))
        (is (string= "invalid_scope"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-authorization-request
                         (provider-params
                          "response_type" "code"
                          "client_id" client-id
                          "redirect_uri" redirect
                          "scope" "documents:read targets:dispatch"
                          "state" "opaque"
                          "code_challenge" challenge
                          "code_challenge_method" "S256")
                         :store store)))))))))

(test oauth-provider-authorization-authenticates-existing-user-and-preserves-state
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (declare (ignore secret))
      (let* ((request
               (star.frontends.http-api::oauth-provider-authorization-request
                (provider-params
                 "response_type" "code"
                 "client_id" (star.auth:oauth-client-record-id client)
                 "redirect_uri" redirect
                 "scope" "documents:read"
                 "state" "state value"
                 "code_challenge" challenge
                 "code_challenge_method" "S256")
                :store store))
             (location
               (star.frontends.http-api::oauth-provider-authorize
                request
                "alice"
                "correct-horse-battery-staple"
                :store store)))
        (is (search (concatenate 'string redirect "?code=") location))
        (is (search "state=state%20value" location))
        (is (null (search "correct-horse-battery-staple" location)))))))

(test oauth-provider-wrong-password-fails-closed-without-secret-reflection
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (declare (ignore secret))
      (let ((request
              (star.frontends.http-api::oauth-provider-authorization-request
               (provider-params
                "response_type" "code"
                "client_id" (star.auth:oauth-client-record-id client)
                "redirect_uri" redirect
                "scope" "documents:read"
                "state" "opaque"
                "code_challenge" challenge
                "code_challenge_method" "S256")
               :store store)))
        (is (string= "access_denied"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-authorize
                         request "alice" "definitely-wrong-password"
                         :store store)))))
        (handler-case
            (star.frontends.http-api::oauth-provider-authorize
             request "alice" "definitely-wrong-password" :store store)
          (star.auth:oauth-error (condition)
            (is (null (search "definitely-wrong-password"
                              (princ-to-string condition))))))))))

(test oauth-provider-token-exchange-is-no-store-bearer-json-and-one-time
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client client-secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (multiple-value-bind (code raw-code)
          (star.auth:issue-oauth-authorization-code
           (star.auth:oauth-client-record-id client)
           redirect "alice" '("documents:read") challenge "S256" :store store)
        (declare (ignore code))
        (multiple-value-bind (body headers)
            (star.frontends.http-api::oauth-provider-token-exchange
             (provider-params
              "grant_type" "authorization_code"
              "code" raw-code
              "client_id" (star.auth:oauth-client-record-id client)
              "client_secret" client-secret
              "redirect_uri" redirect
              "code_verifier" verifier)
             :store store)
          (let ((json (jsown:parse body)))
            (is (string= "Bearer" (jsown:val json "token_type")))
            (is (search "star_at_v1_" (jsown:val json "access_token")))
            (is (= star:*oauth-access-token-seconds* (jsown:val json "expires_in"))))
          (is (string= "no-store" (getf headers :cache-control)))
          (is (string= "no-cache" (getf headers :pragma)))
          (is (null (search client-secret body)))
          (is (string= "invalid_grant"
                       (captured-oauth-code
                        (lambda ()
                          (star.frontends.http-api::oauth-provider-token-exchange
                           (provider-params
                            "grant_type" "authorization_code"
                            "code" raw-code
                            "client_id" (star.auth:oauth-client-record-id client)
                            "client_secret" client-secret
                            "redirect_uri" redirect
                            "code_verifier" verifier)
                           :store store))))))))))

(test oauth-provider-token-endpoint-rejects-client-redirect-pkce-and-grant-substitution
  (let* ((star:*auth-pepper* "oauth-provider-test-pepper")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client client-secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (flet ((new-code ()
               (nth-value
                1
                (star.auth:issue-oauth-authorization-code
                 (star.auth:oauth-client-record-id client)
                 redirect "alice" '("documents:read") challenge "S256"
                 :store store))))
        (is (string= "unsupported_grant_type"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-token-exchange
                         (provider-params
                          "grant_type" "client_credentials"
                          "code" (new-code)
                          "client_id" (star.auth:oauth-client-record-id client)
                          "client_secret" client-secret
                          "redirect_uri" redirect
                          "code_verifier" verifier)
                         :store store)))))
        (is (string= "invalid_client"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-token-exchange
                         (provider-params
                          "grant_type" "authorization_code"
                          "code" (new-code)
                          "client_id" (star.auth:oauth-client-record-id client)
                          "client_secret" "wrong-secret"
                          "redirect_uri" redirect
                          "code_verifier" verifier)
                         :store store)))))
        (is (string= "invalid_grant"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-token-exchange
                         (provider-params
                          "grant_type" "authorization_code"
                          "code" (new-code)
                          "client_id" (star.auth:oauth-client-record-id client)
                          "client_secret" client-secret
                          "redirect_uri" "https://evil.example/cb"
                          "code_verifier" verifier)
                         :store store)))))
        (is (string= "invalid_grant"
                     (captured-oauth-code
                      (lambda ()
                        (star.frontends.http-api::oauth-provider-token-exchange
                         (provider-params
                          "grant_type" "authorization_code"
                          "code" (new-code)
                          "client_id" (star.auth:oauth-client-record-id client)
                          "client_secret" client-secret
                          "redirect_uri" redirect
                          "code_verifier" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                         :store store)))))))))
