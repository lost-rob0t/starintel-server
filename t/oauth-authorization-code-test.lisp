(in-package :star-server-tests)

(def-suite oauth-authorization-code-tests
  :description "OAuth authorization-code, PKCE, scopes, replay, and principal mapping")

(in-suite oauth-authorization-code-tests)

(defun captured-oauth-code (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.auth:oauth-error (condition)
      (star.auth:oauth-error-code condition))))

(defun make-oauth-test-world ()
  (let ((store (star.auth:make-memory-credential-store)))
    (star.auth:create-user
     "alice"
     "correct-horse-battery-staple"
     "human_user"
     '("documents:read" "search:read" "targets:dispatch")
     :must-change-password nil
     :store store)
    store))

(test oauth-client-secret-is-returned-once-and-stored-only-as-verifier
  (let* ((star:*auth-pepper* "oauth-test-pepper")
         (store (make-oauth-test-world)))
    (multiple-value-bind (client raw-secret)
        (star.auth:create-oauth-client
         '("https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         '("documents:read" "search:read")
         :store store)
      (is (plusp (length raw-secret)))
      (is (null (search raw-secret
                        (jsown:to-json
                         (star.auth:oauth-client-metadata-json client)))))
      (is (equal '("documents:read" "search:read")
                 (star.auth:oauth-client-record-allowed-scopes client))))))

(test authorization-code-enforces-exact-client-redirect-scope-and-s256
  (let* ((star:*auth-pepper* "oauth-test-pepper")
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb"))
    (multiple-value-bind (client secret)
        (star.auth:create-oauth-client
         (list redirect)
         '("documents:read" "search:read")
         :store store)
      (declare (ignore secret))
      (let ((client-id (star.auth:oauth-client-record-id client)))
        (is (string= "invalid_redirect_uri"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:issue-oauth-authorization-code
                         client-id "https://evil.example/cb" "alice"
                         '("documents:read")
                         "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
                         "S256"
                         :store store)))))
        (is (string= "invalid_scope"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:issue-oauth-authorization-code
                         client-id redirect "alice"
                         '("targets:dispatch")
                         "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
                         "S256"
                         :store store)))))
        (is (string= "invalid_request"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:issue-oauth-authorization-code
                         client-id redirect "alice"
                         '("documents:read")
                         "plain-challenge"
                         "plain"
                         :store store))))))))

(test pkce-s256-rfc7636-vector-exchanges-once-and-maps-existing-human-principal
  (let* ((star:*auth-pepper* "oauth-test-pepper")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client client-secret)
        (star.auth:create-oauth-client
         (list redirect)
         '("documents:read" "search:read")
         :store store)
      (multiple-value-bind (code-record raw-code)
          (star.auth:issue-oauth-authorization-code
           (star.auth:oauth-client-record-id client)
           redirect
           "alice"
           '("documents:read" "search:read")
           challenge
           "S256"
           :store store)
        (declare (ignore code-record))
        (multiple-value-bind (token raw-token)
            (star.auth:exchange-oauth-authorization-code
             raw-code
             (star.auth:oauth-client-record-id client)
             client-secret
             redirect
             verifier
             :store store)
          (is (search "star_at_v1_" raw-token))
          (is (equal '("documents:read" "search:read")
                     (star.auth:oauth-access-token-record-scopes token)))
          (let* ((context
                   (star.auth:authenticate-oauth-access-token
                    raw-token "corr-oauth" 1000030 :store store))
                 (principal
                   (star.auth:request-security-context-principal context)))
            (is (string= "alice" (star.auth:request-principal-id principal)))
            (is (string= "human_user" (star.auth:request-principal-type principal)))
            (is (equal '("documents:read" "search:read")
                       (star.auth:request-principal-scopes principal))))
          (is (string= "invalid_grant"
                       (captured-oauth-code
                        (lambda ()
                          (star.auth:exchange-oauth-authorization-code
                           raw-code
                           (star.auth:oauth-client-record-id client)
                           client-secret
                           redirect
                           verifier
                           :store store))))))))))

(test code-exchange-rejects-client-secret-pkce-and-redirect-substitution
  (let* ((star:*auth-pepper* "oauth-test-pepper")
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
        (is (string= "invalid_client"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:exchange-oauth-authorization-code
                         (new-code) (star.auth:oauth-client-record-id client)
                         "wrong-secret" redirect verifier :store store)))))
        (is (string= "invalid_grant"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:exchange-oauth-authorization-code
                         (new-code) (star.auth:oauth-client-record-id client)
                         client-secret "https://evil.example/cb" verifier :store store)))))
        (is (string= "invalid_grant"
                     (captured-oauth-code
                      (lambda ()
                        (star.auth:exchange-oauth-authorization-code
                         (new-code) (star.auth:oauth-client-record-id client)
                         client-secret redirect
                         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                         :store store)))))))))

(test expired-and-revoked-oauth-access-tokens-fail-uniformly
  (let* ((star:*auth-pepper* "oauth-test-pepper")
         (now 1000000)
         (star.auth:*auth-clock* (lambda () now))
         (star:*oauth-access-token-seconds* 60)
         (store (make-oauth-test-world))
         (redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client client-secret)
        (star.auth:create-oauth-client (list redirect) '("documents:read") :store store)
      (multiple-value-bind (code raw-code)
          (star.auth:issue-oauth-authorization-code
           (star.auth:oauth-client-record-id client) redirect "alice"
           '("documents:read") challenge "S256" :store store)
        (declare (ignore code))
        (multiple-value-bind (token raw-token)
            (star.auth:exchange-oauth-authorization-code
             raw-code (star.auth:oauth-client-record-id client) client-secret
             redirect verifier :store store)
          (star.auth:revoke-oauth-access-token
           (star.auth:oauth-access-token-record-id token) :store store)
          (is (string= "invalid_credential"
                       (captured-user-authentication-code
                        (lambda ()
                          (star.auth:authenticate-oauth-access-token
                           raw-token "corr" (+ now 30) :store store)))))))
      (multiple-value-bind (code raw-code)
          (star.auth:issue-oauth-authorization-code
           (star.auth:oauth-client-record-id client) redirect "alice"
           '("documents:read") challenge "S256" :store store)
        (declare (ignore code))
        (multiple-value-bind (token raw-token)
            (star.auth:exchange-oauth-authorization-code
             raw-code (star.auth:oauth-client-record-id client) client-secret
             redirect verifier :store store)
          (declare (ignore token))
          (setf now 1000061)
          (is (string= "invalid_credential"
                       (captured-user-authentication-code
                        (lambda ()
                          (star.auth:authenticate-oauth-access-token
                           raw-token "corr" (+ now 30) :store store)))))))))))
