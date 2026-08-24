(in-package :star-server-tests)

(in-suite oauth-authorization-code-tests)

(defun oauth-test-access-token (store)
  (let* ((redirect "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
         (verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
         (challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    (multiple-value-bind (client client-secret)
        (star.auth:create-oauth-client
         (list redirect)
         '("documents:read" "search:read")
         :store store)
      (multiple-value-bind (code raw-code)
          (star.auth:issue-oauth-authorization-code
           (star.auth:oauth-client-record-id client)
           redirect
           "alice"
           '("documents:read" "search:read")
           challenge
           "S256"
           :store store)
        (declare (ignore code))
        (nth-value
         1
         (star.auth:exchange-oauth-authorization-code
          raw-code
          (star.auth:oauth-client-record-id client)
          client-secret
          redirect
          verifier
          :store store))))))

(test oauth-bearer-dispatch-preserves-existing-human-principal
  (let* ((star:*auth-pepper* "oauth-http-test-pepper")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world))
         (raw-token (oauth-test-access-token store))
         (context
           (star.auth:authenticate-bearer-authorization-header
            (format nil "Bearer ~a" raw-token)
            "corr-http-oauth"
            1000030
            :store store))
         (principal
           (star.auth:request-security-context-principal context)))
    (is (string= "alice" (star.auth:request-principal-id principal)))
    (is (string= "human_user" (star.auth:request-principal-type principal)))
    (is (equal '("documents:read" "search:read")
               (star.auth:request-principal-scopes principal)))))

(test oauth-bearer-dispatch-keeps-api-key-compatibility
  (let* ((star:*auth-pepper* "oauth-http-test-pepper")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world)))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "legacy-api-client"
         "api_client"
         '("documents:read")
         :store store)
      (declare (ignore record))
      (let* ((context
               (star.auth:authenticate-bearer-authorization-header
                (format nil "Bearer ~a" raw-key)
                "corr-http-api-key"
                1000030
                :store store))
             (principal
               (star.auth:request-security-context-principal context)))
        (is (string= "legacy-api-client"
                     (star.auth:request-principal-id principal)))
        (is (equal '("documents:read")
                   (star.auth:request-principal-scopes principal)))))))

(test http-auth-middleware-dispatches-oauth-bearer-through-normal-security-context
  (let* ((star:*auth-pepper* "oauth-http-test-pepper")
         (star:*auth-mode* "api-key")
         (star.auth:*auth-clock* (lambda () 1000000))
         (store (make-oauth-test-world))
         (star.auth:*credential-store* store)
         (raw-token (oauth-test-access-token store))
         (context
           (star.frontends.http-api::authenticate-request-env
            (list :http-authorization (format nil "Bearer ~a" raw-token))
            "corr-http-middleware"
            1000030))
         (principal
           (star.auth:request-security-context-principal context)))
    (is (string= "alice" (star.auth:request-principal-id principal)))
    (is (equal '("documents:read" "search:read")
               (star.auth:request-principal-scopes principal)))))
