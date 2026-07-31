(in-package :star-server-tests)

(in-suite http-auth-tests)

(test request-principal-accessors-return-defensive-copies
  (let* ((principal
           (star.auth::%make-request-principal
            :id "principal-original"
            :type "api_client"
            :scopes '("documents:read" "search:read")
            :credential-id "credential-original"))
         (first-id (star.auth:request-principal-id principal))
         (first-scopes (star.auth:request-principal-scopes principal))
         (first-credential-id
           (star.auth:request-principal-credential-id principal)))
    (setf (char first-id 0) #\X
          (char (first first-scopes) 0) #\X
          (char first-credential-id 0) #\X)
    (setf (cdr first-scopes) nil)
    (is (string= "principal-original"
                 (star.auth:request-principal-id principal)))
    (is (equal '("documents:read" "search:read")
               (star.auth:request-principal-scopes principal)))
    (is (string= "credential-original"
                 (star.auth:request-principal-credential-id principal)))))

(test service-call-context-accessors-return-defensive-copies
  (let* ((context
           (star.auth::%make-service-call-context
            :principal-id "service-original"
            :principal-type "actor_component"
            :credential-id "credential-original"
            :scopes '("targets:lease")
            :correlation-id "correlation-original"
            :deadline 1000))
         (principal-id
           (star.auth:service-call-context-principal-id context))
         (scopes (star.auth:service-call-context-scopes context))
         (correlation-id
           (star.auth:service-call-context-correlation-id context)))
    (setf (char principal-id 0) #\X
          (char (first scopes) 0) #\X
          (char correlation-id 0) #\X)
    (is (string= "service-original"
                 (star.auth:service-call-context-principal-id context)))
    (is (equal '("targets:lease")
               (star.auth:service-call-context-scopes context)))
    (is (string= "correlation-original"
                 (star.auth:service-call-context-correlation-id context)))))
