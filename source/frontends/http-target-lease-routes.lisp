(in-package :star.frontends.http-api)

(defparameter +target-lease-api-prefix+ "/api/v1/target-leases")

(defun lease-body-value (body field &optional default)
  (let ((value (jsown:val-safe body field)))
    (if (null value) default value)))

(defun require-lease-body-string (body field)
  (let ((value (lease-body-value body field)))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       400 "invalid_lease_request"
       (format nil "Lease request requires a non-empty ~a field" field)))
    value))

(defun optional-lease-body-string (body field &optional default)
  (let ((value (lease-body-value body field default)))
    (unless (or (null value) (non-empty-string-p value))
      (signal-http-input-error
       400 "invalid_lease_request"
       (format nil "Lease field ~a must be a non-empty string" field)))
    value))

(defun optional-lease-body-positive-integer (body field)
  (let ((value (lease-body-value body field)))
    (when (and value (not (and (integerp value) (plusp value))))
      (signal-http-input-error
       400 "invalid_lease_request"
       (format nil "Lease field ~a must be a positive integer" field)))
    value))

(defun require-lease-body-positive-integer (body field)
  (or (optional-lease-body-positive-integer body field)
      (signal-http-input-error
       400 "invalid_lease_request"
       (format nil "Lease request requires ~a" field))))

(defun lease-request-id-from-body (body &key required-p)
  (let* ((env (lack.request:request-env (ningle:context :request)))
         (header (env-header-value env "Idempotency-Key"))
         (body-id (and body (lease-body-value body "request_id")))
         (request-id (or header body-id)))
    (when (and header body-id (not (string= header body-id)))
      (signal-http-input-error
       400 "idempotency_key_mismatch"
       "Idempotency-Key and request_id must match when both are supplied"))
    (cond
      ((non-empty-string-p request-id) request-id)
      (required-p
       (signal-http-input-error
        400 "idempotency_key_required"
        "A logical request id is required"))
      (t (current-correlation-id)))))

(defun current-lease-deadline ()
  (let ((context star.auth:*request-security-context*))
    (unless context
      (signal-http-input-error 401 "invalid_credential" "Authentication failed"))
    (star.auth:request-security-context-deadline context)))

(defun lease-context-from-body (body &key request-id-required-p)
  (let ((metadata (lease-body-value body "metadata")))
    (when (and metadata (not (json-object-p metadata)))
      (signal-http-input-error
       400 "invalid_lease_request" "metadata must be a JSON object"))
    (star.authorization:make-target-lease-request-context
     :principal (current-policy-principal)
     :tenant-id (require-lease-body-string body "tenant_id")
     :dataset-id (optional-lease-body-string body "dataset_id")
     :program-id (require-lease-body-string body "program_id")
     :target-namespace (require-lease-body-string body "target_namespace")
     :target-id (require-lease-body-string body "target_id")
     :actor-name (require-lease-body-string body "actor_name")
     :workflow-name (optional-lease-body-string body "workflow_name" "default")
     :operation-class
     (optional-lease-body-string body "operation_class" "default")
     :request-id
     (lease-request-id-from-body body :required-p request-id-required-p)
     :deadline (current-lease-deadline)
     :ttl-ms (optional-lease-body-positive-integer body "ttl_ms")
     :maximum-lifetime-ms
     (optional-lease-body-positive-integer body "maximum_lifetime_ms")
     :trace-id (current-correlation-id)
     :metadata metadata)))

(defun lease-query-string (params name &optional default)
  (let ((value (query-value params name)))
    (cond
      ((non-empty-string-p value) value)
      (default default)
      (t
       (signal-http-input-error
        400 "missing_query_parameter"
        (format nil "Query parameter ~a is required" name))))))

(defun lease-context-from-query (params &key identity-required-p)
  (star.authorization:make-target-lease-request-context
   :principal (current-policy-principal)
   :tenant-id (and identity-required-p (lease-query-string params "tenant_id"))
   :dataset-id (query-value params "dataset_id")
   :program-id (and identity-required-p (lease-query-string params "program_id"))
   :target-namespace
   (and identity-required-p (lease-query-string params "target_namespace"))
   :target-id (and identity-required-p (lease-query-string params "target_id"))
   :actor-name (and identity-required-p (lease-query-string params "actor_name"))
   :workflow-name
   (if identity-required-p (lease-query-string params "workflow_name" "default") "default")
   :operation-class
   (if identity-required-p (lease-query-string params "operation_class" "default") "default")
   :request-id (or (query-value params "request_id") (current-correlation-id))
   :deadline (current-lease-deadline)
   :trace-id (current-correlation-id)))

(defun lease-identity-json (identity)
  (jsown:new-js
    ("tenant_id" (star.leases:lease-identity-tenant-id identity))
    ("program_id" (star.leases:lease-identity-program-id identity))
    ("target_namespace" (star.leases:lease-identity-target-namespace identity))
    ("target_id" (star.leases:lease-identity-target-id identity))
    ("actor_name" (star.leases:lease-identity-actor-name identity))
    ("workflow_name" (star.leases:lease-identity-workflow-name identity))
    ("operation_class" (star.leases:lease-identity-operation-class identity))))

(defun lease-record-client-metadata (record)
  (let ((metadata (star.leases:lease-record-metadata record)))
    (or (and metadata
             (jsown:val-safe
              metadata star.authorization::+target-lease-client-metadata-key+))
        (jsown:empty-object))))

(defun target-lease-record-json (record)
  (jsown:new-js
    ("lease_id" (star.leases:lease-record-lease-id record))
    ("identity" (lease-identity-json (star.leases:lease-record-identity record)))
    ("dataset_id"
     (or (star.authorization::target-lease-record-dataset-id record) :null))
    ("owner_principal_id" (star.leases:lease-record-owner-principal-id record))
    ("fencing_token" (star.leases:lease-record-fencing-token record))
    ("acquired_at" (star.leases:lease-record-acquired-at record))
    ("renewed_at" (star.leases:lease-record-renewed-at record))
    ("expires_at" (star.leases:lease-record-expires-at record))
    ("ttl_ms" (star.leases:lease-record-ttl-ms record))
    ("maximum_lifetime_ms"
     (star.leases:lease-record-maximum-lifetime-ms record))
    ("execution_id" (star.leases:lease-record-execution-id record))
    ("job_id" (star.leases:lease-record-job-id record))
    ("trace_id" (star.leases:lease-record-trace-id record))
    ("state" (string-downcase (symbol-name (star.leases:lease-record-state record))))
    ("metadata" (lease-record-client-metadata record))))

(defun lease-result-http-status (code)
  (case code
    (:acquired 201)
    ((:renewed :released :found :listed :revoked) 200)
    (:invalid-request 400)
    (:unauthenticated 401)
    (:unauthorized 403)
    (:not-found 404)
    ((:conflict :idempotency-conflict :not-owner) 409)
    ((:stale-token :expired) 412)
    (:rate-limited 429)
    ((:backend-unavailable :outcome-unknown) 503)
    (:deadline-exceeded 504)
    (otherwise 500)))

(defun lease-result-code-string (code)
  (substitute #\_ #\-
              (string-downcase (symbol-name code))))

(defun lease-result-message (code)
  (case code
    (:acquired "Target lease acquired")
    (:renewed "Target lease renewed")
    (:released "Target lease released")
    (:found "Target lease found")
    (:listed "Target leases listed")
    (:revoked "Target lease revoked")
    (:not-found "Target lease not found")
    (:conflict "Target lease is held by another owner")
    (:stale-token "Target lease fencing token is stale")
    (:not-owner "Target lease ownership does not match")
    (:expired "Target lease has expired")
    (:idempotency-conflict "Request id was reused with conflicting content")
    (:invalid-request "Target lease request is invalid")
    (:rate-limited "Target lease request is rate limited")
    (:backend-unavailable "Target lease backend is unavailable")
    (:deadline-exceeded "Target lease request deadline exceeded")
    (:outcome-unknown "Target lease outcome is uncertain; retry the same request id")
    (:unauthenticated "Authentication failed")
    (:unauthorized "Access denied")
    (otherwise "Target lease operation failed")))

(defun respond-target-lease-result (result)
  (let* ((code (star.authorization:target-lease-service-result-code result))
         (status (lease-result-http-status code))
         (json
           (jsown:new-js
             ("status" (if (< status 400) "success" "error"))
             ("code" (lease-result-code-string code))
             ("msg" (lease-result-message code))
             ("correlation_id" (current-correlation-id)))))
    (setf (lack.response:response-status *response*) status)
    (when (star.authorization:target-lease-service-result-lease result)
      (setf (jsown:val json "lease")
            (target-lease-record-json
             (star.authorization:target-lease-service-result-lease result))))
    (when (eq code :listed)
      (setf (jsown:val json "leases")
            (coerce
             (mapcar #'target-lease-record-json
                     (star.authorization:target-lease-service-result-leases result))
             'vector)))
    (jsown:to-json json)))

(defun require-target-lease-service ()
  (or star.authorization:*target-lease-service*
      (error "Target lease service is not initialized")))

(defun handle-target-lease-acquire-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((body (require-json-object (parse-json-request)))
           (context (lease-context-from-body body :request-id-required-p t)))
      (respond-target-lease-result
       (star.authorization:acquire-target-lease
        (require-target-lease-service) context)))))

(defun handle-target-lease-renew-route (params)
  (with-http-boundary ()
    (let* ((lease-id (require-path-string params "lease-id"))
           (body (require-json-object (parse-json-request)))
           (context (lease-context-from-body body :request-id-required-p t))
           (token (require-lease-body-positive-integer body "fencing_token")))
      (respond-target-lease-result
       (star.authorization:renew-target-lease
        (require-target-lease-service) context lease-id token)))))

(defun handle-target-lease-release-route (params)
  (with-http-boundary ()
    (let* ((lease-id (require-path-string params "lease-id"))
           (body (require-json-object (parse-json-request)))
           (context (lease-context-from-body body :request-id-required-p t))
           (token (require-lease-body-positive-integer body "fencing_token")))
      (respond-target-lease-result
       (star.authorization:release-target-lease
        (require-target-lease-service) context lease-id token)))))

(defun handle-target-lease-get-route (params)
  (with-http-boundary ()
    (let ((lease-id (require-path-string params "lease-id"))
          (context (lease-context-from-query params :identity-required-p t)))
      (respond-target-lease-result
       (star.authorization:get-target-lease
        (require-target-lease-service) context lease-id)))))

(defun handle-target-lease-list-route (params)
  (with-http-boundary ()
    (let ((context (lease-context-from-query params :identity-required-p nil)))
      (respond-target-lease-result
       (star.authorization:list-target-leases
        (require-target-lease-service)
        context
        :owner-principal-id (query-value params "owner_principal_id")
        :target-id (query-value params "target_id")
        :program-id (query-value params "program_id"))))))

(defun handle-target-lease-revoke-route (params)
  (with-http-boundary ()
    (let* ((lease-id (require-path-string params "lease-id"))
           (body (require-json-object (parse-json-request)))
           (context (lease-context-from-body body :request-id-required-p t))
           (token (require-lease-body-positive-integer body "fencing_token"))
           (reason (require-lease-body-string body "reason")))
      (respond-target-lease-result
       (star.authorization:revoke-target-lease
        (require-target-lease-service) context lease-id token reason)))))

(setf (ningle:route *app* "/api/v1/target-leases/acquire" :method :post)
      #'handle-target-lease-acquire-route)
(setf (ningle:route *app* "/api/v1/target-leases/:lease-id/renew" :method :post)
      #'handle-target-lease-renew-route)
(setf (ningle:route *app* "/api/v1/target-leases/:lease-id" :method :delete)
      #'handle-target-lease-release-route)
(setf (ningle:route *app* "/api/v1/target-leases/:lease-id" :method :get)
      #'handle-target-lease-get-route)
(setf (ningle:route *app* "/api/v1/target-leases" :method :get)
      #'handle-target-lease-list-route)
(setf (ningle:route *app* "/api/v1/target-leases/:lease-id/revoke" :method :post)
      #'handle-target-lease-revoke-route)
