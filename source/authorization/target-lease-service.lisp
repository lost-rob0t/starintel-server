(in-package :star.authorization)

(defparameter +target-lease-service-result-codes+
  '(:acquired :renewed :released :found :listed :revoked
    :not-found :conflict :stale-token :not-owner :expired
    :idempotency-conflict :invalid-request :rate-limited
    :backend-unavailable :deadline-exceeded :outcome-unknown
    :unauthenticated :unauthorized))

(defstruct (target-lease-service
             (:constructor %make-target-lease-service
                 (store runtime service-instance-id)))
  store
  runtime
  service-instance-id)

(defstruct (target-lease-request-context
             (:constructor make-target-lease-request-context
                 (&key principal tenant-id dataset-id program-id
                       target-namespace target-id actor-name
                       (workflow-name "default")
                       (operation-class "default")
                       request-id deadline ttl-ms maximum-lifetime-ms
                       owner-client-id execution-id job-id trace-id metadata)))
  principal
  tenant-id
  dataset-id
  program-id
  target-namespace
  target-id
  actor-name
  workflow-name
  operation-class
  request-id
  deadline
  ttl-ms
  maximum-lifetime-ms
  owner-client-id
  execution-id
  job-id
  trace-id
  metadata)

(defstruct (target-lease-service-result
             (:constructor %make-target-lease-service-result
                 (&key code lease leases retryable-p detail decision)))
  code
  lease
  leases
  (retryable-p nil)
  detail
  decision)

(defvar *target-lease-service* nil)

(defun make-target-lease-service (store &key service-instance-id)
  (check-type store star.leases:lease-store)
  (%make-target-lease-service
   store
   (star.leases:make-lease-runtime store)
   (or service-instance-id
       (format nil "starintel-server:~a" (cms-ulid:ulid)))))

(defun target-lease-unix-milliseconds ()
  (* 1000 (- (get-universal-time) 2208988800)))

(defun target-lease-deadline-milliseconds (deadline)
  (cond
    ((null deadline)
     (+ (target-lease-unix-milliseconds)
        star:*auth-default-request-timeout-ms*))
    ;; HTTP/request security contexts use Common Lisp universal seconds.
    ((> deadline 3000000000)
     (* 1000 (- deadline 2208988800)))
    ;; Embedded callers may already provide Unix milliseconds.
    ((> deadline 300000000000)
     deadline)
    (t
     (error "Target lease deadline must be finite universal seconds or Unix milliseconds"))))

(defun target-lease-principal-credential-id (principal)
  (typecase principal
    (star.auth:request-principal
     (star.auth:request-principal-credential-id principal))
    (star.auth:service-call-context
     (star.auth:service-call-context-credential-id principal))
    (trusted-authorization-context "trusted-internal")
    (t nil)))

(defun target-lease-context-identity (context)
  (star.leases:make-lease-identity
   :tenant-id (target-lease-request-context-tenant-id context)
   :program-id (target-lease-request-context-program-id context)
   :target-namespace (target-lease-request-context-target-namespace context)
   :target-id (target-lease-request-context-target-id context)
   :actor-name (target-lease-request-context-actor-name context)
   :workflow-name (target-lease-request-context-workflow-name context)
   :operation-class (target-lease-request-context-operation-class context)))

(defun target-lease-context-resource (context)
  (make-authorization-resource
   :tenant-id (target-lease-request-context-tenant-id context)
   :dataset-id (target-lease-request-context-dataset-id context)
   :program-id (target-lease-request-context-program-id context)
   :target-namespace (target-lease-request-context-target-namespace context)
   :target-id (target-lease-request-context-target-id context)
   :actor-name (target-lease-request-context-actor-name context)))

(defun target-lease-context-metadata (context operation)
  (list :route "target-lease-service"
        :method operation
        :correlation-id
        (or (target-lease-request-context-trace-id context)
            (and star.auth:*request-security-context*
                 (star.auth:request-security-context-correlation-id
                  star.auth:*request-security-context*)))))

(defun target-lease-authorize! (context action operation)
  (authorize!
   action
   :principal (target-lease-request-context-principal context)
   :resource (target-lease-context-resource context)
   :metadata (target-lease-context-metadata context operation)))

(defun target-lease-service-result (code &key lease leases retryable-p detail decision)
  (unless (member code +target-lease-service-result-codes+ :test #'eq)
    (error "Unknown target lease service result code ~s" code))
  (%make-target-lease-service-result
   :code code
   :lease lease
   :leases leases
   :retryable-p retryable-p
   :detail detail
   :decision decision))

(defun translate-lease-outcome (outcome &key decision)
  (let ((code (star.leases:lease-outcome-code outcome)))
    (target-lease-service-result
     (if (eq code :timeout) :deadline-exceeded code)
     :lease (star.leases:lease-outcome-lease outcome)
     :leases (star.leases:lease-outcome-leases outcome)
     :retryable-p (star.leases:lease-outcome-retryable-p outcome)
     :detail (star.leases:lease-outcome-detail outcome)
     :decision decision)))

(defun authorization-denial-result (condition &key hide-scope-p)
  (let* ((decision (authorization-error-decision condition))
         (reason (and decision (authorization-decision-reason decision))))
    (target-lease-service-result
     (if (and hide-scope-p (string= reason "scope_mismatch"))
         :not-found
         :unauthorized)
     :decision decision)))

(defmacro with-target-lease-service-boundary
    ((context &key hide-scope-p) &body body)
  `(handler-case
       (cond
         ((null (target-lease-request-context-principal ,context))
          (target-lease-service-result :unauthenticated))
         ((or (null (target-lease-request-context-request-id ,context))
              (null (target-lease-request-context-deadline ,context)))
          (target-lease-service-result
           :invalid-request
           :detail "request-id and finite deadline are required"))
         (t ,@body))
     (authorization-error (condition)
       (authorization-denial-result condition :hide-scope-p ,hide-scope-p))
     (error (condition)
       (target-lease-service-result
        :invalid-request
        :detail (princ-to-string condition)))))

(defun acquire-target-lease (service context)
  "Authorize and acquire a target lease through the backend-neutral store."
  (with-target-lease-service-boundary (context)
    (let* ((decision (target-lease-authorize! context "targets:lease" "ACQUIRE"))
           (principal (target-lease-request-context-principal context))
           (result
             (star.leases:acquire-lease
              (target-lease-service-store service)
              (target-lease-context-identity context)
              :owner-principal-id (principal-id principal)
              :owner-client-id
              (or (target-lease-request-context-owner-client-id context)
                  (principal-id principal))
              :owner-credential-id
              (or (target-lease-principal-credential-id principal)
                  "trusted-internal")
              :service-instance-id
              (target-lease-service-service-instance-id service)
              :ttl-ms (target-lease-request-context-ttl-ms context)
              :maximum-lifetime-ms
              (target-lease-request-context-maximum-lifetime-ms context)
              :execution-id (target-lease-request-context-execution-id context)
              :job-id (target-lease-request-context-job-id context)
              :trace-id (target-lease-request-context-trace-id context)
              :metadata (target-lease-request-context-metadata context)
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context))))
      (translate-lease-outcome result :decision decision))))

(defun renew-target-lease (service context lease-id fencing-token)
  (with-target-lease-service-boundary (context)
    (let* ((decision (target-lease-authorize! context "targets:lease" "RENEW"))
           (principal (target-lease-request-context-principal context))
           (result
             (star.leases:renew-lease
              (target-lease-service-store service)
              (target-lease-context-identity context)
              :lease-id lease-id
              :owner-principal-id (principal-id principal)
              :service-instance-id
              (target-lease-service-service-instance-id service)
              :fencing-token fencing-token
              :ttl-ms (target-lease-request-context-ttl-ms context)
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context))))
      (translate-lease-outcome result :decision decision))))

(defun release-target-lease (service context lease-id fencing-token)
  (with-target-lease-service-boundary (context)
    (let* ((decision (target-lease-authorize! context "targets:lease" "RELEASE"))
           (principal (target-lease-request-context-principal context))
           (result
             (star.leases:release-lease
              (target-lease-service-store service)
              (target-lease-context-identity context)
              :lease-id lease-id
              :owner-principal-id (principal-id principal)
              :service-instance-id
              (target-lease-service-service-instance-id service)
              :fencing-token fencing-token
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context))))
      (translate-lease-outcome result :decision decision))))

(defun get-target-lease (service context &optional lease-id)
  (with-target-lease-service-boundary (context :hide-scope-p t)
    (let* ((decision (target-lease-authorize! context "targets:lease" "GET"))
           (result
             (star.leases:get-lease
              (target-lease-service-store service)
              (target-lease-context-identity context)
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context)))
           (translated (translate-lease-outcome result :decision decision)))
      (if (and lease-id
               (target-lease-service-result-lease translated)
               (not (string=
                     lease-id
                     (star.leases:lease-record-lease-id
                      (target-lease-service-result-lease translated)))))
          (target-lease-service-result :not-found :decision decision)
          translated))))

(defun lease-record-resource (record &optional dataset-id)
  (let ((identity (star.leases:lease-record-identity record)))
    (make-authorization-resource
     :tenant-id (star.leases:lease-identity-tenant-id identity)
     :dataset-id dataset-id
     :program-id (star.leases:lease-identity-program-id identity)
     :target-namespace (star.leases:lease-identity-target-namespace identity)
     :target-id (star.leases:lease-identity-target-id identity)
     :actor-name (star.leases:lease-identity-actor-name identity))))

(defun lease-record-visible-p (record context)
  (authorization-decision-allowed-p
   (authorize
    "targets:lease"
    :principal (target-lease-request-context-principal context)
    :resource
    (lease-record-resource
     record (target-lease-request-context-dataset-id context))
    :metadata (target-lease-context-metadata context "LIST-ITEM"))))

(defun list-target-leases (service context &key owner-principal-id target-id program-id)
  (with-target-lease-service-boundary (context)
    (let* ((principal (target-lease-request-context-principal context))
           (decision
             (authorize!
              "targets:lease"
              :principal principal
              :metadata (target-lease-context-metadata context "LIST")))
           (scopes (principal-scopes principal))
           (administrator-p (administrator-scopes-p scopes))
           (effective-owner
             (if administrator-p
                 owner-principal-id
                 (principal-id principal)))
           (result
             (star.leases:list-leases
              (target-lease-service-store service)
              :owner-principal-id effective-owner
              :target-id target-id
              :program-id program-id
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context))))
      (if (eq :listed (star.leases:lease-outcome-code result))
          (target-lease-service-result
           :listed
           :leases
           (remove-if-not
            (lambda (record) (lease-record-visible-p record context))
            (star.leases:lease-outcome-leases result))
           :decision decision)
          (translate-lease-outcome result :decision decision)))))

(defun revoke-target-lease (service context lease-id fencing-token reason)
  (with-target-lease-service-boundary (context)
    (let* ((decision
             (target-lease-authorize!
              context "targets:force-release" "REVOKE"))
           (result
             (star.leases:revoke-lease
              (target-lease-service-store service)
              (target-lease-context-identity context)
              :lease-id lease-id
              :fencing-token fencing-token
              :reason reason
              :deadline
              (target-lease-deadline-milliseconds
               (target-lease-request-context-deadline context))
              :request-id (target-lease-request-context-request-id context))))
      (translate-lease-outcome result :decision decision))))

(defun current-target-lease-authority (service context lease-id fencing-token)
  "Resolve a caller-provided lease locator into trusted current server state.
The returned lease record, not request JSON, is the authority used by dispatch."
  (let ((result (get-target-lease service context lease-id)))
    (cond
      ((not (eq :found (target-lease-service-result-code result))) result)
      ((/= fencing-token
           (star.leases:lease-record-fencing-token
            (target-lease-service-result-lease result)))
       (target-lease-service-result :stale-token))
      ((not (string=
             (principal-id (target-lease-request-context-principal context))
             (star.leases:lease-record-owner-principal-id
              (target-lease-service-result-lease result))))
       (target-lease-service-result :not-owner))
      (t result))))

(defun initialize-target-lease-service ()
  "Create the process-owned production Valkey lease service once."
  (or *target-lease-service*
      (setf *target-lease-service*
            (make-target-lease-service
             (star.leases:make-valkey-lease-store
              :host star:*target-lease-valkey-host*
              :port star:*target-lease-valkey-port*
              :password-file star:*target-lease-valkey-password-file*
              :tls-p star:*target-lease-valkey-tls-p*
              :tls-verify-p t
              :tls-ca-file star:*target-lease-valkey-ca-file*
              :pool-size star:*target-lease-valkey-pool-size*
              :pool-wait-timeout-ms
              star:*target-lease-valkey-pool-wait-timeout-ms*
              :operation-timeout-ms
              star:*target-lease-valkey-operation-timeout-ms*
              :reconnect-attempts star:*target-lease-valkey-reconnect-attempts*
              :reconnect-backoff-ms
              star:*target-lease-valkey-reconnect-backoff-ms*
              :idempotency-ttl-ms star:*target-lease-idempotency-ttl-ms*)
             :service-instance-id star:*target-lease-service-instance-id*))))

(defun close-target-lease-service ()
  (when *target-lease-service*
    (unwind-protect
         (star.leases:close-lease-runtime
          (target-lease-service-runtime *target-lease-service*)
          :deadline (+ (target-lease-unix-milliseconds) 5000)
          :request-id (format nil "shutdown:~a" (cms-ulid:ulid)))
      (setf *target-lease-service* nil)))
  t)
