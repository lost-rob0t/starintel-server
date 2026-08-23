(in-package :star.authorization)

(defparameter +target-lease-service-result-codes+
  '(:acquired :renewed :released :found :listed :revoked
    :not-found :conflict :stale-token :not-owner :expired
    :idempotency-conflict :invalid-request :rate-limited
    :backend-unavailable :deadline-exceeded :outcome-unknown
    :unauthenticated :unauthorized))

(defparameter +target-lease-dataset-metadata-key+ "_server_dataset_id")
(defparameter +target-lease-client-metadata-key+ "client")

(defparameter *target-lease-valkey-host*
  (or (uiop:getenv "VALKEY_HOST") "127.0.0.1"))
(defparameter *target-lease-valkey-port*
  (star::environment-integer "VALKEY_PORT" 6379))
(defparameter *target-lease-valkey-password-file*
  (uiop:getenv "VALKEY_PASSWORD_FILE"))
(defparameter *target-lease-valkey-tls-p*
  (not (null (star::environment-boolean "VALKEY_TLS" nil))))
(defparameter *target-lease-valkey-ca-file*
  (uiop:getenv "VALKEY_CA_FILE"))
(defparameter *target-lease-valkey-pool-size*
  (star::environment-integer "STAR_TARGET_LEASE_POOL_SIZE" 8))
(defparameter *target-lease-valkey-pool-wait-timeout-ms*
  (star::environment-integer "STAR_TARGET_LEASE_POOL_WAIT_TIMEOUT_MS" 500))
(defparameter *target-lease-valkey-operation-timeout-ms*
  (star::environment-integer "STAR_TARGET_LEASE_OPERATION_TIMEOUT_MS" 1000))
(defparameter *target-lease-valkey-reconnect-attempts*
  (star::environment-integer "STAR_TARGET_LEASE_RECONNECT_ATTEMPTS" 2))
(defparameter *target-lease-valkey-reconnect-backoff-ms*
  (star::environment-integer "STAR_TARGET_LEASE_RECONNECT_BACKOFF_MS" 25))
(defparameter *target-lease-idempotency-ttl-ms*
  (star::environment-integer "STAR_TARGET_LEASE_IDEMPOTENCY_TTL_MS" 86400000))
(defparameter *target-lease-default-ttl-ms*
  (star::environment-integer "STAR_TARGET_LEASE_DEFAULT_TTL_MS" 30000))
(defparameter *target-lease-maximum-lifetime-ms*
  (star::environment-integer "STAR_TARGET_LEASE_MAXIMUM_LIFETIME_MS" 300000))
(defparameter *target-lease-service-instance-id*
  (uiop:getenv "STAR_TARGET_LEASE_SERVICE_INSTANCE_ID"))

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
  "Normalize a finite trusted deadline to Unix milliseconds."
  (cond
    ((and (integerp deadline) (> deadline 300000000000)) deadline)
    ((and (integerp deadline) (> deadline 3000000000))
     (* 1000 (- deadline 2208988800)))
    (t
     (error "Target lease deadline must be finite universal seconds or Unix milliseconds"))))

(defun target-lease-digest (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (babel:string-to-octets text :encoding :utf-8))))

(defun target-lease-derived-id (prefix request-id)
  (format nil "~a:~a" prefix (target-lease-digest request-id)))

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

(defun target-lease-policy-metadata (context operation)
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
   :metadata (target-lease-policy-metadata context operation)))

(defun target-lease-service-result (code &key lease leases retryable-p detail decision)
  (unless (member code +target-lease-service-result-codes+ :test #'eq)
    (error "Unknown target lease service result code ~s" code))
  (%make-target-lease-service-result
   :code code :lease lease :leases leases
   :retryable-p retryable-p :detail detail :decision decision))

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

(defun valid-target-lease-context-p (context &key identity-required-p)
  (and (typep context 'target-lease-request-context)
       (target-lease-request-context-principal context)
       (star.leases:valid-lease-identifier-p
        (target-lease-request-context-request-id context))
       (integerp (target-lease-request-context-deadline context))
       (or (null (target-lease-request-context-dataset-id context))
           (star.leases:valid-lease-identifier-p
            (target-lease-request-context-dataset-id context)))
       (or (not identity-required-p)
           (handler-case
               (progn (target-lease-context-identity context) t)
             (error () nil)))))

(defun context-validation-result (context &key identity-required-p)
  (cond
    ((not (typep context 'target-lease-request-context))
     (target-lease-service-result :invalid-request))
    ((null (target-lease-request-context-principal context))
     (target-lease-service-result :unauthenticated))
    ((not (valid-target-lease-context-p
           context :identity-required-p identity-required-p))
     (target-lease-service-result
      :invalid-request :detail "invalid lease request context"))
    (t nil)))

(defun call-target-lease-operation
    (context thunk &key (identity-required-p t) hide-scope-p)
  (or (context-validation-result
       context :identity-required-p identity-required-p)
      (handler-case
          (funcall thunk)
        (authorization-error (condition)
          (authorization-denial-result condition :hide-scope-p hide-scope-p))
        (error (condition)
          (log:error "target lease service failure: ~a" condition)
          (target-lease-service-result
           :backend-unavailable
           :retryable-p t
           :detail "lease service unavailable")))))

(defun target-lease-storage-metadata (context)
  (let ((metadata (jsown:empty-object)))
    (setf (jsown:val metadata +target-lease-dataset-metadata-key+)
          (or (target-lease-request-context-dataset-id context) :null)
          (jsown:val metadata +target-lease-client-metadata-key+)
          (or (target-lease-request-context-metadata context)
              (jsown:empty-object)))
    metadata))

(defun target-lease-record-dataset-id (record)
  (let* ((metadata (star.leases:lease-record-metadata record))
         (value
           (and metadata
                (handler-case
                    (jsown:val metadata +target-lease-dataset-metadata-key+)
                  (error () nil)))))
    (if (eq value :null) nil value)))

(defun lease-record-resource (record)
  (let ((identity (star.leases:lease-record-identity record)))
    (make-authorization-resource
     :tenant-id (star.leases:lease-identity-tenant-id identity)
     :dataset-id (target-lease-record-dataset-id record)
     :program-id (star.leases:lease-identity-program-id identity)
     :target-namespace (star.leases:lease-identity-target-namespace identity)
     :target-id (star.leases:lease-identity-target-id identity)
     :actor-name (star.leases:lease-identity-actor-name identity))))

(defun authorize-target-lease-record! (context record action operation)
  (authorize!
   action
   :principal (target-lease-request-context-principal context)
   :resource (lease-record-resource record)
   :metadata (target-lease-policy-metadata context operation)))

(defun target-lease-preflight-current-record (service context action operation)
  "Read the exact current lease and authorize against its stored trusted scope.
This authorization read is not used as a fencing commit check; mutation safety
still comes from the backend compare-and-* operation using lease id and token."
  (let* ((request-id
           (target-lease-derived-id
            (string-downcase operation)
            (target-lease-request-context-request-id context)))
         (outcome
           (star.leases:get-lease
            (target-lease-service-store service)
            (target-lease-context-identity context)
            :deadline
            (target-lease-deadline-milliseconds
             (target-lease-request-context-deadline context))
            :request-id request-id)))
    (if (eq :found (star.leases:lease-outcome-code outcome))
        (let ((record (star.leases:lease-outcome-lease outcome)))
          (authorize-target-lease-record! context record action operation)
          (values record nil))
        (values nil (translate-lease-outcome outcome)))))

(defun acquire-target-lease (service context)
  "Authorize and acquire through the backend-neutral lease-store protocol."
  (call-target-lease-operation
   context
   (lambda ()
     (let* ((decision (target-lease-authorize! context "targets:lease" "ACQUIRE"))
            (principal (target-lease-request-context-principal context))
            (request-id (target-lease-request-context-request-id context))
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
               :ttl-ms
               (or (target-lease-request-context-ttl-ms context)
                   *target-lease-default-ttl-ms*)
               :maximum-lifetime-ms
               (or (target-lease-request-context-maximum-lifetime-ms context)
                   *target-lease-maximum-lifetime-ms*)
               :execution-id
               (or (target-lease-request-context-execution-id context)
                   (target-lease-derived-id "execution" request-id))
               :job-id
               (or (target-lease-request-context-job-id context)
                   (target-lease-derived-id "job" request-id))
               :trace-id
               (or (target-lease-request-context-trace-id context)
                   (target-lease-derived-id "trace" request-id))
               :metadata (target-lease-storage-metadata context)
               :deadline
               (target-lease-deadline-milliseconds
                (target-lease-request-context-deadline context))
               :request-id request-id)))
       (translate-lease-outcome result :decision decision)))))

(defun renew-target-lease (service context lease-id fencing-token)
  (call-target-lease-operation
   context
   (lambda ()
     (target-lease-authorize! context "targets:lease" "RENEW")
     (multiple-value-bind (record preflight)
         (target-lease-preflight-current-record
          service context "targets:lease" "RENEW")
       (if preflight
           preflight
           (let* ((principal (target-lease-request-context-principal context))
                  (result
                    (star.leases:renew-lease
                     (target-lease-service-store service)
                     (target-lease-context-identity context)
                     :lease-id lease-id
                     :owner-principal-id (principal-id principal)
                     :service-instance-id
                     (target-lease-service-service-instance-id service)
                     :fencing-token fencing-token
                     :ttl-ms
                     (or (target-lease-request-context-ttl-ms context)
                         *target-lease-default-ttl-ms*)
                     :deadline
                     (target-lease-deadline-milliseconds
                      (target-lease-request-context-deadline context))
                     :request-id
                     (target-lease-request-context-request-id context))))
             (declare (ignore record))
             (translate-lease-outcome result)))))))

(defun release-target-lease (service context lease-id fencing-token)
  (call-target-lease-operation
   context
   (lambda ()
     (target-lease-authorize! context "targets:lease" "RELEASE")
     (multiple-value-bind (record preflight)
         (target-lease-preflight-current-record
          service context "targets:lease" "RELEASE")
       (if preflight
           preflight
           (let* ((principal (target-lease-request-context-principal context))
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
                     :request-id
                     (target-lease-request-context-request-id context))))
             (declare (ignore record))
             (translate-lease-outcome result))))))))

(defun get-target-lease (service context &optional lease-id)
  (call-target-lease-operation
   context
   (lambda ()
     (target-lease-authorize! context "targets:lease" "GET")
     (let* ((result
              (star.leases:get-lease
               (target-lease-service-store service)
               (target-lease-context-identity context)
               :deadline
               (target-lease-deadline-milliseconds
                (target-lease-request-context-deadline context))
               :request-id (target-lease-request-context-request-id context)))
            (translated (translate-lease-outcome result)))
       (if (not (eq :found (target-lease-service-result-code translated)))
           translated
           (let ((record (target-lease-service-result-lease translated)))
             (authorize-target-lease-record! context record "targets:lease" "GET")
             (if (and lease-id
                      (not (string= lease-id
                                    (star.leases:lease-record-lease-id record))))
                 (target-lease-service-result :not-found)
                 translated)))))
   :hide-scope-p t))

(defun lease-record-visible-p (record context)
  (authorization-decision-allowed-p
   (authorize
    "targets:lease"
    :principal (target-lease-request-context-principal context)
    :resource (lease-record-resource record)
    :metadata (target-lease-policy-metadata context "LIST-ITEM"))))

(defun list-target-leases (service context &key owner-principal-id target-id program-id)
  (call-target-lease-operation
   context
   (lambda ()
     (let* ((principal (target-lease-request-context-principal context))
            (decision
              (authorize!
               "targets:lease"
               :principal principal
               :metadata (target-lease-policy-metadata context "LIST")))
            (scopes (principal-scopes principal))
            (administrator-p (administrator-scopes-p scopes))
            (effective-owner
              (if administrator-p owner-principal-id (principal-id principal)))
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
           (translate-lease-outcome result :decision decision))))
   :identity-required-p nil))

(defun revoke-target-lease (service context lease-id fencing-token reason)
  (call-target-lease-operation
   context
   (lambda ()
     (target-lease-authorize! context "targets:force-release" "REVOKE")
     (multiple-value-bind (record preflight)
         (target-lease-preflight-current-record
          service context "targets:force-release" "REVOKE")
       (if preflight
           preflight
           (let ((result
                   (star.leases:revoke-lease
                    (target-lease-service-store service)
                    (target-lease-context-identity context)
                    :lease-id lease-id
                    :fencing-token fencing-token
                    :reason reason
                    :deadline
                    (target-lease-deadline-milliseconds
                     (target-lease-request-context-deadline context))
                    :request-id
                    (target-lease-request-context-request-id context))))
             (declare (ignore record))
             (translate-lease-outcome result))))))))

(defun current-target-lease-authority (service context lease-id fencing-token)
  "Resolve a caller lease locator to the current trusted lease record."
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
  (unless *target-lease-valkey-password-file*
    (error "VALKEY_PASSWORD_FILE is required for the target lease runtime"))
  (or *target-lease-service*
      (setf *target-lease-service*
            (make-target-lease-service
             (star.leases:make-valkey-lease-store
              :host *target-lease-valkey-host*
              :port *target-lease-valkey-port*
              :password-file *target-lease-valkey-password-file*
              :tls-p *target-lease-valkey-tls-p*
              :tls-verify-p t
              :tls-ca-file *target-lease-valkey-ca-file*
              :pool-size *target-lease-valkey-pool-size*
              :pool-wait-timeout-ms *target-lease-valkey-pool-wait-timeout-ms*
              :operation-timeout-ms *target-lease-valkey-operation-timeout-ms*
              :reconnect-attempts *target-lease-valkey-reconnect-attempts*
              :reconnect-backoff-ms *target-lease-valkey-reconnect-backoff-ms*
              :idempotency-ttl-ms *target-lease-idempotency-ttl-ms*)
             :service-instance-id *target-lease-service-instance-id*))))

(defun close-target-lease-service ()
  (when *target-lease-service*
    (unwind-protect
         (star.leases:close-lease-runtime
          (target-lease-service-runtime *target-lease-service*)
          :deadline (+ (target-lease-unix-milliseconds) 5000)
          :request-id (format nil "shutdown:~a" (cms-ulid:ulid)))
      (setf *target-lease-service* nil)))
  t)
