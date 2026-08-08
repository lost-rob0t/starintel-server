(in-package :star.leases)

(defconstant +lease-record-version+ 1)

(defun normalize-identity-component (name value)
  (unless (and (stringp value) (plusp (length value)))
    (error "~a must be a non-empty string" name))
  (let ((normalized
          (string-downcase
           (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (unless (and (<= (length normalized) 256)
                 (plusp (length normalized))
                 (alphanumericp (char normalized 0))
                 (every (lambda (character)
                          (or (alphanumericp character)
                              (find character "._:-" :test #'char=)))
                        normalized))
      (error "~a is not a canonical lease identity component: ~s"
             name value))
    normalized))

(defstruct (lease-identity
             (:constructor %make-lease-identity
                 (tenant-id program-id target-namespace target-id actor-name
                  workflow-name operation-class)))
  tenant-id
  program-id
  target-namespace
  target-id
  actor-name
  workflow-name
  operation-class)

(defun make-lease-identity
    (&key tenant-id program-id target-namespace target-id actor-name
       (workflow-name "default") (operation-class "default"))
  (%make-lease-identity
   (normalize-identity-component "tenant-id" tenant-id)
   (normalize-identity-component "program-id" program-id)
   (normalize-identity-component "target-namespace" target-namespace)
   (normalize-identity-component "target-id" target-id)
   (normalize-identity-component "actor-name" actor-name)
   (normalize-identity-component "workflow-name" workflow-name)
   (normalize-identity-component "operation-class" operation-class)))

(defun lease-identity-values (identity)
  (list
   (lease-identity-tenant-id identity)
   (lease-identity-program-id identity)
   (lease-identity-target-namespace identity)
   (lease-identity-target-id identity)
   (lease-identity-actor-name identity)
   (lease-identity-workflow-name identity)
   (lease-identity-operation-class identity)))

(defun base64url-octets (octets)
  (let ((alphabet
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
    (with-output-to-string (stream)
      (loop for start from 0 below (length octets) by 3
            for remaining = (- (length octets) start)
            for first = (aref octets start)
            for second = (if (> remaining 1) (aref octets (1+ start)) 0)
            for third = (if (> remaining 2) (aref octets (+ start 2)) 0)
            for bits = (logior (ash first 16) (ash second 8) third)
            do (write-char (char alphabet (ldb (byte 6 18) bits)) stream)
               (write-char (char alphabet (ldb (byte 6 12) bits)) stream)
               (when (> remaining 1)
                 (write-char (char alphabet (ldb (byte 6 6) bits)) stream))
               (when (> remaining 2)
                 (write-char (char alphabet (ldb (byte 6 0) bits)) stream))))))

(defun canonical-target-lock-key (identity)
  (check-type identity lease-identity)
  (let* ((encoded (jsown:to-json (lease-identity-values identity)))
         (digest
           (ironclad:digest-sequence
            :sha256 (babel:string-to-octets encoded :encoding :utf-8))))
    (format nil "starintel:target-lease:v1:~a"
            (base64url-octets digest))))

(defstruct lease-record
  lock-key
  identity
  lease-id
  owner-principal-id
  owner-client-id
  owner-credential-id
  service-instance-id
  fencing-token
  acquired-at
  renewed-at
  expires-at
  ttl-ms
  maximum-lifetime-ms
  execution-id
  job-id
  trace-id
  request-id
  metadata
  state)

(defparameter +lease-outcome-codes+
  '(:acquired :renewed :released :found :listed :revoked :healthy :closed
    :not-found :conflict :stale-token :not-owner :expired
    :idempotency-conflict :invalid-request :timeout :rate-limited
    :backend-unavailable :outcome-unknown))

(defstruct (lease-outcome
             (:constructor %make-lease-outcome
                 (&key code lease leases (retryable-p nil) detail)))
  code
  lease
  leases
  (retryable-p nil)
  detail)

(defun make-lease-outcome (&key code lease leases (retryable-p nil) detail)
  (unless (member code +lease-outcome-codes+ :test #'eq)
    (error "Unknown lease outcome code ~s" code))
  (%make-lease-outcome
   :code code
   :lease lease
   :leases leases
   :retryable-p retryable-p
   :detail detail))

;;; Backend-neutral retryability. Both adapters MUST derive the retryable flag
;;; from this single mapping so equivalent outcomes cannot drift between
;;; backends. Per the normative lease contract, lease contention is retryable:
;;; the caller may retry acquisition under bounded backoff.

(defparameter +retryable-lease-outcome-codes+
  '(:conflict :timeout :rate-limited :backend-unavailable :outcome-unknown))

(defun retryable-lease-outcome-code-p (code)
  (and (member code +retryable-lease-outcome-codes+) t))

;;; Bounded externally supplied identifiers. The KV lease threat model requires
;;; bounded identifier and request sizes before any backend work. Limits are
;;; measured in UTF-8 bytes, not Lisp character count, so multibyte payloads
;;; cannot bypass them. Identifiers and metadata are validated once here; the
;;; adapters consult these predicates instead of redefining local bounds.

(defparameter +lease-identifier-max-bytes+ 256)
(defparameter +lease-reason-max-bytes+ 512)
(defparameter +lease-metadata-max-bytes+ 4096)
(defparameter +lease-metadata-max-keys+ 64)

(defun utf-8-byte-length (string)
  (declare (type string string))
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun valid-lease-identifier-p (value)
  "Non-empty string whose UTF-8 byte size fits the protocol identifier limit."
  (and (stringp value)
       (plusp (length value))
       (<= (utf-8-byte-length value) +lease-identifier-max-bytes+)))

(defun valid-lease-reason-p (value)
  "Non-empty reason/incident identifier bounded by the reason limit."
  (and (stringp value)
       (plusp (length value))
       (<= (utf-8-byte-length value) +lease-reason-max-bytes+)))

(defun lease-metadata-object-p (metadata)
  "True for NIL (empty metadata) or a parsed JSON object (jsown object)."
  (or (null metadata)
      (and (consp metadata) (eq (car metadata) :obj))))

(defun valid-lease-metadata-p (metadata)
  "Metadata must be a bounded JSON object: object shape, bounded key count,
and bounded serialized UTF-8 byte size. Non-object JSON shapes (arrays,
scalars, strings) and oversized payloads are rejected before backend work."
  (cond
    ((null metadata) t)
    ((not (lease-metadata-object-p metadata)) nil)
    (t
     (and (<= (length (jsown:keywords metadata)) +lease-metadata-max-keys+)
          (<= (utf-8-byte-length (jsown:to-json metadata))
              +lease-metadata-max-bytes+)))))

(defclass lease-store () ())

(defgeneric acquire-lease
    (store identity
     &key owner-principal-id owner-client-id owner-credential-id
       service-instance-id ttl-ms maximum-lifetime-ms execution-id job-id
       trace-id metadata deadline request-id))

(defgeneric renew-lease
    (store identity
     &key lease-id owner-principal-id service-instance-id fencing-token ttl-ms
       deadline request-id))

(defgeneric release-lease
    (store identity
     &key lease-id owner-principal-id service-instance-id fencing-token
       deadline request-id))

(defgeneric get-lease (store identity &key deadline request-id))

(defgeneric list-leases
    (store
     &key owner-principal-id target-id program-id deadline request-id))

(defgeneric revoke-lease
    (store identity
     &key lease-id fencing-token reason deadline request-id))

(defgeneric backend-health (store &key deadline request-id))

(defgeneric close-lease-store (store &key deadline request-id))

(defstruct (lease-runtime
             (:constructor %make-lease-runtime (store)))
  store
  (closed-p nil))

(defun make-lease-runtime (store)
  (check-type store lease-store)
  (%make-lease-runtime store))

(defun close-lease-runtime (runtime &key deadline request-id)
  (check-type runtime lease-runtime)
  (let ((outcome
          (close-lease-store
           (lease-runtime-store runtime)
           :deadline deadline
           :request-id request-id)))
    (when (eq :closed (lease-outcome-code outcome))
      (setf (lease-runtime-closed-p runtime) t))
    outcome))

(defun identity-to-json (identity)
  (jsown:new-js
    ("tenant_id" (lease-identity-tenant-id identity))
    ("program_id" (lease-identity-program-id identity))
    ("target_namespace" (lease-identity-target-namespace identity))
    ("target_id" (lease-identity-target-id identity))
    ("actor_name" (lease-identity-actor-name identity))
    ("workflow_name" (lease-identity-workflow-name identity))
    ("operation_class" (lease-identity-operation-class identity))))

(defun serialize-lease-record (record)
  (check-type record lease-record)
  (jsown:to-json
   (jsown:new-js
     ("record_version" +lease-record-version+)
     ("lock_key" (lease-record-lock-key record))
     ("identity" (identity-to-json (lease-record-identity record)))
     ("lease_id" (lease-record-lease-id record))
     ("owner_principal_id" (lease-record-owner-principal-id record))
     ("owner_client_id" (lease-record-owner-client-id record))
     ("owner_credential_id" (lease-record-owner-credential-id record))
     ("service_instance_id" (lease-record-service-instance-id record))
     ("fencing_token" (lease-record-fencing-token record))
     ("acquired_at" (lease-record-acquired-at record))
     ("renewed_at" (lease-record-renewed-at record))
     ("expires_at" (lease-record-expires-at record))
     ("ttl_ms" (lease-record-ttl-ms record))
     ("maximum_lifetime_ms" (lease-record-maximum-lifetime-ms record))
     ("execution_id" (lease-record-execution-id record))
     ("job_id" (lease-record-job-id record))
     ("trace_id" (lease-record-trace-id record))
     ("request_id" (lease-record-request-id record))
     ("metadata" (or (lease-record-metadata record) (jsown:new-js)))
     ("state" (string-downcase (symbol-name (lease-record-state record)))))))

(defun deserialize-lease-record (json)
  (let* ((object (jsown:parse json))
         (version (jsown:val object "record_version")))
    (unless (= version +lease-record-version+)
      (error "Unsupported lease record version ~s" version))
    (let* ((identity-object (jsown:val object "identity"))
           (identity
             (make-lease-identity
              :tenant-id (jsown:val identity-object "tenant_id")
              :program-id (jsown:val identity-object "program_id")
              :target-namespace
              (jsown:val identity-object "target_namespace")
              :target-id (jsown:val identity-object "target_id")
              :actor-name (jsown:val identity-object "actor_name")
              :workflow-name (jsown:val identity-object "workflow_name")
              :operation-class
              (jsown:val identity-object "operation_class")))
           (lock-key (jsown:val object "lock_key"))
           (state
             (intern (string-upcase (jsown:val object "state")) :keyword)))
      (unless (string= lock-key (canonical-target-lock-key identity))
        (error "Lease record lock key does not match its canonical identity"))
      (unless (member state '(:active :expired :released :revoked) :test #'eq)
        (error "Unknown lease record state ~s" state))
      (make-lease-record
       :lock-key lock-key
       :identity identity
       :lease-id (jsown:val object "lease_id")
       :owner-principal-id (jsown:val object "owner_principal_id")
       :owner-client-id (jsown:val object "owner_client_id")
       :owner-credential-id (jsown:val object "owner_credential_id")
       :service-instance-id (jsown:val object "service_instance_id")
       :fencing-token (jsown:val object "fencing_token")
       :acquired-at (jsown:val object "acquired_at")
       :renewed-at (jsown:val object "renewed_at")
       :expires-at (jsown:val object "expires_at")
       :ttl-ms (jsown:val object "ttl_ms")
       :maximum-lifetime-ms (jsown:val object "maximum_lifetime_ms")
       :execution-id (jsown:val object "execution_id")
       :job-id (jsown:val object "job_id")
       :trace-id (jsown:val object "trace_id")
       :request-id (jsown:val object "request_id")
       :metadata (jsown:val object "metadata")
       :state state))))
