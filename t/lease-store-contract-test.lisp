(in-package :star-server-tests)

(def-suite lease-store-contract-tests
  :description "Backend-neutral target lease store contract")

(in-suite lease-store-contract-tests)

(defun test-lease-identity (&key (target-id "target-a"))
  (star.leases:make-lease-identity
   :tenant-id "tenant-a"
   :program-id "program-a"
   :target-namespace "target"
   :target-id target-id
   :actor-name "actor-a"
   :workflow-name "workflow-a"
   :operation-class "default"))

(defun make-test-lease-store (clock &key audit-hook metrics-hook)
  (let ((next-id 0))
    (star.leases:make-memory-lease-store
     :clock clock
     :id-generator
     (lambda () (format nil "lease-~d" (incf next-id)))
     :audit-hook audit-hook
     :metrics-hook metrics-hook)))

(defun acquire-test-lease
    (store identity request-id owner &key (deadline 2000) (ttl-ms 100))
  (star.leases:acquire-lease
   store identity
   :owner-principal-id owner
   :owner-client-id (format nil "client-~a" owner)
   :owner-credential-id (format nil "credential-~a" owner)
   :service-instance-id (format nil "instance-~a" owner)
   :ttl-ms ttl-ms
   :maximum-lifetime-ms 1000
   :execution-id (format nil "execution-~a" owner)
   :job-id (format nil "job-~a" owner)
   :trace-id (format nil "trace-~a" owner)
   :deadline deadline
   :request-id request-id))

;;;; Shared backend-neutral contract.
;;;;
;;;; The identical assertion suite runs against every lease-store backend
;;;; (memory and Valkey) so semantic drift cannot pass CI for one adapter while
;;;; the other enforces the contract. Valkey-only concerns (real concurrency,
;;;; TLS, ACL behavior, pool exhaustion, reconnect/backoff, response
;;;; interruption, authoritative TTL/server time, cluster-safe key layout, and
;;;; no-TTL corruption) live in the Valkey integration suite.

(defstruct lease-contract-fixture
  (store nil)
  (make-identity nil :type (or null function))
  (acquire nil :type (or null function))
  (now-ms nil :type (or null function))
  (deadline nil :type (or null function))
  (advance-time nil :type (or null function))
  (default-ttl-ms 100 :type integer)
  (default-maximum-lifetime-ms 1000 :type integer)
  (short-ttl-ms 10 :type integer)
  (short-maximum-lifetime-ms 1000 :type integer)
  (audit-events nil)
  (metrics-events nil))

(defun contract-instance-of (owner)
  (format nil "instance-~a" owner))

(defun assert-backend-neutral-lease-contract (fx)
  "Run the full backend-neutral lease contract against FIXTURE's store."
  (let* ((store (lease-contract-fixture-store fx))
         (make-identity (lease-contract-fixture-make-identity fx))
         (acquire (lease-contract-fixture-acquire fx))
         (now (lease-contract-fixture-now-ms fx))
         (deadline (lease-contract-fixture-deadline fx))
         (advance (lease-contract-fixture-advance-time fx))
         (default-ttl (lease-contract-fixture-default-ttl-ms fx))
         (default-max (lease-contract-fixture-default-maximum-lifetime-ms fx))
         (short-ttl (lease-contract-fixture-short-ttl-ms fx))
         (short-max (lease-contract-fixture-short-maximum-lifetime-ms fx))
         (identity (funcall make-identity "contract-target")))
    (flet ((acq (id request owner &key (ttl-ms default-ttl)
                 (maximum-lifetime-ms default-max))
             (funcall acquire id request owner
                     :ttl-ms ttl-ms :maximum-lifetime-ms maximum-lifetime-ms))
           (dl () (funcall deadline)))
      ;; Acquisition.
      (let* ((acquired (acq identity "contract-acquire" "contract-owner"))
             (record (star.leases:lease-outcome-lease acquired)))
        (is (eq :acquired (star.leases:lease-outcome-code acquired)))
        (is (typep record 'star.leases:lease-record))
        (is (string= (star.leases:canonical-target-lock-key identity)
                     (star.leases:lease-record-lock-key record)))
        (is (= 1 (star.leases:lease-record-fencing-token record)))
        ;; Inspect/get returns the active lease.
        (let ((observed
                (star.leases:get-lease
                 store identity :deadline (dl) :request-id "contract-get")))
          (is (eq :found (star.leases:lease-outcome-code observed)))
          (is (string= (star.leases:lease-record-lease-id record)
                       (star.leases:lease-record-lease-id
                        (star.leases:lease-outcome-lease observed)))))
        ;; Contention: a second acquire conflicts and is retryable.
        (let ((contended (acq identity "contract-conflict" "contract-rival")))
          (is (eq :conflict (star.leases:lease-outcome-code contended)))
          (is (star.leases:lease-outcome-retryable-p contended))
          (is (eq (star.leases:lease-outcome-retryable-p contended)
                 (star.leases:retryable-lease-outcome-code-p :conflict))))
        ;; Request-ID idempotency: same request + digest returns the same lease.
        (let ((retry (acq identity "contract-acquire" "contract-owner")))
          (is (eq :acquired (star.leases:lease-outcome-code retry)))
          (is (string= (star.leases:lease-record-lease-id record)
                       (star.leases:lease-record-lease-id
                        (star.leases:lease-outcome-lease retry))))
          (is (= (star.leases:lease-record-fencing-token record)
                 (star.leases:lease-record-fencing-token
                  (star.leases:lease-outcome-lease retry)))))
        ;; Changed-input idempotency conflict.
        (let ((changed
                (acq identity "contract-acquire" "contract-owner"
                     :ttl-ms (+ default-ttl 10))))
          (is (eq :idempotency-conflict
                  (star.leases:lease-outcome-code changed))))
        ;; List/filter semantics while the lease is active.
        (let ((listed
                (star.leases:list-leases
                 store :owner-principal-id "contract-owner"
                 :program-id "program-a" :deadline (dl)
                 :request-id "contract-list")))
          (is (eq :listed (star.leases:lease-outcome-code listed)))
          (is (= 1 (length (star.leases:lease-outcome-leases listed)))))
        ;; Renewal by exact owner.
        (let ((renewed
                (star.leases:renew-lease
                 store identity
                 :lease-id (star.leases:lease-record-lease-id record)
                 :owner-principal-id "contract-owner"
                 :service-instance-id (contract-instance-of "contract-owner")
                 :fencing-token (star.leases:lease-record-fencing-token record)
                 :ttl-ms default-ttl :deadline (dl)
                 :request-id "contract-renew")))
          (is (eq :renewed (star.leases:lease-outcome-code renewed))))
        ;; Wrong owner cannot renew.
        (let ((wrong
                (star.leases:renew-lease
                 store identity
                 :lease-id (star.leases:lease-record-lease-id record)
                 :owner-principal-id "contract-rival"
                 :service-instance-id (contract-instance-of "contract-owner")
                 :fencing-token (star.leases:lease-record-fencing-token record)
                 :ttl-ms default-ttl :deadline (dl)
                 :request-id "contract-renew-wrong-owner")))
          (is (eq :not-owner (star.leases:lease-outcome-code wrong))))
        ;; Deadline behavior: a deadline already in the past times out.
        (let ((late
                (star.leases:get-lease
                 store identity :deadline (- (funcall now) 1)
                 :request-id "contract-late-get")))
          (is (eq :timeout (star.leases:lease-outcome-code late))))
        ;; Record/outcome serialization semantics are backend-neutral.
        (let* ((json (star.leases:serialize-lease-record record))
               (decoded (star.leases:deserialize-lease-record json))
               (tampered (jsown:parse json)))
          (is (search "\"record_version\":1" json))
          (is (string= (star.leases:lease-record-lock-key record)
                       (star.leases:lease-record-lock-key decoded)))
          (is (= (star.leases:lease-record-fencing-token record)
                 (star.leases:lease-record-fencing-token decoded)))
          (setf (jsown:val tampered "lock_key")
                "starintel:target-lease:v1:tampered")
          (signals error
            (star.leases:deserialize-lease-record (jsown:to-json tampered))))
        ;; Health.
        (is (eq :healthy
                (star.leases:lease-outcome-code
                 (star.leases:backend-health
                  store :deadline (dl) :request-id "contract-health"))))
        ;; Audit/metrics hooks were emitted in equal numbers.
        (is (plusp (length (car (lease-contract-fixture-audit-events fx)))))
        (is (= (length (car (lease-contract-fixture-audit-events fx)))
               (length (car (lease-contract-fixture-metrics-events fx)))))
        ;; Release by exact owner, then the lease is no longer active.
        (let ((released
                (star.leases:release-lease
                 store identity
                 :lease-id (star.leases:lease-record-lease-id record)
                 :owner-principal-id "contract-owner"
                 :service-instance-id (contract-instance-of "contract-owner")
                 :fencing-token (star.leases:lease-record-fencing-token record)
                 :deadline (dl) :request-id "contract-release")))
          (is (eq :released (star.leases:lease-outcome-code released))))
        (let ((gone
                (star.leases:get-lease
                 store identity :deadline (dl)
                 :request-id "contract-get-released")))
          (is (member (star.leases:lease-outcome-code gone)
                      '(:not-found :expired)))))
      ;; Stale renew/release cannot affect a successor (separate identity).
      (let* ((stale-id (funcall make-identity "contract-stale-target"))
             (first (acq stale-id "contract-stale-acquire" "contract-owner"
                          :ttl-ms short-ttl
                          :maximum-lifetime-ms short-max))
             (old (star.leases:lease-outcome-lease first)))
        (is (eq :acquired (star.leases:lease-outcome-code first)))
        (funcall advance (+ short-ttl 5))
        (let* ((second (acq stale-id "contract-stale-acquire-b" "contract-rival"
                            :ttl-ms default-ttl
                            :maximum-lifetime-ms short-max))
               (current (star.leases:lease-outcome-lease second)))
          (is (eq :acquired (star.leases:lease-outcome-code second)))
          (is (> (star.leases:lease-record-fencing-token current)
                 (star.leases:lease-record-fencing-token old)))
          (let ((renew
                  (star.leases:renew-lease
                   store stale-id
                   :lease-id (star.leases:lease-record-lease-id old)
                   :owner-principal-id "contract-owner"
                   :service-instance-id (contract-instance-of "contract-owner")
                   :fencing-token (star.leases:lease-record-fencing-token old)
                   :ttl-ms default-ttl :deadline (dl)
                   :request-id "contract-stale-renew")))
            (is (member (star.leases:lease-outcome-code renew)
                        '(:stale-token :not-owner :expired))))
          (let ((release
                  (star.leases:release-lease
                   store stale-id
                   :lease-id (star.leases:lease-record-lease-id old)
                   :owner-principal-id "contract-owner"
                   :service-instance-id (contract-instance-of "contract-owner")
                   :fencing-token (star.leases:lease-record-fencing-token old)
                   :deadline (dl)
                   :request-id "contract-stale-release")))
            (is (member (star.leases:lease-outcome-code release)
                        '(:stale-token :not-owner :expired))))
          ;; Stale release after successor acquisition: successor remains.
          (let ((observed
                  (star.leases:get-lease
                   store stale-id :deadline (dl)
                   :request-id "contract-stale-get")))
            (is (string= (star.leases:lease-record-lease-id current)
                         (star.leases:lease-record-lease-id
                          (star.leases:lease-outcome-lease observed)))))))
      ;; Revoke (separate identity).
      (let* ((revoke-id (funcall make-identity "contract-revoke-target"))
             (rev-acquired (acq revoke-id "contract-revoke-acquire"
                                "contract-owner"))
             (rev-record (star.leases:lease-outcome-lease rev-acquired)))
        (let ((revoked
                (star.leases:revoke-lease
                 store revoke-id
                 :lease-id (star.leases:lease-record-lease-id rev-record)
                 :fencing-token (star.leases:lease-record-fencing-token rev-record)
                 :reason "incident-contract-1"
                 :deadline (dl) :request-id "contract-revoke")))
          (is (eq :revoked (star.leases:lease-outcome-code revoked)))))
      ;; Close behavior: closing the runtime rejects new operations.
      (let ((runtime (star.leases:make-lease-runtime store)))
        (is (eq :closed
                (star.leases:lease-outcome-code
                 (star.leases:close-lease-runtime
                  runtime :deadline (dl) :request-id "contract-close"))))
        (is (star.leases:lease-runtime-closed-p runtime))
        (is (eq :closed
                (star.leases:lease-outcome-code
                 (acq (funcall make-identity "contract-after-close")
                      "contract-after-close-request"
                      "contract-after-close-owner"))))
        (is (eq :closed
                (star.leases:lease-outcome-code
                 (star.leases:backend-health
                  store :deadline (dl)
                  :request-id "contract-health-after-close"))))))))

(defun make-memory-lease-contract-fixture ()
  "Build the shared contract fixture around a deterministic memory store."
  (let ((now 1000)
        (audit-box (cons nil nil))
        (metrics-box (cons nil nil)))
    (let* ((store
            (make-test-lease-store
             (lambda () now)
             :audit-hook (lambda (event) (push event (car audit-box)))
             :metrics-hook (lambda (event) (push event (car metrics-box)))))
            (deadline (lambda (&optional (ms 2000)) (+ now ms))))
      (make-lease-contract-fixture
       :store store
       :make-identity
       (lambda (target-id) (test-lease-identity :target-id target-id))
       :acquire
       (lambda (identity request-id owner &key ttl-ms maximum-lifetime-ms)
         (star.leases:acquire-lease
          store identity
          :owner-principal-id owner
          :owner-client-id (format nil "client-~a" owner)
          :owner-credential-id (format nil "credential-~a" owner)
          :service-instance-id (contract-instance-of owner)
          :ttl-ms (or ttl-ms 100)
          :maximum-lifetime-ms (or maximum-lifetime-ms 1000)
          :execution-id (format nil "execution-~a" owner)
          :job-id (format nil "job-~a" owner)
          :trace-id (format nil "trace-~a" owner)
          :deadline (funcall deadline)
          :request-id request-id))
       :now-ms (lambda () now)
       :deadline deadline
       :advance-time (lambda (ms) (incf now ms))
       :default-ttl-ms 100
       :default-maximum-lifetime-ms 1000
       :short-ttl-ms 10
       :short-maximum-lifetime-ms 1000
       :audit-events audit-box
       :metrics-events metrics-box))))

(test canonical-key-is-versioned-deterministic-and-namespaced
  (let* ((identity (test-lease-identity))
         (equivalent
           (star.leases:make-lease-identity
            :tenant-id " TENANT-A "
            :program-id "PROGRAM-A"
            :target-namespace "TARGET"
            :target-id "TARGET-A"
            :actor-name "ACTOR-A"
            :workflow-name "WORKFLOW-A"
            :operation-class "DEFAULT"))
         (different (test-lease-identity :target-id "target-b"))
         (key (star.leases:canonical-target-lock-key identity)))
    (is-true (uiop:string-prefix-p "starintel:target-lease:v1:" key))
    (is (string= key (star.leases:canonical-target-lock-key equivalent)))
    (is-false
     (string= key (star.leases:canonical-target-lock-key different)))
    (signals error
      (star.leases:make-lease-identity
       :tenant-id "tenant-a"
       :program-id "program-a"
       :target-namespace "target"
       :target-id "../raw-key"
       :actor-name "actor-a"
       :workflow-name "workflow-a"
       :operation-class "default"))))

(test acquire-is-atomic-and-returns-structured-outcomes
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (results nil)
         (results-lock (bt:make-lock "lease-results"))
         (threads
           (loop for index below 12
                 collect
                 (let ((thread-index index))
                   (bt:make-thread
                    (lambda ()
                      (let ((result
                              (acquire-test-lease
                               store identity
                               (format nil "request-~d" thread-index)
                               (format nil "owner-~d" thread-index))))
                        (bt:with-lock-held (results-lock)
                          (push result results)))))))))
    (mapc #'bt:join-thread threads)
    (is (= 1 (count :acquired results
                    :key #'star.leases:lease-outcome-code)))
    (is (= 11 (count :conflict results
                     :key #'star.leases:lease-outcome-code)))
    (is (every (lambda (result)
                 (typep result 'star.leases:lease-outcome))
               results))))

(test memory-backend-satisfies-backend-neutral-lease-contract
  (assert-backend-neutral-lease-contract (make-memory-lease-contract-fixture)))

(test record-serialization-has-an-explicit-version
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (record
           (star.leases:lease-outcome-lease
            (acquire-test-lease
             store (test-lease-identity) "request-a" "owner-a")))
         (json (star.leases:serialize-lease-record record))
         (decoded (star.leases:deserialize-lease-record json))
         (tampered (jsown:parse json)))
    (is (= 1 star.leases:+lease-record-version+))
    (is (search "\"record_version\":1" json))
    (is (string= (star.leases:lease-record-lock-key record)
                 (star.leases:lease-record-lock-key decoded)))
    (is (= (star.leases:lease-record-fencing-token record)
           (star.leases:lease-record-fencing-token decoded)))
    (setf (jsown:val tampered "lock_key")
          "starintel:target-lease:v1:tampered")
    (signals error
      (star.leases:deserialize-lease-record (jsown:to-json tampered)))))

;;;; Protocol-level identifier and metadata boundary tests. These exercise the
;;;; shared protocol validators that both backends consult, so the bounds are
;;;; proven once at the protocol boundary.

(test identifiers-reject-oversized-and-multibyte-boundaries
  (let ((max-bytes star.leases:+lease-identifier-max-bytes+)
        (reason-max star.leases:+lease-reason-max-bytes+))
    ;; Maximum accepted identifier.
    (is-true
     (star.leases:valid-lease-identifier-p
      (make-string max-bytes :initial-element #\a)))
    ;; Over maximum rejected (one byte too long).
    (is-false
     (star.leases:valid-lease-identifier-p
      (make-string (1+ max-bytes) :initial-element #\a)))
    ;; Multibyte UTF-8 boundary: 2-byte chars must be measured in bytes.
    ;; max-bytes chars of \u00e9 occupy 2*max-bytes bytes, so rejected.
    (is-false
     (star.leases:valid-lease-identifier-p
      (make-string max-bytes :initial-element #\latin_small_letter_e_with_acute)))
    ;; A multibyte string whose UTF-8 byte length equals the limit is accepted.
    (let ((half (floor max-bytes 2)))
      (is-true
       (star.leases:valid-lease-identifier-p
        (concatenate 'string
                     (make-string half :initial-element #\latin_small_letter_e_with_acute)
                     (make-string (- max-bytes (* half 2))
                                  :initial-element #\a)))))
    ;; Empty/non-string rejected.
    (is-false (star.leases:valid-lease-identifier-p ""))
    (is-false (star.leases:valid-lease-identifier-p nil))
    ;; Reason bound is larger and enforced in bytes.
    (is-true
     (star.leases:valid-lease-reason-p
      (make-string reason-max :initial-element #\a)))
    (is-false
     (star.leases:valid-lease-reason-p
      (make-string (1+ reason-max) :initial-element #\a)))))

(test metadata-shape-and-size-boundaries-are-enforced
  (let ((meta-max star.leases:+lease-metadata-max-bytes+)
        (meta-keys star.leases:+lease-metadata-max-keys+))
    ;; Nil and bounded JSON objects are accepted.
    (is-true (star.leases:valid-lease-metadata-p nil))
    (is-true (star.leases:valid-lease-metadata-p (jsown:new-js ("k" "v"))))
    ;; Malformed metadata shape: JSON array / scalar / string rejected.
    (is-false (star.leases:valid-lease-metadata-p '(:arr 1 2 3)))
    (is-false (star.leases:valid-lease-metadata-p "just-a-string"))
    (is-false (star.leases:valid-lease-metadata-p 42))
    ;; Oversized metadata rejected (serialized bytes exceed the limit).
    (let ((oversized
            (jsown:new-js
             ("payload"
              (concatenate 'string
                           (make-string (1+ meta-max) :initial-element #\x))))))
      (is-false (star.leases:valid-lease-metadata-p oversized)))
    ;; Too many keys rejected.
    (let ((over-keyed (jsown:new-js)))
      (dotimes (i (1+ meta-keys))
        (setf (jsown:val over-keyed (format nil "k~d" i)) i))
      (is-false (star.leases:valid-lease-metadata-p over-keyed)))
    ;; Exactly the key limit accepted.
    (let ((at-limit (jsown:new-js)))
      (dotimes (i meta-keys)
        (setf (jsown:val at-limit (format nil "k~d" i)) i))
      (is-true (star.leases:valid-lease-metadata-p at-limit)))))

(test bounded-identifiers-reject-oversized-requests-in-memory-store
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (oversized
           (make-string (1+ star.leases:+lease-identifier-max-bytes+)
                        :initial-element #\a)))
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:acquire-lease
              store identity
              :owner-principal-id oversized
              :owner-client-id "client-a"
              :owner-credential-id "credential-a"
              :service-instance-id "instance-a"
              :ttl-ms 100 :maximum-lifetime-ms 1000
              :execution-id "execution-a" :job-id "job-a"
              :trace-id "trace-a" :deadline 2000 :request-id "request-a"))))
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:acquire-lease
              store identity
              :owner-principal-id "owner-a"
              :owner-client-id "client-a"
              :owner-credential-id "credential-a"
              :service-instance-id "instance-a"
              :ttl-ms 100 :maximum-lifetime-ms 1000
              :execution-id "execution-a" :job-id "job-a"
              :trace-id "trace-a" :deadline 2000
              :request-id oversized))))
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:acquire-lease
              store identity
              :owner-principal-id "owner-a"
              :owner-client-id "client-a"
              :owner-credential-id "credential-a"
              :service-instance-id "instance-a"
              :ttl-ms 100 :maximum-lifetime-ms 1000
              :execution-id "execution-a" :job-id "job-a"
               :trace-id "trace-a" :deadline 2000 :request-id "request-a"
               :metadata "not-a-json-object"))))))

(test list-lease-filters-are-bounded-before-backend-work
  "Optional list-leases filters (owner-principal-id, target-id, program-id)
are validated before any backend work. nil (omitted) is valid; oversized or
malformed filters return :invalid-request. Both backends behave equivalently."
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (max-bytes star.leases:+lease-identifier-max-bytes+)
         (oversized (make-string (1+ max-bytes) :initial-element #\a))
         (multibyte-over
           (make-string max-bytes
                        :initial-element #\latin_small_letter_e_with_acute)))
    ;; Acquire a lease so list has something to filter.
    (acquire-test-lease store identity "list-filter-acquire" "owner-a")
    ;; nil filters are valid and return the lease.
    (is (eq :listed
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :deadline 2000 :request-id "list-all"))))
    ;; Maximum-length owner filter is accepted.
    (is (eq :listed
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :owner-principal-id (make-string max-bytes :initial-element #\a)
              :deadline 2000 :request-id "list-max-owner"))))
    ;; Oversized owner filter is rejected.
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :owner-principal-id oversized
              :deadline 2000 :request-id "list-oversized-owner"))))
    ;; Multibyte UTF-8 cannot bypass the byte bound.
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :owner-principal-id multibyte-over
              :deadline 2000 :request-id "list-multibyte-owner"))))
    ;; Oversized target-id filter is rejected.
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :target-id oversized
              :deadline 2000 :request-id "list-oversized-target"))))
    ;; Oversized program-id filter is rejected.
    (is (eq :invalid-request
            (star.leases:lease-outcome-code
             (star.leases:list-leases
              store :program-id oversized
              :deadline 2000 :request-id "list-oversized-program"))))))
