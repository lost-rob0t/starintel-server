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

(test retries-are-idempotent-and-input-mismatch-is-rejected
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (first (acquire-test-lease store identity "request-a" "owner-a"))
         (retry (acquire-test-lease store identity "request-a" "owner-a"))
         (changed
           (acquire-test-lease
            store identity "request-a" "owner-a" :ttl-ms 200)))
    (is (eq :acquired (star.leases:lease-outcome-code first)))
    (is (eq :acquired (star.leases:lease-outcome-code retry)))
    (is (string=
         (star.leases:lease-record-lease-id
          (star.leases:lease-outcome-lease first))
         (star.leases:lease-record-lease-id
          (star.leases:lease-outcome-lease retry))))
    (is (= 1
           (star.leases:lease-record-fencing-token
            (star.leases:lease-outcome-lease retry))))
    (is (eq :idempotency-conflict
            (star.leases:lease-outcome-code changed)))))

(test stale-renew-and-release-cannot-affect-a-successor
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (first (acquire-test-lease store identity "request-a" "owner-a"
                                   :ttl-ms 10))
         (old (star.leases:lease-outcome-lease first)))
    (setf now 1011)
    (let* ((second
             (acquire-test-lease store identity "request-b" "owner-b"))
           (current (star.leases:lease-outcome-lease second))
           (renew
             (star.leases:renew-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id old)
              :owner-principal-id "owner-a"
              :service-instance-id "instance-owner-a"
              :fencing-token 1
              :ttl-ms 100
              :deadline 2000
              :request-id "renew-old"))
           (release
             (star.leases:release-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id old)
              :owner-principal-id "owner-a"
              :service-instance-id "instance-owner-a"
              :fencing-token 1
              :deadline 2000
              :request-id "release-old"))
           (observed
             (star.leases:get-lease
              store identity :deadline 2000 :request-id "get-current")))
      (is (= 2 (star.leases:lease-record-fencing-token current)))
      (is (eq :stale-token (star.leases:lease-outcome-code renew)))
      (is (eq :stale-token (star.leases:lease-outcome-code release)))
      (is (string=
           (star.leases:lease-record-lease-id current)
           (star.leases:lease-record-lease-id
            (star.leases:lease-outcome-lease observed)))))))

(test exact-owner-renew-and-release-observe-expiry-and-deadlines
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (identity (test-lease-identity))
         (acquired (acquire-test-lease store identity "request-a" "owner-a"))
         (record (star.leases:lease-outcome-lease acquired)))
    (is (eq :timeout
            (star.leases:lease-outcome-code
             (star.leases:get-lease
              store identity :deadline 999 :request-id "late-get"))))
    (is (eq :not-owner
            (star.leases:lease-outcome-code
             (star.leases:renew-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id record)
              :owner-principal-id "owner-b"
              :service-instance-id "instance-owner-a"
              :fencing-token 1
              :ttl-ms 150
              :deadline 2000
              :request-id "renew-wrong-owner"))))
    (let ((renewed
            (star.leases:renew-lease
             store identity
             :lease-id (star.leases:lease-record-lease-id record)
             :owner-principal-id "owner-a"
             :service-instance-id "instance-owner-a"
             :fencing-token 1
             :ttl-ms 150
             :deadline 2000
             :request-id "renew-a")))
      (is (eq :renewed (star.leases:lease-outcome-code renewed))))
    (let ((released
            (star.leases:release-lease
             store identity
             :lease-id (star.leases:lease-record-lease-id record)
             :owner-principal-id "owner-a"
             :service-instance-id "instance-owner-a"
             :fencing-token 1
             :deadline 2000
             :request-id "release-a")))
      (is (eq :released (star.leases:lease-outcome-code released))))
    (is (eq :not-found
            (star.leases:lease-outcome-code
             (star.leases:get-lease
              store identity :deadline 2000 :request-id "get-missing"))))))

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

(test list-revoke-health-and-hooks-use-protocol-outcomes
  (let ((now 1000)
        (audit-events nil)
        (metric-events nil))
    (let* ((store
             (make-test-lease-store
              (lambda () now)
              :audit-hook (lambda (event) (push event audit-events))
              :metrics-hook (lambda (event) (push event metric-events))))
           (identity (test-lease-identity))
           (record
             (star.leases:lease-outcome-lease
              (acquire-test-lease store identity "request-a" "owner-a")))
           (listed
             (star.leases:list-leases
              store :owner-principal-id "owner-a"
              :deadline 2000 :request-id "list-a"))
           (revoked
             (star.leases:revoke-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id record)
              :fencing-token 1
              :reason "incident-1"
              :deadline 2000 :request-id "revoke-a")))
      (is (eq :listed (star.leases:lease-outcome-code listed)))
      (is (= 1 (length (star.leases:lease-outcome-leases listed))))
      (is (eq :revoked (star.leases:lease-outcome-code revoked)))
      (is (eq :healthy
              (star.leases:lease-outcome-code
               (star.leases:backend-health
                store :deadline 2000 :request-id "health-a"))))
      (is (plusp (length audit-events)))
      (is (= (length audit-events) (length metric-events))))))

(test closing-runtime-closes-store-and-rejects-new-operations
  (let* ((now 1000)
         (store (make-test-lease-store (lambda () now)))
         (runtime (star.leases:make-lease-runtime store)))
    (is (eq :closed
            (star.leases:lease-outcome-code
             (star.leases:close-lease-runtime
              runtime :deadline 2000 :request-id "close-a"))))
    (is-true (star.leases:lease-runtime-closed-p runtime))
    (is (eq :closed
            (star.leases:lease-outcome-code
             (acquire-test-lease
              store (test-lease-identity) "request-a" "owner-a"))))
    (is (eq :closed
            (star.leases:lease-outcome-code
             (star.leases:backend-health
              store :deadline 2000 :request-id "health-after-close"))))))
