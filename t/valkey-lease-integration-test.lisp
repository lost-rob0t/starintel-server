(in-package :star-server-tests)

(def-suite valkey-lease-integration-tests
  :description "Real Valkey target-lease atomicity and lifecycle tests")

(in-suite valkey-lease-integration-tests)

(defun required-test-environment (name)
  (or (uiop:getenv name)
      (error "Required Valkey integration environment variable ~a is unset"
             name)))

(defun test-valkey-port (&optional (name "STAR_TEST_VALKEY_PORT"))
  (parse-integer (required-test-environment name)))

(defun real-unix-milliseconds ()
  (multiple-value-bind (seconds microseconds)
      (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun real-deadline (&optional (milliseconds 3000))
  (+ (real-unix-milliseconds) milliseconds))

(defun unique-valkey-prefix (label)
  (format nil "starintel:test:~a:~a"
          label
          (string-downcase (format nil "~36r" (random (expt 36 12))))))

(defun make-real-valkey-store
    (&key (label "lease") password-file tls-p tls-ca-file
       (tls-verify-p t) (pool-size 8) (pool-wait-timeout-ms 3000)
       (operation-timeout-ms 1000) (reconnect-attempts 2)
       (reconnect-backoff-ms 10) port key-prefix audit-hook metrics-hook
       after-submit-hook)
  (star.leases:make-valkey-lease-store
   :host (required-test-environment "STAR_TEST_VALKEY_HOST")
   :port (or port
             (test-valkey-port
              (if tls-p "STAR_TEST_VALKEY_TLS_PORT"
                  "STAR_TEST_VALKEY_PORT")))
   :password-file
   (or password-file
       (required-test-environment "STAR_TEST_VALKEY_PASSWORD_FILE"))
   :tls-p tls-p
   :tls-verify-p tls-verify-p
   :tls-ca-file tls-ca-file
   :pool-size pool-size
   :pool-wait-timeout-ms pool-wait-timeout-ms
   :operation-timeout-ms operation-timeout-ms
   :reconnect-attempts reconnect-attempts
   :reconnect-backoff-ms reconnect-backoff-ms
   :idempotency-ttl-ms 60000
   :key-prefix (or key-prefix (unique-valkey-prefix label))
   :audit-hook audit-hook
   :metrics-hook metrics-hook
   :after-submit-hook after-submit-hook))

(defun real-valkey-identity (&optional (target-id "target-a"))
  (star.leases:make-lease-identity
   :tenant-id "tenant-a"
   :program-id "program-a"
   :target-namespace "target"
   :target-id target-id
   :actor-name "actor-a"
   :workflow-name "workflow-a"
   :operation-class "default"))

(defun acquire-real-valkey-lease
    (store identity request-id owner &key (ttl-ms 1000)
       (maximum-lifetime-ms 10000) (deadline-ms 3000))
  (star.leases:acquire-lease
   store identity
   :owner-principal-id owner
   :owner-client-id (format nil "client-~a" owner)
   :owner-credential-id (format nil "credential-~a" owner)
   :service-instance-id (format nil "instance-~a" owner)
   :ttl-ms ttl-ms
   :maximum-lifetime-ms maximum-lifetime-ms
   :execution-id (format nil "execution-~a" owner)
   :job-id (format nil "job-~a" owner)
   :trace-id (format nil "trace-~a" owner)
   :metadata (jsown:new-js ("safe_label" "integration-test"))
   :deadline (real-deadline deadline-ms)
   :request-id request-id))

(defun close-real-valkey-store (store request-id)
  (star.leases:close-lease-store
   store :deadline (real-deadline) :request-id request-id))

(defmacro with-real-valkey-store ((name &rest arguments) &body body)
  `(let ((,name (make-real-valkey-store ,@arguments)))
     (unwind-protect
          (progn ,@body)
       (close-real-valkey-store ,name
                                (format nil "close-~a" (gensym))))))

(defun make-valkey-lease-contract-fixture (&key (label "contract"))
  "Build the shared contract fixture around a real Valkey adapter.

   The fixture wires the same backend-neutral assertions used by the memory
   store to a live Valkey instance, so the two backends cannot drift."
  (let ((audit-box (cons nil nil))
        (metrics-box (cons nil nil)))
    (let ((store
            (make-real-valkey-store
             :label label
             :audit-hook (lambda (event) (push event (car audit-box)))
             :metrics-hook (lambda (event) (push event (car metrics-box))))))
      (make-lease-contract-fixture
       :store store
       :make-identity (lambda (target-id) (real-valkey-identity target-id))
       :acquire
       (lambda (identity request-id owner &key ttl-ms maximum-lifetime-ms)
         (star.leases:acquire-lease
          store identity
          :owner-principal-id owner
          :owner-client-id (format nil "client-~a" owner)
          :owner-credential-id (format nil "credential-~a" owner)
          :service-instance-id (contract-instance-of owner)
          :ttl-ms (or ttl-ms 2000)
          :maximum-lifetime-ms (or maximum-lifetime-ms 10000)
          :execution-id (format nil "execution-~a" owner)
          :job-id (format nil "job-~a" owner)
          :trace-id (format nil "trace-~a" owner)
          :metadata (jsown:new-js ("safe_label" "contract"))
          :deadline (real-deadline)
          :request-id request-id))
       :now-ms #'real-unix-milliseconds
       :deadline (lambda (&optional (ms 3000))
                   (+ (real-unix-milliseconds) ms))
       :advance-time
       (lambda (ms) (sleep (/ (max (+ ms 100) 50) 1000.0)))
       :default-ttl-ms 2000
       :default-maximum-lifetime-ms 10000
       :short-ttl-ms 200
       :short-maximum-lifetime-ms 10000
       :audit-events audit-box
       :metrics-events metrics-box))))

(test valkey-backend-satisfies-backend-neutral-lease-contract
  "The identical backend-neutral contract suite runs against real Valkey."
  (let ((fx (make-valkey-lease-contract-fixture :label "contract")))
    (unwind-protect
         (assert-backend-neutral-lease-contract fx)
      (close-real-valkey-store
       (lease-contract-fixture-store fx)
       (format nil "close-contract-~a" (gensym))))))

(test acl-restricts-unrelated-keys-and-dangerous-commands
  "Least-privilege ACL: lease ops still work, unrelated keys and out-of-surface
   commands are rejected."
  (with-real-valkey-store (store :label "acl")
    (let* ((identity (real-valkey-identity "acl-target"))
           (acquired
             (acquire-real-valkey-lease store identity "acl-acquire" "acl-owner"))
           (active-key (star.leases::valkey-active-key store identity)))
      (is (eq :acquired (star.leases:lease-outcome-code acquired)))
      (is (eq :found
              (star.leases:lease-outcome-code
               (star.leases:get-lease store identity
                                      :deadline (real-deadline)
                                      :request-id "acl-get"))))
      (signals error
        (star.leases::valkey-test-command
         store (real-deadline) "SET" "unrelated:key" "rejected"))
      (signals error
        (star.leases::valkey-test-command
         store (real-deadline) "FLUSHDB"))
      (is (plusp
           (length
            (star.leases::valkey-test-command
             store (real-deadline) "GET" active-key)))))))

(test no-ttl-active-key-fails-closed-without-replacement
  "An active lease key with no TTL is corrupt state: acquisition fails closed
   without deleting/replacing the record or allocating a new fencing token."
  (with-real-valkey-store (store :label "no-ttl")
    (let* ((identity (real-valkey-identity "no-ttl-target"))
           (first
             (acquire-real-valkey-lease store identity "no-ttl-acquire" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (fence-key (star.leases::valkey-fence-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (is (= 1 (star.leases:lease-record-fencing-token record)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json)
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (let ((fence-before
              (star.leases::valkey-test-command
               store (real-deadline) "GET" fence-key)))
        (is (string= "1" fence-before))
        (let ((second
                (acquire-real-valkey-lease
                 store identity "no-ttl-acquire-b" "owner-b")))
          (is (eq :backend-unavailable
                  (star.leases:lease-outcome-code second)))
          (is-false (star.leases:lease-outcome-lease second)))
        (is (string= original-json
                     (star.leases::valkey-test-command
                      store (real-deadline) "GET" active-key)))
        (is (= -1
               (star.leases::valkey-test-command
                store (real-deadline) "PTTL" active-key)))
         (is (string= fence-before
                      (star.leases::valkey-test-command
                       store (real-deadline) "GET" fence-key)))))))

(test idempotent-replay-over-no-ttl-state-fails-closed
  "Replaying the SAME request ID after the active key loses its TTL must not
return an authoritative active :acquired lease. The idempotency fast path
validates current authority-bearing state before returning an active result."
  (with-real-valkey-store (store :label "idem-no-ttl")
    (let* ((identity (real-valkey-identity "idem-no-ttl-target"))
           (first
             (acquire-real-valkey-lease store identity "idem-replay-a" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (fence-key (star.leases::valkey-fence-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (is (string= "1"
                   (star.leases::valkey-test-command
                    store (real-deadline) "GET" fence-key)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json)
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (let ((replay
              (acquire-real-valkey-lease
               store identity "idem-replay-a" "owner-a")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code replay)))
        (is-false (star.leases:lease-outcome-lease replay)))
      (is (string= "1"
                   (star.leases::valkey-test-command
                    store (real-deadline) "GET" fence-key)))
      (is (string= original-json
                   (star.leases::valkey-test-command
                    store (real-deadline) "GET" active-key))))))

(test no-ttl-renewal-fails-closed-without-repair
  "Renewal of a no-TTL active key must fail closed. The script must not
reattach a TTL or silently repair/normalize the corrupt state."
  (with-real-valkey-store (store :label "no-ttl-renew")
    (let* ((identity (real-valkey-identity "no-ttl-renew-target"))
           (first
             (acquire-real-valkey-lease store identity "no-ttl-renew-a" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json)
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (let ((renew
              (star.leases:renew-lease
               store identity
               :lease-id (star.leases:lease-record-lease-id record)
               :owner-principal-id "owner-a"
               :service-instance-id "instance-owner-a"
               :fencing-token (star.leases:lease-record-fencing-token record)
               :ttl-ms 2000 :deadline (real-deadline)
               :request-id "no-ttl-renew")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code renew))))
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (is (string= original-json
                   (star.leases::valkey-test-command
                    store (real-deadline) "GET" active-key))))))

(test logical-expiry-renewal-cannot-revive-a-surviving-expired-key
  "A surviving active-shaped key whose stored expires_at is past must not be
renewable. Renewal cannot revive an expired lease even if the backend TTL
is still positive."
  (with-real-valkey-store (store :label "logical-expiry-renew")
    (let* ((identity (real-valkey-identity "logical-expiry-target"))
           (first
             (acquire-real-valkey-lease store identity "lexp-a" "owner-a"
                                        :ttl-ms 200 :maximum-lifetime-ms 1000))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json "PX" 60000)
      (sleep 0.3)
      (let ((renew
              (star.leases:renew-lease
               store identity
               :lease-id (star.leases:lease-record-lease-id record)
               :owner-principal-id "owner-a"
               :service-instance-id "instance-owner-a"
               :fencing-token (star.leases:lease-record-fencing-token record)
               :ttl-ms 2000 :deadline (real-deadline)
               :request-id "lexp-renew")))
        (is (member (star.leases:lease-outcome-code renew)
                    '(:expired :backend-unavailable)))))))

(test no-ttl-get-lease-and-fenced-commit-fail-closed
  "get-lease must not report an authoritative active lease for a no-TTL key.
fenced commit must be rejected. No authoritative value is written."
  (with-real-valkey-store (store :label "no-ttl-get-commit")
    (let* ((identity (real-valkey-identity "no-ttl-get-commit-target"))
           (first
             (acquire-real-valkey-lease store identity "ntgc-a" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (commit-key (star.leases::valkey-fenced-value-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json)
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (let ((observed
              (star.leases:get-lease
               store identity :deadline (real-deadline)
               :request-id "ntgc-get")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code observed)))
        (is-false (star.leases:lease-outcome-lease observed)))
      (is (eq :backend-unavailable
              (star.leases::valkey-fenced-set
               store identity record commit-key "corrupt-commit"
               :deadline (real-deadline)
               :request-id "ntgc-commit")))
      (is-false
       (star.leases::valkey-test-command
        store (real-deadline) "GET" commit-key)))))

(test no-ttl-release-and-revoke-clean-up-corrupt-state
  "Release and revoke are terminal invalidation operations: they intentionally
proceed on a no-TTL key to clean up corrupt state. They still require the
exact ownership tuple. This is cleanup, not authority continuation."
  (with-real-valkey-store (store :label "no-ttl-cleanup")
    (let* ((identity (real-valkey-identity "no-ttl-cleanup-target"))
           (first
             (acquire-real-valkey-lease store identity "ntcl-a" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (star.leases::valkey-test-command
       store (real-deadline) "SET" active-key original-json)
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key)))
      (let ((released
              (star.leases:release-lease
               store identity
               :lease-id (star.leases:lease-record-lease-id record)
               :owner-principal-id "owner-a"
               :service-instance-id "instance-owner-a"
               :fencing-token (star.leases:lease-record-fencing-token record)
               :deadline (real-deadline)
               :request-id "ntcl-release")))
        (is (eq :released (star.leases:lease-outcome-code released))))
      (is-false
       (star.leases::valkey-test-command
        store (real-deadline) "GET" active-key))
      (let* ((second
               (acquire-real-valkey-lease store identity "ntcl-b" "owner-b"))
             (record-b (star.leases:lease-outcome-lease second))
             (json-b
               (star.leases::valkey-test-command
                store (real-deadline) "GET" active-key)))
        (is (eq :acquired (star.leases:lease-outcome-code second)))
        (star.leases::valkey-test-command
         store (real-deadline) "SET" active-key json-b)
        (is (= -1
               (star.leases::valkey-test-command
                store (real-deadline) "PTTL" active-key)))
        (let ((revoked
                (star.leases:revoke-lease
                 store identity
                 :lease-id (star.leases:lease-record-lease-id record-b)
                 :fencing-token (star.leases:lease-record-fencing-token record-b)
                 :reason "incident-corrupt-cleanup"
                 :deadline (real-deadline)
                 :request-id "ntcl-revoke")))
          (is (eq :revoked (star.leases:lease-outcome-code revoked))))
        (is-false
         (star.leases::valkey-test-command
          store (real-deadline) "GET" active-key))))))

(test valid-ttl-fenced-commit-still-works
  "A valid active lease with a proper TTL can still authorize a fenced commit.
The no-TTL guard does not break the normal path."
  (with-real-valkey-store (store :label "valid-commit")
    (let* ((identity (real-valkey-identity "valid-commit-target"))
           (first
             (acquire-real-valkey-lease store identity "vc-a" "owner-a"))
           (record (star.leases:lease-outcome-lease first))
           (commit-key (star.leases::valkey-fenced-value-key store identity)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (is (eq :committed
              (star.leases::valkey-fenced-set
               store identity record commit-key "valid-authorized"
               :deadline (real-deadline)
               :request-id "vc-commit")))
      (is (string= "valid-authorized"
                   (star.leases::valkey-test-command
                    store (real-deadline) "GET" commit-key))))))

(test list-leases-excludes-corrupt-and-expired-state
  "list-leases must not return no-TTL or logically expired keys as active
leases. The list script applies the same corrupt-state rules as get-lease."
  (with-real-valkey-store (store :label "list-corrupt")
    (let* ((identity-a (real-valkey-identity "list-corrupt-a"))
           (identity-b (real-valkey-identity "list-corrupt-b"))
           (active-key-a (star.leases::valkey-active-key store identity-a))
           (active-key-b (star.leases::valkey-active-key store identity-b)))
      (declare (ignore active-key-b))
      (acquire-real-valkey-lease store identity-a "lc-a" "owner-a"
                                 :ttl-ms 5000 :maximum-lifetime-ms 10000)
      (acquire-real-valkey-lease store identity-b "lc-b" "owner-b"
                                 :ttl-ms 200 :maximum-lifetime-ms 1000)
      (let ((valid-list
              (star.leases:list-leases
               store :deadline (real-deadline) :request-id "lc-list-valid")))
        (is (eq :listed (star.leases:lease-outcome-code valid-list)))
        (is (= 2 (length (star.leases:lease-outcome-leases valid-list)))))
      (let ((json-a
              (star.leases::valkey-test-command
               store (real-deadline) "GET" active-key-a)))
        (star.leases::valkey-test-command
         store (real-deadline) "SET" active-key-a json-a))
      (is (= -1
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key-a)))
      (sleep 0.3)
      (let ((corrupt-list
              (star.leases:list-leases
               store :deadline (real-deadline) :request-id "lc-list-corrupt")))
        (is (eq :listed (star.leases:lease-outcome-code corrupt-list)))
        (is (= 0 (length (star.leases:lease-outcome-leases corrupt-list))))))))

(test corrupt-backend-record-returns-typed-outcome-not-raw-error
  "A syntactically valid but contract-invalid record (canonical identity
mismatch) must return :backend-unavailable from the public API, not signal a
raw Lisp error. Exercises valkey-script-outcome's handler-case."
  (with-real-valkey-store (store :label "corrupt-record")
    (let* ((identity (real-valkey-identity "corrupt-record-target"))
           (first
             (acquire-real-valkey-lease store identity "cr-a" "owner-a"))
           (active-key (star.leases::valkey-active-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (let ((tampered (jsown:parse original-json)))
        (setf (jsown:val tampered "lock_key")
              "starintel:target-lease:v1:tampereddeadbeef")
        (star.leases::valkey-test-command
         store (real-deadline) "SET" active-key
         (jsown:to-json tampered) "PX" 5000))
      (let ((result
              (star.leases:get-lease
               store identity :deadline (real-deadline)
               :request-id "cr-get")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code result)))
        (is-false (star.leases:lease-outcome-lease result)))
      (let ((listed
              (star.leases:list-leases
               store :deadline (real-deadline) :request-id "cr-list")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code listed)))
        (is (= 0 (length (star.leases:lease-outcome-leases listed))))))))

(test one-hundred-concurrent-acquires-have-exactly-one-observed-owner
  (with-real-valkey-store
      (store :label "concurrency" :pool-size 16 :pool-wait-timeout-ms 10000)
    (let ((identity (real-valkey-identity "concurrent-target"))
          (results nil)
          (results-lock (bt:make-lock "valkey-acquire-results")))
      (let ((threads
              (loop for index below 100
                    collect
                    (let ((thread-index index))
                      (bt:make-thread
                       (lambda ()
                         (let ((result
                                 (acquire-real-valkey-lease
                                  store identity
                                  (format nil "concurrent-request-~d"
                                          thread-index)
                                  (format nil "owner-~d" thread-index)
                                  :deadline-ms 10000)))
                           (bt:with-lock-held (results-lock)
                             (push result results)))))))))
        (mapc #'bt:join-thread threads))
      (is (= 1 (count :acquired results
                      :key #'star.leases:lease-outcome-code)))
      (is (= 99 (count :conflict results
                       :key #'star.leases:lease-outcome-code)))
      (let* ((winner
               (find :acquired results :key #'star.leases:lease-outcome-code))
             (observed
               (star.leases:get-lease
                store identity :deadline (real-deadline)
                :request-id "observe-concurrent-winner")))
        (is (eq :found (star.leases:lease-outcome-code observed)))
        (is (string=
             (star.leases:lease-record-lease-id
              (star.leases:lease-outcome-lease winner))
             (star.leases:lease-record-lease-id
              (star.leases:lease-outcome-lease observed))))
        (is (= 1 (star.leases:lease-record-fencing-token
                  (star.leases:lease-outcome-lease observed))))))))

(test expiry-reacquire-and-fenced-commit-exclude-the-stale-holder
  (let ((prefix (unique-valkey-prefix "expiry")))
    (with-real-valkey-store (first-store :key-prefix prefix)
      (let* ((identity (real-valkey-identity "expiry-target"))
             (first
               (acquire-real-valkey-lease
                first-store identity "expiry-acquire-a" "owner-a"
                :ttl-ms 150 :maximum-lifetime-ms 1000))
             (old (star.leases:lease-outcome-lease first))
             (commit-key
               (star.leases::valkey-fenced-value-key first-store identity)))
        (is (eq :committed
                (star.leases::valkey-fenced-set
                 first-store identity old commit-key "old-authorized"
                 :deadline (real-deadline)
                 :request-id "commit-before-expiry")))
        (sleep 0.25)
        (close-real-valkey-store first-store "close-before-reopen")
        (setf first-store (make-real-valkey-store :key-prefix prefix))
        (let* ((second
                 (acquire-real-valkey-lease
                  first-store identity "expiry-acquire-b" "owner-b"))
               (current (star.leases:lease-outcome-lease second))
               (original-retry
                 (acquire-real-valkey-lease
                  first-store identity "expiry-acquire-a" "owner-a"
                  :ttl-ms 150 :maximum-lifetime-ms 1000))
               (renew
                 (star.leases:renew-lease
                  first-store identity
                  :lease-id (star.leases:lease-record-lease-id old)
                  :owner-principal-id "owner-a"
                  :service-instance-id "instance-owner-a"
                  :fencing-token
                  (star.leases:lease-record-fencing-token old)
                  :ttl-ms 1000 :deadline (real-deadline)
                  :request-id "stale-renew"))
               (release
                 (star.leases:release-lease
                  first-store identity
                  :lease-id (star.leases:lease-record-lease-id old)
                  :owner-principal-id "owner-a"
                  :service-instance-id "instance-owner-a"
                  :fencing-token
                  (star.leases:lease-record-fencing-token old)
                  :deadline (real-deadline)
                  :request-id "stale-release")))
          (is (> (star.leases:lease-record-fencing-token current)
                 (star.leases:lease-record-fencing-token old)))
          (is (eq :acquired
                  (star.leases:lease-outcome-code original-retry)))
          (is (eq :expired
                  (star.leases:lease-record-state
                   (star.leases:lease-outcome-lease original-retry))))
          (is (string=
               (star.leases:lease-record-lease-id old)
               (star.leases:lease-record-lease-id
                (star.leases:lease-outcome-lease original-retry))))
          (is (member (star.leases:lease-outcome-code renew)
                      '(:stale-token :not-owner :expired)))
          (is (member (star.leases:lease-outcome-code release)
                      '(:stale-token :not-owner :expired)))
          (is (eq :stale-token
                  (star.leases::valkey-fenced-set
                   first-store identity old commit-key "stale-write"
                   :deadline (real-deadline)
                   :request-id "commit-after-reacquire")))
          (is (string=
               "old-authorized"
               (star.leases::valkey-test-command
                first-store (real-deadline) "GET" commit-key)))
          (let ((observed
                  (star.leases:get-lease
                   first-store identity :deadline (real-deadline)
                   :request-id "observe-successor")))
            (is (string=
                 (star.leases:lease-record-lease-id current)
                 (star.leases:lease-record-lease-id
                  (star.leases:lease-outcome-lease observed))))))))))

(test interrupted-acquire-is-resolved-idempotently-by-request-id
  (let ((interrupted nil))
    (with-real-valkey-store
        (store :label "unknown"
         :after-submit-hook
         (lambda (connection)
           (unless interrupted
             (setf interrupted t)
             (star.leases::close-valkey-connection connection)
             (error "injected response interruption"))))
      (let* ((identity (real-valkey-identity "unknown-target"))
             (unknown
               (acquire-real-valkey-lease
                store identity "unknown-acquire" "owner-a")))
        (is (eq :outcome-unknown
                (star.leases:lease-outcome-code unknown)))
        (let* ((resolved
                 (acquire-real-valkey-lease
                  store identity "unknown-acquire" "owner-a"))
               (retry
                 (acquire-real-valkey-lease
                  store identity "unknown-acquire" "owner-a")))
          (is (eq :acquired (star.leases:lease-outcome-code resolved)))
          (is (string=
               (star.leases:lease-record-lease-id
                (star.leases:lease-outcome-lease resolved))
               (star.leases:lease-record-lease-id
                (star.leases:lease-outcome-lease retry))))
          (is (= 1 (star.leases:lease-record-fencing-token
                    (star.leases:lease-outcome-lease retry)))))))))

(test invalid-authentication-fails-closed-without-secret-disclosure
  (with-real-valkey-store
      (store
       :label "bad-auth"
       :password-file
       (required-test-environment "STAR_TEST_VALKEY_BAD_PASSWORD_FILE"))
    (let* ((result
             (star.leases:backend-health
              store :deadline (real-deadline)
              :request-id "invalid-auth-health"))
           (detail (or (star.leases:lease-outcome-detail result) "")))
      (is (eq :backend-unavailable (star.leases:lease-outcome-code result)))
      (is-false (search "wrong-valkey-password" detail :test #'char-equal))
      (is-false
       (search (uiop:read-file-string
                (required-test-environment
                 "STAR_TEST_VALKEY_PASSWORD_FILE"))
               detail :test #'char-equal)))))

(test observability-hooks-redact-credentials-and-metadata
  (let ((events nil))
    (with-real-valkey-store
        (store :label "redaction"
         :audit-hook (lambda (event) (push event events))
         :metrics-hook (lambda (event) (push event events)))
      (let* ((identity (real-valkey-identity "redaction-target"))
             (result
               (star.leases:acquire-lease
                store identity
                :owner-principal-id "redaction-owner"
                :owner-client-id "redaction-client"
                :owner-credential-id "credential-secret-value"
                :service-instance-id "redaction-instance"
                :ttl-ms 1000 :maximum-lifetime-ms 10000
                :execution-id "redaction-execution"
                :job-id "redaction-job" :trace-id "redaction-trace"
                :metadata (jsown:new-js ("private" "metadata-secret-value"))
                :deadline (real-deadline) :request-id "redaction-acquire"))
             (printed (format nil "~s" events)))
        (is (eq :acquired (star.leases:lease-outcome-code result)))
        (is (= 2 (length events)))
        (is-false (search "credential-secret-value" printed))
        (is-false (search "metadata-secret-value" printed))))))

(test tls-requires-certificate-and-hostname-verification
  (signals error
    (make-real-valkey-store
     :label "tls-disabled" :tls-p t :tls-verify-p nil
     :tls-ca-file
     (required-test-environment "STAR_TEST_VALKEY_CA_FILE")))
  (with-real-valkey-store
      (store :label "tls" :tls-p t
       :tls-ca-file
       (required-test-environment "STAR_TEST_VALKEY_CA_FILE"))
    (is (eq :healthy
            (star.leases:lease-outcome-code
             (star.leases:backend-health
              store :deadline (real-deadline)
              :request-id "verified-tls-health")))))
  (with-real-valkey-store
      (store :label "wrong-ca" :tls-p t
       :tls-ca-file
       (required-test-environment "STAR_TEST_VALKEY_WRONG_CA_FILE"))
    (is (eq :backend-unavailable
            (star.leases:lease-outcome-code
             (star.leases:backend-health
              store :deadline (real-deadline)
              :request-id "wrong-ca-health"))))))

(test pool-acquisition-deadline-is-finite-and-bounded
  (with-real-valkey-store
      (store :label "pool" :pool-size 1 :pool-wait-timeout-ms 100)
    (let ((entered nil)
          (release nil)
          (lock (bt:make-lock "pool-holder"))
          (condition (bt:make-condition-variable)))
      (let ((holder
              (bt:make-thread
               (lambda ()
                 (star.leases::call-with-valkey-connection
                  store (real-deadline 2000)
                  (lambda (connection)
                    (declare (ignore connection))
                    (bt:with-lock-held (lock)
                      (setf entered t)
                      (bt:condition-notify condition)
                      (loop until release
                            do (bt:condition-wait condition lock)))))))))
        (bt:with-lock-held (lock)
          (loop until entered
                do (bt:condition-wait condition lock)))
        (let* ((started (get-internal-real-time))
               (result
                 (star.leases:backend-health
                  store :deadline (real-deadline 1000)
                  :request-id "pool-timeout"))
               (elapsed
                 (/ (- (get-internal-real-time) started)
                    internal-time-units-per-second)))
          (is (eq :timeout (star.leases:lease-outcome-code result)))
          (is (< elapsed 1.0)))
        (bt:with-lock-held (lock)
          (setf release t)
          (bt:condition-notify condition))
        (bt:join-thread holder)))))

(test reconnect-and-backoff-are-bounded-when-valkey-is-unavailable
  (with-real-valkey-store
      (store :label "unavailable"
       :port (test-valkey-port "STAR_TEST_VALKEY_UNUSED_PORT")
       :operation-timeout-ms 100 :reconnect-attempts 2
       :reconnect-backoff-ms 20)
    (let* ((started (get-internal-real-time))
           (result
             (star.leases:backend-health
              store :deadline (real-deadline 1000)
              :request-id "unavailable-health"))
           (elapsed
             (/ (- (get-internal-real-time) started)
                internal-time-units-per-second)))
      (is (eq :backend-unavailable
              (star.leases:lease-outcome-code result)))
      (is (< elapsed 1.0)))))

(test authoritative-server-time-and-ttl-match-observed-valkey-state
  (with-real-valkey-store (store :label "ttl")
    (let* ((identity (real-valkey-identity "ttl-target"))
           (before (real-unix-milliseconds))
           (result
             (acquire-real-valkey-lease
              store identity "ttl-acquire" "ttl-owner" :ttl-ms 1200))
           (record (star.leases:lease-outcome-lease result))
           (active-key
             (star.leases::valkey-active-key store identity))
           (observed-ttl
             (star.leases::valkey-test-command
              store (real-deadline) "PTTL" active-key))
           (after (real-unix-milliseconds)))
      (is (<= before (star.leases:lease-record-acquired-at record)
              after))
      (is (<= 1 observed-ttl 1200))
      (is (= (star.leases:lease-record-expires-at record)
             (+ (star.leases:lease-record-acquired-at record)
                (star.leases:lease-record-ttl-ms record))))
      (is (string=
           (star.leases:lease-record-lease-id record)
           (star.leases:lease-record-lease-id
            (star.leases:lease-outcome-lease
             (star.leases:get-lease
              store identity :deadline (real-deadline)
              :request-id "ttl-observe"))))))))

(test cluster-safe-key-layout-health-closure-and-cleanup-are-observed
  (let* ((store (make-real-valkey-store :label "lifecycle" :pool-size 2))
         (runtime (star.leases:make-lease-runtime store)))
    (let* ((identity (real-valkey-identity "lifecycle-target"))
           (keys (star.leases::valkey-key-family store identity)))
      (is (= 4 (length keys)))
      (let ((hash-tag
              (subseq (first keys)
                      (position #\{ (first keys))
                      (1+ (position #\} (first keys))))))
        (is (every (lambda (key) (search hash-tag key)) keys)))
      (is-false (some (lambda (key) (search "lifecycle-target" key)) keys))
      (is (eq :healthy
              (star.leases:lease-outcome-code
               (star.leases:backend-health
                store :deadline (real-deadline)
                :request-id "lifecycle-health"))))
      (is (plusp (star.leases::valkey-pool-open-count store)))
      (let ((closed
              (star.leases:close-lease-runtime
               runtime :deadline (real-deadline)
               :request-id "lifecycle-close")))
        (is (eq :closed (star.leases:lease-outcome-code closed))))
      (is (star.leases:lease-runtime-closed-p runtime))
      (is (zerop (star.leases::valkey-pool-open-count store)))
      (is (eq :closed
              (star.leases:lease-outcome-code
               (star.leases:backend-health
                store :deadline (real-deadline)
                :request-id "health-after-close")))))))

(defun lease-org-lisp-blocks ()
  (let ((path
          (merge-pathnames
           "docs/lease-store-usage.org"
           (uiop:ensure-directory-pathname
            (or (uiop:getenv "STARINTEL_SOURCE_ROOT")
                (uiop:pathname-parent-directory-pathname
                 (asdf:system-source-directory :starintel-gserver))))))
        (inside nil)
        (current nil)
        (blocks nil))
    (dolist (line (uiop:read-file-lines path))
      (cond
        ((string-equal line "#+begin_src lisp")
         (setf inside t current nil))
        ((and inside (string-equal line "#+end_src"))
         (push (format nil "~{~a~%~}" (nreverse current)) blocks)
         (setf inside nil current nil))
        (inside (push line current))))
    (nreverse blocks)))

(test documented-org-lisp-examples-execute-against-real-valkey
  (let ((*package* (find-package :star-server-tests)))
    (dolist (block (lease-org-lisp-blocks))
      (with-input-from-string (stream block)
        (loop for form = (read stream nil stream)
              until (eq form stream)
              do (eval form))))))
