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
       (tls-verify-p t) (pool-size 8) (pool-wait-timeout-ms 500)
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
       (maximum-lifetime-ms 10000))
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
   :deadline (real-deadline)
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

(defun assert-real-backend-contract (store)
  "Reusable backend-neutral contract exercised by the real Valkey adapter."
  (let* ((identity (real-valkey-identity "contract-target"))
         (acquired
           (acquire-real-valkey-lease
            store identity "contract-acquire" "contract-owner"))
         (record (star.leases:lease-outcome-lease acquired)))
    (is (eq :acquired (star.leases:lease-outcome-code acquired)))
    (is (typep record 'star.leases:lease-record))
    (let ((observed
            (star.leases:get-lease
             store identity :deadline (real-deadline)
             :request-id "contract-get")))
      (is (eq :found (star.leases:lease-outcome-code observed)))
      (is (string= (star.leases:lease-record-lease-id record)
                   (star.leases:lease-record-lease-id
                    (star.leases:lease-outcome-lease observed)))))
    (let ((renewed
            (star.leases:renew-lease
             store identity
             :lease-id (star.leases:lease-record-lease-id record)
             :owner-principal-id "contract-owner"
             :service-instance-id "instance-contract-owner"
             :fencing-token (star.leases:lease-record-fencing-token record)
             :ttl-ms 1500
             :deadline (real-deadline)
             :request-id "contract-renew")))
      (is (eq :renewed (star.leases:lease-outcome-code renewed)))
      (is (plusp (- (star.leases:lease-record-expires-at
                     (star.leases:lease-outcome-lease renewed))
                    (real-unix-milliseconds)))))
    (let ((listed
            (star.leases:list-leases
             store :owner-principal-id "contract-owner"
             :program-id "program-a"
             :deadline (real-deadline)
             :request-id "contract-list")))
      (is (eq :listed (star.leases:lease-outcome-code listed)))
      (is (= 1 (length (star.leases:lease-outcome-leases listed)))))
    (is (eq :healthy
            (star.leases:lease-outcome-code
             (star.leases:backend-health
              store :deadline (real-deadline)
              :request-id "contract-health"))))
    (let ((released
            (star.leases:release-lease
             store identity
             :lease-id (star.leases:lease-record-lease-id record)
             :owner-principal-id "contract-owner"
             :service-instance-id "instance-contract-owner"
             :fencing-token (star.leases:lease-record-fencing-token record)
             :deadline (real-deadline)
             :request-id "contract-release")))
      (is (eq :released (star.leases:lease-outcome-code released))))
    (is (eq :not-found
            (star.leases:lease-outcome-code
             (star.leases:get-lease
              store identity :deadline (real-deadline)
              :request-id "contract-get-released"))))))

(test reusable-backend-contract-passes-against-real-valkey
  (with-real-valkey-store (store :label "contract")
    (assert-real-backend-contract store)))

(test one-hundred-concurrent-acquires-have-exactly-one-observed-owner
  (with-real-valkey-store (store :label "concurrency" :pool-size 16)
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
                                  (format nil "owner-~d" thread-index))))
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
