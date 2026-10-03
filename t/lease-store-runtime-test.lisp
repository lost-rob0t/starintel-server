(in-package :star-server-tests)

;; Runtime lease-store composition root tests. Hermetic: no Valkey binary,
;; no network. The valkey backend is only constructed (which validates
;; configuration and opens no connections); its live behavior is covered by
;; the valkey-lease-integration-tests suite.

(def-suite lease-store-runtime-tests
    :description "Runtime lease-store backend selection and metrics wiring")

(in-suite lease-store-runtime-tests)

(defmacro with-lease-store-fixture (&body body)
  "Run BODY with metrics enabled for signal assertions, then restore the
global lease store and exporter state so neighboring suites are unaffected."
  `(let ((star.observability::*observability-endpoint* "http://127.0.0.1:4318")
         (star.observability::*observability-enabled* "true")
         (star.observability::*exporter-running* t)
         (saved-store star:*lease-store*))
     (star.observability:reset-exporter-state)
     (unwind-protect
          (progn ,@body)
       (ignore-errors (star:shutdown-lease-store))
       (setf star:*lease-store* saved-store)
       (star.observability:reset-exporter-state))))

(defun drained-lease-metric-names ()
  "Drain the queued OTLP metric samples and return their counter names."
  (mapcar #'star.observability::metric-record-name
          (drained-lease-metric-samples)))

(defun drained-lease-metric-samples ()
  "Drain the queued OTLP metric samples."
  (star.observability::queue-drain
   (getf star.observability::*otlp-queues* :metrics)))

(defun lease-metric-attribute (record)
  (cdr (assoc "outcome" (star.observability::metric-record-attributes record)
              :test #'string=)))

(defun lease-test-deadline (&optional (milliseconds 3000))
  (+ (* 1000 (- (get-universal-time) 2208988800)) milliseconds))

(test default-lease-backend-is-memory
  "Without operator opt-in the runtime selects the in-process memory
backend, matching the historical inline lease behavior."
  (is (string= "memory" star:*lease-store-backend*)))

(test initialize-lease-store-selects-memory-store-by-default
  "The default backend builds a memory store and installation is idempotent."
  (with-lease-store-fixture
    (let ((store (star:initialize-lease-store :force t)))
      (is (typep store 'star.leases:memory-lease-store))
      (is (eq store star:*lease-store*))
      (is (functionp (star.leases::memory-store-metrics-hook store)))
      (is (eq store (star:initialize-lease-store))))))

(test initialize-lease-store-selects-valkey-store-per-config
  "Selecting valkey builds a valkey store from the VALKEY_* knobs with the
observability hook attached, without any connection attempt."
  (uiop:with-temporary-file (:stream stream :pathname password-path
                             :suffix "valkey-password")
    (write-line "lease-unit-test-password" stream)
    (finish-output stream)
    (close stream)
    (with-lease-store-fixture
      (let ((star:*lease-store-backend* "valkey")
            (star:*valkey-lease-host* "127.0.0.1")
            (star:*valkey-lease-port* 6390)
            (star:*valkey-lease-password-file* (namestring password-path)))
        (let ((store (star:initialize-lease-store :force t)))
          (is (typep store 'star.leases:valkey-lease-store))
          (is (string= "127.0.0.1" (star.leases::valkey-store-host store)))
          (is (= 6390 (star.leases::valkey-store-port store)))
          (is (functionp (star.leases::valkey-store-metrics-hook store)))
          (is (null (star.leases::valkey-store-audit-hook store)))
          (is (zerop (star.leases::valkey-pool-open-count store))))))))

(test valkey-backend-without-password-file-signals-config-error
  "An explicit valkey selection without a password file fails fast instead
of surfacing as a runtime backend error."
  (with-lease-store-fixture
    (let ((star:*lease-store-backend* "valkey")
          (star:*valkey-lease-password-file* nil))
      (signals error (star:initialize-lease-store :force t))
      (is (null star:*lease-store*)))))

(test unsupported-lease-backend-signals-config-error
  "An unknown backend value is a loud configuration error, not a fallback."
  (with-lease-store-fixture
    (let ((star:*lease-store-backend* "consul"))
      (signals error (star:initialize-lease-store :force t)))))

(test shutdown-lease-store-is-idempotent
  "Shutting down an uninitialized store is a no-op; after shutdown the
global is cleared and a second shutdown does not signal."
  (with-lease-store-fixture
    (let ((star:*lease-store-backend* "memory"))
      (star:initialize-lease-store :force t)
      (is (star:shutdown-lease-store))
      (is (null star:*lease-store*))
      (is-false (star:shutdown-lease-store)))))

(test observability-lease-metrics-hook-dispatches-outcome-counters
  "The hook parses the store event plist and selects the matching counters."
  (with-lease-store-fixture
    (let ((hook (star:observability-lease-metrics-hook)))
      (funcall hook (list :operation :renew :request-id "hook-renew"
                          :code :stale-token :retryable-p nil))
      (funcall hook (list :operation :acquire :request-id "hook-conflict"
                          :code :conflict :retryable-p t))
      (funcall hook (list :operation :acquire :request-id "hook-acquire"
                          :code :acquired :retryable-p nil))
      (funcall hook (list :operation :release :request-id "hook-release"
                          :code :released :retryable-p nil))
      (let ((names (drained-lease-metric-names)))
        (is (= 1 (count "starintel_lease_stale_writer_rejections_total"
                        names :test #'string=)))
        (is (= 1 (count "starintel_lease_conflicts_total"
                        names :test #'string=)))
        (is (= 1 (count "starintel_lease_acquisitions_total"
                        names :test #'string=)))
        (is (= 1 (count "starintel_lease_outcomes_total"
                        names :test #'string=)))))))

(test stale-writer-rejection-increments-counter-through-store
  "End to end over the installed store: acquisition, contention and a stale
fencing-token renewal increment the observability counters."
  (with-lease-store-fixture
    (let* ((star:*lease-store-backend* "memory")
           (store (star:initialize-lease-store :force t))
           (identity
             (star.leases:make-lease-identity
              :tenant-id "tenant-a" :program-id "program-a"
              :target-namespace "target" :target-id "runtime-metrics-target"
              :actor-name "actor-a" :workflow-name "workflow-a"
              :operation-class "default"))
           (acquired
             (star.leases:acquire-lease
              store identity
              :owner-principal-id "runtime-owner"
              :owner-client-id "runtime-client"
              :owner-credential-id "runtime-credential"
              :service-instance-id "runtime-instance"
              :ttl-ms 1000 :maximum-lifetime-ms 10000
              :execution-id "runtime-execution"
              :job-id "runtime-job"
              :trace-id "runtime-trace"
              :deadline (lease-test-deadline)
              :request-id "metrics-acquire"))
           (rival
             (star.leases:acquire-lease
              store identity
              :owner-principal-id "runtime-rival"
              :owner-client-id "rival-client"
              :owner-credential-id "rival-credential"
              :service-instance-id "rival-instance"
              :ttl-ms 1000 :maximum-lifetime-ms 10000
              :execution-id "rival-execution"
              :job-id "rival-job"
              :trace-id "rival-trace"
              :deadline (lease-test-deadline)
              :request-id "metrics-conflict"))
           (record (star.leases:lease-outcome-lease acquired))
           (stale
             (star.leases:renew-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id record)
              :owner-principal-id "runtime-owner"
              :service-instance-id "runtime-instance"
              :fencing-token 99
              :ttl-ms 1000
              :deadline (lease-test-deadline)
              :request-id "metrics-stale-renew")))
      (is (eq :acquired (star.leases:lease-outcome-code acquired)))
      (is (eq :conflict (star.leases:lease-outcome-code rival)))
      (is (eq :stale-token (star.leases:lease-outcome-code stale)))
      (let ((names (drained-lease-metric-names)))
        (is (= 1 (count "starintel_lease_acquisitions_total"
                        names :test #'string=)))
        (is (= 1 (count "starintel_lease_conflicts_total"
                        names :test #'string=)))
        (is (= 1 (count "starintel_lease_stale_writer_rejections_total"
                        names :test #'string=)))))))

(test non-special-outcomes-use-bounded-outcome-counter
  "Outcomes without a dedicated counter land in starintel_lease_outcomes_total
with the outcome name as a bounded label."
  (with-lease-store-fixture
    (let* ((star:*lease-store-backend* "memory")
           (store (star:initialize-lease-store :force t))
           (identity
             (star.leases:make-lease-identity
              :tenant-id "tenant-a" :program-id "program-a"
              :target-namespace "target" :target-id "runtime-release-target"
              :actor-name "actor-a" :workflow-name "workflow-a"
              :operation-class "default"))
           (acquired
             (star.leases:acquire-lease
              store identity
              :owner-principal-id "runtime-owner"
              :owner-client-id "runtime-client"
              :owner-credential-id "runtime-credential"
              :service-instance-id "runtime-instance"
              :ttl-ms 1000 :maximum-lifetime-ms 10000
              :execution-id "runtime-execution"
              :job-id "runtime-job"
              :trace-id "runtime-trace"
              :deadline (lease-test-deadline)
              :request-id "release-acquire"))
           (record (star.leases:lease-outcome-lease acquired))
           (released
             (star.leases:release-lease
              store identity
              :lease-id (star.leases:lease-record-lease-id record)
              :owner-principal-id "runtime-owner"
              :service-instance-id "runtime-instance"
              :fencing-token (star.leases:lease-record-fencing-token record)
              :deadline (lease-test-deadline)
              :request-id "release-request")))
      (is (eq :released (star.leases:lease-outcome-code released)))
      (let* ((samples (drained-lease-metric-samples))
             (outcome-samples
               (loop for sample in samples
                     when (string= "starintel_lease_outcomes_total"
                                   (star.observability::metric-record-name
                                    sample))
                       collect sample)))
        (is (= 1 (length outcome-samples)))
        (is (= 1 (count "released"
                        (mapcar #'lease-metric-attribute outcome-samples)
                        :test #'string=)))))))