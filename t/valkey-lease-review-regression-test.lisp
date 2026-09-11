(in-package :star-server-tests)

(in-suite valkey-lease-integration-tests)

(test atomic-list-validation-never-returns-a-stale-predecessor
  "A key discovered before release/reacquire is validated atomically at read
  time, so the predecessor cannot inherit the successor's TTL or authority."
  (with-real-valkey-store (store :label "list-race")
    (let* ((identity (real-valkey-identity "list-race-target"))
           (first
             (acquire-real-valkey-lease
              store identity "list-race-a" "owner-a"
              :ttl-ms 5000 :maximum-lifetime-ms 10000))
           (old (star.leases:lease-outcome-lease first))
           (active-key (star.leases::valkey-active-key store identity)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      ;; Treat ACTIVE-KEY as a key already discovered by SCAN. Release the old
      ;; holder and create a successor before validation occurs.
      (let ((released
              (star.leases:release-lease
               store identity
               :lease-id (star.leases:lease-record-lease-id old)
               :owner-principal-id (star.leases:lease-record-owner-principal-id old)
               :service-instance-id (star.leases:lease-record-service-instance-id old)
               :fencing-token (star.leases:lease-record-fencing-token old)
               :deadline (real-deadline)
               :request-id "list-race-release")))
        (is (eq :released (star.leases:lease-outcome-code released))))
      (let* ((second
               (acquire-real-valkey-lease
                store identity "list-race-b" "owner-b"
                :ttl-ms 5000 :maximum-lifetime-ms 10000))
             (current (star.leases:lease-outcome-lease second)))
        (is (eq :acquired (star.leases:lease-outcome-code second)))
        (is (> (star.leases:lease-record-fencing-token current)
               (star.leases:lease-record-fencing-token old)))
        (multiple-value-bind (validated failure)
            (star.leases::valkey-validated-active-record
             store active-key (real-deadline))
          (is-false failure)
          (is-true validated)
          (is (string=
               (star.leases:lease-record-lease-id current)
               (star.leases:lease-record-lease-id validated)))
          (is-false
           (string=
            (star.leases:lease-record-lease-id old)
            (star.leases:lease-record-lease-id validated))))
        (let ((listed
                (star.leases:list-leases
                 store :deadline (real-deadline)
                 :request-id "list-race-list")))
          (is (eq :listed (star.leases:lease-outcome-code listed)))
          (is (= 1 (length (star.leases:lease-outcome-leases listed))))
          (is (string=
               (star.leases:lease-record-lease-id current)
               (star.leases:lease-record-lease-id
                (first (star.leases:lease-outcome-leases listed))))))
        ;; A missing key reports PTTL=-2 and must never resurrect the previously
        ;; validated record.
        (star.leases::valkey-test-command
         store (real-deadline) "DEL" active-key)
        (is (= -2
               (star.leases::valkey-test-command
                store (real-deadline) "PTTL" active-key)))
        (multiple-value-bind (missing failure)
            (star.leases::valkey-validated-active-record
             store active-key (real-deadline))
          (is-false failure)
          (is-false missing))
        (let ((listed
                (star.leases:list-leases
                 store :deadline (real-deadline)
                 :request-id "list-race-list-missing")))
          (is (eq :listed (star.leases:lease-outcome-code listed)))
          (is (= 0 (length (star.leases:lease-outcome-leases listed)))))))))

(test corrupt-record-version-exercises-lisp-deserialization-guard
  "The Lua get path accepts the record shape, but Lisp rejects its unsupported
  record_version and translates that decode failure to :backend-unavailable."
  (with-real-valkey-store (store :label "corrupt-version")
    (let* ((identity (real-valkey-identity "corrupt-version-target"))
           (first
             (acquire-real-valkey-lease
              store identity "corrupt-version-a" "owner-a"
              :ttl-ms 5000 :maximum-lifetime-ms 10000))
           (active-key (star.leases::valkey-active-key store identity))
           (original-json
             (star.leases::valkey-test-command
              store (real-deadline) "GET" active-key)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      ;; Keep lock_key and identity valid so +valkey-get-script+ returns FOUND;
      ;; only the Lisp deserializer rejects this unsupported record version.
      (let ((tampered (jsown:parse original-json)))
        (setf (jsown:val tampered "record_version") 999)
        (star.leases::valkey-test-command
         store (real-deadline) "SET" active-key
         (jsown:to-json tampered) "PX" 5000))
      (let ((result
              (star.leases:get-lease
               store identity :deadline (real-deadline)
               :request-id "corrupt-version-get")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code result)))
        (is-false (star.leases:lease-outcome-lease result)))
      ;; Listing uses the same contract decoder after atomic per-key validation,
      ;; so the invalid record fails closed rather than escaping as a Lisp error.
      (let ((listed
              (star.leases:list-leases
               store :deadline (real-deadline)
               :request-id "corrupt-version-list")))
        (is (eq :backend-unavailable
                (star.leases:lease-outcome-code listed)))
        (is (= 0 (length (star.leases:lease-outcome-leases listed))))))))

(test pool-wait-rechecks-idle-connections-after-timed-out-wait
  "A waiter whose condition-wait expires without a delivered notify must
  recheck idle-connections at the loop head instead of failing with
  valkey-pool-timeout while a connection sits idle in the pool."
  (with-real-valkey-store (store :label "pool-recheck"
                                 :pool-size 1 :pool-wait-timeout-ms 500)
    (let ((seed (star.leases::open-valkey-connection store (real-deadline)))
          (started nil)
          (latch-lock (bt:make-lock "pool-recheck-latch"))
          (latch-cv (bt:make-condition-variable))
          (got-connection nil))
      ;; Occupy the pool's only slot before the waiter starts so it must
      ;; condition-wait: the seed connection is open and counted, not idle.
      (bt:with-lock-held ((star.leases::valkey-store-pool-lock store))
        (push seed (star.leases::valkey-store-all-connections store))
        (setf (star.leases::valkey-store-open-count store) 1))
      (let ((waiter
              (bt:make-thread
               (lambda ()
                 (bt:with-lock-held (latch-lock)
                   (setf started t)
                   (bt:condition-notify latch-cv))
                 (star.leases::call-with-valkey-connection
                  store (real-deadline 5000)
                  (lambda (connection)
                    (setf got-connection
                          (star.leases::valkey-connection-open-p connection))))))))
        (bt:with-lock-held (latch-lock)
          (loop until started do (bt:condition-wait latch-cv latch-lock)))
        ;; Land the idle connection while the waiter is inside its first
        ;; wait. Deliberately NO condition-notify: only the fixed loop's
        ;; post-timeout predicate recheck can observe the idle connection.
        (sleep 0.2)
        (bt:with-lock-held ((star.leases::valkey-store-pool-lock store))
          (push seed (star.leases::valkey-store-idle-connections store)))
        (bt:join-thread waiter)
        (is-true got-connection)))))
