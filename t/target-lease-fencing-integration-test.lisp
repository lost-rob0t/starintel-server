(in-package :star-server-tests)

(def-suite target-lease-fencing-integration-tests
  :description "Real Valkey fenced commit linearization tests")

(in-suite target-lease-fencing-integration-tests)

(test stale-holder-cannot-commit-after-successor-acquires
  "Lease N must lose commit authority immediately when lease N+1 is acquired."
  (with-real-valkey-store (store :label "fenced-commit")
    (let* ((identity (real-valkey-identity "fenced-target"))
           (first
             (acquire-real-valkey-lease
              store identity "fenced-acquire-a" "owner-a"
              :ttl-ms 200 :maximum-lifetime-ms 10000))
           (lease-a (star.leases:lease-outcome-lease first)))
      (is (eq :acquired (star.leases:lease-outcome-code first)))
      (sleep 0.3)
      (let* ((second
               (acquire-real-valkey-lease
                store identity "fenced-acquire-b" "owner-b"
                :ttl-ms 2000 :maximum-lifetime-ms 10000))
             (lease-b (star.leases:lease-outcome-lease second)))
        (is (eq :acquired (star.leases:lease-outcome-code second)))
        (is (< (star.leases:lease-record-fencing-token lease-a)
               (star.leases:lease-record-fencing-token lease-b)))
        (is (eq :stale-token
                (star.leases:commit-fenced-intent
                 store identity lease-a
                 "intent-a" "{\"result\":\"stale\"}"
                 :deadline (real-deadline)
                 :request-id "fenced-commit-a")))
        (is (eq :committed
                (star.leases:commit-fenced-intent
                 store identity lease-b
                 "intent-b" "{\"result\":\"current\"}"
                 :deadline (real-deadline)
                 :request-id "fenced-commit-b")))
        (is-false
         (star.leases::valkey-test-command
          store (real-deadline) "GET"
          (star.leases::valkey-fenced-intent-key
           store identity "intent-a")))
        (is (string=
             "{\"result\":\"current\"}"
             (star.leases::valkey-test-command
              store (real-deadline) "GET"
              (star.leases::valkey-fenced-intent-key
               store identity "intent-b"))))))))
