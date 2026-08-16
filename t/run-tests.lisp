(in-package :star-server-tests)

(defparameter *required-unit-suites*
  '(runner-tests
    consumer-tests
    init-loader-tests
    target-routing-tests
    system-load-tests
    couchdb-actor-tests
    event-actor-tests
    dataset-export-tests
    couchdb-view-request-tests
    lease-store-contract-tests
    target-lease-service-tests
    http-boundary-tests
    http-auth-tests
    auth-users-tests
    authorization-policy-tests))

(defun run-all-gserver-tests ()
  "Run every hermetic unit suite and fail on empty, skipped, or failed tests."
  (format t "~&StarIntel Gserver unit tests~%")
  (run-required-suites *required-unit-suites*)
  t)
