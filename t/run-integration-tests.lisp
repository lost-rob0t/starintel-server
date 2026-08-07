(in-package :star-server-tests)

(defun run-all-integration-tests ()
  (run-required-suite 'valkey-lease-integration-tests)
  (run-required-suite
   'couchdb-view-integration-tests
   :setup #'setup-couchdb-view-integration-tests
   :teardown #'teardown-couchdb-view-integration-tests)
  (run-http-api-tests)
  t)
