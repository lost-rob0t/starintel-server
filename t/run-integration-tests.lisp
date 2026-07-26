(in-package :star-server-tests)

(defun run-all-integration-tests ()
  (run-http-api-tests)
  t)
