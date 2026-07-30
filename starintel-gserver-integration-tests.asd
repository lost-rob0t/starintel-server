(asdf:defsystem :starintel-gserver-integration-tests
  :version "0.1.0"
  :description "Service-backed integration tests for starintel-gserver"
  :author "nsaspy@airmail.cc"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:starintel-gserver-tests)
  :components ((:module "t"
                :serial t
                :components ((:file "http-api-test")
                             (:file "run-integration-tests"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :star-server-tests
                               :run-all-integration-tests)))
