(asdf:defsystem :starintel-gserver-integration-tests
  :version "0.1.0"
  :description "Service-backed integration tests for starintel-gserver"
  :author "nsaspy@airmail.cc"
  :license "GPL v3"
  :serial t
  :depends-on (#:starintel-gserver-tests)
  :components ((:module "t"
                :serial t
                :components ((:file "couchdb-view-integration-test")
                             (:file "valkey-lease-integration-test")
                             (:file "http-api-test")
                             (:file "run-integration-tests"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :star-server-tests
                               :run-all-integration-tests)))
