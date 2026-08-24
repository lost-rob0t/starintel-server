(asdf:defsystem :starintel-gserver-tests
  :version "0.1.0"
  :description "Hermetic unit test suite for starintel-gserver"
  :author "nsaspy@airmail.cc"
  :license "GPL v3"
  :serial t
  :depends-on
  (#:starintel-gserver
   #:starintel-gserver-client
   #:star-cli
   #:star-ui
   #:star-migrations
   #:fiveam
   #:dexador
   #:bordeaux-threads
   #:jsown)
  :components
  ((:module "t"
    :serial t
    :components
    ((:file "package")
     (:file "test-runner")
     (:file "test-runner-test")
     (:file "consumers-test")
     (:file "init-loader-test")
     (:file "target-routing-test")
     (:file "system-load-test")
     (:file "couchdb-actor-test")
     (:file "couchdb-session-test")
     (:file "event-actor-test")
     (:file "dataset-export-test")
     (:file "couchdb-view-request-test")
     (:file "lease-store-contract-test")
     (:file "http-boundary-test")
     (:file "http-capabilities-test")
     (:file "http-auth-test")
     (:file "auth-users-test")
     (:file "oauth-authorization-code-test")
     (:file "oauth-http-bearer-test")
     (:file "oauth-http-provider-test")
     (:file "addon-system-test")
     (:file "http-auth-oracle-test")
     (:file "http-auth-immutability-test")
     (:file "authorization-policy-test")
     (:file "authorization-services-final-test")
     (:file "authorization-quota-test")
     (:file "gserver-client-test")
     (:file "gserver-client-final-test")
     (:file "runtime-lifecycle-test")
     (:file "run-tests"))))
  :perform
  (test-op (operation component)
    (declare (ignore operation component))
    (uiop:symbol-call
     :star-server-tests
     :run-all-gserver-tests)))

;;;; Canonical unit entry point:
;;;;   (asdf:test-system :starintel-gserver-tests)
;;;; Service-backed coverage is isolated in
;;;; :starintel-gserver-integration-tests.
