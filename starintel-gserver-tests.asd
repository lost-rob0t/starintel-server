(asdf:defsystem :starintel-gserver-tests
  :version      "0.1.0"
  :description  "Hermetic unit test suite for starintel-gserver"
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial t
  :depends-on   (#:starintel-gserver
                  #:starintel-gserver-client
                  #:star-cli
                  #:star-ui
                  #:star-migrations
                  #:fiveam
                  #:dexador
                  #:bordeaux-threads
                  #:jsown)
  :components   ((:module "t"
                   :serial t
                   :components ((:file "package")
                                (:file "test-runner")
                                (:file "test-runner-test")
                                (:file "consumers-test")
                                (:file "init-loader-test")
                                (:file "target-routing-test")
                                (:file "system-load-test")
                                (:file "couchdb-actor-test")
                                (:file "run-tests"))))
  :perform (test-op (o c)
                     (declare (ignore o c))
                     (uiop:symbol-call :star-server-tests :run-all-gserver-tests)))

;;;; Canonical unit entry point:
;;;;   (asdf:test-system :starintel-gserver-tests)
;;;;
;;;; Service-backed coverage is intentionally isolated in
;;;; :starintel-gserver-integration-tests.
