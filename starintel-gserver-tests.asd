(asdf:defsystem :starintel-gserver-tests
  :version      "0.1.0"
  :description  "Test suite for starintel-gserver"
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial t
  :depends-on   (#:starintel-gserver
                 #:fiveam
                 #:dexador
                 #:bordeaux-threads
                 #:jsown)
  :components   ((:module "t"
                  :serial t
                  :components ((:file "package")
                               (:file "consumers-test")
                               (:file "http-api-test")
                               (:file "run-tests"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :star-server-tests :run-all-gserver-tests)))

;;;; Test System for StarIntel Gserver
;;;;
;;;; This test system provides comprehensive unit tests for:
;;;; - Document consumer threads and RabbitMQ integration
;;;; - HTTP API endpoints and request handling
;;;; - Error handling and edge cases
;;;;
;;;; Running Tests:
;;;;
;;;; From REPL:
;;;;   (asdf:test-system :starintel-gserver)
;;;;
;;;; Or individually:
;;;;   (ql:quickload :starintel-gserver-tests)
;;;;   (star-server.tests:run-all-tests)
;;;;   (star-server.tests:run-consumer-tests)
;;;;   (star-server.tests:run-http-api-tests)
;;;;
;;;; Note: Some tests require running services (CouchDB, RabbitMQ)
;;;; and will gracefully skip if services are unavailable.
