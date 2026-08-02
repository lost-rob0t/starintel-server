(uiop:define-package :star-server-tests
  (:use :cl :fiveam)
  (:import-from #:star
                #:*ingest-workers*
                #:*couchdb-default-database*
                #:*couchdb-event-log-database*
                #:*rabbit-address*
                #:*rabbit-port*)
  (:import-from #:star.consumers
                #:consumer
                #:consumer-name
                #:consumer-worker-count
                #:consumer-take
                #:consumer-lock
                #:consumer-filter
                #:consumer-fn
                #:consumer-stream
                #:consumer-state
                #:consumer-in-flight
                #:consumer-unsettled
                #:consumer-failures
                #:consume
                #:with-consumer-lock
                #:rabbit-consumer
                #:rabbit-queue-stream
                #:rabbit-stream-host
                #:rabbit-stream-port
                #:rabbit-stream-user
                #:rabbit-stream-queue-name
                #:rabbit-stream-exchange
                #:rabbit-stream-routing-key
                #:rabbit-stream-open-p)
  (:import-from #:star.rabbit
                #:transient-p)
  (:import-from #:sento.actor-system
                #:make-actor-system)
  (:import-from #:sento.actor-context
                #:actor-of)
  (:import-from #:sento.actor
                #:tell
                #:reply)
  (:import-from #:sento.agent
                #:make-agent
                #:agent-get
                #:agent-update)
  (:import-from #:star.frontends.http-api
                #:*app*)
  (:import-from #:star.databases.couchdb
                #:init-db
                #:init-event-db)
  (:export #:run-all-gserver-tests
           #:run-all-integration-tests
           #:run-required-suite
           #:run-required-suites
           #:suite-summary
           #:suite-summary-name
           #:suite-summary-discovered
           #:suite-summary-executed
           #:suite-summary-passed
           #:suite-summary-failed
           #:suite-summary-skipped
           #:run-consumer-tests
           #:run-http-api-tests
           #:run-init-loader-tests
           #:run-target-routing-tests
           #:runner-tests
           #:system-load-tests
           #:consumer-tests
           #:http-api-tests
           #:init-loader-tests
           #:target-routing-tests)
  (:documentation "Unit and integration test support for starintel-gserver"))

(in-package :star-server-tests)
