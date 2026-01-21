(uiop:define-package :star-server-tests
  (:use :cl :fiveam)
  (:import-from #:star
                #:*injest-workers*
                #:*couchdb-default-database*
                #:*couchdb-event-log-database*
                #:*rabbit-address*
                #:*rabbit-port*)
  (:import-from #:star.consumers
                #:consumer
                #:consumer-name
                #:consumer-channel
                #:consumer-take
                #:consumer-lock
                #:consumer-filter
                #:consumer-fn
                #:consumer-stream
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
  (:import-from #:star.actors
                #:make-actor-system
                #:actor-of
                #:tell
                #:reply
                #:make-agent
                #:agent-get
                #:agent-update)
  (:import-from #:star.frontends.http-api
                #:*app*)
  (:import-from #:star.databases.couchdb
                #:init-db
                #:init-event-db)
  (:export #:run-all-gserver-tests
           #:run-consumer-tests
           #:run-http-api-tests
           #:run-init-loader-tests
           #:run-target-routing-tests
           #:consumer-tests
           #:http-api-tests
           #:init-loader-tests
           #:target-routing-tests)
  (:documentation "Test suite for starintel-gserver including consumer threads and HTTP API tests"))

(in-package :star-server-tests)
