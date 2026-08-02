(in-package :star-server-tests)

(def-suite event-actor-tests
  :description "Actor-event codec, idempotency, and owner-thread settlement tests")

(in-suite event-actor-tests)

(defun valid-event-payload (&key (id "event-1")
                              (timestamp 1770000000)
                              (dtype "actorevent")
                              (actor-name "crawler")
                              (component "url-fetcher")
                              (event-type "document.fetched")
                              (details "ok")
                              (source-id "source-1")
                              (trace-id "trace-1")
                              (generation 2))
  (jsown:to-json
   (jsown:new-js
     ("_id" id)
     ("timestamp" timestamp)
     ("dtype" dtype)
     ("actorName" actor-name)
     ("component" component)
     ("eventType" event-type)
     ("details" details)
     ("sourceId" source-id)
     ("traceId" trace-id)
     ("generation" generation))))

(test actor-event-timestamp-and-dtype-initialize-independently
  (let ((event
          (star.actors:make-actor-event
           :id "event-independent"
           :timestamp 123456789
           :dtype "actorevent"
           :actor-name "scheduler"
           :event-type "target.started")))
    (is (= 123456789 (star.actors::event-timestamp event)))
    (is (string= "actorevent" (star.actors::doc-type event)))
    (is (string= "scheduler" (star.actors:event-component event)))))

(test legacy-event-fixture-migrates-through-one-codec
  (let* ((payload
           "{\"_id\":\"legacy-1\",\"timestamp\":1770000001,\"actorName\":\"legacy-actor\",\"eventType\":\"legacy.event\",\"details\":\"migrated\",\"sourceId\":\"source-old\"}")
         (event (star.actors:decode-actor-event payload)))
    (is (string= "legacy-1" (star.actors::event-id event)))
    (is (string= "actorevent" (star.actors::doc-type event)))
    (is (string= "legacy-actor" (star.actors:event-component event)))
    (is (= 0 (star.actors:event-generation event)))
    (is (string= "source-old"
                 (star.actors::event-source-document event)))))

(test valid-event-is-persisted-once-and-acked
  (let ((persist-count 0)
        (persisted-id nil))
    (let ((settlement
            (star.actors:process-event-delivery
             (valid-event-payload)
             :persist-fn
             (lambda (event)
               (incf persist-count)
               (setf persisted-id (star.actors::event-id event))
               (star.actors::make-couchdb-result
                :status :success
                :operation :insert
                :document-id persisted-id)))))
      (is (= 1 persist-count))
      (is (string= "event-1" persisted-id))
      (is (eq :ack
              (star.consumers:consumer-settlement-action settlement)))
      (is (eq :persisted
              (star.consumers:consumer-settlement-reason settlement))))))

(test duplicate-event-delivery-is-idempotently-acked
  (let ((persist-count 0))
    (let ((settlement
            (star.actors:process-event-delivery
             (valid-event-payload :id "duplicate-1")
             :persist-fn
             (lambda (event)
               (declare (ignore event))
               (incf persist-count)
               (star.actors::make-couchdb-result
                :status :exists
                :operation :insert
                :document-id "duplicate-1")))))
      (is (= 1 persist-count))
      (is (eq :ack
              (star.consumers:consumer-settlement-action settlement)))
      (is (eq :duplicate
              (star.consumers:consumer-settlement-reason settlement))))))

(test invalid-event-is-dead-lettered-without-persistence
  (let ((persist-called-p nil))
    (let ((settlement
            (star.actors:process-event-delivery
             "{\"_id\":\"invalid-1\",\"timestamp\":1770000002,\"actorName\":\"crawler\"}"
             :persist-fn
             (lambda (event)
               (declare (ignore event))
               (setf persist-called-p t)
               (error "Invalid events must not persist.")))))
      (is-false persist-called-p)
      (is (eq :dead-letter
              (star.consumers:consumer-settlement-action settlement)))
      (is (eq :invalid-event
              (star.consumers:consumer-settlement-reason settlement)))
      (is (typep (star.consumers:consumer-settlement-condition settlement)
                 'star.actors:invalid-actor-event)))))

(test persistence-failure-is-retried
  (let ((settlement
          (star.actors:process-event-delivery
           (valid-event-payload :id "retry-1")
           :persist-fn
           (lambda (event)
             (declare (ignore event))
             (star.actors::make-couchdb-result
              :status :error
              :operation :insert
              :document-id "retry-1"
              :error-message "CouchDB unavailable")))))
    (is (eq :retry
            (star.consumers:consumer-settlement-action settlement)))
    (is (eq :persistence-failed
            (star.consumers:consumer-settlement-reason settlement)))))

(test event-consumer-builds-bounded-retry-runtime
  (let* ((consumer
           (star.consumers:create-rabbit-consumer
            :name "events-test"
            :queue-name "events"
            :exchange-name "events"
            :routing-key "event.#"
            :queue-durable t
            :exchange-durable t
            :max-retries 4
            :quarantine-exchange "events.quarantine"
            :quarantine-queue "events.quarantine.queue"
            :handler-fn #'identity))
         (stream (star.consumers:consumer-stream consumer)))
    (is (typep consumer 'star.consumers:retrying-rabbit-consumer))
    (is (typep stream 'star.consumers:retrying-rabbit-queue-stream))
    (is-true (star.consumers:rabbit-stream-queue-durable-p stream))
    (is-true (star.consumers:rabbit-exchange-durable-p stream))
    (is (= 4
           (star.consumers:retry-policy-max-retries
            (star.consumers:retry-stream-policy stream))))))
