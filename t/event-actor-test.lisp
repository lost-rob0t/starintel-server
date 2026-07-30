(in-package :star-server-tests)

(def-suite event-actor-tests
  :description "Actor-event codec, idempotency, and Rabbit settlement tests")

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
              (star.consumers:rabbit-settlement-action settlement)))
      (is (eq :persisted
              (star.consumers:rabbit-settlement-reason settlement))))))

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
              (star.consumers:rabbit-settlement-action settlement)))
      (is (eq :duplicate
              (star.consumers:rabbit-settlement-reason settlement))))))

(test invalid-event-is-quarantined-and-settled
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
      (is (eq :nack
              (star.consumers:rabbit-settlement-action settlement)))
      (is-false
       (star.consumers:rabbit-settlement-requeue settlement))
      (is (eq :invalid-event
              (star.consumers:rabbit-settlement-reason settlement))))))

(test persistence-failure-is-requeued
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
    (is (eq :nack
            (star.consumers:rabbit-settlement-action settlement)))
    (is-true
     (star.consumers:rabbit-settlement-requeue settlement))
    (is (eq :persistence-failed
            (star.consumers:rabbit-settlement-reason settlement)))))

(test rabbit-settlement-uses-owning-connection-and-delivery-tag
  (let* ((stream
           (make-instance
            'star.consumers:settled-rabbit-queue-stream
            :queue-name "events"
            :exchange-name "events"
            :routing-key "event.#"
            :rabbit-connection :owning-connection))
         (consumer
           (make-instance
            'star.consumers:rabbit-consumer
            :name "settlement-test"
            :workers 1
            :stream stream))
         (ack-arguments nil)
         (nack-arguments nil))
    (star.consumers:settle-rabbit-delivery
     consumer
     (cons "{}" 41)
     (star.consumers:rabbit-ack :reason :persisted)
     :ack-fn
     (lambda (connection channel delivery-tag &key multiple)
       (setf ack-arguments
             (list connection channel delivery-tag multiple)))
     :nack-fn
     (lambda (&rest arguments)
       (setf nack-arguments arguments)))
    (is (equal '(:owning-connection 1 41 nil) ack-arguments))
    (is (null nack-arguments))
    (star.consumers:settle-rabbit-delivery
     consumer
     (cons "{}" 42)
     (star.consumers:rabbit-nack
      :reason :invalid-event
      :requeue nil)
     :ack-fn
     (lambda (&rest arguments)
       (setf ack-arguments arguments))
     :nack-fn
     (lambda (connection channel delivery-tag &key multiple requeue)
       (setf nack-arguments
             (list connection channel delivery-tag multiple requeue))))
    (is (equal '(:owning-connection 1 42 nil nil)
               nack-arguments))))

(test event-consumer-declares-durable-dead-letter-policy
  (let* ((consumer
           (star.consumers:create-rabbit-consumer
            :name "events-test"
            :queue-name "events"
            :exchange-name "events"
            :routing-key "event.#"
            :queue-durable t
            :exchange-durable t
            :dead-letter-exchange "events.dead-letter"
            :dead-letter-routing-key "events.invalid"
            :dead-letter-queue "events.quarantine"
            :handler-fn #'identity))
         (stream (star.consumers:consumer-stream consumer)))
    (is (typep stream 'star.consumers:settled-rabbit-queue-stream))
    (is-true (star.consumers:rabbit-stream-queue-durable-p stream))
    (is-true (star.consumers:rabbit-exchange-durable-p stream))
    (is (string= "events.dead-letter"
                 (star.consumers:rabbit-stream-dead-letter-exchange stream)))
    (is (string= "events.invalid"
                 (star.consumers:rabbit-stream-dead-letter-routing-key stream)))
    (is (string= "events.quarantine"
                 (star.consumers:rabbit-stream-dead-letter-queue stream)))))
