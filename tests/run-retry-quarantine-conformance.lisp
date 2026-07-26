(in-package :cl-user)

(defclass retry-test-stream (star.consumers:retrying-rabbit-queue-stream)
  ((settlements
    :initform nil
    :accessor retry-test-stream-settlements)))

(defmethod star.consumers:stream-settle
    ((stream retry-test-stream) delivery settlement)
  (declare (ignore delivery))
  (push settlement (retry-test-stream-settlements stream))
  settlement)

(defun retry-test-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun retry-test-properties (attempt)
  (list
   (cons :message-id "message-1")
   (cons :correlation-id "trace-1")
   (cons :headers
         (list
          (cons "x-starintel-attempt" attempt)
          (cons "x-starintel-trace-id" "trace-1")
          (cons "x-starintel-first-seen-at" "2026-07-26T00:00:00Z")))))

(defun retry-test-stream (&key (attempt 0) (body "payload"))
  (let ((stream
          (make-instance
           'retry-test-stream
           :queue-name "retry-test"
           :exchange-name "documents"
           :routing-key "documents.ingest.person"
           :retry-policy
           (star.consumers:make-retry-policy
            :max-retries 2
            :base-delay-ms 100
            :max-delay-ms 1000
            :jitter-ratio 0.0d0))))
    (setf (star.consumers:retry-stream-current-body stream) body
          (star.consumers:retry-stream-current-properties stream)
          (retry-test-properties attempt)
          (star.consumers:retry-stream-current-exchange stream) "documents"
          (star.consumers:retry-stream-current-routing-key stream)
          "documents.ingest.person"
          (star.consumers:retry-stream-current-received-at stream)
          "2026-07-26T00:00:01Z")
    stream))

(defun retry-test-consumer (stream handler &key (max-retries 2))
  (make-instance
   'star.consumers:retrying-rabbit-consumer
   :name "retry-test"
   :workers 1
   :stream stream
   :fn handler
   :test-fn #'identity
   :on-error :retry
   :on-filter :filtered-ack
   :retry-policy
   (star.consumers:make-retry-policy
    :max-retries max-retries
    :base-delay-ms 100
    :max-delay-ms 1000
    :jitter-ratio 0.0d0)))

(defun retry-test-single-settlement (stream)
  (let ((settlements (retry-test-stream-settlements stream)))
    (retry-test-check (= 1 (length settlements))
                      "Expected one settlement, got ~d"
                      (length settlements))
    (first settlements)))

(defun test-invalid-json-dead-letters-immediately ()
  (let* ((stream (retry-test-stream :attempt 0 :body "{not-json"))
         (consumer
           (retry-test-consumer
            stream
            (lambda (self delivery)
              (declare (ignore self))
              (star.rabbit:decode-rabbit-document delivery)))))
    (star.consumers:consumer-process-delivery
     consumer (cons "{not-json" 1))
    (let ((settlement (retry-test-single-settlement stream)))
      (retry-test-check
       (eq :dead-letter
           (star.consumers:consumer-settlement-action settlement))
       "Invalid JSON did not dead-letter")
      (retry-test-check
       (typep (star.consumers:consumer-settlement-condition settlement)
              'star.consumers:schema-invalid-delivery-error)
       "Invalid JSON was not classified as schema-invalid"))))

(defun test-transient-failure-is-bounded ()
  (loop for attempt from 0 to 2
        for expected in '(:retry :retry :dead-letter)
        do
           (let* ((stream (retry-test-stream :attempt attempt))
                  (consumer
                    (retry-test-consumer
                     stream
                     (lambda (self delivery)
                       (declare (ignore self delivery))
                       (error 'star.consumers:transient-delivery-error
                              :reason "temporary outage"))
                     :max-retries 2)))
             (star.consumers:consumer-process-delivery
              consumer (cons "payload" attempt))
             (let ((settlement (retry-test-single-settlement stream)))
               (retry-test-check
                (eq expected
                    (star.consumers:consumer-settlement-action settlement))
                "Attempt ~d expected ~s but got ~s"
                attempt expected
                (star.consumers:consumer-settlement-action settlement))))))

(defun test-conflict-is-permanent-and-idempotency-aware ()
  (let* ((condition
           (make-condition
            'star.databases.couchdb:mutation-conflict
            :mutation-id "mutation-1"
            :document-id "person:1"
            :reason "idempotency key reused"))
         (failure
           (star.consumers:classify-delivery-condition condition))
         (policy
           (star.consumers:make-retry-policy :max-retries 9)))
    (retry-test-check
     (typep failure 'star.consumers:conflict-delivery-error)
     "Mutation conflict did not map to conflict failure")
    (retry-test-check
     (eq :dead-letter
         (star.consumers:retry-action-for policy failure 0))
     "Conflict was incorrectly made retryable")))

(defun test-retry-backoff-is_exponential_bounded_and_jittered ()
  (let ((policy
          (star.consumers:make-retry-policy
           :max-retries 4
           :base-delay-ms 100
           :max-delay-ms 250
           :jitter-ratio 0.20d0)))
    (retry-test-check
     (= 80 (star.consumers:retry-delay-ms policy 0 0.0d0))
     "Lower jitter bound was wrong")
    (retry-test-check
     (= 120 (star.consumers:retry-delay-ms policy 0 1.0d0))
     "Upper jitter bound was wrong")
    (retry-test-check
     (= 250 (star.consumers:retry-delay-ms policy 5 0.5d0))
     "Retry delay exceeded cap")))

(defun test-quarantine-record-preserves-provenance ()
  (let* ((stream (retry-test-stream :attempt 2 :body "broken-body"))
         (settlement
           (star.consumers:settlement-dead-letter
            "bad schema"
            (make-condition
             'star.consumers:schema-invalid-delivery-error
             :reason "bad schema")))
         (record (star.consumers:quarantine-record stream settlement)))
    (retry-test-check
     (string= "quarantined" (jsown:val record "status"))
     "Quarantine status missing")
    (retry-test-check
     (string= "schema-invalid" (jsown:val record "failure_class"))
     "Failure class missing")
    (retry-test-check
     (string= "documents.ingest.person"
              (jsown:val record "original_routing_key"))
     "Original routing key missing")
    (retry-test-check
     (string= "message-1" (jsown:val record "message_id"))
     "Message id missing")
    (retry-test-check
     (string= "trace-1" (jsown:val record "trace_id"))
     "Trace id missing")
    (retry-test-check
     (= 2 (jsown:val record "attempt_count"))
     "Attempt count missing")
    (retry-test-check
     (string= "broken-body" (jsown:val record "original_body"))
     "Original body missing")))

(defun test-corrected-replay-resets_attempts_and_preserves_lineage ()
  (let ((record (jsown:empty-object)))
    (setf (jsown:val record "_id") "quarantine:1"
          (jsown:val record "trace_id") "old-trace"
          (jsown:val record "replay_count") 1
          (jsown:val record "original_body") "bad"
          (jsown:val record "original_exchange") "documents"
          (jsown:val record "original_routing_key")
          "documents.ingest.person")
    (multiple-value-bind (body properties exchange routing-key)
        (star.consumers:quarantine-replay-envelope
         record :corrected-body "corrected")
      (retry-test-check (string= "corrected" body)
                        "Corrected body was not used")
      (retry-test-check (string= "documents" exchange)
                        "Original exchange was lost")
      (retry-test-check
       (string= "documents.ingest.person" routing-key)
       "Original routing key was lost")
      (retry-test-check
       (= 0 (star.consumers:delivery-attempt properties))
       "Replay did not reset attempt count")
      (retry-test-check
       (string= "old-trace"
                (star.consumers:rabbit-header
                 properties "x-starintel-parent-trace-id"))
       "Replay did not preserve trace lineage")
      (retry-test-check
       (string= "quarantine:1"
                (star.consumers:rabbit-header
                 properties "x-starintel-replay-of"))
       "Replay did not preserve quarantine provenance")
      (retry-test-check
       (= 2
          (star.consumers:rabbit-header
           properties "x-starintel-replay-count"))
       "Replay count did not advance"))))

(defun run-retry-quarantine-conformance-tests ()
  (format t "~&Running bounded retry and quarantine tests~%")
  (test-invalid-json-dead-letters-immediately)
  (test-transient-failure-is-bounded)
  (test-conflict-is-permanent-and-idempotency-aware)
  (test-retry-backoff-is_exponential_bounded_and_jittered)
  (test-quarantine-record-preserves-provenance)
  (test-corrected-replay-resets_attempts_and_preserves_lineage)
  (format t "~&Bounded retry and quarantine tests passed~%")
  t)

(run-retry-quarantine-conformance-tests)
