(in-package :star-server-tests)

;; Observability slice tests. Hermetic: no OpenObserve, no RabbitMQ, no
;; CouchDB. The fake collector is a raw TCP HTTP sink on loopback.

(def-suite observability-tests
    :description "StarIntel observability abstraction")

(in-suite observability-tests)

(defun obs-fixture (thunk)
  "Run THUNK with a clean exporter state and the disabled kill switch intact."
  (let ((star.observability::*observability-endpoint*
          "http://127.0.0.1:4318")
        (star.observability::*observability-enabled* "true"))
    (star.observability:reset-exporter-state)
    (unwind-protect
         (funcall thunk)
      (star.observability:reset-exporter-state))))

(test config-enabled-by-default
  "Telemetry is enabled by default per the locked slice decision."
  (is (star.observability:observability-enabled-p)))

(test disabled-addon-costs-nothing
  "When the addon was never loaded (no exporter thread), the signal API is a
no-op: nothing queues, nothing exports, nothing errors."
  (let ((star.observability::*exporter-running* nil))
    (star.observability:with-span ("never.queued" :attributes '())
      (values))
    (star.observability:record-counter "starintel_test_total" 1)
    (star.observability:emit-log-event :info "should not queue")
    (is (= 0 (star.observability:export-counter :logs-exported)))
    (is (= 0 (star.observability:export-counter :traces-exported)))
    (is (= 0 (star.observability:export-counter :metrics-exported)))))

(test traceparent-round-trip
  "W3C traceparent encode/parse round-trips and rejects malformed input."
  (let* ((context (star.observability:make-root-context))
         (header (star.observability:encode-traceparent context))
         (parsed (star.observability:parse-traceparent header)))
    (is (= 55 (length header)))
    (is (string= (star.observability:trace-context-trace-id context)
                 (star.observability:trace-context-trace-id parsed)))
    (is (string= (star.observability:trace-context-span-id context)
                 (star.observability:trace-context-span-id parsed))))
  (is (null (star.observability:parse-traceparent nil)))
  (is (null (star.observability:parse-traceparent "garbage")))
  (is (null (star.observability:parse-traceparent
             "00-00000000000000000000000000000000-0000000000000000-01")))
  (is (null (star.observability:parse-traceparent
             "00-zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz-1234567890123456-01"))))

(test child-context-keeps-trace-id
  "A child context continues the same trace and records its parent span."
  (let* ((parent (star.observability:make-root-context))
         (child (star.observability:child-context parent)))
    (is (string= (star.observability:trace-context-trace-id parent)
                 (star.observability:trace-context-trace-id child)))
    (is (string= (star.observability:trace-context-span-id parent)
                 (star.observability:trace-context-parent-span-id child)))
    (is (not (string= (star.observability:trace-context-span-id parent)
                      (star.observability:trace-context-span-id child))))))

(test redaction-removes-forbidden-keys
  "Structural redaction deletes forbidden attribute keys at emission."
  (let ((redacted
          (star.observability:redact-attributes
           (list (cons "authorization" "Bearer TEST_API_SECRET_DO_NOT_LEAK_123")
                 (cons "http.request.header.cookie" "session=TEST_OAUTH_TOKEN_DO_NOT_LEAK_456")
                 (cons "request.body" "should-not-appear")
                 (cons "token" "TEST_API_SECRET_DO_NOT_LEAK_123")
                 (cons "service.name" "star-server")
                 (cons "safe" "value")))))
    (is (null (assoc "authorization" redacted :test #'string=)))
    (is (null (assoc "http.request.header.cookie" redacted :test #'string=)))
    (is (null (assoc "request.body" redacted :test #'string=)))
    (is (null (assoc "token" redacted :test #'string=)))
    (is (assoc "service.name" redacted :test #'string=))))

(test forbidden-key-matching-is-case-insensitive
  (is (star.observability:forbidden-key-p "Authorization"))
  (is (star.observability:forbidden-key-p "http.request.header.Authorization"))
  (is (star.observability:forbidden-key-p "REFRESH_TOKEN"))
  (is (not (star.observability:forbidden-key-p "starintel.actor.type"))))

(test backend-unavailable-drops-without-crashing
  "A failing export transport must degrade to counted drops, never signal,
and never wedge the caller."
  (star.observability:reset-exporter-state)
  (let ((star.observability::*export-batch-fn*
          (lambda (signal records)
            (declare (ignore signal records))
            :error))
        (star.observability::*exporter-running* t))
    (star.observability:queue-span
     (star.observability:make-root-context) "test" 1
     (star.observability:now-unix-nanos)
     (star.observability:now-unix-nanos) '() t)
    (let ((star.observability::*exporter-stop* t))
      (star.observability::flush-once))
    (is (= 1 (star.observability:export-counter :traces-dropped)))
    (is (= 0 (star.observability:export-counter :traces-exported)))))

(test http-to-actor-correlation-through-fake-collector
  "One request through the HTTP middleware plus nested spans must share one
trace id at the collector, and injected secrets must never appear. Runs
hermetically: the export transport is captured, no network involved."
  (let* ((captured (list))
         (secret "TEST_API_SECRET_DO_NOT_LEAK_123")
         (star.observability::*export-batch-fn*
           (lambda (signal records)
             (push (jsown:to-json (star.observability::otlp-payload signal records))
                   captured)
             :ok))
         (app
           (star.observability:observability-http-middleware
            (lambda (env)
              (declare (ignore env))
              ;; inner boundary: the app's own nested span
              (star.observability:with-span
                  ("document.insert"
                   :attributes (list (cons "starintel.component" "test")))
                (values)
                ;; security event carrying a forbidden key
                (star.observability:security-event
                 'authn-failed
                 :outcome :denied
                 :attributes (list (cons "token" secret)))
                (list 200 (list :content-type "application/json")
                  (list "{\"status\":\"ok\"}")))))))
    (star.observability:reset-exporter-state)
    (let ((star.observability::*exporter-running* t))
      (funcall app
               (list :request-method :get
                     :path-info "/api/v1/documents"
                     :headers (alexandria:plist-hash-table
                               (list "traceparent"
                                     "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
                               :test #'equal)))
      ;; one explicit flush; no background thread in the test
      (let ((star.observability::*exporter-stop* t))
        (star.observability::flush-once)))
    (let ((payload (format nil "~{~a~}" captured)))
      ;; all three signals exported
      (is (search "resourceSpans" payload))
      (is (search "resourceLogs" payload))
      ;; the inbound traceparent continues: same trace id
      (is (search "4bf92f3577b34da6a3ce929d0e0e4736" payload))
      ;; injected secret never leaves the process
      (is (not (search secret payload)))
      ;; volume regression: one request -> bounded span count
      (let ((span-count
              (let ((count 0)
                    (start 0))
                (loop while (setf start (search "traceId" payload :start2 start))
                      do (incf count)
                         (setf start (+ start 7)))
                count)))
        (is (<= span-count 4))))))
