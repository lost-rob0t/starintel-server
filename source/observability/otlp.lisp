(in-package :star.observability)

;; OTLP/HTTP-JSON exporter. Bounded per-signal queues; a background thread
;; flushes to the collector; drops are counted and surfaced as self-metrics.
;; Every failure path returns/drops rather than blocking or killing callers.

(defun make-queue ()
  "A simple bounded ring-free queue: (cons list list) with tail pointer."
  (cons nil nil))

(defparameter *otlp-queues*
  (list :logs (make-queue) :traces (make-queue) :metrics (make-queue))
  "Internal bounded queues per signal.")

(defparameter *export-counters*
  (list :logs-exported 0 :logs-dropped 0
        :traces-exported 0 :traces-dropped 0
        :metrics-exported 0 :metrics-dropped 0
        :export-failures 0)
  "Counters for self-observation. Incremented under the exporter lock.")

(defparameter *export-lock* (bt:make-lock "otlp-export")
  "Lock guarding queues and counters.")

(defparameter *exporter-thread* nil)
(defparameter *exporter-stop* nil)
(defparameter *exporter-running* nil
  "Non-nil while the exporter thread is active. All signal API entry points
check this so an addon that was never loaded by init.lisp costs nothing: no
queues, no ids, no counters.")

(defun exporter-running-p ()
  "True when the exporter thread was started via the addon lifecycle."
  (and (observability-enabled-p) *exporter-running*))

(defun queue-push (queue value limit)
  "Push VALUE onto QUEUE, dropping the push itself when at LIMIT. Returns
:queued or :dropped."
  (bt:with-lock-held (*export-lock*)
    (let ((count (+ (length (car queue)) (length (cdr queue)))))
      (if (>= count limit)
          :dropped
          (progn
            (push value (cdr queue))
            :queued)))))

(defun queue-drain (queue)
  "Pop all queued values (FIFO) into a fresh list, emptying QUEUE."
  (bt:with-lock-held (*export-lock*)
    (let ((reversed (nreverse (cdr queue))))
      (setf (cdr queue) nil)
      reversed)))

(defun counter-increment (name &optional (delta 1))
  (bt:with-lock-held (*export-lock*)
    (setf (getf *export-counters* name)
          (+ (getf *export-counters* name 0) delta))))

(defun export-counter (name)
  "Current value of exporter counter NAME (for tests and self-observation)."
  (bt:with-lock-held (*export-lock*)
    (getf *export-counters* name 0)))

(defun now-unix-nanos ()
  "Current time as OTLP unix nanos."
  (* (- (get-universal-time) 2208988800) 1000000000))

(defun attribute-value-json (value)
  "Encode an attribute VALUE as an OTLP AnyValue JSON object."
  (cond
    ((stringp value) (list "stringValue" value))
    ((integerp value) (list "intValue" value))
    ((floatp value) (list "doubleValue" value))
    ((eq value t) (list "boolValue" :true))
    ((null value) (list "boolValue" :false))
    (t (list "stringValue" (princ-to-string value)))))

(defun attributes-json (attributes)
  "Encode an attribute alist into OTLP JSON attribute objects."
  (let ((attributes (redact-attributes attributes)))
    (when attributes
      (loop for (key . value) in attributes
            collect (list "key" (string-downcase
                                 (if (symbolp key)
                                     (symbol-name key)
                                     (princ-to-string key)))
                          "value" (attribute-value-json value))))))

(defun encode-span (span)
  "Encode a span record into OTLP JSON."
  (let ((context (span-record-context span)))
    (list "traceId" (trace-context-trace-id context)
          "spanId" (trace-context-span-id context)
          "parentSpanId" (or (trace-context-parent-span-id context) "")
          "name" (span-record-name span)
          "kind" (span-record-kind span)
          "startTimeUnixNano" (span-record-start-nanos span)
          "endTimeUnixNano" (span-record-end-nanos span)
          "attributes" (attributes-json (span-record-attributes span))
          "status"
          (if (eq (span-record-status span) :error)
              (list "code" 2)
              (list "code" 1)))))

(defun span-record-p (record)
  (and (consp record) (span-record-p-1 record)))

(defun encode-log-record (record)
  (list "timeUnixNano" (log-record-time-nanos record)
        "severityText" (log-record-severity record)
        "body" (list "stringValue" (log-record-body record))
        "attributes" (attributes-json (log-record-attributes record))
        "traceId" (trace-context-trace-id (log-record-context record))
        "spanId" (trace-context-span-id (log-record-context record))))

(defun encode-metric (metric)
  "Encode a monotonic counter sample as an OTLP sum with delta temporality
(required for OpenObserve counters to aggregate correctly)."
  (list "name" (metric-record-name metric)
        "sum"
        (list "dataPoints"
              (list (list "asDouble"
                          (metric-record-value metric)
                          "timeUnixNano" (metric-record-time-nanos metric)
                          "attributes"
                          (attributes-json (metric-record-attributes metric))))
              "aggregationTemporality" 1
              "isMonotonic" :true)))

(defun otlp-payload (signal records)
  "Build the full OTLP/JSON payload for one signal batch."
  (let ((resource
          (list "resource"
                (list "attributes"
                      (attributes-json
                       (list (cons "service.name" *service-name*)
                             (cons "service.version" *service-version*)
                             (cons "service.instance.id" *service-instance-id*)
                             (cons "deployment.environment.name" *deployment-environment*)
                             (cons "starintel.node.id" *node-id*)
                             (cons "starintel.component" *component*)
                             (cons "starintel.runtime" (format nil "SBCL ~a" (lisp-implementation-version))))))
                "scopeLogs" nil "scopeSpans" nil "scopeMetrics" nil)))
    (case signal
      (:logs (list "resourceLogs"
                   (list (append resource
                                 (list "scopeLogs"
                                       (list (list "scope" (list "name" "star.observability")
                                                   "logRecords"
                                                   (mapcar #'encode-log-record records))))))))
      (:traces (list "resourceSpans"
                     (list (append resource
                                   (list "scopeSpans"
                                         (list (list "scope" (list "name" "star.observability")
                                                     "spans"
                                                     (mapcar #'encode-span records))))))))
      (:metrics (list "resourceMetrics"
                      (list (append resource
                                    (list "scopeMetrics"
                                          (list (list "scope" (list "name" "star.observability")
                                                      "metrics"
                                                      (mapcar #'encode-metric records)))))))))))

(defun otlp-path (signal)
  (case signal
    (:logs "/v1/logs")
    (:traces "/v1/traces")
    (:metrics "/v1/metrics")))

(defun export-batch-http (signal records)
  "POST one batch to the collector. Returns :ok or :error; never signals.
A wall-clock timeout wraps the HTTP call because dexador's refusal handling
varies between versions; telemetry must never wedge a caller."
  (when (null records)
    (return-from export-batch-http :ok))
  (handler-case
      (progn
        (bt:with-timeout
            ((+ 1 *export-timeout-seconds*))
          (dexador:request
           (concatenate 'string *observability-endpoint* (otlp-path signal))
           :method :post
           :headers (list (cons "Content-Type" "application/json"))
           :content (jsown:to-json (otlp-payload signal records))
           :connect-timeout *export-timeout-seconds*
           :timeout *export-timeout-seconds*
           :keep-alive nil))
        :ok)
    (error ()
      (counter-increment :export-failures)
      :error)))

(defparameter *export-batch-fn* #'export-batch-http
  "Transport for one export batch (SIGNAL RECORDS) -> :ok | :error.
Replaceable for tests; production uses the dexador HTTP transport.")

(defun export-batch (signal records)
  "Export one batch through the configured transport. Never signals."
  (funcall *export-batch-fn* signal records))

(defun flush-once ()
  "Drain all queues and export. Counts exported/dropped per signal."
  (dolist (signal '(:logs :traces :metrics))
    (let* ((records (queue-drain (getf *otlp-queues* signal)))
           (count (length records)))
      (when (plusp count)
        (if (and (signal-enabled-p signal)
                 (eq :ok (export-batch signal records)))
            (counter-increment
             (case signal (:logs :logs-exported) (:traces :traces-exported)
                   (otherwise :metrics-exported)) count)
            (counter-increment
             (case signal (:logs :logs-dropped) (:traces :traces-dropped)
                   (otherwise :metrics-dropped)) count))))))

(defun exporter-loop ()
  (loop until *exporter-stop*
        do (sleep *export-interval-seconds*)
           (ignore-errors (flush-once)))
  (ignore-errors (flush-once)))

(defun start-exporter ()
  "Start the background exporter thread when telemetry is enabled."
  (when (and (observability-enabled-p) (null *exporter-thread*))
    (setf *exporter-stop* nil
          *exporter-running* t
          *exporter-thread*
          (bt:make-thread #'exporter-loop
                          :name "starintel-otlp-exporter")))
  (values))

(defun stop-exporter ()
  "Stop the background exporter and do a final flush."
  (when *exporter-thread*
    (setf *exporter-stop* t)
    (let ((thread *exporter-thread*))
      (setf *exporter-thread* nil)
      (when thread (ignore-errors (bt:join-thread thread)))))
  (setf *exporter-running* nil)
  (values))

(defun reset-exporter-state ()
  "Reset queues and counters; used by tests."
  (bt:with-lock-held (*export-lock*)
    (setf *otlp-queues*
          (list :logs (make-queue) :traces (make-queue)
                :metrics (make-queue))
          *export-counters*
          (list :logs-exported 0 :logs-dropped 0
                :traces-exported 0 :traces-dropped 0
                :metrics-exported 0 :metrics-dropped 0
                :export-failures 0)))
  (values))
