(in-package :star.observability)

(defparameter *observability-enabled*
  (or (uiop:getenv "STAR_OBSERVABILITY_ENABLED") "true")
  "Whether telemetry is exported. Enabled by default per the locked
observability-slice decision; set STAR_OBSERVABILITY_ENABLED=false to disable.
Disabled mode starts no exporter thread and performs no network I/O.")

(defparameter *observability-endpoint*
  (or (uiop:getenv "STAR_OBSERVABILITY_ENDPOINT")
      "http://127.0.0.1:4318")
  "OTLP/HTTP base endpoint of the telemetry collector. The exporter appends
/v1/logs, /v1/traces and /v1/metrics. Applications never talk to OpenObserve
directly and hold no OpenObserve credential.")

(defparameter *observability-signals*
  (or (uiop:getenv "STAR_OBSERVABILITY_SIGNALS") "logs,metrics,traces")
  "Comma-separated signal list accepted by the collector path.")

(defparameter *trace-sample-rate*
  (let ((value (uiop:getenv "STAR_TRACE_SAMPLE_RATE")))
    (if value
        (or (ignore-errors
             (with-input-from-string (stream value)
               (let ((parsed (read stream nil nil)))
                 (when (realp parsed) (coerce parsed 'double-float)))))
            1.0d0)
        1.0d0))
  "Baseline SDK-side sampling rate (0.0..1.0). Tail sampling on the collector
is the policy authority; SDK sampling is a volume guard. The default sends
every trace and lets the collector sample (5% baseline with retention
classes).")

(defparameter *include-payloads* nil
  "When non-nil, span attributes may include message/document payload
snapshots. ALWAYS nil unless STAR_INCLUDE_PAYLOADS=true is set by an explicit
debug policy; forbidden keys are redacted regardless.")

(defparameter *actor-detail*
  (or (uiop:getenv "STAR_ACTOR_DETAIL") "normal")
  "Actor span detail level: minimal, normal, or verbose. Verbose adds
mailbox-depth gauges per send; message payloads are never recorded.")

(defparameter *prolog-detail*
  (or (uiop:getenv "STAR_PROLOG_DETAIL") "goal")
  "Prolog span detail: goal (default), phase, or predicate. Predicate-level
tracing is an explicit debug mode and must not be enabled in production.")

(defparameter *export-queue-limit* 8192
  "Maximum queued OTLP records per signal before the exporter drops and
counts. Observability must never block or OOM the application.")

(defparameter *export-batch-size* 256
  "Records per exporter flush.")

(defparameter *export-interval-seconds* 2
  "Seconds between exporter flush attempts.")

(defparameter *export-timeout-seconds* 2
  "Per-request HTTP timeout for OTLP export. Short by design: telemetry must
not slow the application.")

(defun observability-enabled-p ()
  "True when telemetry export is enabled by configuration."
  (member (string-downcase *observability-enabled*)
          '("1" "true" "yes" "on")
          :test #'string=))

(defun signal-enabled-p (signal)
  "True when SIGNAL (a keyword: :logs :metrics :traces) should be recorded:
the addon lifecycle started the exporter, the kill switch is on, and the
signal is configured."
  (and *exporter-running*
       (observability-enabled-p)
       (member (string-downcase (symbol-name signal))
               (uiop:split-string *observability-signals* :separator ",")
               :test #'string=)))
