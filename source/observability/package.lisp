(uiop:define-package :star.observability
  (:use :cl)
  (:import-from #:jsown)
  (:export
   ;; config
   #:observability-enabled-p
   #:signal-enabled-p
   #:exporter-running-p
   #:*observability-enabled*
   #:*observability-endpoint*
   #:*observability-signals*
   #:*trace-sample-rate*
   #:*include-payloads*
   #:*actor-detail*
   #:*prolog-detail*
   #:*service-name*
   #:*service-version*
   #:*service-instance-id*
   #:*deployment-environment*
   #:*node-id*
   #:*component*
   ;; trace context
   #:trace-context
   #:trace-context-trace-id
   #:trace-context-span-id
   #:trace-context-parent-span-id
   #:make-root-context
   #:child-context
   #:current-context
   #:parse-traceparent
   #:encode-traceparent
   #:*current-trace-context*
   ;; redaction
   #:redact-attributes
   #:forbidden-key-p
   ;; otlp exporter
   #:start-exporter
   #:stop-exporter
   #:export-counter
   #:reset-exporter-state
   #:now-unix-nanos
   #:queue-span
   #:flush-once
   ;; api
   #:with-span
   #:emit-log-event
   #:record-counter
   #:security-event
   ;; rabbit trace propagation helpers
   #:inject-rabbit-trace-context
   #:extract-rabbit-trace-context
   ;; http middleware
   #:observability-http-middleware
   #:status-class
   #:normalized-route)
  (:documentation
   "StarIntel telemetry abstraction.

Applications depend on this package, never on an OpenObserve client. The
exporter speaks OTLP/HTTP-JSON to a local OpenTelemetry Collector; no
credential is ever held by application code.

The disabled configuration is a fully functional no-op: no exporter thread,
no network calls, no external dependency."))
(in-package :star.observability)
