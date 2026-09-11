(in-package :star.consumers)

;; Observability around-methods for consumer delivery. These join a
;; producer's trace via AMQP headers and record consume metrics. Handlers
;; and settlement semantics are untouched; any failure re-signals so the
;; existing retry/quarantine behavior is preserved. All of it is inactive
;; until the observability addon is loaded via init.lisp.

(defmethod consume :around
    ((consumer consumer) delivery)
  "Record consume metrics and a processing span around every delivery."
  (if star:*observability-active*
      (let* ((properties (delivery-observability-properties consumer))
             (trace-value (and properties (rabbit-header properties "traceparent")))
             (inbound (and trace-value (star.observability:parse-traceparent trace-value)))
             (start (star.observability:now-unix-nanos))
             (*current-trace-context*
               (star.observability:child-context
                (or inbound (star.observability:current-context)))))
        (star.observability:record-counter "starintel_rabbit_consumed_total" 1)
        (multiple-value-bind (result error-condition)
            (handler-case (values (call-next-method))
              (error (condition) (values nil condition)))
          (star.observability:queue-span
           (star.observability:current-context)
           "rabbit.process" 1
           start (star.observability:now-unix-nanos)
           (list (cons "messaging.system" "rabbitmq")
                 (cons "messaging.destination"
                       (or (ignore-errors
                            (rabbit-stream-queue-name (consumer-stream consumer)))
                           "unknown")))
           (null error-condition))
          (when error-condition
            (star.observability:record-counter
             "starintel_rabbit_consume_failures_total" 1))
          (if error-condition
              (error error-condition)
              result)))
      (call-next-method)))

(defun delivery-observability-properties (consumer)
  "AMQP properties captured when the delivery was read, if the consumer's
stream recorded them (retrying consumers do; plain rabbit consumers stash
them in consumer-read)."
  (let ((stream (consumer-stream consumer)))
    (or (and (slot-exists-p stream 'current-properties)
             (slot-value stream 'current-properties))
        nil)))
