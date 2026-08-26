(in-package :star.actors)

(defun target-delivery-context (consumer)
  (let ((stream
          (and consumer
               (star.consumers:consumer-stream consumer))))
    (if (typep stream 'star.consumers:retrying-rabbit-queue-stream)
        (values
         (star.consumers:delivery-attempt
          (star.consumers:retry-stream-current-properties stream))
         (star.consumers:delivery-trace-id
          (star.consumers:retry-stream-current-properties stream)))
        (values 0 nil))))

(defun target-dispatch-fingerprint-json (value)
  (handler-case
      (jsown:to-json value)
    (error ()
      (princ-to-string value))))

(defun target-dispatch-fingerprint (envelope)
  "Fingerprint all target semantics that may change dispatch behavior.

The schedule identity alone is an idempotency key, not proof that a retried
request is equivalent. Changed target content under one schedule identity must
conflict instead of being silently treated as a duplicate."
  (let ((record (target-dispatch-envelope-record envelope))
        (destination (target-dispatch-envelope-destination envelope)))
    (target-dispatch-digest
     (format nil "~a|~a|~a|~a|~a|~a|~a|~a|~a|~a|~a"
             (target-dispatch-envelope-schedule-id envelope)
             (target-record-id record)
             (or (target-record-revision record) "unrevisioned")
             (target-destination-handle-kind destination)
             (target-destination-handle-name destination)
             (target-record-actor record)
             (target-record-target record)
             (target-record-delay record)
             (if (target-record-recurring-p record) "true" "false")
             (target-dispatch-fingerprint-json
              (target-record-options record))
             (or (target-record-deadline record) "no-deadline")))))