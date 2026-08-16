(in-package :star.consumers)

(defun normalize-settlement (value)
  "Convert handler results to a structured settlement.

Known settlement keywords are accepted. NIL and ordinary success values remain
backward-compatible ACK results, while unknown keywords fail closed so a typo
cannot silently acknowledge a Rabbit delivery."
  (cond
    ((consumer-settlement-p value) value)
    ((valid-settlement-action-p value) (make-settlement value))
    ((keywordp value)
     (error "Unknown consumer settlement action: ~s" value))
    (t (settlement-ack))))

(defun update-consumer-metric-slot (consumer slot-name delta)
  (bt:with-lock-held ((consumer-metrics-lock consumer))
    (let ((value (+ (slot-value consumer slot-name) delta)))
      (when (minusp value)
        (error "Consumer metric ~a underflow for ~a"
               slot-name
               (consumer-name consumer)))
      (setf (slot-value consumer slot-name) value)
      value)))

(defun increment-in-flight (consumer)
  (update-consumer-metric-slot consumer 'in-flight 1))

(defun decrement-in-flight (consumer)
  (update-consumer-metric-slot consumer 'in-flight -1))

(defun increment-unsettled (consumer)
  (update-consumer-metric-slot consumer 'unsettled 1))

(defun decrement-unsettled (consumer)
  (update-consumer-metric-slot consumer 'unsettled -1))

(defun increment-failures (consumer)
  (update-consumer-metric-slot consumer 'failures 1))

(defun consumer-process-delivery (consumer delivery)
  "Run filter/handler and settle DELIVERY exactly once on the owner thread."
  (increment-unsettled consumer)
  (let ((settlement
          (handler-case
              (if (funcall (consumer-filter consumer) delivery)
                  (progn
                    (increment-in-flight consumer)
                    (unwind-protect
                         (normalize-settlement
                          (funcall
                           (consumer-fn consumer)
                           consumer
                           delivery))
                      (decrement-in-flight consumer)))
                  (configured-filter-settlement consumer))
            (error (condition)
              (increment-failures consumer)
              (configured-failure-settlement consumer condition)))))
    ;; If settlement fails, UNSETTLED remains non-zero. The owner loop exits
    ;; rather than pretending Rabbit prefetch credit was restored.
    (stream-settle (consumer-stream consumer) delivery settlement)
    (decrement-unsettled consumer)
    (increment-settlement-count
     consumer
     (consumer-settlement-action settlement))
    settlement))

(defmethod open-stream ((stream rabbit-queue-stream))
  (when (rabbit-stream-open-p stream)
    (error "Rabbit stream is already open"))
  (setf (rabbit-stream-owner-thread stream) (bt:current-thread))
  (handler-case
      (let* ((connection (cl-rabbit:new-connection))
             (socket (cl-rabbit:tcp-socket-new connection))
             (channel (rabbit-stream-channel stream)))
        (setf (rabbit-stream-connection stream) connection)
        (cl-rabbit:socket-open
         socket
         (rabbit-stream-host stream)
         (rabbit-stream-port stream))
        (when (and (rabbit-stream-user stream)
                   (rabbit-stream-password stream))
          (cl-rabbit:login-sasl-plain
           connection
           (rabbit-stream-vhost stream)
           (rabbit-stream-user stream)
           (rabbit-stream-password stream)))
        (cl-rabbit:channel-open connection channel)
        (cl-rabbit:basic-qos
         connection
         channel
         :prefetch-count (rabbit-stream-prefetch-count stream))
        (cl-rabbit:exchange-declare
         connection
         channel
         (rabbit-stream-exchange stream)
         (rabbit-exchange-type stream)
         :durable (rabbit-exchange-durable-p stream))
        (cl-rabbit:queue-declare
         connection
         channel
         :queue (rabbit-stream-queue-name stream)
         :durable (rabbit-stream-queue-durable-p stream))
        (cl-rabbit:queue-bind
         connection
         channel
         :queue (rabbit-stream-queue-name stream)
         :exchange (rabbit-stream-exchange stream)
         :routing-key (rabbit-stream-routing-key stream))
        (cl-rabbit:basic-consume
         connection
         channel
         (rabbit-stream-queue-name stream))
        (setf (rabbit-stream-open-p stream) t)
        stream)
    (condition (condition)
      (setf (rabbit-stream-owner-thread stream) nil
            (rabbit-stream-connection stream) nil
            (rabbit-stream-open-p stream) nil)
      (error condition))))
