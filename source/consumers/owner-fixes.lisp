(in-package :star.consumers)

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
