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
