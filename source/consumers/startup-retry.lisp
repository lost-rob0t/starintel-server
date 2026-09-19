(in-package :star.consumers)

(defun rabbit-startup-retryable-p (condition)
  "Return true only for transport/server conditions safe to retry at stream open."
  (or (condition-name-contains-p condition "RABBITMQ")
      (condition-name-contains-p condition "CONNECTION")
      (condition-name-contains-p condition "SOCKET")
      (condition-name-contains-p condition "TIMEOUT")))

(defun reset-rabbit-stream-after-open-failure (stream)
  "Best-effort cleanup of STREAM after an interrupted open attempt."
  (when (rabbit-stream-open-p stream)
    (handler-case
        (close-stream stream)
      (condition (condition)
        (log:warn "Rabbit stream cleanup after open failure failed: ~a"
                  condition))))
  (setf (rabbit-stream-open-p stream) nil
        (rabbit-stream-connection stream) nil
        (rabbit-stream-owner-thread stream) nil)
  stream)

(defun call-with-rabbit-startup-retry
    (stream policy operation &key (sleep-fn *retry-sleep-function*))
  "Call OPERATION with bounded retries for transient Rabbit stream-open errors."
  (loop for attempt from 0
        do (handler-case
               (return (funcall operation))
             (condition (condition)
               (reset-rabbit-stream-after-open-failure stream)
               (unless (and (rabbit-startup-retryable-p condition)
                            (< attempt (retry-policy-max-retries policy)))
                 (error condition))
               (let ((delay-ms (retry-delay-ms policy attempt)))
                 (log:warn
                  "Rabbit stream open failed (~a); retrying in ~d ms (~d/~d)"
                  condition
                  delay-ms
                  (1+ attempt)
                  (retry-policy-max-retries policy))
                 (funcall sleep-fn (/ delay-ms 1000.0d0)))))))

(defmethod open-stream :around ((stream retrying-rabbit-queue-stream))
  (call-with-rabbit-startup-retry
   stream
   (retry-stream-policy stream)
   (lambda () (call-next-method))))
