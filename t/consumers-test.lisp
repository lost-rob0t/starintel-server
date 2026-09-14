(in-package :star-server-tests)

(def-suite consumer-tests
  :description "Owner-thread consumer and settlement behavior")

(in-suite consumer-tests)

(defclass test-settlement-stream ()
  ((settlements
    :initform nil
    :accessor test-stream-settlements)))

(defmethod star.consumers:stream-settle
    ((stream test-settlement-stream) delivery settlement)
  (push (cons delivery settlement) (test-stream-settlements stream))
  settlement)

(defun make-test-consumer
    (&key
       (name "test-consumer")
       (workers 1)
       (test-fn #'identity)
       (handler-fn (lambda (consumer message)
                     (declare (ignore consumer message))
                     (star.consumers:settlement-ack))))
  (make-instance
   'star.consumers:consumer
   :name name
   :workers workers
   :test-fn test-fn
   :fn handler-fn
   :stream (make-instance 'test-settlement-stream)))

(defun make-mock-rabbit-stream (&key (host "localhost") (port 5672))
  (make-instance
   'star.consumers:rabbit-queue-stream
   :host host
   :port port
   :queue-name "test-queue"
   :exchange-name "test-exchange"
   :routing-key "test.key"))

(defun make-mock-retrying-rabbit-stream ()
  (make-instance
   'star.consumers:retrying-rabbit-queue-stream
   :host "localhost"
   :port 5672
   :queue-name "test-retrying-queue"
   :exchange-name "test-exchange"
   :routing-key "test.key"))

(define-condition test-rabbitmq-server-error (error) ())

(test consumer-creation
  (let ((consumer (make-test-consumer :name "test" :workers 2)))
    (is (string= "test" (star.consumers:consumer-name consumer)))
    (is (= 2 (star.consumers:consumer-worker-count consumer)))
    (is (not (null (star.consumers:consumer-lock consumer))))
    (is (eq :created (star.consumers:consumer-state consumer)))))

(test settlement-normalization
  (is (eq :ack
          (star.consumers:consumer-settlement-action
           (star.consumers:normalize-settlement nil))))
  (is (eq :retry
          (star.consumers:consumer-settlement-action
           (star.consumers:normalize-settlement :retry))))
  (signals error
    (star.consumers:normalize-settlement :unknown-action)))

(test accepted-delivery-settles-once
  (let* ((handled nil)
         (consumer
           (make-test-consumer
            :handler-fn
            (lambda (self message)
              (declare (ignore self))
              (setf handled message)
              (star.consumers:settlement-ack "stored"))))
         (stream (star.consumers:consumer-stream consumer))
         (settlement (star.consumers:consume consumer "message")))
    (is (string= "message" handled))
    (is (eq :ack (star.consumers:consumer-settlement-action settlement)))
    (is (= 1 (length (test-stream-settlements stream))))
    (is (= 0 (star.consumers:consumer-in-flight consumer)))
    (is (= 0 (star.consumers:consumer-unsettled consumer)))
    (is (= 1 (star.consumers:consumer-settlement-count consumer :ack)))))

(test filtered-delivery-restores-credit
  (let* ((handled nil)
         (consumer
           (make-test-consumer
            :test-fn (constantly nil)
            :handler-fn
            (lambda (self message)
              (declare (ignore self message))
              (setf handled t))))
         (settlement (star.consumers:consume consumer "ignored")))
    (is (not handled))
    (is (eq :filtered-ack
            (star.consumers:consumer-settlement-action settlement)))
    (is (= 0 (star.consumers:consumer-unsettled consumer)))
    (is (= 1
           (star.consumers:consumer-settlement-count
            consumer
            :filtered-ack)))))

(test handler-failure-uses-configured-settlement
  (let* ((consumer
           (make-instance
            'star.consumers:consumer
            :name "failure-test"
            :workers 1
            :test-fn #'identity
            :on-error :dead-letter
            :fn (lambda (self message)
                  (declare (ignore self message))
                  (error "boom"))
            :stream (make-instance 'test-settlement-stream)))
         (settlement (star.consumers:consume consumer "message")))
    (is (eq :dead-letter
            (star.consumers:consumer-settlement-action settlement)))
    (is (= 1 (star.consumers:consumer-failures consumer)))
    (is (= 1
           (star.consumers:consumer-settlement-count
            consumer
            :dead-letter)))))

(test rabbit-stream-configuration
  (let ((stream (make-mock-rabbit-stream)))
    (is (string= "localhost" (star.consumers:rabbit-stream-host stream)))
    (is (= 5672 (star.consumers:rabbit-stream-port stream)))
    (is (string= "guest" (star.consumers:rabbit-stream-user stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name stream)))
    (is (string= "test-exchange"
                 (star.consumers:rabbit-stream-exchange stream)))
    (is (string= "test.key"
                 (star.consumers:rabbit-stream-routing-key stream)))
    (is (not (star.consumers:rabbit-stream-open-p stream)))))

(test rabbit-workers-own-distinct-streams
  (let* ((configuration
           (make-instance
            'star.consumers:rabbit-consumer
            :name "rabbit-test"
            :workers 2
            :stream (make-mock-rabbit-stream)
            :fn (lambda (self message)
                  (declare (ignore self message))
                  :ack)))
         (left (star.consumers:make-rabbit-worker-consumer configuration 1))
         (right (star.consumers:make-rabbit-worker-consumer configuration 2))
         (left-stream (star.consumers:consumer-stream left))
         (right-stream (star.consumers:consumer-stream right)))
    (is (not (eq left-stream right-stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name left-stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name right-stream)))
    (is (= 1 (star.consumers:consumer-worker-count left)))
    (is (= 1 (star.consumers:consumer-worker-count right)))))

(test wrong-thread-settlement-is-rejected
  (let ((stream (make-mock-rabbit-stream)))
    (signals star.consumers:wrong-stream-owner
      (star.consumers:stream-settle
       stream
       (cons "message" 1)
       (star.consumers:settlement-ack)))))

(test bounded-retry-policy
  (let ((policy
          (star.consumers:make-retry-policy
           :max-retries 2
           :base-delay-ms 100
           :max-delay-ms 1000
           :jitter-ratio 0.0d0)))
    (is (= 100 (star.consumers:retry-delay-ms policy 0 0.5d0)))
    (is (= 200 (star.consumers:retry-delay-ms policy 1 0.5d0)))
    (is (eq :retry
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:transient-delivery-error)
             1)))
    (is (eq :dead-letter
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:transient-delivery-error)
             2)))
    (is (eq :dead-letter
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:permanent-delivery-error)
             0)))))

(test rabbit-startup-retry-recovers-from-transient-server-error
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 2
            :base-delay-ms 10
            :max-delay-ms 100
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps nil)
         (result
           (star.consumers::call-with-rabbit-startup-retry
            stream
            policy
            (lambda ()
              (incf attempts)
              (if (< attempts 3)
                  (progn
                    (setf (star.consumers:rabbit-stream-owner-thread stream)
                          (bt:current-thread))
                    (error 'test-rabbitmq-server-error))
                  :opened))
            :sleep-fn (lambda (seconds) (push seconds sleeps)))))
    (is (eq :opened result))
    (is (= 3 attempts))
    (is (equal '(0.01d0 0.02d0) (reverse sleeps)))
    (is (null (star.consumers:rabbit-stream-owner-thread stream)))
    (is (null (star.consumers:rabbit-stream-connection stream)))
    (is (not (star.consumers:rabbit-stream-open-p stream)))))

(test rabbit-startup-retry-exhaustion-is-visible
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 2
            :base-delay-ms 1
            :max-delay-ms 10
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps 0))
    (signals test-rabbitmq-server-error
      (star.consumers::call-with-rabbit-startup-retry
       stream
       policy
       (lambda ()
         (incf attempts)
         (error 'test-rabbitmq-server-error))
       :sleep-fn (lambda (seconds)
                   (declare (ignore seconds))
                   (incf sleeps))))
    (is (= 3 attempts))
    (is (= 2 sleeps))))

(test rabbit-startup-retry-does-not-hide-programming-errors
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 5
            :base-delay-ms 1
            :max-delay-ms 10
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps 0))
    (signals simple-error
      (star.consumers::call-with-rabbit-startup-retry
       stream
       policy
       (lambda ()
         (incf attempts)
         (error "programming bug"))
       :sleep-fn (lambda (seconds)
                   (declare (ignore seconds))
                   (incf sleeps))))
    (is (= 1 attempts))
    (is (zerop sleeps))))

(test transient-document-predicate
  (let ((transient
          (cons
           "{\"_id\":\"one\",\"dtype\":\"note\",\"transient\":true}"
           1))
        (persistent
          (cons
           "{\"_id\":\"two\",\"dtype\":\"note\",\"transient\":false}"
           2)))
    (is (star.rabbit:transient-p transient))
    (is (not (star.rabbit:transient-p persistent)))))

(defun run-consumer-tests ()
  (run! 'consumer-tests))
