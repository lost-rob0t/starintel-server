(in-package :cl-user)

(defclass owner-test-stream ()
  ((owner-thread
    :initarg :owner-thread
    :accessor owner-test-stream-owner-thread)
   (settlements
    :initform nil
    :accessor owner-test-stream-settlements)
   (settlement-threads
    :initform nil
    :accessor owner-test-stream-settlement-threads)))

(defun owner-test-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defmethod star.consumers:stream-settle
    ((stream owner-test-stream) delivery settlement)
  (declare (ignore delivery))
  (let ((actual (bt:current-thread))
        (expected (owner-test-stream-owner-thread stream)))
    (unless (eq actual expected)
      (error 'star.consumers:wrong-stream-owner
             :expected expected
             :actual actual))
    (push settlement (owner-test-stream-settlements stream))
    (push actual (owner-test-stream-settlement-threads stream))
    settlement))

(defun owner-test-consumer
    (stream handler &key (filter #'identity)
                         (on-error :retry)
                         (on-filter :filtered-ack))
  (star.consumers:make-consumer
   :name "owner-test"
   :stream stream
   :fn handler
   :test-fn filter
   :on-error on-error
   :on-filter on-filter))

(defun owner-test-single-settlement (stream)
  (let ((settlements (owner-test-stream-settlements stream)))
    (owner-test-check (= 1 (length settlements))
                      "Expected one settlement, got ~d"
                      (length settlements))
    (first settlements)))

(defun test-handler-and-settlement-use-owner-thread ()
  (let* ((owner (bt:current-thread))
         (stream (make-instance 'owner-test-stream :owner-thread owner))
         (handler-thread nil)
         (consumer
           (owner-test-consumer
            stream
            (lambda (self delivery)
              (declare (ignore self delivery))
              (setf handler-thread (bt:current-thread))
              (star.consumers:settlement-ack "handled")))))
    (star.consumers:consumer-process-delivery consumer '("body" . 1))
    (let ((settlement (owner-test-single-settlement stream)))
      (owner-test-check (eq owner handler-thread)
                        "Handler ran outside the owner thread")
      (owner-test-check
       (every (lambda (thread) (eq owner thread))
              (owner-test-stream-settlement-threads stream))
       "Settlement ran outside the owner thread")
      (owner-test-check
       (eq :ack (star.consumers:consumer-settlement-action settlement))
       "Expected ACK settlement")
      (owner-test-check (= 0 (star.consumers:consumer-in-flight consumer))
                        "In-flight metric did not return to zero")
      (owner-test-check (= 0 (star.consumers:consumer-unsettled consumer))
                        "Unsettled metric did not return to zero")
      (owner-test-check
       (= 1 (star.consumers:consumer-settlement-count consumer :ack))
       "ACK metric was not incremented exactly once"))))

(defun test-filtered-delivery-restores-prefetch-credit ()
  (let* ((owner (bt:current-thread))
         (stream (make-instance 'owner-test-stream :owner-thread owner))
         (handler-called nil)
         (consumer
           (owner-test-consumer
            stream
            (lambda (self delivery)
              (declare (ignore self delivery))
              (setf handler-called t)
              (star.consumers:settlement-ack))
            :filter (lambda (delivery)
                      (declare (ignore delivery))
                      nil))))
    (star.consumers:consumer-process-delivery consumer '("filtered" . 2))
    (let ((settlement (owner-test-single-settlement stream)))
      (owner-test-check (not handler-called)
                        "Filtered delivery reached the handler")
      (owner-test-check
       (eq :filtered-ack
           (star.consumers:consumer-settlement-action settlement))
       "Filtered delivery was not ACKed")
      (owner-test-check (= 0 (star.consumers:consumer-unsettled consumer))
                        "Filtered delivery retained prefetch credit")
      (owner-test-check
       (= 1
          (star.consumers:consumer-settlement-count
           consumer :filtered-ack))
       "Filtered ACK metric was not incremented exactly once"))))

(defun test-handler-failure-settles-exactly-once ()
  (let* ((owner (bt:current-thread))
         (stream (make-instance 'owner-test-stream :owner-thread owner))
         (handler-count 0)
         (consumer
           (owner-test-consumer
            stream
            (lambda (self delivery)
              (declare (ignore self delivery))
              (incf handler-count)
              (error "forced handler failure"))
            :on-error :dead-letter)))
    (star.consumers:consumer-process-delivery consumer '("bad" . 3))
    (let ((settlement (owner-test-single-settlement stream)))
      (owner-test-check (= 1 handler-count)
                        "Handler executed ~d times" handler-count)
      (owner-test-check
       (eq :dead-letter
           (star.consumers:consumer-settlement-action settlement))
       "Configured failure did not dead-letter")
      (owner-test-check (= 1 (star.consumers:consumer-failures consumer))
                        "Failure metric was not incremented")
      (owner-test-check (= 0 (star.consumers:consumer-in-flight consumer))
                        "Failed handler remained in flight")
      (owner-test-check (= 0 (star.consumers:consumer-unsettled consumer))
                        "Failed delivery remained unsettled")
      (owner-test-check
       (= 1
          (star.consumers:consumer-settlement-count
           consumer :dead-letter))
       "Failure settlement metric was not exactly one"))))

(defun test-wrong-thread-settlement-is-rejected ()
  (let* ((owner (bt:current-thread))
         (stream (make-instance 'owner-test-stream :owner-thread owner))
         (condition nil)
         (thread
           (bt:make-thread
            (lambda ()
              (handler-case
                  (star.consumers:stream-settle
                   stream
                   '("wrong" . 4)
                   (star.consumers:settlement-ack))
                (star.consumers:wrong-stream-owner (error)
                  (setf condition error))))
            :name "wrong-settlement-thread")))
    (bt:join-thread thread)
    (owner-test-check
     (typep condition 'star.consumers:wrong-stream-owner)
     "Wrong-thread settlement was accepted")
    (owner-test-check
     (null (owner-test-stream-settlements stream))
     "Wrong-thread settlement mutated stream state")))

(defun test-concurrent-workers-do-not-share-stream-or-channel ()
  (let* ((configuration
           (star.consumers:create-rabbit-consumer
            :name "fresh-channel-test"
            :n 2
            :queue-name "fresh-channel-test"
            :exchange-name "documents"
            :routing-key "documents.test.#"
            :handler-fn
            (lambda (self delivery)
              (declare (ignore self delivery))
              (star.consumers:settlement-ack))))
         (worker-a
           (star.consumers:make-rabbit-worker-consumer configuration 1))
         (worker-b
           (star.consumers:make-rabbit-worker-consumer configuration 2))
         (stream-a (star.consumers:consumer-stream worker-a))
         (stream-b (star.consumers:consumer-stream worker-b))
         (connection-a (list :connection-a))
         (connection-b (list :connection-b))
         (channel-a (list :channel-a))
         (channel-b (list :channel-b)))
    (setf (star.consumers:rabbit-stream-connection stream-a) connection-a
          (star.consumers:rabbit-stream-connection stream-b) connection-b
          (star.consumers:rabbit-stream-channel stream-a) channel-a
          (star.consumers:rabbit-stream-channel stream-b) channel-b)
    (owner-test-check (not (eq stream-a stream-b))
                      "Concurrent workers share a stream object")
    (owner-test-check
     (not (eq (star.consumers:rabbit-stream-connection stream-a)
              (star.consumers:rabbit-stream-connection stream-b)))
     "Concurrent workers share a Rabbit connection")
    (owner-test-check
     (not (eq (star.consumers:rabbit-stream-channel stream-a)
              (star.consumers:rabbit-stream-channel stream-b)))
     "Concurrent workers share a Rabbit channel")))

(defun test-structured-handler-actions-are-preserved ()
  (dolist (action '(:ack :filtered-ack :retry :dead-letter :reject))
    (let ((settlement (star.consumers:normalize-settlement action)))
      (owner-test-check
       (eq action (star.consumers:consumer-settlement-action settlement))
       "Action ~s normalized to ~s"
       action
       (star.consumers:consumer-settlement-action settlement)))))

(defun run-consumer-owner-conformance-tests ()
  (format t "~&Running Rabbit owner-thread settlement tests~%")
  (test-handler-and-settlement-use-owner-thread)
  (test-filtered-delivery-restores-prefetch-credit)
  (test-handler-failure-settles-exactly-once)
  (test-wrong-thread-settlement-is-rejected)
  (test-concurrent-workers-do-not-share-stream-or-channel)
  (test-structured-handler-actions-are-preserved)
  (format t "~&Rabbit owner-thread settlement tests passed~%")
  t)

(run-consumer-owner-conformance-tests)
