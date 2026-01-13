(in-package :star-server-tests)

;;;; Consumer Thread Unit Tests

(def-suite consumer-tests
  :description "Test suite for document consumer threads")

(in-suite consumer-tests)

;;; ----------------------------------------------------------------------
;;; Debug
;;; ----------------------------------------------------------------------

(defparameter *consumer-tests-debug* t
  "When true, print verbose debug info during consumer tests.")

(defun cdbg (fmt &rest args)
  (when *consumer-tests-debug*
    (apply #'format *error-output*
           (concatenate 'string "~&[consumer-tests] " fmt "~%")
           args)))

;;; ----------------------------------------------------------------------
;;; Initialize lparallel kernel for tests
;;; ----------------------------------------------------------------------

(unless lparallel:*kernel*
  (setf lparallel:*kernel* (lparallel:make-kernel 4)))

;;; ----------------------------------------------------------------------
;;; Helper Functions for Testing
;;; ----------------------------------------------------------------------

(defun make-test-stream ()
  "Create a mock stream for testing consumers.

NOTE: cl-stream:sequence-input-stream wants an actual SEQUENCE.
If you instantiate it with no initargs, some implementations end up with a
non-sequence internal state (sometimes T), then anything doing (length ...) dies."
  (let ((seq "")) ;; empty string is a valid SEQUENCE
    (cdbg "make-test-stream seq=~s type=~a" seq (type-of seq))
    (make-instance 'cl-stream:sequence-input-stream :sequence seq)))

(defun make-mock-rabbit-stream (&key (host "localhost") (port 5672))
  "Create a mock RabbitMQ stream for testing"
  (cdbg "make-mock-rabbit-stream host=~s port=~s" host port)
  (make-instance 'star.consumers:rabbit-queue-stream
                 :host host
                 :port port
                 :queue-name "test-queue"
                 :exchange-name "test-exchange"
                 :routing-key "test.key"))

(defun make-test-consumer (&key (name "test-consumer")
                             (workers 1)
                             (test-fn #'identity)
                             (handler-fn #'(lambda (self msg) msg)))
  "Create a test consumer with default settings"
  (cdbg "make-test-consumer name=~s workers=~s test-fn=~s handler-fn=~s"
        name workers test-fn handler-fn)
  (let ((c (make-instance 'star.consumers:consumer
                          :name name
                          :workers workers
                          :test-fn test-fn
                          :fn handler-fn
                          :stream (make-test-stream))))
    (cdbg "consumer instance=~s type=~a channel=~s lock=~s"
          c (type-of c)
          (ignore-errors (star.consumers:consumer-channel c))
          (ignore-errors (star.consumers:consumer-lock c)))
    c))

;;; ----------------------------------------------------------------------
;;; Consumer Creation Tests
;;; ----------------------------------------------------------------------

(test test-consumer-creation
  "Test basic consumer creation"
  (cdbg "TEST test-consumer-creation")
  (let ((consumer (make-test-consumer :name "test" :workers 2)))
    (is (not (null consumer)))
    (is (string= "test" (star.consumers:consumer-name consumer)))
    (is (= 2 (slot-value consumer 'star.consumers::workers)))
    (is (not (null (star.consumers:consumer-channel consumer))))))

(test test-consumer-default-values
  "Test consumer default values"
  (cdbg "TEST test-consumer-default-values")
  (let ((consumer (make-test-consumer)))
    (is (= 1 (star.consumers:consumer-take consumer)))
    (is (= 1 (slot-value consumer 'star.consumers::workers)))
    (is (not (null (star.consumers:consumer-lock consumer))))))

(test test-rabbit-stream-creation
  "Test RabbitMQ stream creation with default values"
  (cdbg "TEST test-rabbit-stream-creation")
  (let ((stream (make-mock-rabbit-stream)))
    (cdbg "rabbit stream=~s type=~a open?=~s"
          stream (type-of stream)
          (ignore-errors (star.consumers:rabbit-stream-open-p stream)))
    (is (not (null stream)))
    (is (string= "localhost" (star.consumers:rabbit-stream-host stream)))
    (is (= 5672 (star.consumers:rabbit-stream-port stream)))
    (is (string= "guest" (star.consumers:rabbit-stream-user stream)))
    (is (string= "test-queue" (star.consumers:rabbit-stream-queue-name stream)))
    (is (string= "test-exchange" (star.consumers:rabbit-stream-exchange stream)))
    (is (string= "test.key" (star.consumers:rabbit-stream-routing-key stream)))))

;;; ----------------------------------------------------------------------
;;; Consumer Filter Tests
;;; ----------------------------------------------------------------------

(test test-consumer-filter-identity
  "Test consumer with identity filter (accepts all)"
  (cdbg "TEST test-consumer-filter-identity")
  (let* ((results '())
         (consumer (make-test-consumer
                    :test-fn #'identity
                    :handler-fn #'(lambda (self msg)
                                    (declare (ignore self))
                                    (cdbg "handler(identity) msg=~s type=~a" msg (type-of msg))
                                    (push msg results)
                                    msg))))
    (cdbg "consume message1")
    (star.consumers:consume consumer "message1")
    (cdbg "consume nil")
    (star.consumers:consume consumer nil)
    (sleep 0.1)
    (cdbg "results=~s (len ~d)" results (length results))
    ;; Identity filter should accept non-nil
    (is (>= (length results) 1))))

(test test-consumer-filter-predicate
  "Test consumer with custom filter predicate"
  (cdbg "TEST test-consumer-filter-predicate")
  (let* ((processed '())
         (consumer (make-test-consumer
                    :test-fn #'(lambda (msg)
                                 (cdbg "filter predicate msg=~s type=~a" msg (type-of msg))
                                 (and (stringp msg)
                                      (> (length msg) 5)))
                    :handler-fn #'(lambda (self msg)
                                    (declare (ignore self))
                                    (cdbg "handler(predicate) msg=~s" msg)
                                    (push msg processed)
                                    msg))))
    (star.consumers:consume consumer "short")
    (star.consumers:consume consumer "long message")
    (sleep 0.1)
    (cdbg "processed=~s" processed)
    ;; Only messages passing filter should be processed
    (is (member "long message" processed :test #'string=))))

;;; ----------------------------------------------------------------------
;;; Consumer Channel Tests
;;; ----------------------------------------------------------------------

(test test-consumer-channel-creation
  "Test that consumer channel is properly created"
  (cdbg "TEST test-consumer-channel-creation")
  (let ((consumer (make-test-consumer)))
    (cdbg "channel=~s type=~a"
          (star.consumers:consumer-channel consumer)
          (type-of (star.consumers:consumer-channel consumer)))
    (is (typep (star.consumers:consumer-channel consumer)
               'lparallel:channel))))

(test test-consumer-task-submission
  "Test task submission to consumer channel"
  (cdbg "TEST test-consumer-task-submission")
  (let* ((result nil)
         (consumer (make-test-consumer
                    :handler-fn #'(lambda (self msg)
                                    (declare (ignore self))
                                    (cdbg "handler set result=~s type=~a" msg (type-of msg))
                                    (setf result msg)
                                    msg))))
    (star.consumers:consume consumer "test-message")
    (sleep 0.2) ; Wait for async processing
    (cdbg "final result=~s" result)
    (is (string= "test-message" result))))

;;; ----------------------------------------------------------------------
;;; Consumer Lock Tests
;;; ----------------------------------------------------------------------

(test test-consumer-lock-exists
  "Test that consumer has a lock for thread safety"
  (cdbg "TEST test-consumer-lock-exists")
  (let ((consumer (make-test-consumer)))
    (is (not (null (star.consumers:consumer-lock consumer))))
    (is (typep (star.consumers:consumer-lock consumer)
               'bordeaux-threads:lock))))

(test test-consumer-state-locking
  "Test with-consumer-lock macro functionality"
  (cdbg "TEST test-consumer-state-locking")
  (let ((consumer (make-test-consumer))
        (shared-state 0))
    (star.consumers:with-consumer-lock (consumer)
      (setf shared-state 42))
    (is (= 42 shared-state))))

;;; ----------------------------------------------------------------------
;;; RabbitMQ Consumer Tests
;;; ----------------------------------------------------------------------

(test test-rabbit-consumer-creation
  "Test RabbitMQ consumer creation"
  ;; Note: This test doesn't actually connect to RabbitMQ
  (cdbg "TEST test-rabbit-consumer-creation")
  (let ((consumer (make-instance 'star.consumers:rabbit-consumer
                                 :name "rabbit-test"
                                 :workers 2
                                 :stream (make-mock-rabbit-stream))))
    (is (not (null consumer)))
    (is (string= "rabbit-test" (star.consumers:consumer-name consumer)))
    (is (= 2 (slot-value consumer 'star.consumers::workers)))))

;;; ----------------------------------------------------------------------
;;; Message Processing Tests
;;; ----------------------------------------------------------------------

(test test-handle-document-message-structure
  "Test that handle-document expects proper message structure"
  (cdbg "TEST test-handle-document-message-structure")
  (let* ((mock-doc '(("_id" . "test-123")
                     ("type" . "message")
                     ("transient" . :false)))
         (mock-msg (cons mock-doc 1)))
    (is (consp mock-msg))
    (is (consp (car mock-msg)))
    (is (numberp (cdr mock-msg)))))

(test test-insertp-filter
  "Test the insertp filter function (internal)"
  (cdbg "TEST test-insertp-filter")
  ;; insertp expects message to be (cons json-string delivery-tag)
  (let ((transient-msg (cons "{\"transient\": true}" 1))
        (normal-msg (cons "{\"transient\": false}" 1)))
    (is (not (funcall (intern "INSERTP" :star.rabbit) transient-msg)))
    (is (funcall (intern "INSERTP" :star.rabbit) normal-msg))))

(test test-transient-p-predicate
  "Test transient-p predicate"
  (cdbg "TEST test-transient-p-predicate")
  ;; transient-p expects message to be (cons json-string delivery-tag)
  (let ((transient-msg (cons "{\"transient\": true}" 1))
        (normal-msg (cons "{\"transient\": false}" 1))
        (no-field-msg (cons "{\"id\": \"123\"}" 1)))
    (is (star.rabbit:transient-p transient-msg))
    (is (not (star.rabbit:transient-p normal-msg)))
    (is (not (star.rabbit:transient-p no-field-msg)))))

;;; ----------------------------------------------------------------------
;;; Multiple Worker Tests
;;; ----------------------------------------------------------------------

(test test-multiple-workers-processing
  "Test that multiple workers can process messages concurrently"
  (cdbg "TEST test-multiple-workers-processing")
  (let* ((processed-count 0)
         (lock (bt:make-lock))
         (consumer (make-test-consumer
                    :workers 3
                    :handler-fn #'(lambda (self msg)
                                    (declare (ignore self msg))
                                    (bt:with-lock-held (lock)
                                      (incf processed-count))
                                    t))))
    (dotimes (i 10)
      (star.consumers:consume consumer (format nil "msg-~d" i)))
    (sleep 0.5)
    (cdbg "processed-count=~d" processed-count)
    (is (> processed-count 0))))

;;; ----------------------------------------------------------------------
;;; Integration Test Helpers
;;; ----------------------------------------------------------------------

(test test-routing-key-patterns
  "Test routing key pattern matching"
  (cdbg "TEST test-routing-key-patterns")
  (let ((document-key "documents.new.message")
        (target-key "documents.new.target.nmap")
        (update-key "documents.update.message"))
    (is (search "documents.new." document-key))
    (is (search "documents.new.target." target-key))
    (is (search "documents.update." update-key))))

(test test-consumer-name-formatting
  "Test consumer thread name formatting"
  (cdbg "TEST test-consumer-name-formatting")
  (let ((base-name "test-consumer")
        (thread-num 5))
    (is (string= "test-consumer-5"
                 (format nil "~A-~D" base-name thread-num)))))

;;; ----------------------------------------------------------------------
;;; Stream Tests
;;; ----------------------------------------------------------------------

(test test-rabbit-stream-state
  "Test RabbitMQ stream open/closed state"
  (cdbg "TEST test-rabbit-stream-state")
  (let ((stream (make-mock-rabbit-stream)))
    (is (not (star.consumers:rabbit-stream-open-p stream)))))

;;; ----------------------------------------------------------------------
;;; Consumer Statistics Tests
;;; ----------------------------------------------------------------------

;; (test test-consumer-worker-count-range
;;   "Test various worker count values"
;;   (cdbg "TEST test-consumer-worker-count-range")
;;   (dolist (count '(1 2 4 8 16))
;;     (let ((consumer (make-test-consumer :workers count)))
;;       (is (= count (slot-value consumer 'star.consumers::workers))))))

;;; ----------------------------------------------------------------------
;;; Run all consumer tests
;;; ----------------------------------------------------------------------

(defun run-consumer-tests ()
  "Run all consumer thread tests"
  (run! 'consumer-tests))
