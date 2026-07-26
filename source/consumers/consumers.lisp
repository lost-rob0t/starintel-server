(in-package :star.consumers)

(defstruct (consumer-settlement
             (:constructor %make-consumer-settlement
                 (action &key reason condition)))
  action
  reason
  condition)

(defun valid-settlement-action-p (action)
  (member action
          '(:ack :filtered-ack :retry :dead-letter :reject)
          :test #'eq))

(defun make-settlement (action &key reason condition)
  (unless (valid-settlement-action-p action)
    (error "Unknown consumer settlement action: ~s" action))
  (%make-consumer-settlement action
                             :reason reason
                             :condition condition))

(defun settlement-ack (&optional reason)
  (make-settlement :ack :reason reason))

(defun settlement-filtered-ack (&optional (reason "filtered"))
  (make-settlement :filtered-ack :reason reason))

(defun settlement-retry (&optional reason condition)
  (make-settlement :retry :reason reason :condition condition))

(defun settlement-dead-letter (&optional reason condition)
  (make-settlement :dead-letter :reason reason :condition condition))

(defun settlement-reject (&optional reason condition)
  (make-settlement :reject :reason reason :condition condition))

(defun normalize-settlement (value)
  "Convert handler results to a structured settlement.

NIL and non-settlement success values remain backward-compatible ACK results."
  (cond
    ((consumer-settlement-p value) value)
    ((valid-settlement-action-p value) (make-settlement value))
    (t (settlement-ack))))

(define-condition wrong-stream-owner (error)
  ((expected
    :initarg :expected
    :reader wrong-stream-owner-expected)
   (actual
    :initarg :actual
    :reader wrong-stream-owner-actual))
  (:report
   (lambda (condition stream)
     (format stream
             "Rabbit stream operation ran on ~s; owner is ~s"
             (wrong-stream-owner-actual condition)
             (wrong-stream-owner-expected condition)))))

(defclass consumer ()
  ((name
    :initarg :name
    :accessor consumer-name
    :initform "")
   (predicate
    :initarg :test-fn
    :accessor consumer-filter
    :initform #'identity)
   (workers
    :initarg :workers
    :accessor consumer-worker-count
    :initform 1)
   (fn
    :initarg :fn
    :accessor consumer-fn
    :initform (lambda (consumer message)
                (declare (ignore consumer))
                (print message)))
   (take
    :initarg :take
    :accessor consumer-take
    :initform 1)
   (worker-channel
    :initarg :consumer-channel
    :accessor consumer-channel
    :initform nil)
   (state
    :initarg :state
    :accessor consumer-state
    :initform :created)
   (consumer-stream
    :initarg :stream
    :accessor consumer-stream)
   (lock
    :initform (bt:make-lock "consumer-state")
    :accessor consumer-lock)
   (metrics-lock
    :initform (bt:make-lock "consumer-metrics")
    :reader consumer-metrics-lock)
   (in-flight
    :initform 0
    :accessor consumer-in-flight)
   (unsettled
    :initform 0
    :accessor consumer-unsettled)
   (failures
    :initform 0
    :accessor consumer-failures)
   (settlement-counts
    :initform (make-hash-table :test #'eq)
    :reader consumer-settlement-counts)
   (failure-action
    :initarg :on-error
    :accessor consumer-failure-action
    :initform :retry)
   (filtered-action
    :initarg :on-filter
    :accessor consumer-filtered-action
    :initform :filtered-ack)
   (worker-instances
    :initform nil
    :accessor consumer-worker-instances)
   (threads
    :initform nil
    :accessor consumer-threads)
   (running-p
    :initform nil
    :accessor consumer-running-p))
  (:documentation
   "A stream consumer whose worker owns its stream and settlement lifecycle."))

(defgeneric consumer-update-state (consumer new-state))
(defgeneric consumer-cleanup (consumer))
(defgeneric consumer-update (consumer new-state))
(defgeneric consumer-read (consumer))
(defgeneric consume (consumer data))
(defgeneric start-consumer (consumer))
(defgeneric stop-consumer (consumer))
(defgeneric open-stream (stream))
(defgeneric close-stream (stream))
(defgeneric stream-read (stream))
(defgeneric stream-settle (stream delivery settlement))

(defmacro with-consumer-lock ((consumer) &body body)
  `(bt:with-lock-held ((consumer-lock ,consumer))
     ,@body))

(defmethod consumer-update-state ((consumer consumer) new-state)
  (with-consumer-lock (consumer)
    (setf (consumer-state consumer) new-state)))

(defmethod consumer-update ((consumer consumer) new-state)
  (consumer-update-state consumer new-state))

(defmethod consumer-cleanup ((consumer consumer))
  (when (consumer-running-p consumer)
    (stop-consumer consumer))
  consumer)

(defun adjust-consumer-metric (consumer accessor delta)
  (bt:with-lock-held ((consumer-metrics-lock consumer))
    (let ((value (+ (funcall accessor consumer) delta)))
      (when (minusp value)
        (error "Consumer metric underflow for ~a" (consumer-name consumer)))
      (setf (slot-value consumer
                        (ecase accessor
                          (#'consumer-in-flight 'in-flight)
                          (#'consumer-unsettled 'unsettled)
                          (#'consumer-failures 'failures)))
            value)
      value)))

(defun increment-in-flight (consumer)
  (adjust-consumer-metric consumer #'consumer-in-flight 1))

(defun decrement-in-flight (consumer)
  (adjust-consumer-metric consumer #'consumer-in-flight -1))

(defun increment-unsettled (consumer)
  (adjust-consumer-metric consumer #'consumer-unsettled 1))

(defun decrement-unsettled (consumer)
  (adjust-consumer-metric consumer #'consumer-unsettled -1))

(defun increment-failures (consumer)
  (adjust-consumer-metric consumer #'consumer-failures 1))

(defun increment-settlement-count (consumer action)
  (bt:with-lock-held ((consumer-metrics-lock consumer))
    (incf (gethash action (consumer-settlement-counts consumer) 0))))

(defun consumer-settlement-count (consumer action)
  (bt:with-lock-held ((consumer-metrics-lock consumer))
    (gethash action (consumer-settlement-counts consumer) 0)))

(defun consumer-metrics (consumer)
  (bt:with-lock-held ((consumer-metrics-lock consumer))
    (list :in-flight (consumer-in-flight consumer)
          :unsettled (consumer-unsettled consumer)
          :failures (consumer-failures consumer)
          :ack (gethash :ack (consumer-settlement-counts consumer) 0)
          :filtered-ack
          (gethash :filtered-ack (consumer-settlement-counts consumer) 0)
          :retry (gethash :retry (consumer-settlement-counts consumer) 0)
          :dead-letter
          (gethash :dead-letter (consumer-settlement-counts consumer) 0)
          :reject (gethash :reject (consumer-settlement-counts consumer) 0))))

(defun configured-failure-settlement (consumer condition)
  (make-settlement
   (consumer-failure-action consumer)
   :reason (princ-to-string condition)
   :condition condition))

(defun configured-filter-settlement (consumer)
  (make-settlement
   (consumer-filtered-action consumer)
   :reason "consumer filter declined delivery"))

(defmethod consumer-read ((consumer consumer))
  (stream-read (consumer-stream consumer)))

(defun consumer-process-delivery (consumer delivery)
  "Run filter/handler and settle DELIVERY exactly once on the current owner thread."
  (increment-unsettled consumer)
  (let ((handler-active nil)
        (settlement nil))
    (setf settlement
          (handler-case
              (if (funcall (consumer-filter consumer) delivery)
                  (progn
                    (setf handler-active t)
                    (increment-in-flight consumer)
                    (normalize-settlement
                     (funcall (consumer-fn consumer) consumer delivery)))
                  (configured-filter-settlement consumer))
            (condition (error)
              (increment-failures consumer)
              (configured-failure-settlement consumer error))))
    (when handler-active
      (decrement-in-flight consumer))
    ;; If settlement itself fails, UNSETTLED remains non-zero and the owner loop
    ;; terminates instead of pretending prefetch credit was restored.
    (stream-settle (consumer-stream consumer) delivery settlement)
    (decrement-unsettled consumer)
    (increment-settlement-count
     consumer
     (consumer-settlement-action settlement))
    settlement))

(defmethod consume ((consumer consumer) data)
  (consumer-process-delivery consumer data))

(defun run-consumer (consumer)
  "Open, consume, handle, and settle on one explicit owner thread."
  (unwind-protect
       (progn
         (open-stream (consumer-stream consumer))
         (setf (consumer-running-p consumer) t)
         (consumer-update-state consumer :running)
         (loop until (eq (consumer-state consumer) :stopping)
               do (handler-case
                      (consumer-process-delivery
                       consumer
                       (consumer-read consumer))
                    (end-of-file ()
                      (return)))))
    (when (consumer-running-p consumer)
      (close-stream (consumer-stream consumer)))
    (setf (consumer-running-p consumer) nil)
    (consumer-update-state consumer :stopped))
  consumer)

(defmethod start-consumer ((consumer consumer))
  (when (> (consumer-worker-count consumer) 1)
    (error "Generic consumers cannot share one stream across multiple workers"))
  (let ((thread
          (bt:make-thread
           (lambda () (run-consumer consumer))
           :name (consumer-name consumer))))
    (setf (consumer-threads consumer) (list thread)
          (consumer-worker-instances consumer) (list consumer))
    consumer))

(defmethod stop-consumer ((consumer consumer))
  (consumer-update-state consumer :stopping)
  consumer)

(defun make-consumer (&rest args)
  (apply #'make-instance 'consumer args))

(defclass rabbit-queue-stream (cl-stream:sequence-input-stream)
  ((exchange
    :initform "amq.topic"
    :initarg :exchange-name
    :accessor rabbit-stream-exchange)
   (exchange-type
    :initform "topic"
    :initarg :exchange-type
    :accessor rabbit-exchange-type)
   (exchange-durable
    :initform t
    :initarg :exchange-durable
    :accessor rabbit-exchange-durable-p)
   (routing-key
    :initform ""
    :initarg :routing-key
    :accessor rabbit-stream-routing-key)
   (user
    :initform "guest"
    :initarg :user
    :accessor rabbit-stream-user)
   (password
    :initform "guest"
    :initarg :password
    :accessor rabbit-stream-password)
   (vhost
    :initform "/"
    :initarg :vhost
    :accessor rabbit-stream-vhost)
   (port
    :initform 5672
    :initarg :port
    :accessor rabbit-stream-port)
   (host
    :initform "localhost"
    :initarg :host
    :accessor rabbit-stream-host)
   (queue-durable-p
    :initform t
    :initarg :queue-durable
    :accessor rabbit-stream-queue-durable-p)
   (queue-name
    :initarg :queue-name
    :accessor rabbit-stream-queue-name)
   (prefetch-count
    :initarg :prefetch-count
    :initform 200
    :accessor rabbit-stream-prefetch-count)
   (conn
    :initform nil
    :accessor rabbit-stream-connection)
   (chan
    :initform 1
    :accessor rabbit-stream-channel)
   (owner-thread
    :initform nil
    :accessor rabbit-stream-owner-thread)
   (open
    :initform nil
    :accessor rabbit-stream-open-p))
  (:documentation "A Rabbit queue/channel owned by exactly one thread."))

(defun assert-rabbit-stream-owner (stream)
  (let ((actual (bt:current-thread))
        (expected (rabbit-stream-owner-thread stream)))
    (unless (and expected (eq actual expected))
      (error 'wrong-stream-owner
             :expected expected
             :actual actual))))

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
    (condition (error)
      (setf (rabbit-stream-owner-thread stream) nil
            (rabbit-stream-connection stream) nil)
      (signal error))))

(defmethod close-stream ((stream rabbit-queue-stream))
  (assert-rabbit-stream-owner stream)
  (when (rabbit-stream-open-p stream)
    (cl-rabbit:channel-close
     (rabbit-stream-connection stream)
     (rabbit-stream-channel stream))
    (cl-rabbit:destroy-connection
     (rabbit-stream-connection stream)))
  (setf (rabbit-stream-open-p stream) nil
        (rabbit-stream-connection stream) nil
        (rabbit-stream-owner-thread stream) nil)
  stream)

(defmethod stream-read ((stream rabbit-queue-stream))
  (assert-rabbit-stream-owner stream)
  (cl-rabbit:consume-message (rabbit-stream-connection stream)))

(defun rabbit-delivery-tag (delivery)
  (cdr delivery))

(defmethod stream-settle
    ((stream rabbit-queue-stream) delivery settlement)
  (assert-rabbit-stream-owner stream)
  (let ((connection (rabbit-stream-connection stream))
        (channel (rabbit-stream-channel stream))
        (delivery-tag (rabbit-delivery-tag delivery)))
    (ecase (consumer-settlement-action settlement)
      ((:ack :filtered-ack)
       (cl-rabbit:basic-ack connection channel delivery-tag))
      (:retry
       (cl-rabbit:basic-nack
        connection channel delivery-tag :requeue t))
      ((:dead-letter :reject)
       (cl-rabbit:basic-nack
        connection channel delivery-tag :requeue nil))))
  settlement)

(defclass rabbit-consumer (consumer) ()
  (:documentation "A Rabbit consumer configuration or channel-owning worker."))

(defmethod consumer-read ((consumer rabbit-consumer))
  (let ((envelope (stream-read (consumer-stream consumer))))
    (cons
     (babel:octets-to-string
      (cl-rabbit:message/body (cl-rabbit:envelope/message envelope))
      :encoding :utf-8)
     (cl-rabbit:envelope/delivery-tag envelope))))

(defun make-rabbit-worker-consumer (consumer worker-number)
  "Clone CONSUMER configuration with a fresh stream/connection owner."
  (let ((stream (consumer-stream consumer)))
    (make-instance
     'rabbit-consumer
     :name (format nil "~a-~d" (consumer-name consumer) worker-number)
     :workers 1
     :stream
     (make-instance
      'rabbit-queue-stream
      :queue-name (rabbit-stream-queue-name stream)
      :exchange-name (rabbit-stream-exchange stream)
      :exchange-type (rabbit-exchange-type stream)
      :exchange-durable (rabbit-exchange-durable-p stream)
      :routing-key (rabbit-stream-routing-key stream)
      :host (rabbit-stream-host stream)
      :port (rabbit-stream-port stream)
      :user (rabbit-stream-user stream)
      :password (rabbit-stream-password stream)
      :vhost (rabbit-stream-vhost stream)
      :queue-durable (rabbit-stream-queue-durable-p stream)
      :prefetch-count (rabbit-stream-prefetch-count stream))
     :fn (consumer-fn consumer)
     :test-fn (consumer-filter consumer)
     :on-error (consumer-failure-action consumer)
     :on-filter (consumer-filtered-action consumer))))

(defmethod start-consumer ((consumer rabbit-consumer))
  (let ((workers nil)
        (threads nil))
    (loop for worker-number from 1 to (consumer-worker-count consumer)
          do
             (let ((worker
                     (make-rabbit-worker-consumer
                      consumer worker-number)))
               (push worker workers)
               (push
                (bt:make-thread
                 (lambda () (run-consumer worker))
                 :name (consumer-name worker))
                threads)))
    (setf (consumer-worker-instances consumer) (nreverse workers)
          (consumer-threads consumer) (nreverse threads)
          (consumer-running-p consumer) t)
    (consumer-update-state consumer :running)
    consumer))

(defmethod stop-consumer ((consumer rabbit-consumer))
  (dolist (worker (consumer-worker-instances consumer))
    (consumer-update-state worker :stopping))
  (consumer-update-state consumer :stopping)
  consumer)

(defun create-rabbit-consumer
    (&key
       (name (error "Consumer name is required"))
       (n 1)
       (queue-name (error "Queue name is required"))
       (exchange-name "documents")
       (exchange-type "topic")
       (exchange-durable t)
       (routing-key (error "Routing key is required"))
       (host "localhost")
       (port 5672)
       (username "guest")
       (password "guest")
       (vhost "/")
       (queue-durable t)
       (prefetch-count 200)
       (on-error :retry)
       (on-filter :filtered-ack)
       (test-fn #'identity)
       (handler-fn (error "Handler function is required")))
  "Create a Rabbit consumer configuration. Each worker receives a fresh stream."
  (unless (plusp n)
    (error "Rabbit consumer worker count must be positive"))
  (make-instance
   'rabbit-consumer
   :name (string-downcase (string name))
   :stream
   (make-instance
    'rabbit-queue-stream
    :queue-name queue-name
    :exchange-name exchange-name
    :exchange-type exchange-type
    :exchange-durable exchange-durable
    :routing-key routing-key
    :host host
    :port port
    :user username
    :password password
    :vhost vhost
    :queue-durable queue-durable
    :prefetch-count prefetch-count)
   :workers n
   :fn handler-fn
   :test-fn test-fn
   :on-error on-error
   :on-filter on-filter))
