(in-package :star.consumers)

(defstruct (rabbit-settlement
            (:constructor make-rabbit-settlement
                (&key action reason (requeue nil) value error)))
  action
  reason
  requeue
  value
  error)

(defun rabbit-ack (&key reason value)
  (make-rabbit-settlement
   :action :ack
   :reason reason
   :value value))

(defun rabbit-nack (&key reason (requeue nil) value error)
  (make-rabbit-settlement
   :action :nack
   :reason reason
   :requeue requeue
   :value value
   :error error))

(defun normalize-rabbit-settlement (result)
  "Translate a consumer result into an explicit Rabbit settlement decision."
  (typecase result
    (rabbit-settlement result)
    (t (rabbit-ack :reason :handler-complete :value result))))

(defun settle-rabbit-delivery (consumer delivery settlement
                               &key
                                 (ack-fn #'cl-rabbit:basic-ack)
                                 (nack-fn #'cl-rabbit:basic-nack))
  "Settle DELIVERY on the same connection and channel that received it."
  (let* ((stream (consumer-stream consumer))
         (connection (rabbit-stream-connection stream))
         (delivery-tag (cdr delivery))
         (decision (normalize-rabbit-settlement settlement)))
    (ecase (rabbit-settlement-action decision)
      (:ack
       (funcall ack-fn connection 1 delivery-tag :multiple nil))
      (:nack
       (funcall nack-fn
                connection
                1
                delivery-tag
                :multiple nil
                :requeue (rabbit-settlement-requeue decision))))
    decision))

(defclass settled-rabbit-queue-stream (rabbit-queue-stream)
  ((queue-arguments
    :initarg :queue-arguments
    :initform nil
    :accessor rabbit-stream-queue-arguments)
   (dead-letter-exchange
    :initarg :dead-letter-exchange
    :initform nil
    :accessor rabbit-stream-dead-letter-exchange)
   (dead-letter-routing-key
    :initarg :dead-letter-routing-key
    :initform nil
    :accessor rabbit-stream-dead-letter-routing-key)
   (dead-letter-queue
    :initarg :dead-letter-queue
    :initform nil
    :accessor rabbit-stream-dead-letter-queue))
  (:documentation "Rabbit stream with explicit settlement and dead-letter policy."))

(defun rabbit-dead-letter-arguments (stream)
  (let ((exchange (rabbit-stream-dead-letter-exchange stream))
        (routing-key (rabbit-stream-dead-letter-routing-key stream)))
    (append
     (rabbit-stream-queue-arguments stream)
     (when exchange
       (list (cons "x-dead-letter-exchange" exchange)))
     (when routing-key
       (list (cons "x-dead-letter-routing-key" routing-key))))))

(defun declare-queue-with-retry (stream connection)
  "Declare the queue with its configured arguments.

  RabbitMQ rejects queue-declare when an existing queue has inequivalent
  arguments (PRECONDITION_FAILED, reply code 406). This commonly happens
  when a durable queue was created without dead-letter policy and the
  code is later upgraded to use one.

  When that occurs and the stale queue is empty, we delete it and
  redeclare with the correct arguments. If the queue still holds
  messages we refuse to drop them and surface a clear, actionable
  error so an operator can drain or explicitly delete the queue."
  (flet ((declare-queue ()
           (cl-rabbit:queue-declare
            connection
            1
            :queue (rabbit-stream-queue-name stream)
            :durable (rabbit-stream-queue-durable-p stream)
            :arguments (rabbit-dead-letter-arguments stream))))
    (handler-case
        (declare-queue)
      (cl-rabbit:rabbitmq-server-error (condition)
        (unless (= cl-rabbit:+amqp-precondition-failed+
                   (cl-rabbit:rabbitmq-server-error/reply-code condition))
          (error condition))
        (log:warn (format nil
                          "Queue ~a has inequivalent arguments (PRECONDITION_FAILED); ~
                           attempting to recreate empty queue"
                          (rabbit-stream-queue-name stream)))
        ;; The failed declare closes the channel; reopen it before retrying.
        (cl-rabbit:channel-open connection 1)
        (handler-case
            (cl-rabbit:queue-delete
             connection
             1
             (rabbit-stream-queue-name stream)
             :if-empty t
             :if-unused t)
          (cl-rabbit:rabbitmq-server-error (delete-condition)
            (error "Cannot recreate queue ~a: it is not empty. ~
                    Drain or delete the stale queue (and its bindings) ~
                    before restarting. Server message: ~a"
                   (rabbit-stream-queue-name stream)
                   (cl-rabbit:rabbitmq-server-error/message delete-condition))))
        (log:info (format nil
                          "Recreated queue ~a with current dead-letter policy"
                          (rabbit-stream-queue-name stream)))
        (declare-queue)))))

(defmethod open-stream ((stream settled-rabbit-queue-stream))
  (let* ((connection (cl-rabbit:new-connection))
         (socket (cl-rabbit:tcp-socket-new connection))
         (username (rabbit-stream-user stream))
         (password (rabbit-stream-password stream))
         (dead-letter-exchange
           (rabbit-stream-dead-letter-exchange stream))
         (dead-letter-queue
           (rabbit-stream-dead-letter-queue stream))
         (dead-letter-routing-key
           (or (rabbit-stream-dead-letter-routing-key stream) "#")))
    (setf (rabbit-stream-connection stream) connection)
    (cl-rabbit:socket-open
     socket
     (rabbit-stream-host stream)
     (rabbit-stream-port stream))
    (when (or username password)
      (cl-rabbit:login-sasl-plain
       connection
       (rabbit-stream-vhost stream)
       username
       password))
    (cl-rabbit:channel-open connection 1)
    (cl-rabbit:basic-qos connection 1 :prefetch-count 200)
    (cl-rabbit:exchange-declare
     connection
     1
     (rabbit-stream-exchange stream)
     (rabbit-exchange-type stream)
     :durable (rabbit-exchange-durable-p stream))
    (when dead-letter-exchange
      (cl-rabbit:exchange-declare
       connection
       1
       dead-letter-exchange
       "topic"
       :durable t))
    (declare-queue-with-retry stream connection)
    (cl-rabbit:queue-bind
     connection
     1
     :queue (rabbit-stream-queue-name stream)
     :exchange (rabbit-stream-exchange stream)
     :routing-key (rabbit-stream-routing-key stream))
    (when (and dead-letter-exchange dead-letter-queue)
      (cl-rabbit:queue-declare
       connection
       1
       :queue dead-letter-queue
       :durable t)
      (cl-rabbit:queue-bind
       connection
       1
       :queue dead-letter-queue
       :exchange dead-letter-exchange
       :routing-key dead-letter-routing-key))
    (cl-rabbit:basic-consume
     connection
     1
     (rabbit-stream-queue-name stream)
     :no-ack nil)
    (setf (rabbit-stream-open-p stream) t)))

(defun copy-rabbit-stream-options (stream)
  (list
   :queue-name (rabbit-stream-queue-name stream)
   :exchange-name (rabbit-stream-exchange stream)
   :exchange-type (rabbit-exchange-type stream)
   :exchange-durable (rabbit-exchange-durable-p stream)
   :queue-durable (rabbit-stream-queue-durable-p stream)
   :routing-key (rabbit-stream-routing-key stream)
   :host (rabbit-stream-host stream)
   :port (rabbit-stream-port stream)
   :vhost (rabbit-stream-vhost stream)
   :username (rabbit-stream-user stream)
   :password (rabbit-stream-password stream)
   :queue-arguments
   (and (typep stream 'settled-rabbit-queue-stream)
        (rabbit-stream-queue-arguments stream))
   :dead-letter-exchange
   (and (typep stream 'settled-rabbit-queue-stream)
        (rabbit-stream-dead-letter-exchange stream))
   :dead-letter-routing-key
   (and (typep stream 'settled-rabbit-queue-stream)
        (rabbit-stream-dead-letter-routing-key stream))
   :dead-letter-queue
   (and (typep stream 'settled-rabbit-queue-stream)
        (rabbit-stream-dead-letter-queue stream))))

(defmethod start-consumer ((consumer rabbit-consumer))
  "Run each Rabbit worker with its own connection and settle every delivery."
  (let ((create-thread-consumer
          (lambda (thread-number)
            (let* ((thread-consumer
                     (apply #'create-rabbit-consumer
                            :name
                            (format nil "~A-~D"
                                    (consumer-name consumer)
                                    thread-number)
                            :n 1
                            :test-fn (consumer-filter consumer)
                            :handler-fn (consumer-fn consumer)
                            (copy-rabbit-stream-options
                             (consumer-stream consumer)))))
              (open-stream (consumer-stream thread-consumer))
              (assert
               (rabbit-stream-open-p (consumer-stream thread-consumer))
               nil
               "RabbitMQ stream was not opened.")
              (lambda ()
                (loop
                  for delivery = (consumer-read thread-consumer)
                  for settlement =
                    (handler-case
                        (progn
                          (consume thread-consumer delivery)
                          (normalize-rabbit-settlement
                           (receive-result
                            (consumer-channel thread-consumer))))
                      (error (condition)
                        (rabbit-nack
                         :reason :handler-error
                         :requeue t
                         :error condition)))
                  do (settle-rabbit-delivery
                      thread-consumer
                      delivery
                      settlement)))))))
    (loop for thread-number from 1 to (consumer-worker-count consumer)
          do (bt:make-thread
              (funcall create-thread-consumer thread-number)
              :name (format nil "~A-~D"
                            (consumer-name consumer)
                            thread-number)))))

(defun create-rabbit-consumer (&key
                                 (name (error "Consumer name is required"))
                                 (n 1)
                                 (queue-name
                                   (error "Queue name is required"))
                                 (exchange-name "documents")
                                 (exchange-type "topic")
                                 (exchange-durable t)
                                 (queue-durable t)
                                 (queue-arguments nil)
                                 (dead-letter-exchange nil)
                                 (dead-letter-routing-key nil)
                                 (dead-letter-queue nil)
                                 (routing-key
                                   (error "Routing key is required"))
                                 (host "localhost")
                                 (port 5672)
                                 (vhost "/")
                                 (username "guest")
                                 (password "guest")
                                 (test-fn #'identity)
                                 (handler-fn
                                   (error "Handler function is required")))
  "Create a Rabbit consumer with explicit durability and settlement policy."
  (make-instance
   'rabbit-consumer
   :name (string-downcase (string name))
   :stream
   (make-instance
    'settled-rabbit-queue-stream
    :queue-name queue-name
    :exchange-name exchange-name
    :exchange-type exchange-type
    :exchange-durable exchange-durable
    :queue-durable queue-durable
    :queue-arguments queue-arguments
    :dead-letter-exchange dead-letter-exchange
    :dead-letter-routing-key dead-letter-routing-key
    :dead-letter-queue dead-letter-queue
    :routing-key routing-key
    :host host
    :port port
    :vhost vhost
    :user username
    :password password)
   :workers n
   :fn handler-fn
   :test-fn test-fn))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(rabbit-settlement
            make-rabbit-settlement
            rabbit-settlement-action
            rabbit-settlement-reason
            rabbit-settlement-requeue
            rabbit-settlement-value
            rabbit-settlement-error
            rabbit-ack
            rabbit-nack
            normalize-rabbit-settlement
            settle-rabbit-delivery
            settled-rabbit-queue-stream
            rabbit-stream-queue-arguments
            rabbit-stream-dead-letter-exchange
            rabbit-stream-dead-letter-routing-key
            rabbit-stream-dead-letter-queue)
          :star.consumers))
