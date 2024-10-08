(in-package :star.consumers)

(defclass consumer ()
  ((name :initarg :name :accessor consumer-name :initform "")
   (predicate :initarg :test-fn :initform (lambda (self message)
                                            (not (null message)))
              :accessor consumer-filter)
   (workers :initarg :workers :initform 1 :accessor consumer-worker-count)
   (fn :initarg :fn :accessor consumer-fn :initform (lambda (self message)
                                                      (print message)) :type function)
   (take :initarg :take :accessor consumer-take :type integer :initform 1)
   (worker-channel :initarg :consumer-channel :accessor consumer-channel :type lparallel:channel :initform (make-channel))
   (state :initarg :state :accessor consumer-state)
   (consumer-stream :initarg :stream :reader consumer-stream)
   (lock :initform (bt:make-lock) :accessor consumer-lock))
  (:documentation "Consumers process a stream of data."))

(defgeneric consumer-state (consumer)
  (:documentation "Return the consumer's state"))

(defgeneric consumer-update-state (consumer new-state)
  (:documentation "Update the consumer state"))

(defgeneric consumer-cleanup (consumer)
  (:documentation "Close any streams and de-init the consumer"))

(defmacro with-consumer-lock ((consumer) &body body)
  `(bt:with-lock-held ((consumer-lock ,consumer))
     ,@body))

(defmethod consumer-update ((consumer consumer) new-state)
  (setf (consumer-state consumer) new-state))

(defmethod consumer-read ((consumer consumer))
  (with-consumer-lock (consumer)
    (stream-read (consumer-stream consumer))))

(defmethod consume  ((consumer consumer) data)
  (submit-task (consumer-channel consumer) (lambda ()
                                             (if (funcall (consumer-filter consumer) data)
                                                 (funcall (consumer-fn consumer) consumer data)))))




(defmethod start-consumer ((consumer consumer))
  (loop for i from 1 to (consumer-worker-count consumer)
        do (bt:make-thread (lambda ()
                             (loop
                               for data = (consumer-read consumer)
                               do (consume consumer data)
                               do (receive-result (consumer-channel consumer))))
                           :name (consumer-name consumer))))






(defun make-consumer (&rest args)
  (apply #'make-instance 'consumer args))

(defclass rabbit-queue-stream (cl-stream:sequence-input-stream)
  ((exchange :initform "amq.topic" :initarg :exchange-name :accessor rabbit-stream-exchange)
   (exchange-type :initform "topic" :initarg :exchange-type :accessor rabbit-exchange-type)
   (exchange-durable :initform t :initarg :exchange-durable :accessor rabbit-exchange-durable-p)
   (routing-key :initform "" :initarg :routing-key :accessor rabbit-stream-routing-key)
   (user :initform "guest" :initarg :user :accessor rabbit-stream-user)
   (password :initform "guest" :initarg :password :accessor rabbit-stream-password)
   (vhost :initform "/" :initarg :vhost :accessor rabbit-stream-vhost)
   (port :initform 5672 :initarg :port :accessor rabbit-stream-port)
   (host :initform "localhost" :initarg :host :accessor rabbit-stream-host)
   (queue-durable-p :initform t :initarg :queue-durable :accessor rabbit-stream-queue-durable-p)
   (queue-name :initarg :queue-name :accessor rabbit-stream-queue-name)
   (conn :initform nil :initarg :rabbit-connection :accessor rabbit-stream-connection)
   (chan :initform nil :initarg :rabbit-channel :accessor rabbit-stream-channel)
   (open :initform nil :accessor rabbit-stream-open-p))
  (:documentation "RabbitMQ queue stream representation"))

(defmethod open-stream ((stream rabbit-queue-stream))
  (let* ((connection (cl-rabbit:new-connection))
         (sock (cl-rabbit:tcp-socket-new connection))
         (username (rabbit-stream-user stream))
         (password (rabbit-stream-password stream)))
    (setf (rabbit-stream-connection stream) connection)
    (cl-rabbit:socket-open sock (rabbit-stream-host stream) (rabbit-stream-port stream))
    (when (or username password)
      (cl-rabbit:login-sasl-plain connection (rabbit-stream-vhost stream) username password))
    (cl-rabbit:channel-open connection 1)
    (cl-rabbit:basic-qos connection 1 :prefetch-count 200)
    (cl-rabbit:exchange-declare connection 1 (rabbit-stream-exchange stream) (rabbit-exchange-type stream) :durable (rabbit-exchange-durable-p stream))
    (cl-rabbit:queue-declare connection 1 :queue (rabbit-stream-queue-name stream) :durable (rabbit-stream-queue-durable-p stream))
    (cl-rabbit:queue-bind connection 1 :queue (rabbit-stream-queue-name stream) :exchange (rabbit-stream-exchange stream) :routing-key (rabbit-stream-routing-key stream))
    (cl-rabbit:basic-consume connection 1 (rabbit-stream-queue-name stream))
    (setf (rabbit-stream-open-p stream) t)))

(defmethod close-stream ((stream rabbit-queue-stream))
  (cl-rabbit:channel-close (rabbit-stream-connection stream) 1)
  (cl-rabbit:destroy-connection (rabbit-stream-connection stream))
  (setf (rabbit-stream-open-p stream) nil))

(defmethod stream-read ((stream rabbit-queue-stream))
  (let ((conn (rabbit-stream-connection stream)))
    (cl-rabbit:consume-message conn)))

(defclass rabbit-consumer (consumer)
  ()
  (:documentation "Custom consumer class for RabbitMQ"))

(defmethod consumer-read ((consumer rabbit-consumer))
  (let ((msg (stream-read (consumer-stream consumer))))
    (cons (babel:octets-to-string (cl-rabbit:message/body (cl-rabbit:envelope/message msg)) :encoding :utf-8)
          (cl-rabbit:envelope/delivery-tag msg))))


(defmethod start-consumer ((consumer rabbit-consumer))
  (let ((create-thread-consumer
          (lambda (thread-number)
            (let* ((thread-consumer (create-rabbit-consumer
                                     :name (format nil "~A-~D" (consumer-name consumer) thread-number)
                                     :n 1
                                     :queue-name (rabbit-stream-queue-name (consumer-stream consumer))
                                     :exchange-name (rabbit-stream-exchange (consumer-stream consumer))
                                     :routing-key (rabbit-stream-routing-key (consumer-stream consumer))
                                     :host (rabbit-stream-host (consumer-stream consumer))
                                     :port (rabbit-stream-port (consumer-stream consumer))
                                     :username (rabbit-stream-user (consumer-stream consumer))
                                     :password (rabbit-stream-password (consumer-stream consumer))
                                     :test-fn (consumer-filter consumer)
                                     :handler-fn (consumer-fn consumer))))
              (open-stream (consumer-stream thread-consumer))
              (assert (rabbit-stream-open-p (consumer-stream thread-consumer)) nil "Rabbitmq stream wasnt opened!!!!")
              (lambda ()
                (loop
                  for data = (consumer-read thread-consumer)
                  do (consume thread-consumer data)
                  do (receive-result (consumer-channel thread-consumer))))))))
    (loop for i from 1 to (consumer-worker-count consumer)
          do (bt:make-thread (funcall create-thread-consumer i)
                             :name (format nil "~A-~D" (consumer-name consumer) i)))))

(defun create-rabbit-consumer (&key
                                 (name (error "Consumer name is required"))
                                 (n 1)
                                 (queue-name (error "Queue name is required"))
                                 (exchange-name "documents")
                                 (routing-key (error "Routing key is required"))
                                 (host "localhost")
                                 (port 5672)
                                 (username "guest")
                                 (password "guest")
                                 (test-fn #'identity)
                                 (handler-fn (error "Handler function is required")))
  "Create a RabbitMQ consumer with the given configuration.
This function returns a rabbit-consumer instance.
Arguments:
- NAME: String, the name of the consumer.
- N: Integer, the number of consumer threads to start (default: 1).
- QUEUE-NAME: String, the name of the RabbitMQ queue to consume from.
- EXCHANGE-NAME: String, the name of the RabbitMQ exchange (default: 'documents').
- ROUTING-KEY: String, the routing key for the queue binding.
- HOST: String, the RabbitMQ server host (default: 'localhost').
- PORT: Integer, the RabbitMQ server port (default: 5672).
- USERNAME: String, the RabbitMQ username (default: 'guest').
- PASSWORD: String, the RabbitMQ password (default: 'guest').
- TEST-FN: Function, a function to test if a message should be processed (default: #'identity).
- HANDLER-FN: Function, the message handling logic."
  (make-instance 'rabbit-consumer
                 :name (string-downcase (string name))
                 :stream (make-instance 'rabbit-queue-stream
                                        :queue-name queue-name
                                        :exchange-name exchange-name
                                        :routing-key routing-key
                                        :host host
                                        :port port
                                        :user username
                                        :password password)
                 :workers n
                 :fn handler-fn
                 :test-fn test-fn))
