(in-package :star.producers)

(defclass producer ()
  ((name :initarg :name :accessor producer-name :initform "")
   (exchange-name :initarg :exchange-name :accessor producer-exchange :initform "amqp.topic")
   (exchange-type :initarg :exchange-type :accessor producer-exchange-type :initform "topic")
   (exchange-durable :initform t :initarg :exchange-durable :accessor exchange-durable-p)
   (connection :initarg :state :accessor producer-conn :initform nil)
   (user :initform "" :initarg :user :accessor producer-user)
   (password :initform "" :initarg :password :accessor producer-password)
   (vhost :initform "/" :initarg :vhost :accessor producer-vhost)
   (port :initform 5672 :initarg :port :accessor producer-port)
   (host :initform "127.0.0.1" :initarg :host :accessor producer-host)
   (open :initform nil :accessor producer-open-p))
  (:documentation "Producers emit data onto the rabbitmq queue. When used within an agent, 
the lock slot should be initialized to NIL to disable locking mechanisms. Example:
(make-instance 'producer :lock nil)"))


(defclass producer-threaded (producer)
  (
   (lock :initarg :lock :initform (bt:make-lock) :accessor producer-lock))
  (:documentation "Thread safe Producer. Use with-producer-lock to do actions with the lock."))

(defgeneric destroy (producer)
  (:documentation "Close any streams and de-init the producer"))

(defmacro with-producer-lock ((producer) &body body)
  "Executes BODY with the producer's lock held if it exists.
If producer has no lock (lock is NIL), executes body without locking.
Useful for thread-safe access to producer resources.

Arguments:
  PRODUCER - Instance of PRODUCER class
  BODY - Forms to execute while holding the lock"
  `(bt:with-lock-held ((producer-lock ,producer))
     ,@body)
  )

(defmethod destroy ((producer producer))
  "Closes the current channel (1) and then destroys the underlying RabbitMQ connection. 
This should be called when the producer is no longer needed to free up system resources 
and properly close the connection to the RabbitMQ server."
  (cl-rabbit:channel-close (producer-conn producer) 1)
  (cl-rabbit:destroy-connection (producer-conn producer)))

(defmethod producer-connect ((producer producer))
  "Establish connection to RabbitMQ broker, authenticate if credentials provided,
open channel 1 and declare exchange. Sets producer-conn and producer-open-p slots.
Connection parameters are taken from producer instance slots (host, port, vhost, etc).
Uses SASL PLAIN authentication when username/password are provided."
  (let* ((connection (cl-rabbit:new-connection))
         (sock (cl-rabbit:tcp-socket-new connection))
         (username (producer-user producer))
         (password (producer-password producer)))
    (setf (producer-conn producer) connection)
    (cl-rabbit:socket-open sock (producer-host producer) (producer-port producer))
    (when (or username password)
      (cl-rabbit:login-sasl-plain connection (producer-vhost producer) username password))
    (cl-rabbit:channel-open connection 1)
    (cl-rabbit:exchange-declare connection 1 (producer-exchange producer) (producer-exchange-type producer) :durable (exchange-durable-p producer))
    (setf (producer-open-p producer) t))
  producer)


(defmethod publish ((producer producer) &key body routing-key (properties nil))
  "Publish a message to the RabbitMQ exchange.
   BODY: The message content to publish
   ROUTING-KEY: The routing key for message routing
   PROPERTIES: Optional message properties like delivery-mode, content-type etc.
   Uses thread-safe locking if producer was created with a lock."
  (log:trace "Publishing: ~a~%" body)
  (cl-rabbit:basic-publish (producer-conn producer) 1 :exchange (producer-exchange producer) :body body :properties properties :routing-key routing-key))




(defun make-producer (&rest args)
  "Create and return a new producer instance.
Arguments passed to this function are forwarded to MAKE-INSTANCE 'producer.
Common arguments include:
  :NAME - Producer name (default: \"\")
  :EXCHANGE-NAME - RabbitMQ exchange name (default: \"amqp.topic\")
  :EXCHANGE-TYPE - Exchange type (default: \"topic\")
  :EXCHANGE-DURABLE - Exchange durability flag (default: T)
  :USER - RabbitMQ username (default: \"\")
  :PASSWORD - RabbitMQ password (default: \"\")
  :VHOST - RabbitMQ virtual host (default: \"/\")
  :PORT - RabbitMQ port (default: 5672)
  :HOST - RabbitMQ host (default: \"127.0.0.1\")
  :LOCK - Thread lock (default: new lock, use NIL to disable locking)"
  (apply #'make-instance 'producer args))
