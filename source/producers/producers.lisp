(in-package :star.producers)


(defclass producer ()
  ((name :initarg :name :accessor producer-name :initform "")
   (exchange-name :initarg :exchange-name :accessor producer-exchange :initform "amqp.topic")
   (exchange-type :initarg :exchange-type :accessor producer-exchange-type :initform "topic")
   (exchange-durable :initform t :initarg :exchange-durable :accessor exchange-durable-p)
   (max-queue-size :initarg :max-size :accessor producer-max-size :type integer)
   (connection :initarg :state :accessor producer-conn)
   (user :initform "" :initarg :user :accessor producer-user)
   (password :initform "" :initarg :password :accessor producer-password)
   (vhost :initform "/" :initarg :vhost :accessor producer-vhost)
   (port :initform 5672 :initarg :port :accessor producer-port)
   (host :initform "127.0.0.1" :initarg :host :accessor producer-host)
   (open :initform nil :accessor producer-open-p)
   (lock :initform (bt:make-lock) :accessor producer-lock))
  (:documentation "Producers emit data onto the rabbitmq queue."))


(defgeneric destroy (producer)
  (:documentation "Close any streams and de-init the producer"))

(defmacro with-producer-lock ((producer) &body body)
  `(bt:with-lock-held ((producer-lock ,producer))
     ,@body))

(defmethod destroy ((producer producer))
  (cl-rabbit:channel-close (producer-conn producer) 1)
  (cl-rabbit:destroy-connection (producer-conn producer)))

(defmethod producer-connect ((producer producer))
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
    (setf (producer-open-p producer) t)))


(defmethod publish ((producer producer) &key body routing-key (properties nil))
  (with-producer-lock (producer)
    (cl-rabbit:basic-publish (producer-conn producer) 1 :exchange (producer-exchange producer) :body body :properties properties :routing-key routing-key)))




(defun make-producer (&rest args)
  (apply #'make-instance 'producer args))
