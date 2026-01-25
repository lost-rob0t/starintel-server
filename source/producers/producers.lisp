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
  (handler-case
      (progn
        (when (producer-conn producer)
          (ignore-errors (cl-rabbit:channel-close (producer-conn producer) 1))
          (ignore-errors (cl-rabbit:destroy-connection (producer-conn producer)))
          (setf (producer-conn producer) nil)
          (setf (producer-open-p producer) nil)))
    (error (e)
      (log:warn "[~a] Error during destroy (ignoring): ~a" (producer-name producer) e))))

(defmethod producer-connect ((producer producer))
  (log:info "[~a] Connecting to RabbitMQ ~a:~a"
            (producer-name producer)
            (producer-host producer)
            (producer-port producer))
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
    (setf (producer-open-p producer) t)
    (log:info "[~a] Connected successfully to RabbitMQ" (producer-name producer))))


(defmethod publish ((producer producer) &key body routing-key (properties nil))
  (with-producer-lock (producer)
    (handler-case
        (progn
          (unless (producer-open-p producer)
            (log:warn "[~a] Producer not connected, reconnecting..." (producer-name producer))
            (producer-connect producer))
          (cl-rabbit:basic-publish (producer-conn producer) 1
                                   :exchange (producer-exchange producer)
                                   :body body
                                   :properties properties
                                   :routing-key routing-key)
          (log:debug "[~a] Published message to exchange: ~a routing-key: ~a"
                     (producer-name producer)
                     (producer-exchange producer)
                     routing-key))
      (cl-rabbit:rabbitmq-library-error (e)
        (log:error "[~a] RabbitMQ library error during publish: ~a" (producer-name producer) e)
        (log:info "[~a] Destroying connection and reconnecting..." (producer-name producer))
        (destroy producer)
        (sleep 1)
        (handler-case
            (progn
              (producer-connect producer)
              (log:info "[~a] Reconnected, retrying publish..." (producer-name producer))
              (cl-rabbit:basic-publish (producer-conn producer) 1
                                       :exchange (producer-exchange producer)
                                       :body body
                                       :properties properties
                                       :routing-key routing-key)
              (log:info "[~a] Retry successful" (producer-name producer)))
          (error (retry-err)
            (log:error "[~a] Retry failed: ~a" (producer-name producer) retry-err)
            (error retry-err))))
      (error (e)
        (log:error "[~a] Unexpected error during publish: ~a" (producer-name producer) e)
        (log:info "[~a] Destroying connection and reconnecting..." (producer-name producer))
        (destroy producer)
        (sleep 1)
        (handler-case
            (progn
              (producer-connect producer)
              (log:info "[~a] Reconnected, retrying publish..." (producer-name producer))
              (cl-rabbit:basic-publish (producer-conn producer) 1
                                       :exchange (producer-exchange producer)
                                       :body body
                                       :properties properties
                                       :routing-key routing-key)
              (log:info "[~a] Retry successful" (producer-name producer)))
          (error (retry-err)
            (log:error "[~a] Retry failed: ~a" (producer-name producer) retry-err)
            (error retry-err)))))))




(defun make-producer (&rest args)
  (apply #'make-instance 'producer args))
