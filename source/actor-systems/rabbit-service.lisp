(uiop:define-package   :star.actors.rabbitmq
  (:use       :cl :log4cl :act :asys :tasks)
  (:documentation "RabbitMQ Service Actor and agent.")
  (:export
   #:make-rabbitmq-agent
   #:publish
   #:make-producer-agent
   #:producer-agent-connect
   #:producer-agent-close
   #:agent-publish
   #:producer-agent->producer
   #:*publish-service*
   #:*producer-agent*
   #:rabbit!
   #:rabbit?
   #:make-publish-service))


(in-package :star.actors.rabbitmq)


(defun make-producer-agent (context &rest init-args)
  "Create a new producer agent that wraps a RabbitMQ producer.
The agent manages producer lifecycle including connection and message publishing.
The producer will have no lock, As sento provides this functionality.
Arguments:
  INIT-ARGS - Key-value pairs passed to make-producer (name, exchange-name, etc)
  CONTEXT  - Optional execution context for the agent

Returns:
  A new producer agent instance with connected RabbitMQ producer"
  (sento.agent:make-agent (lambda ()
                            (let ((producer
                                    (apply #'star.producers:make-producer  init-args)))
                              (log:debug "Creating ~A producer agent" (star.producers:producer-name producer))
                              producer))
                          context))

(defun producer-agent-connect (agent)
  "Connect the producer held by the agent to the RabbitMQ broker.
Uses agent-update to safely connect the underlying producer instance.
This establishes a TCP connection, performs SASL authentication if credentials are provided,
opens channel 1 and declares the configured exchange.

Arguments:
  AGENT - The producer agent holding a producer instance"
  (sento.agent:agent-update agent (lambda (state)
                                    (star.producers:producer-connect state))))

(defun producer-agent-close (agent)
  "Close the RabbitMQ producer agent and destroy its underlying connection.
This will close the current channel and clean up the connection to the RabbitMQ broker.
Call this when you are done with the producer agent to free system resources.
Argument:
  AGENT - The producer agent to close"
  (sento.agent:agent-update agent (lambda (state)
                                    (star.producers:destroy state))))

(defun producer-agent->producer (agent)
  "Extract the underlying producer instance from a producer agent.
Returns the raw producer object held by the agent for direct access to producer functionality."
  (sento.agent:agent-get agent #'identity))

(defun agent-publish (agent &key body (properties nil) routing-key)
  "Publish a message to RabbitMQ via a producer agent.
Arguments:
  AGENT - The producer agent
  BODY - Message content that will be converted to JSON
  PROPERTIES - Optional message properties like delivery-mode, content-type etc.
  ROUTING-KEY - The routing key for message routing

Returns the publish response from the underlying RabbitMQ producer.
Messages are traced with log4cl."
  (let ((resp (star.producers:publish (sento.agent:agent-get agent #'identity)
                                      :body (jsown:to-json body)
                                      :properties properties
                                      :routing-key routing-key)))
    (log:trace resp)
    resp))


;; Note: This is the refactored version using actor-of instead of define-agent-service
(defun make-publish-service (agent actor-var system &key (name "rabbit-publish") (workers 2) (dispatcher-id :rabbitmq))
  "Creates and starts a RabbitMQ actor service for publishing messages.
Arguments:
  AGENT - The RabbitMQ producer agent that handles the connection and publishing
  ACTOR-VAR - Variable to store the created actor service
  SYSTEM - The actor system to create the service in
  DISPATCHER-ID - Optional dispatcher ID for the actor service (default: :rabbitmq)

Returns:
  The created actor service for publishing messages to RabbitMQ.

The actor service accepts messages with :body, :properties, and :routing-key arguments
and publishes them to RabbitMQ using the provided producer agent."
  (ac:actor-of system
               :receive (lambda (msg)
                          (log:debug (list 
                                      :body (getf msg :body)
                                      :properties (getf msg :properties nil)
                                      :routing-key (getf msg :routing-key)))
                          (agent-publish agent
                                         :body (getf msg :body)
                                         :properties (getf msg :properties)
                                         :routing-key (getf msg :routing-key)))
               :name name
               ))

(defun rabbit! (actor &rest args)
  "Send a message to a rabbitmq actor and do not expect a response. Expects &key style messages"
  (act:tell actor (apply #'list args)))

(defun rabbit? (actor timeout &rest args)
  "Send a message to a rabbitmq actor for response. Expects &key style messages"
  (act:ask actor (apply #'list args) :time-out timeout))
