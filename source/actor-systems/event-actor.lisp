(in-package #:star.actors)

(defparameter *actor-event-log* nil)



(defclass actor-event ()
  ((timestamp :initarg :timestamp :accessor event-timestamp)
   (actor-name :initarg :actor-name :accessor event-actor-name)
   (event-type :initarg :event-type :accessor event-type)
   (details :initarg :details :accessor event-details)
   (source-id :initarg :source-id :accessor event-source-document)))


(defun make-actor-event (&key actor-name event-type details source-id)
  (make-instance 'actor-event
                 :actor-name actor-name
                 :event-type event-type
                 :details details
                 :source-id source-id))






(define-actor (*actor-event-receiver* *sys*)
  (lambda (event)
    (let ((event-json (jsown:to-json
                       (jsown:extend-js (star.databases.couchdb:as-json event)
                         ("_id" (cms-ulid:ulid))))))
      (tell *couchdb-inserts* (list :database star:*couchdb-event-log-database* :document event-json)))))




(defun handle-event-message (message)
  "Handler function for processing event messages."
  (let* ((jdoc (jsown:parse message)))
    (tell *actor-event-receiver* (star.databases.couchdb:from-json jdoc 'actor-event))))


(defun start-event-consumer (n)
  "Initialize and set up the event consumer."
  (let ((consumer (star.consumers:create-rabbit-consumer
                   :name "event-consumers"
                   :n n
                   :host star:*rabbit-address*
                   :port star:*rabbit-port*
                   :username star:*rabbit-user*
                   :password star:*rabbit-password*
                   :queue-name "events"
                   :exchange-name "events"
                   :routing-key "event.#"
                   :test-fn #'star.rabbit::insertp
                   :handler-fn #'handle-event-message)))
    (star.consumers:start-consumer consumer)))

(defun log-actor-event (actor-name &key event-type details source-id)
  (log:debug "Told *actor-event-reciver*")
  (tell *actor-event-log* (make-actor-event actor-name event-type details source-id)))
