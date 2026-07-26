(in-package #:star.actors)

(in-package :star.actors)
(defclass actor-event ()
  ((_id :initarg :id :accessor event-id :initform (cms-ulid:ulid))
   (timestamp  :initarg :timestamp :accessor event-timestamp :initform (spec:unix-now))
   (dtype  :initarg :timestamp :accessor doc-type :initform "actorevent")
   (actor-name :initarg :actor-name :accessor event-actor-name :initform "")
   (event-type :initarg :event-type :accessor event-type :initform "")
   (details :initarg :details :accessor event-details :initform "")
   (source-id :initarg :source-id :accessor event-source-document :initform "")))




(defun make-actor-event (&key actor-name event-type details source-id)
  (make-instance 'actor-event
                 :actor-name actor-name
                 :event-type event-type
                 :details details
                 :source-id source-id))





(define-actor (*actor-event-receiver* *sys*)
    (lambda (event)
      (let ((event-json (jsown:to-json (as-json event))))
        (tell *couchdb-inserts* (list :id (event-id event) :database star:*couchdb-event-log-database* :document event-json)))))





(defun handle-event-message (self message)
  "Handler function for processing event messages."
  (let* ((jdoc (jsown:parse (car message))))
    (log:trace "Got Event: ~a" (spec:decode jdoc 'actor-event))
    (tell *actor-event-receiver* 
          (make-instance 'actor-event
                         :id (jsown:val jdoc "_id")
                         :timestamp (jsown:val jdoc "timestamp")
                         :actor-name (jsown:val jdoc "actorName")
                         :event-type (jsown:val jdoc "eventType")
                         :details (jsown:val jdoc "details")
                         :source-id (jsown:val jdoc "sourceId")))))


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
  (tell *actor-event-receiver* (make-actor-event :actor-name actor-name :event-type event-type :details  details :source-id source-id)))

(nhooks:add-hook star:*actors-start-hook*
                 (lambda () (star.actors:register-actor "actor-event-receiver" *actor-event-receiver*)))
