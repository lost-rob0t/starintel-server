(in-package #:star.actors)

(defparameter *actor-event-log* nil)

(defclass actor-event ()
  ((_id :initarg :id :accessor event-id :initform (cms-ulid:ulid))
   (timestamp :initarg :timestamp :accessor event-timestamp :initform (spec:unix-now))
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
    (let ((event-json (jsown:to-json (as-json event))))
      (tell *couchdb-inserts*
            (list :id (event-id event)
                  :database star:*couchdb-event-log-database*
                  :document event-json)))))

(defun decode-actor-event-delivery (message)
  (handler-case
      (let ((json-document
              (jsown:with-injective-reader
                (jsown:parse (car message)))))
        (star.databases.couchdb:from-json json-document 'actor-event))
    (star.consumers:delivery-processing-error (condition)
      (error condition))
    (error (condition)
      (error 'star.consumers:schema-invalid-delivery-error
             :cause condition
             :reason (princ-to-string condition)))))

(defun handle-event-message (self message)
  "Decode one event delivery and return settlement to the Rabbit owner thread."
  (declare (ignore self))
  (tell *actor-event-receiver* (decode-actor-event-delivery message))
  (star.consumers:settlement-ack "actor event accepted"))

(defun start-event-consumer (n)
  "Initialize bounded-retry owner-thread event consumers."
  (let ((consumer
          (star.consumers:create-rabbit-consumer
           :name "event-consumers"
           :n n
           :host star:*rabbit-address*
           :port star:*rabbit-port*
           :username star:*rabbit-user*
           :password star:*rabbit-password*
           :queue-name "events"
           :exchange-name "events"
           :routing-key "event.#"
           :test-fn #'identity
           :handler-fn #'handle-event-message
           :on-error :retry
           :on-filter :filtered-ack
           :max-retries star:*rabbit-max-retries*
           :retry-base-delay-ms star:*rabbit-retry-base-delay-ms*
           :retry-max-delay-ms star:*rabbit-retry-max-delay-ms*
           :retry-jitter-ratio star:*rabbit-retry-jitter-ratio*
           :quarantine-fn #'star.rabbit:persist-quarantine-record
           :quarantine-exchange star:*rabbit-quarantine-exchange*
           :quarantine-queue star:*rabbit-quarantine-queue*)))
    (star.consumers:start-consumer consumer)))

(defun log-actor-event (actor-name &key event-type details source-id)
  (log:debug "Told *actor-event-receiver*")
  (tell *actor-event-receiver*
        (make-actor-event :actor-name actor-name
                          :event-type event-type
                          :details details
                          :source-id source-id)))
