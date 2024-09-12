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



(defun start-event-log-consumers (n &key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*))
  (log:info (format nil "Creating ~a event threads." n))
  (loop for i from 1 to n
        for stream = (make-instance 'rabbit-queue-stream :host host :port port :user username :password password :queue-name "injest-events" :exchange-name "events" :routing-key "events.#")
        for consumer = (make-instance 'rabbit-consumer :name (format nil "~a-~a" "event-consumer" i) :stream stream :fn #'handle-event :test-fn #'insertp)
        do (open-stream stream)
        do (start-consumer consumer)))

(defun log-actor-event (actor-name event-type details)
  (log:debug "Told *actor-event-reciver*")
  (tell *actor-event-log* (make-actor-event actor-name event-type details)))
