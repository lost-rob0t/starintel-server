(in-package :star.actors)

(defparameter +event-exchange+ "events")
(defparameter +event-queue+ "events")
(defparameter +event-routing-key+ "event.#")
(defparameter +event-dead-letter-exchange+ "events.dead-letter")
(defparameter +event-dead-letter-queue+ "events.quarantine")
(defparameter +event-dead-letter-routing-key+ "events.invalid")
(defparameter +event-persistence-timeout-seconds+ 10)

(define-condition invalid-actor-event (error)
  ((reason
    :initarg :reason
    :reader invalid-actor-event-reason)
   (payload
    :initarg :payload
    :reader invalid-actor-event-payload))
  (:report
   (lambda (condition stream)
     (format stream
             "Invalid actor event: ~a"
             (invalid-actor-event-reason condition)))))

(defclass actor-event ()
  ((_id
    :initarg :id
    :accessor event-id
    :initform (cms-ulid:ulid)
    :type string)
   (timestamp
    :initarg :timestamp
    :accessor event-timestamp
    :initform (spec:unix-now)
    :type integer)
   (dtype
    :initarg :dtype
    :accessor doc-type
    :initform "actorevent"
    :type string)
   (actor-name
    :initarg :actor-name
    :accessor event-actor-name
    :initform ""
    :type string)
   (component
    :initarg :component
    :accessor event-component
    :initform ""
    :type string)
   (event-type
    :initarg :event-type
    :accessor event-type
    :initform ""
    :type string)
   (details
    :initarg :details
    :accessor event-details
    :initform ""
    :type string)
   (source-id
    :initarg :source-id
    :accessor event-source-document
    :initform ""
    :type string)
   (trace-id
    :initarg :trace-id
    :accessor event-trace-id
    :initform ""
    :type string)
   (generation
    :initarg :generation
    :accessor event-generation
    :initform 0
    :type integer)))

(defun make-actor-event (&key actor-name component event-type details source-id
                           trace-id (generation 0) timestamp dtype id)
  (make-instance
   'actor-event
   :id (or id (cms-ulid:ulid))
   :timestamp (or timestamp (spec:unix-now))
   :dtype (or dtype "actorevent")
   :actor-name (or actor-name component "")
   :component (or component actor-name "")
   :event-type (or event-type "")
   :details (or details "")
   :source-id (or source-id "")
   :trace-id (or trace-id "")
   :generation generation))

(defun non-empty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun validate-actor-event (event &optional payload)
  (flet ((invalid (reason)
           (error 'invalid-actor-event
                  :reason reason
                  :payload payload)))
    (unless (non-empty-string-p (event-id event))
      (invalid "_id must be a non-empty string"))
    (unless (and (integerp (event-timestamp event))
                 (plusp (event-timestamp event)))
      (invalid "timestamp must be a positive integer"))
    (unless (string= "actorevent" (doc-type event))
      (invalid "dtype must be actorevent"))
    (unless (or (non-empty-string-p (event-actor-name event))
                (non-empty-string-p (event-component event)))
      (invalid "actorName or component is required"))
    (unless (non-empty-string-p (event-type event))
      (invalid "eventType is required"))
    (unless (and (integerp (event-generation event))
                 (not (minusp (event-generation event))))
      (invalid "generation must be a non-negative integer"))
    event))

(defun actor-event-json-object (payload)
  (let ((json
          (etypecase payload
            (string (jsown:parse payload))
            (list payload))))
    (unless (jsown:val-safe json "dtype")
      (setf (jsown:val json "dtype") "actorevent"))
    (unless (jsown:val-safe json "generation")
      (setf (jsown:val json "generation") 0))
    (let ((actor-name (jsown:val-safe json "actorName"))
          (component (jsown:val-safe json "component")))
      (when (and actor-name (not component))
        (setf (jsown:val json "component") actor-name))
      (when (and component (not actor-name))
        (setf (jsown:val json "actorName") component)))
    json))

(defun decode-actor-event (payload)
  "Decode and validate an event through the StarIntel object codec."
  (handler-case
      (let* ((json (actor-event-json-object payload))
             (event
               (star.databases.couchdb:from-json json 'actor-event)))
        (validate-actor-event event payload))
    (invalid-actor-event (condition)
      (error condition))
    (error (condition)
      (error 'invalid-actor-event
             :reason (princ-to-string condition)
             :payload payload))))

(defun encode-actor-event (event)
  (jsown:to-json
   (star.databases.couchdb:as-json
    (validate-actor-event event))))

(defun actor-event-insert-request (event)
  (make-couchdb-insert-request
   :database star:*couchdb-event-log-database*
   :document-id (event-id event)
   :document (encode-actor-event event)))

(defun persist-actor-event (event &optional (insert-actor *couchdb-inserts*))
  (sento.actor:ask-s
   insert-actor
   (actor-event-insert-request event)
   :time-out +event-persistence-timeout-seconds+))

(defun actor-event-settlement (event persistence-result)
  (declare (ignore event))
  (cond
    ((not (typep persistence-result 'couchdb-result))
     (star.consumers:settlement-retry
      :persistence-protocol-error
      persistence-result))
    ((member (couchdb-result-status persistence-result)
             '(:success :exists :conflict))
     (star.consumers:settlement-ack
      (if (eq :success (couchdb-result-status persistence-result))
          :persisted
          :duplicate)))
    (t
     (star.consumers:settlement-retry
      :persistence-failed
      (couchdb-result-error-message persistence-result)))))

(defun process-event-delivery (payload &key (persist-fn #'persist-actor-event))
  "Decode, validate, persist idempotently, and return an owner-thread settlement."
  (handler-case
      (let ((event (decode-actor-event payload)))
        (actor-event-settlement event (funcall persist-fn event)))
    (invalid-actor-event (condition)
      (star.consumers:settlement-dead-letter
       :invalid-event
       condition))
    (error (condition)
      (star.consumers:settlement-retry
       :event-handler-error
       condition))))

(define-actor (*actor-event-receiver* *sys*)
  (lambda (event)
    (tell *couchdb-inserts* (actor-event-insert-request event))))

(defun handle-event-message (consumer message)
  (declare (ignore consumer))
  (process-event-delivery (car message)))

(defun start-event-consumer (n)
  "Start durable owner-thread event consumers with bounded retry and quarantine."
  (let ((consumer
          (star.consumers:create-rabbit-consumer
           :name "event-consumers"
           :n n
           :host star:*rabbit-address*
           :port star:*rabbit-port*
           :username star:*rabbit-user*
           :password star:*rabbit-password*
           :queue-name +event-queue+
           :queue-durable t
           :exchange-name +event-exchange+
           :exchange-type "topic"
           :exchange-durable t
           :routing-key +event-routing-key+
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

(defun log-actor-event (actor-name &key event-type details source-id trace-id
                                     component (generation 0))
  (tell
   *actor-event-receiver*
   (make-actor-event
    :actor-name actor-name
    :component component
    :event-type event-type
    :details details
    :source-id source-id
    :trace-id trace-id
    :generation generation)))

(nhooks:add-hook
 star:*actors-start-hook*
 (lambda ()
   (star.actors:register-actor
    "actor-event-receiver"
    *actor-event-receiver*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(actor-event
            invalid-actor-event
            invalid-actor-event-reason
            make-actor-event
            validate-actor-event
            decode-actor-event
            encode-actor-event
            process-event-delivery
            event-component
            event-trace-id
            event-generation)
          :star.actors))
