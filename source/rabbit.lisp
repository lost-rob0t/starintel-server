(in-package :star.rabbit)

(defvar +injest-queue+ "documents-ingest")
(defvar +updates-queue+ "documents-updates")
(defvar +injest-key+ "documents.ingest.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.*")

(defun publish-raw-message
    (exchange routing-key body properties
     &key
       (port star:*rabbit-port*)
       (host star:*rabbit-address*)
       (username star:*rabbit-user*)
       (password star:*rabbit-password*)
       (vhost "/"))
  "Publish raw Rabbit content with caller-supplied provenance properties."
  (cl-rabbit:with-connection (connection)
    (let ((socket (cl-rabbit:tcp-socket-new connection)))
      (cl-rabbit:socket-open socket host port)
      (when (and username password)
        (cl-rabbit:login-sasl-plain
         connection vhost username password))
      (cl-rabbit:with-channel (connection 1)
        (cl-rabbit:exchange-declare
         connection 1 exchange "topic" :durable t)
        (cl-rabbit:basic-publish
         connection 1
         :routing-key routing-key
         :exchange exchange
         :properties properties
         :body body))))
  t)

(defun emit-document
    (exchange routing-key body
     &key
       (properties nil)
       (port star:*rabbit-port*)
       (host star:*rabbit-address*)
       (username star:*rabbit-user*)
       (password star:*rabbit-password*)
       (vhost "/"))
  (publish-raw-message
   exchange
   routing-key
   (star.documents:v09-document-json body)
   properties
   :port port
   :host host
   :username username
   :password password
   :vhost vhost))

(defun decode-rabbit-document (message &key route-dtype)
  "Parse and validate one delivery body as a permanent schema boundary."
  (handler-case
      (star.documents:ensure-v09-document
       (car message)
       :route-dtype route-dtype)
    (star.consumers:delivery-processing-error (condition)
      (error condition))
    (error (condition)
      (error 'star.consumers:schema-invalid-delivery-error
             :cause condition
             :reason (princ-to-string condition)))))

(defun publish-outbox-event (routing-key payload event-id)
  "Publish one physical delivery carrying a stable logical EVENT-ID."
  (emit-document
   "documents"
   routing-key
   payload
   :properties
   (list (cons :content-type "application/json")
         (cons :delivery-mode 2)
         (cons :message-id event-id)))
  t)

(defun persist-quarantine-record (record)
  "Durably persist RECORD before its original Rabbit delivery is ACKed."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb:couchdb-save-quarantine-record
     client
     star:*couchdb-default-database*
     record)))

(defun inspect-quarantine (&key (status "quarantined") (limit 100))
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb:couchdb-list-quarantine-records
     client
     star:*couchdb-default-database*
     :status status
     :limit limit)))

(defun replay-quarantined-message (quarantine-id &key corrected-body)
  "Replay one corrected quarantine record with a new trace and attempt history."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb:replay-quarantine-record
     client
     star:*couchdb-default-database*
     quarantine-id
     #'publish-raw-message
     :corrected-body corrected-body)))

(defun process-rabbit-document-mutation (message operation)
  (let ((document (decode-rabbit-document message)))
    (if (star.documents:document-transient-p document)
        (settlement-filtered-ack
         "transient document intentionally not persisted")
        (progn
          (anypool:with-connection
              (client star.databases.couchdb:*couchdb-pool*)
            (star.databases.couchdb:couchdb-process-outbox-mutation
             client
             star:*couchdb-default-database*
             #'publish-outbox-event
             document
             operation))
          (settlement-ack
           "durable mutation and publication completed")))))

(defun handle-document (self message)
  (declare (ignore self))
  (process-rabbit-document-mutation message :new))

(defun handle-update-document (self message)
  (declare (ignore self))
  (process-rabbit-document-mutation message :updated))

(defun recover-pending-publications ()
  "Republish pending durable outbox entries in per-document sequence order."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb:recover-couchdb-outbox
     client
     star:*couchdb-default-database*
     #'publish-outbox-event)))

(defun transient-p (message)
  (star.documents:document-transient-p
   (decode-rabbit-document message)))

(defun target-outcome-settlement (outcome)
  "Translate durable target acceptance outcomes to owner-thread settlement."
  (case (star.actors:target-dispatch-outcome-status outcome)
    ((:accepted :duplicate)
     (settlement-ack
      (or (star.actors:target-dispatch-outcome-reason outcome)
          "target dispatch durably accepted")))
    (:invalid
     (settlement-dead-letter
      (star.actors:target-dispatch-outcome-reason outcome)
      (make-condition
       'star.consumers:permanent-delivery-error
       :reason (star.actors:target-dispatch-outcome-reason outcome))))
    ((:overloaded :unavailable :failed)
     (settlement-retry
      (star.actors:target-dispatch-outcome-reason outcome)
      (make-condition
       'star.consumers:transient-delivery-error
       :reason (star.actors:target-dispatch-outcome-reason outcome))))
    (otherwise
     (settlement-reject
      "unknown target dispatch outcome"
      (make-condition
       'star.consumers:permanent-delivery-error
       :reason "unknown target dispatch outcome")))))

(defun handle-target (self message)
  (let ((body (decode-rabbit-document message :route-dtype "target")))
    (target-outcome-settlement
     (star.actors:accept-target-delivery self body))))

(defun consumer-retry-options ()
  (list
   :max-retries star:*rabbit-max-retries*
   :retry-base-delay-ms star:*rabbit-retry-base-delay-ms*
   :retry-max-delay-ms star:*rabbit-retry-max-delay-ms*
   :retry-jitter-ratio star:*rabbit-retry-jitter-ratio*
   :quarantine-fn #'persist-quarantine-record
   :quarantine-exchange star:*rabbit-quarantine-exchange*
   :quarantine-queue star:*rabbit-quarantine-queue*))

(defun make-document-consumer
    (&key name queue-name routing-key handler-fn)
  (apply
   #'create-rabbit-consumer
   :name name
   :n star:*injest-workers*
   :queue-name queue-name
   :exchange-name "documents"
   :routing-key routing-key
   :username star:*rabbit-user*
   :password star:*rabbit-password*
   :host star:*rabbit-address*
   :port star:*rabbit-port*
   :handler-fn handler-fn
   :test-fn #'identity
   :on-error :retry
   :on-filter :filtered-ack
   (consumer-retry-options)))

(defun start-consumers ()
  (log:info "Starting bounded-retry v0.9 document consumers.")
  (let ((document-consumers
          (make-document-consumer
           :name "documents-ingest"
           :queue-name +injest-queue+
           :routing-key +injest-key+
           :handler-fn #'handle-document))
        (update-consumers
          (make-document-consumer
           :name "documents-update"
           :queue-name +updates-queue+
           :routing-key +update-key+
           :handler-fn #'handle-update-document))
        (target-consumers
          (make-document-consumer
           :name "documents-targets"
           :queue-name "documents-targets"
           :routing-key +targets-key+
           :handler-fn #'handle-target)))
    (start-consumer document-consumers)
    (start-consumer update-consumers)
    (start-consumer target-consumers)
    (recover-pending-publications)))
