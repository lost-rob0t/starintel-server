(in-package :star.rabbit)

(defparameter +documents-exchange+ "documents")
(defparameter +documents-exchange-type+ "topic")
(defparameter +ingest-key+ "documents.ingest.#")
(defparameter +update-key+ "documents.update.#")
(defparameter +ingest-queue+ "documents.ingest")
(defparameter +updates-queue+ "documents.update")
(defparameter +targets-key+ "documents.new.target.#")
(defparameter +targets-queue+ "documents.targets")
(defparameter +ingest-fmt-key+ "documents.ingest.~a")
(defparameter +new-documents-key+ "documents.new.#")
(defparameter +new-documents-fmt-key+ "documents.new.~a")
(defparameter +updated-documents-key+ "documents.updated.#")
(defparameter +updated-documents-fmt-key+ "documents.updated.~a")

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
         connection 1 exchange +documents-exchange-type+ :durable t)
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
       properties
       (port star:*rabbit-port*)
       (host star:*rabbit-address*)
       (username star:*rabbit-user*)
       (password star:*rabbit-password*)
       (vhost "/"))
  "Publish one StarIntel document after transport-level normalization."
  (publish-raw-message
   exchange
   routing-key
   (star.documents:document-json body)
   (or properties
       (list (cons :content-type "application/json")
             (cons :delivery-mode 2)))
   :port port
   :host host
   :username username
   :password password
   :vhost vhost))

(defun decode-rabbit-document
    (message &key route-dtype (strict-schema-p t))
  "Parse one Rabbit delivery and convert malformed payloads to permanent errors.

Canonical document mutation queues enforce the StarIntel v0.9 schema. Legacy
compatibility queues must opt out explicitly."
  (handler-case
      (let ((document
              (star.documents:ensure-document
               (car message)
               :route-dtype route-dtype)))
        (when strict-schema-p
          (star.documents:validate-v09-document document))
        document)
    (star.consumers:delivery-processing-error (condition)
      (error condition))
    (error (condition)
      (error 'star.consumers:schema-invalid-delivery-error
             :cause condition
             :reason (princ-to-string condition)))))

(defun publish-outbox-event (routing-key payload event-id)
  "Publish one physical delivery carrying a stable logical EVENT-ID."
  (emit-document
   +documents-exchange+
   routing-key
   payload
   :properties
   (list (cons :content-type "application/json")
         (cons :delivery-mode 2)
         (cons :message-id event-id)))
  t)

(defun persist-quarantine-record (record)
  "Persist RECORD before its original Rabbit delivery is acknowledged."
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

(defun handle-document (consumer message)
  (declare (ignore consumer))
  (process-rabbit-document-mutation message :new))

(defun handle-update-document (consumer message)
  (declare (ignore consumer))
  (process-rabbit-document-mutation message :updated))

(defun recover-pending-publications ()
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb:recover-couchdb-outbox
     client
     star:*couchdb-default-database*
     #'publish-outbox-event)))

(defun transient-p (message)
  "Inspect legacy transport metadata without weakening canonical mutation validation."
  (star.documents:document-transient-p
   (decode-rabbit-document message :strict-schema-p nil)))

(defun target-outcome-settlement (outcome)
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

(defun handle-target (consumer message)
  (target-outcome-settlement
   (star.actors:accept-target-delivery
    consumer
    (decode-rabbit-document
     message
     :route-dtype "target"
     :strict-schema-p nil))))

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
   :n star:*ingest-workers*
   :queue-name queue-name
   :exchange-name +documents-exchange+
   :exchange-type +documents-exchange-type+
   :exchange-durable t
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
  "Start owner-thread Rabbit consumers and recover pending outbox events."
  (let ((ingest
          (make-document-consumer
           :name "documents-ingest"
           :queue-name +ingest-queue+
           :routing-key +ingest-key+
           :handler-fn #'handle-document))
        (updates
          (make-document-consumer
           :name "documents-update"
           :queue-name +updates-queue+
           :routing-key +update-key+
           :handler-fn #'handle-update-document))
        (targets
          (make-document-consumer
           :name "documents-targets"
           :queue-name +targets-queue+
           :routing-key +targets-key+
           :handler-fn #'handle-target)))
    (start-consumer ingest)
    (start-consumer updates)
    (start-consumer targets)
    (recover-pending-publications)
    (list ingest updates targets)))
