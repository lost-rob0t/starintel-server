(in-package :star.rabbit)

(defvar +injest-queue+ "documents-ingest")
(defvar +updates-queue+ "documents-updates")
(defvar +injest-key+ "documents.ingest.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.*")

(defun settle-rabbit-delivery (connection channel delivery-tag settlement)
  (ecase (consumer-settlement-action settlement)
    ((:ack :filtered-ack)
     (cl-rabbit:basic-ack connection channel delivery-tag))
    (:retry
     (cl-rabbit:basic-nack
      connection channel delivery-tag :requeue t))
    ((:dead-letter :reject)
     (cl-rabbit:basic-nack
      connection channel delivery-tag :requeue nil)))
  settlement)

(defmacro with-rabbit-recv
    ((queue-name exchange-name exchange-type routing-key
      &key
        (port star:*rabbit-port*)
        (host star:*rabbit-address*)
        (username star:*rabbit-user*)
        (password star:*rabbit-password*)
        (vhost "/")
        (durable nil)
        (exclusive nil)
        (auto-delete nil))
     &body body)
  "Compatibility owner-thread receive loop with structured settlement results."
  `(cl-rabbit:with-connection (connection)
     (let ((socket (cl-rabbit:tcp-socket-new connection)))
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (cl-rabbit:login-sasl-plain
          connection ,vhost ,username ,password))
       (cl-rabbit:with-channel (connection 1)
         (cl-rabbit:exchange-declare
          connection 1 ,exchange-name ,exchange-type)
         (cl-rabbit:queue-declare
          connection 1
          :queue ,queue-name
          :durable ,durable
          :auto-delete ,auto-delete
          :exclusive ,exclusive)
         (cl-rabbit:queue-bind
          connection 1
          :queue ,queue-name
          :exchange ,exchange-name
          :routing-key ,routing-key)
         (cl-rabbit:basic-consume connection 1 ,queue-name)
         (loop
           for result = (cl-rabbit:consume-message connection)
           for message = (cl-rabbit:envelope/message result)
           for delivery-tag = (cl-rabbit:envelope/delivery-tag result)
           for settlement =
             (handler-case
                 (normalize-settlement (progn ,@body))
               (condition (error)
                 (log:error "Rabbit handler failed: ~a" error)
                 (settlement-retry (princ-to-string error) error)))
           do (settle-rabbit-delivery
               connection 1 delivery-tag settlement))))))

(defun emit-document
    (exchange routing-key body
     &key
       (properties nil)
       (immediate nil)
       (mandatory nil)
       (port star:*rabbit-port*)
       (host star:*rabbit-address*)
       (username star:*rabbit-user*)
       (password star:*rabbit-password*)
       (vhost "/"))
  (let ((canonical-body (star.documents:v09-document-json body)))
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
           connection
           1
           :routing-key routing-key
           :exchange exchange
           :mandatory mandatory
           :immediate immediate
           :properties properties
           :body canonical-body))))))

(defun message->string (message &key (encoding :utf-8))
  (babel:octets-to-string
   (cl-rabbit:message/body message)
   :encoding encoding))

(defun message->object (message)
  (star.documents:ensure-v09-document (message->string message)))

(defun insert (client database document)
  (format nil "~a~%"
          (couch:create-document
           client
           database
           (star.documents:v09-document-json document))))

(defun publish-outbox-event (routing-key payload event-id)
  "Publish one physical delivery carrying a stable logical EVENT-ID."
  (declare (ignore event-id))
  (emit-document "documents" routing-key payload)
  t)

(defun process-rabbit-document-mutation (self message operation)
  (declare (ignore self))
  (let ((document
          (star.documents:ensure-v09-document (car message))))
    (anypool:with-connection
        (client star.databases.couchdb:*couchdb-pool*)
      (star.databases.couchdb:couchdb-process-outbox-mutation
       client
       star:*couchdb-default-database*
       #'publish-outbox-event
       document
       operation))
    (settlement-ack "outbox mutation persisted and published")))

(defun handle-document (self message)
  (process-rabbit-document-mutation self message :new))

(defun handle-update-document (self message)
  (process-rabbit-document-mutation self message :updated))

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
   (star.documents:ensure-v09-document (car message))))

(defun insertp (message)
  (not (transient-p message)))

(defun handle-target (self message)
  (declare (ignore self))
  (let ((body
          (star.documents:ensure-v09-document
           (car message)
           :route-dtype "target")))
    (tell star.actors:*targets* (cons 1 body))
    (settlement-ack "target submitted to local actor")))

(defun start-consumers ()
  (log:info "Starting owner-thread v0.9 document consumers.")
  (let ((document-consumers
          (create-rabbit-consumer
           :name "documents-ingest"
           :n star:*injest-workers*
           :queue-name +injest-queue+
           :exchange-name "documents"
           :routing-key +injest-key+
           :username star:*rabbit-user*
           :password star:*rabbit-password*
           :host star:*rabbit-address*
           :port star:*rabbit-port*
           :handler-fn #'handle-document
           :test-fn #'insertp
           :on-error :retry
           :on-filter :filtered-ack))
        (update-consumers
          (create-rabbit-consumer
           :name "documents-update"
           :n star:*injest-workers*
           :queue-name +updates-queue+
           :exchange-name "documents"
           :routing-key +update-key+
           :username star:*rabbit-user*
           :password star:*rabbit-password*
           :host star:*rabbit-address*
           :port star:*rabbit-port*
           :handler-fn #'handle-update-document
           :test-fn #'insertp
           :on-error :retry
           :on-filter :filtered-ack))
        (target-consumers
          (create-rabbit-consumer
           :name "documents-targets"
           :n star:*injest-workers*
           :queue-name "documents-targets"
           :exchange-name "documents"
           :routing-key +targets-key+
           :username star:*rabbit-user*
           :password star:*rabbit-password*
           :host star:*rabbit-address*
           :port star:*rabbit-port*
           :handler-fn #'handle-target
           :test-fn #'insertp
           :on-error :retry
           :on-filter :filtered-ack)))
    (start-consumer document-consumers)
    (start-consumer update-consumers)
    (start-consumer target-consumers)
    (recover-pending-publications)))
