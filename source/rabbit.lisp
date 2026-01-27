(in-package :star.rabbit)

;;; ----------------------------------------------------------------------
;;; constants

(defparameter +documents-exchange+ "documents"
  "RabbitMQ exchange name for document traffic (ingest/new/updated).")


(defparameter +documents-exchange-type+ "topic"
  "Exchange type for +documents-exchange+. Must be \"topic\" so routing keys like
documents.ingest.#, documents.new.#, and documents.updated.# work.")

;; routing keys (canonical)
(defparameter +ingest-key+ "documents.ingest.#"
  "Wildcard routing key for ALL ingest messages (initial insert path).")


(defparameter +ingest-fmt-key+ "documents.ingest.~a"
  "Format string for ingest routing keys by dtype. Example: (format nil +ingest-fmt-key+ \"target\")
=> \"documents.ingest.target\"")


(defparameter +new-documents-key+ "documents.new.#"
  "Wildcard routing key for post-ingest 'new document' messages (after CouchDB insert/_rev enrichment).")


(defparameter +new-documents-fmt-key+ "documents.new.~a"
  "Format string for documents.new routing keys by dtype. Example: \"documents.new.url\".")


(defparameter +updated-documents-key+ "documents.updated.#"
  "Wildcard routing key for document update messages emitted by actors/services.")


(defparameter +updated-documents-fmt-key+ "documents.updated.~a"
  "Format string for documents.updated routing keys by dtype. Example: \"documents.updated.host\".")


(defparameter +targets-key+ "documents.ingest.target.#"
  "Wildcard routing key for ingest-phase target messages (initial targets coming into the system).")


(defparameter +new-targets-key+ "documents.new.target.#"
  "Wildcard routing key for post-ingest target messages (targets after CouchDB insert/_rev enrichment).")


(defparameter +targets-fmt-key+ "documents.ingest.target.#"
  "Alias for +targets-key+. Name is historical; value is intentionally the same wildcard.");;; ----------------------------------------------------------------------
;;; rabbit helpers

(defmacro with-rabbit-recv ((queue-name exchange-name exchange-type routing-key
                             &key (port star:*rabbit-port*)
                               (host star:*rabbit-address*)
                               (username star:*rabbit-user*)
                               (password star:*rabbit-password*)
                               (vhost "/")
                               (durable nil)
                               (exclusive nil)
                               (auto-delete nil))
                            &body body)
  "Open a RabbitMQ connection and continuously consume messages from a bound queue.

Binds/declares:
- Exchange: EXCHANGE-NAME of type EXCHANGE-TYPE (typically +documents-exchange+ / +documents-exchange-type+).
- Queue: QUEUE-NAME with durability/exclusive/auto-delete options.
- Binding: queue -> exchange with ROUTING-KEY (wildcards allowed for topic exchanges).

Consume loop:
- Starts a consumer via BASIC-CONSUME, then repeatedly calls CONSUME-MESSAGE.
- For each message:
  - Evaluates BODY with `msg` bound to the message payload (via cl-rabbit:envelope/message).
  - On success: ACKs by delivery-tag.
  - On error: logs, NACKs, and requeues the message (requeue t).

Notes:
- This macro owns the connection lifecycle; it does not return until the loop exits.
- Authentication uses SASL PLAIN when USERNAME and PASSWORD are non-nil.
- Uses channel 1 (single channel)."
  `(cl-rabbit:with-connection (conn)
     (log:info "Creating RabbitMQ connection to ~a:~a" ,host ,port)
     (let ((socket (cl-rabbit:tcp-socket-new conn)))
       (log:debug "Opening socket to ~a:~a" ,host ,port)
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (log:debug "Authenticating with user: ~a vhost: ~a" ,username ,vhost)
         (cl-rabbit:login-sasl-plain conn ,vhost ,username ,password))
       (log:info "RabbitMQ connection established")
       (cl-rabbit:with-channel (conn 1)
         (log:info "Declaring exchange: ~a type: ~a" ,exchange-name ,exchange-type)
         (cl-rabbit:exchange-declare conn 1 ,exchange-name ,exchange-type)

         (log:info "Declaring queue: ~a (durable: ~a, exclusive: ~a, auto-delete: ~a)"
                   ,queue-name ,durable ,exclusive ,auto-delete)
         (cl-rabbit:queue-declare conn 1
                                  :queue ,queue-name
                                  :durable ,durable
                                  :auto-delete ,auto-delete
                                  :exclusive ,exclusive)

         (log:info "Binding queue ~a to exchange ~a with routing-key: ~a"
                   ,queue-name ,exchange-name ,routing-key)
         (cl-rabbit:queue-bind conn 1
                               :queue ,queue-name
                               :exchange ,exchange-name
                               :routing-key ,routing-key)

         (log:info "Starting consumer on queue: ~a" ,queue-name)
         (cl-rabbit:basic-consume conn 1 ,queue-name)

         (loop
           for result = (cl-rabbit:consume-message conn)
           for msg = (cl-rabbit:envelope/message result)
           do (handler-case
                  (progn
                    (log:debug "Received message with delivery-tag: ~a"
                               (cl-rabbit:envelope/delivery-tag result))
                    ,@body
                    (log:debug "Acknowledging message with delivery-tag: ~a"
                               (cl-rabbit:envelope/delivery-tag result))
                    (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag result)))
                (error (e)
                  (log:error "Error processing message with delivery-tag ~a: ~a"
                             (cl-rabbit:envelope/delivery-tag result) e)
                  (cl-rabbit:basic-nack conn 1 (cl-rabbit:envelope/delivery-tag result) :requeue t)
                  (log:warn "Message with delivery-tag ~a requeued"
                            (cl-rabbit:envelope/delivery-tag result)))))))))

(defun emit-document (exchange routing-key body &key (properties nil)
                                                  (immediate nil)
                                                  (mandatory nil)
                                                  (port star:*rabbit-port*)
                                                  (host star:*rabbit-address*)
                                                  (username star:*rabbit-user*)
                                                  (password star:*rabbit-password*)
                                                  (vhost "/"))
  "Publish a document message to RabbitMQ.

Arguments:
- EXCHANGE: exchange name (typically +documents-exchange+).
- ROUTING-KEY: topic routing key (e.g. \"documents.ingest.target\", \"documents.new.url\").
- BODY: message payload string/bytes (commonly a JSON string).

Keyword args:
- PROPERTIES: AMQP message properties alist/plist as expected by cl-rabbit.
- MANDATORY: if true, broker returns unroutable messages (requires return handler on the channel).
- IMMEDIATE: if true, broker requires immediate delivery to a consumer (often unsupported/ignored by brokers).
- HOST/PORT/USERNAME/PASSWORD/VHOST: connection parameters.

Behavior:
- Opens a fresh connection and channel per call, publishes once, then closes.
- Uses SASL PLAIN auth when USERNAME and PASSWORD are non-nil.
- Logs publish intent and basic payload metadata (body length)."
  (log:info "Publishing document to exchange: ~a routing-key: ~a" exchange routing-key)
  (log:debug "Publish properties: ~a body-length: ~a" properties (length body))
  (cl-rabbit:with-connection (conn)
    (let ((socket (cl-rabbit:tcp-socket-new conn)))
      (log:debug "Opening socket for publish to ~a:~a" host port)
      (cl-rabbit:socket-open socket host port)
      (when (and username password)
        (log:debug "Authenticating publish connection user: ~a vhost: ~a" username vhost)
        (cl-rabbit:login-sasl-plain conn vhost username password))
      (cl-rabbit:with-channel (conn 1)
        (cl-rabbit:basic-publish conn 1
                                 :routing-key routing-key
                                 :exchange exchange
                                 :mandatory mandatory
                                 :immediate immediate
                                 :properties properties
                                 :body body)
        (log:info "Document published successfully to ~a with routing-key: ~a"
                  exchange routing-key)))))

(defun message->string (msg &key (encoding :utf-8))
  "Take a RabbitMQ message and return the body as a string."
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

(defun message->object (msg)
  "TODO: Parse a RabbitMQ message into an object based on message properties."
  (declare (ignore msg))
  nil)

;;; ----------------------------------------------------------------------
;;; couch helpers

(defun normalize-id (document)
  "Ensure document has a usable _id. Returns the (possibly updated) document."
  (assert document () "normalize-id: document is NIL")
  (let ((id (jsown:val-safe document "_id")))
    (cond
      ((and (stringp id) (not (string= id "")))
       document)
      (t
       (jsown:extend-js document ("_id" (cms-ulid:ulid)))))))

(defun insert-document (client document &key (database star:*couchdb-default-database*))
  "Normalize _id, insert to CouchDB, then return document with _rev set."
  (assert client () "insert-document: client is NIL")
  (assert document () "insert-document: document is NIL")

  (setf document (normalize-id document))

  (log:info "Creating document in database: ~a" database)
  (let* ((resp (cl-couch:create-document client database (jsown:to-json document)))
         (obj  (jsown:parse resp))
         (rev  (or (jsown:val-safe obj "rev")
                   (jsown:val-safe obj "_rev")))
         (id   (or (jsown:val-safe obj "id")
                   (jsown:val-safe obj "_id"))))
    (when id
      (setf document (jsown:extend-js document ("_id" id))))
    (when rev
      (setf document (jsown:extend-js document ("_rev" rev))))
    (log:info "Document created successfully (_id=~a _rev=~a)" id rev)
    document))

(defun transient-p (message)
  (let ((result (jsown:val-safe (jsown:parse (car message)) "transient")))
    (log:debug "Message transient check: ~a" result)
    result))

(defun insertp (message)
  (let ((result (null (transient-p message))))
    (log:debug "Message insert check: ~a" result)
    result))

(defun target-p (message)
  "Test if message dtype is 'target'."
  (let* ((body (jsown:parse (car message)))
         (dtype (jsown:val-safe body "dtype"))
         (result (string= dtype "target")))
    (log:debug "Message target-p check: dtype=~a result=~a" dtype result)
    result))

;;; ----------------------------------------------------------------------
;;; handlers

(defun handle-new-document (self message)
  "Handle an ingested document: insert to CouchDB, then republish as documents.new.<dtype>."
  (log:debug "handle-new-document called with message-key: ~a" (cdr message))
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (doc        (jsown:parse (car message)))
         (msg-key    (cdr message))
         (dtype       (jsown:val-safe doc "dtype")))
    (assert (and dtype (stringp dtype) (not (string= dtype ""))) ()
            "handle-new-document: missing/invalid dtype (msg-key=~S)" msg-key)

    (setf doc (normalize-id doc))

    (handler-case
        (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
          (setf doc (insert-document client doc))
          (let ((routing-key (format nil "documents.new.~a" dtype)))
            (star.actors:publish star.actors:*producer-agent*
                                 :body (jsown:to-json doc)
                                 :routing-key routing-key
                                 :properties (list (cons :type dtype))))
          (cl-rabbit:basic-ack connection 1 msg-key))

      (dex:http-request-bad-request (e)
        (log:error "Bad request creating document (msg-key=~a): ~a"
                   msg-key (dexador.error:response-body e))
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t))

      (dex:http-request-conflict (e)
        (log:warn "Document conflict (msg-key=~a): ~a" msg-key e)
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t))

      (error (e)
        (log:error "Unexpected error creating document (msg-key=~a): ~a" msg-key e)
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t)))))

(defun handle-new-target (self message)
  "Handle target doc: if transient => route directly; else insert to CouchDB then route."
  (log:debug "handle-new-target called with message-key: ~a" (cdr message))
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (body       (jsown:parse (car message)))
         (msg-key    (cdr message)))
    (setf body (normalize-id body))

    (let* ((dtype     (jsown:val-safe body "dtype"))
           (transient (jsown:val-safe body "transient"))
           (id        (jsown:val-safe body "_id")))
      (when (or (null dtype) (not (stringp dtype)) (string= dtype ""))
        (setf body (jsown:extend-js body ("dtype" "target"))))

      (cond
        (transient
         (log:info "Transient target => skipping DB (_id=~a actor=~a)"
                   id (jsown:val-safe body "actor"))
         (tell star.actors:*targets* (cons 1 body))
         (cl-rabbit:basic-ack connection 1 msg-key))

        (t
         (handler-case
             (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
               (setf body (insert-document client body))
               (log:info "Target inserted => routing (_id=~a _rev=~a actor=~a)"
                         (jsown:val-safe body "_id")
                         (jsown:val-safe body "_rev")
                         (jsown:val-safe body "actor"))
               (tell star.actors:*targets* (cons 1 body))
               (cl-rabbit:basic-ack connection 1 msg-key))

           (dex:http-request-conflict (e)
             ;; If it exists, fetch _rev and still route.
             (log:warn "Target conflict => fetching _rev and routing anyway (_id=~a): ~a" id e)
             (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
               (let* ((existing (cl-couch:get-document* client star:*couchdb-default-database* id))
                      (rev      (jsown:val-safe existing "_rev")))
                 (when rev
                   (setf body (jsown:extend-js body ("_rev" rev)))))
               (tell star.actors:*targets* (cons 1 body))
               (cl-rabbit:basic-ack connection 1 msg-key)))

           (dex:http-request-bad-request (e)
             (log:error "Bad request creating target (msg-key=~a _id=~a): ~a"
                        msg-key id (dexador.error:response-body e))
             (cl-rabbit:basic-nack connection 1 msg-key :requeue t))

           (error (e)
             (log:error "Unexpected error creating target (msg-key=~a _id=~a): ~a"
                        msg-key id e)
             (cl-rabbit:basic-nack connection 1 msg-key :requeue t))))))))

(defun handle-update (self message)
  "Persist document updates to CouchDB. Requires _id, _rev, dtype."
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (body       (jsown:parse (car message)))
         (msg-key    (cdr message))
         (id         (jsown:val-safe body "_id"))
         (_rev       (jsown:val-safe body "_rev"))
         (dtype      (jsown:val-safe body "dtype")))
    (assert (and id _rev dtype) ()
            "handle-update: missing required fields:~@[ _id~]~@[ _rev~]~@[ dtype~] (msg-key=~S)"
            (null id) (null _rev) (null dtype) msg-key)

    (handler-case
        (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
          (let* ((resp (cl-couch:update-document* client star:*couchdb-default-database* body _rev))
                 (new-rev (jsown:val-safe resp "rev")))
            (when (and new-rev (stringp new-rev) (not (string= new-rev "")))
              (setf body (jsown:extend-js body ("_rev" new-rev))))
            (log:info "Update persisted (dtype=~a _id=~a _rev=~a)"
                      dtype id (jsown:val-safe body "_rev"))
            (cl-rabbit:basic-ack connection 1 msg-key)))

      (dex:http-request-conflict (e)
        (log:warn "Update conflict (dtype=~a _id=~a): ~a" dtype id e)
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t))

      (dex:http-request-bad-request (e)
        (log:error "Bad request updating doc (dtype=~a _id=~a): ~a"
                   dtype id (dexador.error:response-body e))
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t))

      (error (e)
        (log:error "Unexpected error updating doc (dtype=~a _id=~a): ~a" dtype id e)
        (cl-rabbit:basic-nack connection 1 msg-key :requeue t)))))

;;; ----------------------------------------------------------------------
;;; consumer wiring

(defun start-consumers ()
  (log:info "Starting Consumers.")

  (log:info "Creating ingest consumer - workers: ~a queue: ~a exchange: documents routing-key: ~a"
            star:*ingest-workers* +ingest-queue+ +ingest-key+)

  (let ((document-consumers
          (create-rabbit-consumer :name "ingest"
                                  :n star:*ingest-workers*
                                  :queue-name +ingest-queue+
                                  :exchange-name +documents-exchange+
                                  :routing-key +ingest-key+
                                  :username star:*rabbit-user*
                                  :password star:*rabbit-password*
                                  :host star:*rabbit-address*
                                  :port star:*rabbit-port*
                                  :handler-fn #'handle-new-document
                                  :test-fn #'insertp))

        (updates-consumers
          (create-rabbit-consumer :name "documents-updates"
                                  :n star:*ingest-workers*
                                  :queue-name +updates-queue+
                                  :exchange-name +documents-exchange+
                                  :routing-key +updated-documents-key+
                                  :username star:*rabbit-user*
                                  :password star:*rabbit-password*
                                  :host star:*rabbit-address*
                                  :port star:*rabbit-port*
                                  :handler-fn #'handle-update
                                  :test-fn #'insertp))

        (target-consumers
          (create-rabbit-consumer :name "documents-targets"
                                  :n star:*ingest-workers*
                                  :queue-name +ingest-targets-queue+
                                  :exchange-name +documents-exchange+
                                  :routing-key +targets-key+
                                  :username star:*rabbit-user*
                                  :password star:*rabbit-password*
                                  :host star:*rabbit-address*
                                  :port star:*rabbit-port*
                                  :handler-fn #'handle-new-target
                                  :test-fn #'target-p)))

    (log:info "Starting ingest consumers")
    (start-consumer document-consumers)

    (log:info "Starting updates consumers")
    (start-consumer updates-consumers)

    (log:info "Starting target consumers")
    (start-consumer target-consumers)

    (log:info "All consumers started successfully")))
