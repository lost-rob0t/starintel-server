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
         (cl-rabbit:queue-declare conn 1 :queue ,queue-name :durable ,auto-delete ,auto-delete :exclusive ,exclusive)
         (log:info "Binding queue ~a to exchange ~a with routing-key: ~a"
                   ,queue-name ,exchange-name ,routing-key)
         (cl-rabbit:queue-bind conn 1 :queue ,queue-name :exchange ,exchange-name :routing-key ,routing-key)

         (log:info "Starting consumer on queue: ~a" ,queue-name)
         (cl-rabbit:basic-consume conn 1 ,queue-name)
         (loop
           for result = (cl-rabbit:consume-message conn)
           for msg = (cl-rabbit:envelope/message result)
           do (handler-case (progn
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
        (cl-rabbit:basic-publish conn 1 :routing-key routing-key :exchange exchange :mandatory mandatory :immediate immediate :properties properties :body body)
        (log:info "Document published successfully to ~a with routing-key: ~a" exchange routing-key)))))

(defun message->string (msg &key (encoding :utf-8))
  "take a rabbitmq message and return the boddy as a string"
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

                                        ;TODO
(defun message->object (msg)
  "Tale a rabbbitmq message and return a object. The object that will be returned depends on the message property 'dtype`.")




(defun normalize-id (document)
  "Ensure the ID is set, if not create a ULID id for it."
  (let ((id (jsown:val-safe document "_id")))
    (when (or (null id)
              (string= id "")
              (not (stringp id)))
      (jsown:extend-js document
        ("_id" (cms-ulid:ulid))))))


(defun insert-document (client document)
  "Normalizes the document then inserts it."
  (normalize-id document)
  (log:info "Creating document in database: ~a" star:*couchdb-default-database*)
  (let ((response (jsown:parse (couch:create-document client star:*couchdb-default-database* (jsown:to-json document-json)))))
    (jsown:extend-js document
      ("_rev" (jsown:val-safe response "_rev"))))
  
  
  (log:info "Document created successfully"))



(defun handle-new-document (self message)
  (log:debug "handle-new-document called with message-key: ~a" (cdr message))
  (let* ((response nil)
         (connection (rabbit-stream-connection (consumer-stream self)))
         (document-json (jsown:parse (car message)))
         (msg-key (cdr message))
         (dtype (jsown:val-safe document-json "dtype"))
         (routing-key (format nil "documents.new.~a" dtype)))
    (setf document-json (normalize-id document-json))
    (log:debug "Processing document with msg-key: ~a" msg-key)
    (handler-case (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
                    (setf response (insert-document client document-json))
                    (star.actors:publish star.actors:*producer-agent* :body (jsown:to-json (jsown:extend-js document-json
                                                                                             ("_rev" (jsown:val response "_rev")))) :routing-key routing-key :properties (list (cons :type dtype)))
                    
                    (log:debug "Acknowledging message with key: ~a" msg-key)
                    (cl-rabbit:basic-ack connection 1 msg-key))
      
      (dex:http-request-bad-request (e)
        (log:error "Bad request creating document (~a): ~a"
                   msg-key (dexador.error:response-body e)))
      (dex:http-request-conflict (e)
        (log:warn "Document conflict (~a): ~a" msg-key e))
      (error (e)
        (log:error "Unexpected error creating document (~a): ~a" msg-key e)))))




(defun transient-p (message)
  (let ((result (jsown:val-safe (jsown:parse (car message)) "transient")))
    (log:debug "Message transient check: ~a" result)
    result))

(defun insertp (message)
  (let ((result (null (transient-p message))))
    (log:debug "Message insert check: ~a" result)
    result))

(defun target-p (message)
  "Test if message dtype is 'target'"
  (let* ((body (jsown:parse (car message)))
         (dtype (jsown:val-safe body "dtype"))
         (result (string= dtype "target")))
    (log:debug "Message target-p check: dtype=~a result=~a" dtype result)
    result))


(defun handle-new-target (self message)
  "Handle target doc - persist to CouchDB if not transient, then route to *targets* actor."
  (log:debug "handle-new-target called with message-key: ~a" (cdr message))
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (body (jsown:parse (car message)))
         (msg-key (cdr message))
         (id (jsown:val-safe body "_id"))
         (dtype (jsown:val-safe body "dtype"))
         (transient (jsown:val-safe body "transient")))
    ;; ----------------------------------------------------------------------
    ;; normalize doc
    (normalize-id)
    (when (or (null dtype) (not (stringp dtype)) (string= dtype ""))
      (setf body (jsown:extend-js body
                   ("dtype" "target"))))

    ;; ----------------------------------------------------------------------
    ;; If transient, skip db write and route directly to actor
    (log:info "Transient target, skipping database - routing to *targets* actor (_id=~a actor=~a)"
              (jsown:val-safe body "_id")
              (jsown:val-safe body "actor"))

    (tell star.actors:*targets* (cons 1 body))
    (cl-rabbit:basic-ack connection 1 msg-key)
    (log:debug "Transient target sent to *targets* actor, message acknowledged")))

;; Non-transient: write to db then send on success
;; (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
;;   (handler-case
;;       (progn
;;         (log:info "Creating target in database: ~a (_id=~a actor=~a)"
;;                   star:*couchdb-default-database*
;;                   (jsown:val-safe body "_id")
;;                   (jsown:val-safe body "actor"))

;;         (couch:create-document client
;;                                star:*couchdb-default-database*
;;                                (jsown:to-json body))

;;         (log:info "Target created, routing to *targets* actor - actor: ~a"
;;                   (jsown:val-safe body "actor"))

;;         (log:debug "About to tell *targets* actor. *targets*=~a body=~a"
;;                    star.actors:*targets* (jsown:to-json body))
;;         (tell star.actors:*targets* (cons 1 body))
;;         (log:debug "Tell completed")

;;         (log:debug "Target sent to *targets* actor, acknowledging message")
;;         (cl-rabbit:basic-ack connection 1 msg-key)
;;         (log:debug "Message with key ~a acknowledged" msg-key))

;;     (dex:http-request-conflict (e)
;;       (log:warn "Target conflict (already exists). msg-key=~a _id=~a err=~a"
;;                 msg-key (jsown:val-safe body "_id") e)
;;       (log:info "Target already exists, still routing to *targets* actor - actor: ~a"
;;                 (jsown:val-safe body "actor"))
;;       ;; Still route to *targets* actor even if doc exists
;;       (log:debug "About to tell *targets* actor. *targets*=~a body=~a"
;;                  star.actors:*targets* (jsown:to-json body))
;;       (tell star.actors:*targets* (cons 1 body))
;;       (log:debug "Tell completed, acknowledging message")
;;       (cl-rabbit:basic-ack connection 1 msg-key)
;;       (log:debug "Conflict handled: target sent to *targets* actor, message acknowledged"))

;;     (dex:http-request-bad-request (e)
;;       (log:error "Bad request creating target. msg-key=~a _id=~a err=~a doc=~a"
;;                  msg-key (jsown:val-safe body "_id") e (jsown:to-json body))
;;       ;; do NOT ack; let it retry / dead-letter based on your broker policy
;;       nil)

;;     (error (e)
;;       (log:error "Unexpected error creating target. msg-key=~a _id=~a err=~a doc=~a"
;;                  msg-key (jsown:val-safe body "_id") e (jsown:to-json body))
;;       ;; do NOT ack
;;       nil)))

(defun handle-update (self message)
  "Handle document updates and publish them to the resulting documents.updates.<dtype>"
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (body (jsown:parse (car message)))
         (msg-key (cdr message))
         (_rev (jsown:val-safe body "_rev"))
         (dtype (jsown:val-safe body "dtype")))
    (assert (and _rev dtype) ()
            "handle-update: missing required fields:~@[ _rev~]~@[ dtype~] (routing-key=~S)"
            (null _rev) (null dtype) msg-key)
    (handler-case (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*))
      (setf response (cl-couch:update-document client  (jsown:to-json body)))
      (star.actors:publish star.actors:*producer-agent* :body (jsown:to-json (jsown:extend-js body ("_rev" (jsown:val response "_rev")))) :routing-key routing-key :properties (list (cons :type dtype))) 
      
      (dex:http-request-bad-request (e)
        (log:error "Bad request creating document (~a): ~a"
                   msg-key (dexador.error:response-body e)))
      (dex:http-request-conflict (e)
        (log:warn "Document conflict (~a): ~a" msg-key e))
      (error (e)
        (log:error "Unexpected error creating document (~a): ~a" msg-key e)))))



(defun start-consumers ()
  (log:info "Starting Consumers.")
  (log:info "Creating document consumer - workers: ~a queue: ingest exchange: documents routing-key: ~a"
            star:*ingest-workers* +ingest-key+)
  (let ((document-consumers (create-rabbit-consumer :name "documents"
                                                    :n star:*ingest-workers*
                                                    :queue-name +ingest-queue+
                                                    :exchange-name "documents"
                                                    :routing-key +ingest-key+
                                                    :username star:*rabbit-user*
                                                    :password star:*rabbit-password*
                                                    :host star:*rabbit-address*
                                                    :port star:*rabbit-port*
                                                    :handler-fn #'handle-new-document
                                                    :test-fn #'insertp))
        (updates-consumers (create-rabbit-consumer :name "documents"
                                                   :n star:*ingest-workers*
                                                   :queue-name +ingest-updates-queue+
                                                   :exchange-name "documents"
                                                   :routing-key +updates-key+
                                                   :username star:*rabbit-user*
                                                   :password star:*rabbit-password*
                                                   :host star:*rabbit-address*
                                                   :port star:*rabbit-port*
                                                   :handler-fn #'handle-new-target
                                                   :test-fn #'insertp))

        
        (target-consumers (create-rabbit-consumer :name "documents"
                                                  :n star:*ingest-workers*
                                                  :queue-name +ingest-targets-queue+
                                                  :exchange-name "documents"
                                                  :routing-key +targets-key+
                                                  :username star:*rabbit-user*
                                                  :password star:*rabbit-password*
                                                  :host star:*rabbit-address*
                                                  :port star:*rabbit-port*
                                                  :handler-fn #'handle-new-target
                                                  :test-fn #'target-p)))
    (log:info "Starting document consumers")
    (start-consumer document-consumers)
    (log:info "Document consumers started")
    (log:info "Creating target consumer - workers: ~a queue: ingest-targets routing-key: ~a"
              star:*ingest-workers* +targets-key+)
    (log:info "Starting target consumers")
    (start-consumer target-consumers)
    (log:info "Target consumers started")
    (log:info "All consumers started successfully")))
