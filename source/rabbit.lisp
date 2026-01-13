(in-package :star.rabbit)

(defvar +injest-queue+ "injest")
(defvar +updates-queue+ "documents-updates")
(defvar +injest-key+ "documents.new.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.*")

(defmacro with-rabbit-recv ((queue-name exchange-name exchange-type routing-key &key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
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

(defun handle-new-document (msg)
  "Handles any new incoming documents and sends it to the appropriate actors."
  (log:debug "Handling new document")
  (let* ((props (cl-rabbit:message/properties msg))
         (dtype (assoc :type props :test #'equal))
         (body-string (message->string msg)))
    (log:info "New document dtype: ~a" (cdr dtype))
    (cons (cdr dtype) body-string)))


(defun insert (client database document)
  (log:debug "Inserting document into database: ~a" database)
  (format nil "~a~%" (couch:create-document client database document)))
;; (dex:http-request-conflict (e) (log:warn e))
;; (dex:http-request-unauthorized (e) (log:error e))

(defun handle-document (self message)
  (log:debug "handle-document called with message-key: ~a" (cdr message))
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (document-json (car message))
        (msg-key (cdr message)))
    (log:debug "Processing document with msg-key: ~a" msg-key)
    (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
      (handler-case (progn
                      (log:info "Creating document in database: ~a" star:*couchdb-default-database*)
                      (couch:create-document client star:*couchdb-default-database* document-json)
                      (log:info "Document created successfully"))


        (dex:http-request-conflict (e)
          (log:warn "Document conflict detected for msg-key: ~a - ~a" msg-key e)
          nil)))

    (log:debug "Acknowledging message with key: ~a" msg-key)
    (cl-rabbit:basic-ack connection 1 msg-key)))


(defun transient-p (message)
  (let ((result (jsown:val-safe (jsown:parse (car message)) "transient")))
    (log:debug "Message transient check: ~a" result)
    result))

(defun insertp (message)
  (let ((result (null (transient-p message))))
    (log:debug "Message insert check: ~a" result)
    result))



(defun handle-target (self message)
  "Handles any new incoming documents and sends it to the appropriate actors."
  (log:debug "handle-target called with message-key: ~a" (cdr message))
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (body (jsown:parse (car message)))
        (msg-key (cdr message)))
    (log:info "Routing target to *targets* actor - actor: ~a"
              (jsown:val-safe body "actor"))
    (tell star.actors:*targets* (cons 1 body))
    (log:debug "Target sent to *targets* actor, acknowledging message")
    (cl-rabbit:basic-ack connection 1 msg-key)
    (log:debug "Message with key ~a acknowledged" msg-key)))

(defun start-consumers ()
  (log:info "Starting Consumers.")
  (log:info "Creating document consumer - workers: ~a queue: injest exchange: documents routing-key: ~a"
            star:*injest-workers* +injest-key+)
  (let ((document-consumers (create-rabbit-consumer :name "documents"
                                                    :n star:*injest-workers*
                                                    :queue-name "injest"
                                                    :exchange-name "documents"
                                                    :routing-key +injest-key+
                                                    :username star:*rabbit-user*
                                                    :password star:*rabbit-password*
                                                    :host star:*rabbit-address*
                                                    :port star:*rabbit-port*
                                                    :handler-fn #'handle-document
                                                    :test-fn #'insertp))

        (target-consumers (create-rabbit-consumer :name "documents"
                                                  :n star:*injest-workers*
                                                  :queue-name "injest-targets"
                                                  :exchange-name "documents"
                                                  :routing-key +targets-key+
                                                  :username star:*rabbit-user*
                                                  :password star:*rabbit-password*
                                                  :host star:*rabbit-address*
                                                  :port star:*rabbit-port*
                                                  :handler-fn #'handle-target
                                                  :test-fn #'insertp)))
    (log:info "Starting document consumers")
    (start-consumer document-consumers)
    (log:info "Document consumers started")
    (log:info "Creating target consumer - workers: ~a queue: injest-targets routing-key: ~a"
              star:*injest-workers* +targets-key+)
    (log:info "Starting target consumers")
    (start-consumer target-consumers)
    (log:info "Target consumers started")
    (log:info "All consumers started successfully")))
