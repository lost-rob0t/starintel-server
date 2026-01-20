(in-package :star.rabbit)

(defvar +injest-queue+ "injest")
(defvar +updates-queue+ "documents-updates")
(defvar +injest-key+ "documents.new.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.#")

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




(defun insert (client database document)
  (log:debug "Inserting document into database: ~a" database)
  (format nil "~a~%" (couch:create-document client database document)))
;; (dex:http-request-conflict (e) (log:warn e))
;; (dex:http-request-unauthorized (e) (log:error e))

(defun handle-new-document (self message)
  (log:debug "handle-new-document called with message-key: ~a" (cdr message))
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (document-json (jsown:parse (car message)))
         (msg-key (cdr message))
         (id (jsown:val-safe document-json "_id")))
    ;; ensure a valid _id
    (when (or (null id)
              (string= id "")
              (not (stringp id)))
      (setf document-json
            (jsown:extend-js document-json
                             ("_id" (cms-ulid:ulid)))))
    (log:debug "Processing document with msg-key: ~a" msg-key)
    (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
      (handler-case
          (progn
            (log:info "Creating document in database: ~a" star:*couchdb-default-database*)
            (couch:create-document client star:*couchdb-default-database* (jsown:to-json document-json))
            (log:info "Document created successfully"))
        (dex:http-request-bad-request (e)
          (log:error "Bad request creating document (~a): ~a"
                     msg-key (dexador.error:response-body e)))
        (dex:http-request-conflict (e)
          (log:warn "Document conflict (~a): ~a" msg-key e))
        (error (e)
          (log:error "Unexpected error creating document (~a): ~a" msg-key e))))
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
    (when (or (null id) (not (stringp id)) (string= id ""))
      (setf body (jsown:extend-js body
                                  ("_id" (cms-ulid:ulid)))))
    (when (or (null dtype) (not (stringp dtype)) (string= dtype ""))
      (setf body (jsown:extend-js body
                                  ("dtype" "target"))))

    ;; ----------------------------------------------------------------------
    ;; If transient, skip db write and route directly to actor
    (if transient
        (progn
          (log:info "Transient target, skipping database - routing to *targets* actor (_id=~a actor=~a)"
                    (jsown:val-safe body "_id")
                    (jsown:val-safe body "actor"))
          (tell star.actors:*targets* (cons 1 body))
          (cl-rabbit:basic-ack connection 1 msg-key)
          (log:debug "Transient target sent to *targets* actor, message acknowledged")))))

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
                                                    :handler-fn #'handle-new-document
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
                                                  :handler-fn #'handle-new-target
                                                  :test-fn #'target-p)))
    (log:info "Starting document consumers")
    (start-consumer document-consumers)
    (log:info "Document consumers started")
    (log:info "Creating target consumer - workers: ~a queue: injest-targets routing-key: ~a"
              star:*injest-workers* +targets-key+)
    (log:info "Starting target consumers")
    (start-consumer target-consumers)
    (log:info "Target consumers started")
    (log:info "All consumers started successfully")))
