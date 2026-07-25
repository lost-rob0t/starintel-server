(in-package :star.rabbit)

(defvar +injest-queue+ "injest")
(defvar +updates-queue+ "documents-updates")
(defvar +injest-key+ "documents.new.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.*")

(defmacro with-rabbit-recv ((queue-name exchange-name exchange-type routing-key &key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
  `(cl-rabbit:with-connection (conn)
     (let ((socket (cl-rabbit:tcp-socket-new conn)))
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (cl-rabbit:login-sasl-plain conn ,vhost ,username ,password))
       (cl-rabbit:with-channel (conn 1)
         (cl-rabbit:exchange-declare conn 1 ,exchange-name ,exchange-type)
         (cl-rabbit:queue-declare conn 1 :queue ,queue-name :durable ,auto-delete ,auto-delete :exclusive ,exclusive)
         (cl-rabbit:queue-bind conn 1 :queue ,queue-name :exchange ,exchange-name :routing-key ,routing-key)
         (cl-rabbit:basic-consume conn 1 ,queue-name)
         (loop
           for result = (cl-rabbit:consume-message conn)
           for msg = (cl-rabbit:envelope/message result)
           do (handler-case
                  (progn
                    ,@body
                    (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag result)))
                (error (error)
                  (log:error "Rabbit document rejected: ~a" error)
                  (cl-rabbit:basic-nack conn 1 (cl-rabbit:envelope/delivery-tag result) :requeue nil))))))))

(defun emit-document (exchange routing-key body &key (properties nil)
                                                   (immediate nil)
                                                   (mandatory nil)
                                                   (port star:*rabbit-port*)
                                                   (host star:*rabbit-address*)
                                                   (username star:*rabbit-user*)
                                                   (password star:*rabbit-password*)
                                                   (vhost "/"))
  (let ((canonical-body (star.documents.v09:v09-document-json body)))
    (cl-rabbit:with-connection (conn)
      (let ((socket (cl-rabbit:tcp-socket-new conn)))
        (cl-rabbit:socket-open socket host port)
        (when (and username password)
          (cl-rabbit:login-sasl-plain conn vhost username password))
        (cl-rabbit:with-channel (conn 1)
          (cl-rabbit:basic-publish conn 1
                                   :routing-key routing-key
                                   :exchange exchange
                                   :mandatory mandatory
                                   :immediate immediate
                                   :properties properties
                                   :body canonical-body))))))

(defun message->string (msg &key (encoding :utf-8))
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

(defun message->object (msg)
  (star.documents.v09:ensure-v09-document (message->string msg)))

(defun handle-new-document (msg)
  (let* ((properties (cl-rabbit:message/properties msg))
         (route-type (cdr (assoc :type properties :test #'equal)))
         (body (star.documents.v09:ensure-v09-document
                (message->string msg)
                :route-dtype route-type)))
    (cons (star.documents.v09:document-dtype body) body)))

(defun insert (client database document)
  (format nil "~a~%"
          (couch:create-document client database
                                 (star.documents.v09:v09-document-json document))))

(defun handle-document (self message)
  (let* ((connection (rabbit-stream-connection (consumer-stream self)))
         (document (star.documents.v09:ensure-v09-document (car message)))
         (msg-key (cdr message)))
    (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
      (handler-case
          (couch:create-document client
                                 star:*couchdb-default-database*
                                 (jsown:to-json document))
        (dex:http-request-conflict (error)
          (declare (ignore error)))))
    (cl-rabbit:basic-ack connection 1 msg-key)))

(defun transient-p (message)
  (star.documents.v09:document-transient-p
   (star.documents.v09:ensure-v09-document (car message))))

(defun insertp (message)
  (not (transient-p message)))

(defun handle-target (self message)
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (body (star.documents.v09:ensure-v09-document (car message) :route-dtype "target"))
        (msg-key (cdr message)))
    (tell star.actors:*targets* (cons 1 body))
    (cl-rabbit:basic-ack connection 1 msg-key)))

(defun start-consumers ()
  (log:info "Starting v0.9 document consumers.")
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
    (start-consumer document-consumers)
    (start-consumer target-consumers)))
