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
           do (handler-case (progn
                              ,@body
                              (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag result)))
                (error (e) (cl-rabbit:basic-nack conn 1 (cl-rabbit:envelope/delivery-tag result) :requeue t))))))))

(defun emit-document (exchange routing-key body &key (properties nil)
                                                  (immediate nil)
                                                  (mandatory nil)
                                                  (port star:*rabbit-port*)
                                                  (host star:*rabbit-address*)
                                                  (username star:*rabbit-user*)
                                                  (password star:*rabbit-password*)
                                                  (vhost "/"))
  (cl-rabbit:with-connection (conn)
    (let ((socket (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open socket host port)
      (when (and username password)
        (cl-rabbit:login-sasl-plain conn vhost username password))
      (cl-rabbit:with-channel (conn 1)
        (cl-rabbit:basic-publish conn 1 :routing-key routing-key :exchange exchange :mandatory mandatory :immediate immediate :properties properties :body body)))))

(defun message->string (msg &key (encoding :utf-8))
  "take a rabbitmq message and return the boddy as a string"
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

                                        ;TODO
(defun message->object (msg)
  "Tale a rabbbitmq message and return a object. The object that will be returned depends on the message property 'dtype`.")

(defun handle-new-document (msg)
  "Handles any new incoming documents and sends it to the appropriate actors."
  (let* ((props (cl-rabbit:message/properties msg))
         (dtype (assoc :type props :test #'equal))
         (body (jsown:parse (message->string msg))))
    (cons (cdr dtype) body)))


(defun insert (client database document)
  (format nil "~a~%" (couch:create-document client database (jsown:to-json* document))))
;; (dex:http-request-conflict (e) (log:warn e))
;; (dex:http-request-unauthorized (e) (log:error e))

(defun handle-document (self message)
  (let (
        (connection (rabbit-stream-connection (consumer-stream self)))
        (document (car message))
        (msg-key (cdr message)))
    (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
      (handler-case (progn (couch:create-document client star:*couchdb-default-database* document))


        (dex:http-request-conflict (e) nil)))

    (cl-rabbit:basic-ack connection 1 msg-key)))


(defun transient-p (message)
  (jsown:val-safe (jsown:parse (car message)) "transient"))

(defun insertp (message)
  (null (transient-p message)))


;; Handle New Document consumers:2 ends here

;; [[file:../source.org::*Handle New Target consumers][Handle New Target consumers:1]]
(defun handle-target (self message)
  "Handles any new incoming documents and sends it to the appropriate actors."
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (body (jsown:parse (car message)))
        (msg-key (cdr message)))
    (tell star.actors:*targets* (cons 1 body))
    (cl-rabbit:basic-ack connection 1 msg-key)))

(defun start-consumers ()
  (log:info "Starting Consumers.")
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
