(in-package :starintel-gserver)
(in-package :sento-user)
(defmacro with-rabbit-recv ((queue-name exchange-name exchange-type routing-key &key (port *rabbit-port*) (host *rabbit-address*) (username *rabbit-user*) (password *rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
  `(cl-rabbit:with-connection (conn)
     (let ((socket (cl-rabbit:tcp-socket-new conn)))
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (cl-rabbit:login-sasl-plain conn ,vhost ,username ,password))
       (cl-rabbit:with-channel (conn 1)
         (cl-rabbit:exchange-declare conn 1 ,exchange-name ,exchange-type)

         (cl-rabbit:queue-declare conn 1 :queue ,queue-name :durable ,durable :auto-delete ,auto-delete :exclusive ,exclusive)
         (cl-rabbit:queue-bind conn 1 :queue ,queue-name :exchange ,exchange-name :routing-key ,routing-key)
         (cl-rabbit:basic-consume conn 1 ,queue-name)
         ,@body))))

(defmacro with-rabbit-send ((queue-name exchange-name exchange-type routing-key &key (port *rabbit-port*) (host *rabbit-address*) (username *rabbit-user*) (password *rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
  `(cl-rabbit:with-connection (conn)
     (let ((socket (cl-rabbit:tcp-socket-new conn)))
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (cl-rabbit:login-sasl-plain conn ,vhost ,username ,password))
       (cl-rabbit:with-channel (conn 1)

         ,@body))))

(defun message->string (msg &key (encoding :utf-8))
  "take a rabbitmq message and return the boddy as a string"
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

(defun message->object (msg)
  "Tale a rabbbitmq message and return a object. The object that will be returned depends on the message property 'dtype`.")



(defun handle-new-document (msg)
  "Handles any new incomming documents and sends it to the apropiate actors. "
  (let ((body (message->string msg))
        (dtype (assoc "dtype" (assoc :headers (cl-rabbit:message/properties msg)))))
    (cons dtype body)))


(bt:make-thread
 (lambda ()
   (with-rabbit-recv ("new-documents" "documents" "topic" "document.new.*")
     (loop
       for msg = (cl-rabbit:consume-message conn)
       for data = (handle-new-document msg)
       do (ask *couchdb-insert* data)
       do (publish *sys* (new-event :topic (str:title-case (car data) ) :data (cdr data))))))

 :name "*new-documents*")


(defun make-doc ()

  (with-output-to-string (str) (cl-json:encode-json (starintel:set-meta (make-instance  'starintel:person :id (ulid:ulid) :lname "doe" :fname "john") "starintel") str)))

(defun test-send ()
  (cl-rabbit:with-connection (conn)
    (let ((socket (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open socket "localhost" 5672)
      (cl-rabbit:login-sasl-plain conn "/" "guest" "guest")
      (cl-rabbit:with-channel (conn 1)
        (cl-rabbit:basic-publish conn 1
                                 :exchange "documents"
                                 :routing-key "new.document.Person"
                                 :body (make-doc)
                                 :properties '((:headers . (("dtype"  . "Person")))))))))
