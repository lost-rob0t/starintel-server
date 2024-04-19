(in-package :star.rabbit)

(defmacro with-rabbit-recv ((queue-name exchange-name exchange-type routing-key &key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
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
         (loop
           for msg = (cl-rabbit:envelope/message (cl-rabbit:consume-message conn))
           do ,@body)))))

(defmacro with-rabbit-send ((queue-name exchange-name exchange-type routing-key &key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*) (vhost "/") (durable nil) (exclusive nil) (auto-delete nil)) &body body)
  `(cl-rabbit:with-connection (conn)
     (let ((socket (cl-rabbit:tcp-socket-new conn)))
       (cl-rabbit:socket-open socket ,host ,port)
       (when (and ,username ,password)
         (cl-rabbit:login-sasl-plain conn ,vhost ,username ,password))
       (cl-rabbit:with-channel (conn 1)

         ,@body))))

(defun emit-document (queue-name exchange routing-key body &key (properties nil)
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

        (cl-rabbit:queue-bind conn 1 :queue queue-name :exchange exchange :routing-key routing-key)
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
         (headers (assoc :HEADERS props :test #'equal))
         (dtype (when headers (cdr (assoc "dtype" (cdr headers) :test #'equal))))
         (body (message->string msg)))
    (cons dtype body)))

(defun start-rabbit-document-thread (&key (port star:*rabbit-port*) (host star:*rabbit-address*) (username star:*rabbit-user*) (password star:*rabbit-password*))
  (loop for i from 0 to 4
        do (bt:make-thread
            (lambda ()
              (with-rabbit-recv ("injest" "documents" "topic" "documents.new.*")
                (let (
                      (data (handle-new-document msg)))
                  (sento-user::ask sento-user::*couchdb-inserts* data))))
            ;; (sento-user::publish sento-user::*sys* (sento-user::new-event :topic (string-downcase (car data)) :data (cdr data)))


            :name "*new-documents*")))

(defun test-make-doc ()

  (with-output-to-string (str) (cl-json:encode-json (starintel:set-meta (make-instance  'starintel:person :id (uuid:make-v4-uuid) :lname "doe" :fname "john") "starintel") str)))

(defun test-send ()
  (cl-rabbit:with-connection (conn)
    (let ((socket (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open socket "localhost" 5672)
      (cl-rabbit:login-sasl-plain conn "/" "guest" "guest")
      (cl-rabbit:with-channel (conn 1)
        (cl-rabbit:basic-publish conn 1
                                 :exchange "documents"
                                 :routing-key "documents.new.Person"
                                 :body (test-make-doc)
                                 :properties '((:headers . (("dtype"  . "Person")))))))))
