(in-package :starintel-gserver)
(cl-rabbit:with-connection (conn)
  (with-connection (conn)
    (let ((socket (tcp-socket-new conn)))
      (socket-open socket "localhost" 5672)
      (login-sasl-plain conn "/" "guest" "guest")
      (with-channel (conn 1)
        (exchange-declare conn 1 "test-ex" "topic")
        (let ((queue-name "foo"))
          (queue-declare conn 1 :queue queue-name)
          (queue-bind conn 1 :queue queue-name :exchange "test-ex" :routing-key "xx")
          (basic-consume conn 1 queue-name)
          (let* ((result (consume-message conn))
                 (message (envelope/message result)))
            (format t "Got message: ~s~%content: ~s~%props: ~s"
                    result (babel:octets-to-string (message/body message) :encoding :utf-8)
                    (message/properties message))
            (cl-rabbit:basic-ack conn 1 (envelope/delivery-tag result))))))))

(defun message->string (msg &key (encoding :utf-8))
  (babel:octets-to-string (message/body msg) :encoding encoding)
  "take a rabbitmq message and return a string")

(defun message->object (msg)
  "Tale a rabbbitmq message and return a object. The object that will be returned depends on the message property 'dtype`.")

(let ((out *standard-output*))
  (bt:make-thread
   (lambda ())
   (with-rabbit-recv ("test" "test" "topic" "new.document.*")
     (loop
       for msg = (message->string (basic-consume conn 1 "test"))
       if (> (length msg) 0)

         do (format out "~a" msg)))))
