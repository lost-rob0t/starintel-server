; Issue #350: tests-only actual-broker RED for retry/quarantine settlement.
(in-package :star-server-tests)

(def-suite retry-settlement-integration-tests
  :description "Real Rabbit retry/quarantine must not lose unroutable replacements")
(in-suite retry-settlement-integration-tests)

(defun retry-proof-consume (connection)
  "Return a real delivery or NIL after a bounded two-second broker wait."
  (handler-case
      (cl-rabbit:consume-message connection :timeout 2000000)
    (cl-rabbit:rabbitmq-library-error (condition)
      (if (eq :amqp-status-timeout
              (cl-rabbit:rabbitmq-library-error/error-code condition))
          nil
          (error condition)))))

(defun call-with-retry-proof (kind)
  "Exercise settlement against an owned, deliberately unbound destination."
  (let* ((nonce (string-downcase (star.ids:ulid)))
         (exchange (format nil "readiness.retry.~a" nonce))
         (queue (format nil "readiness.original.~a" nonce))
         (destination (format nil "readiness.unbound.~a" nonce))
         (body "synthetic retry settlement fixture")
         (connection (cl-rabbit:new-connection))
         (control-open nil)
         (stream (make-instance 'star.consumers:retrying-rabbit-queue-stream
                  :host star:*rabbit-address* :port star:*rabbit-port*
                  :user star:*rabbit-user* :password star:*rabbit-password*
                  :queue-name queue :exchange-name exchange :routing-key "original"
                  :quarantine-exchange destination
                  :quarantine-queue (format nil "unused.~a" nonce))))
    (unwind-protect
         (progn
           (cl-rabbit:socket-open (cl-rabbit:tcp-socket-new connection)
                                  star:*rabbit-address* star:*rabbit-port*)
           (cl-rabbit:login-sasl-plain connection "/"
                                       star:*rabbit-user* star:*rabbit-password*)
           (cl-rabbit:channel-open connection 1)
           (setf control-open t)
           (cl-rabbit:exchange-declare connection 1 exchange "topic" :durable t)
           ;; No queue is bound to DESTINATION. Publishing there must never
           ;; authorize acknowledging the only copy in the original queue.
           (cl-rabbit:exchange-declare connection 1 destination "topic" :durable t)
           (cl-rabbit:queue-declare connection 1 :queue queue :durable t)
           (cl-rabbit:queue-bind connection 1 :queue queue :exchange exchange
                                  :routing-key "original")
           ;; Transaction commit is a real broker barrier for test setup only.
           (cl-rabbit:tx-select connection 1)
           (cl-rabbit:basic-publish connection 1 :exchange exchange
                                    :routing-key "original" :body body
                                    :properties (list (cons :delivery-mode 2)
                                                      (cons :message-id nonce)))
           (cl-rabbit:tx-commit connection 1)
           (cl-rabbit:channel-open connection 2)
           (cl-rabbit:basic-consume connection 2 queue :no-ack nil)
           (let ((original (retry-proof-consume connection)))
             (unless original
               (error "Synthetic broker setup did not deliver original; not a behavior RED."))
             (setf (star.consumers::rabbit-stream-connection stream) connection
                   (star.consumers::rabbit-stream-channel stream) 2
                   (star.consumers::rabbit-stream-owner-thread stream) (bt:current-thread)
                   (star.consumers::retry-stream-current-body stream) body
                   (star.consumers::retry-stream-current-properties stream)
                   (cl-rabbit:message/properties (cl-rabbit:envelope/message original))
                   (star.consumers::retry-stream-current-exchange stream) destination
                   (star.consumers::retry-stream-current-routing-key stream) "unbound")
             (let ((rejected nil)
                   (delivery (cons body (cl-rabbit:envelope/delivery-tag original))))
               (handler-case
                   (let ((star.consumers::*retry-sleep-function*
                           (lambda (seconds) (declare (ignore seconds)))))
                     (ecase kind
                       (:retry (star.consumers::republish-retry
                                stream delivery (star.consumers:settlement-retry)))
                       (:quarantine (star.consumers::persist-and-publish-quarantine
                                     stream delivery (star.consumers:settlement-reject)))))
                 (error () (setf rejected t)))
               ;; Synchronous channel.close orders all preceding ACKs and
               ;; requeues any unacknowledged original before the new consume.
               (cl-rabbit:channel-close connection 2)
               (cl-rabbit:channel-open connection 3)
               (cl-rabbit:basic-consume connection 3 queue :no-ack nil)
               (let ((recovered (retry-proof-consume connection)))
                 (is rejected "Unroutable replacement must fail settlement.")
                 (is (not (null recovered))
                     "Original must redeliver after failed replacement publication.")
                 (when recovered
                   (is (string= nonce
                                (cdr (assoc :message-id
                                            (cl-rabbit:message/properties
                                             (cl-rabbit:envelope/message recovered))))))
                   (is (string= body
                                (babel:octets-to-string
                                 (cl-rabbit:message/body (cl-rabbit:envelope/message recovered))
                                 :encoding :utf-8)))
                   (cl-rabbit:basic-ack connection 3
                                        (cl-rabbit:envelope/delivery-tag recovered)))))))
      ;; Only generated resources from this test are eligible for deletion.
      (when control-open
        (ignore-errors (cl-rabbit:queue-delete connection 1 queue))
        (ignore-errors (cl-rabbit:exchange-delete connection 1 exchange))
        (ignore-errors (cl-rabbit:exchange-delete connection 1 destination)))
      (cl-rabbit:destroy-connection connection))))

(test retry-unroutable-replacement-preserves-original
  (call-with-retry-proof :retry))

(test quarantine-unroutable-replacement-preserves-original
  (call-with-retry-proof :quarantine))
