(in-package :star-server-tests)

(def-suite consumer-tests
  :description "Owner-thread consumer and settlement behavior")

(in-suite consumer-tests)

(defclass test-settlement-stream ()
  ((settlements
    :initform nil
    :accessor test-stream-settlements)))

(defmethod star.consumers:stream-settle
    ((stream test-settlement-stream) delivery settlement)
  (push (cons delivery settlement) (test-stream-settlements stream))
  settlement)

(defun make-test-consumer
    (&key
       (name "test-consumer")
       (workers 1)
       (test-fn #'identity)
       (handler-fn (lambda (consumer message)
                     (declare (ignore consumer message))
                     (star.consumers:settlement-ack))))
  (make-instance
   'star.consumers:consumer
   :name name
   :workers workers
   :test-fn test-fn
   :fn handler-fn
   :stream (make-instance 'test-settlement-stream)))

(defun make-mock-rabbit-stream (&key (host "localhost") (port 5672))
  (make-instance
   'star.consumers:rabbit-queue-stream
   :host host
   :port port
   :queue-name "test-queue"
   :exchange-name "test-exchange"
   :routing-key "test.key"))

(defun make-mock-retrying-rabbit-stream ()
  (make-instance
   'star.consumers:retrying-rabbit-queue-stream
   :host "localhost"
   :port 5672
   :queue-name "test-retrying-queue"
   :exchange-name "test-exchange"
   :routing-key "test.key"))

(define-condition test-rabbitmq-server-error (error) ())

(defclass test-failed-settlement-stream ()
  ((closed-p :initform nil :accessor test-stream-closed-p)
   (settlements :initform 0 :accessor test-stream-settlement-attempts)))

(defmethod star.consumers:open-stream ((stream test-failed-settlement-stream))
  stream)

(defmethod star.consumers:close-stream ((stream test-failed-settlement-stream))
  (setf (test-stream-closed-p stream) t))

(defmethod star.consumers:stream-settle
    ((stream test-failed-settlement-stream) delivery settlement)
  (declare (ignore delivery settlement))
  (incf (test-stream-settlement-attempts stream))
  (error 'test-rabbitmq-server-error))

(defclass test-failed-settlement-consumer (star.consumers:consumer)
  ((read-once-p :initform nil :accessor test-consumer-read-once-p)))

(defmethod star.consumers:consumer-read
    ((consumer test-failed-settlement-consumer))
  ;; One delivery, then a graceful end: supervision must close the stream
  ;; for redelivery of the failed settlement and exit on the quiet reader
  ;; instead of dying permanently after the first failure.
  (if (test-consumer-read-once-p consumer)
      (signal 'end-of-file)
      (progn
        (setf (test-consumer-read-once-p consumer) t)
        "delivery")))

(test concurrent-server-ulids-remain-distinct
  ;; The pinned cms-ulid generator has one process-wide mutable LAST value.
  ;; Both workers would enter RANDOM and choose the same value without the
  ;; server lock. The timeout also lets the locked implementation finish.
  (let* ((arrivals 0)
         (arrival-lock (bt:make-lock "ulid-test-arrivals"))
         (timestamp 123456789000000)
         (random (lambda (limit)
                   (declare (ignore limit))
                   (bt:with-lock-held (arrival-lock)
                     (incf arrivals))
                   (let ((deadline (+ (get-internal-real-time)
                                      (/ internal-time-units-per-second 10))))
                     (loop until (or (= 2 arrivals)
                                     (>= (get-internal-real-time) deadline))
                           do (sleep 0.001)))
                   17))
         (left (bt:make-thread
                (lambda () (star.ids:ulid :time timestamp :random random))))
         (right (bt:make-thread
                 (lambda () (star.ids:ulid :time timestamp :random random))))
         (ids (list (bt:join-thread left) (bt:join-thread right))))
    (is (= 26 (length (first ids))))
    (is (= 26 (length (second ids))))
    (is (not (string= (first ids) (second ids))))))

(test concurrent-quarantine-records-have-distinct-ids
  (let* ((timestamp 123456789000111)
         (arrivals 0)
         (arrival-lock (bt:make-lock "quarantine-ulid-test"))
         (original-clock (symbol-function 'cms-ulid::unix-ts-ms))
         (original-random (symbol-function 'crypto:strong-random))
         (ids nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'cms-ulid::unix-ts-ms)
                 (lambda () timestamp)
                 (symbol-function 'crypto:strong-random)
                 (lambda (limit)
                   (declare (ignore limit))
                   (bt:with-lock-held (arrival-lock)
                     (incf arrivals))
                   (let ((deadline (+ (get-internal-real-time)
                                      (/ internal-time-units-per-second 10))))
                     (loop until (or (= 2 arrivals)
                                     (>= (get-internal-real-time) deadline))
                           do (sleep 0.001)))
                   17))
           (setf ids
                 (loop for index below 2
                       collect
                       (bt:make-thread
                        (lambda ()
                          (let ((stream (make-mock-retrying-rabbit-stream)))
                            (setf (star.consumers:retry-stream-current-properties
                                   stream)
                                  (list (cons :message-id
                                              (format nil "delivery-~d" index))
                                        (cons :correlation-id "trace")))
                            (jsown:val
                             (star.consumers::quarantine-record
                              stream (star.consumers:settlement-reject))
                             "_id"))))))
           (setf ids (mapcar #'bt:join-thread ids)))
      (setf (symbol-function 'cms-ulid::unix-ts-ms) original-clock
            (symbol-function 'crypto:strong-random) original-random))
    (is (= 2 (length ids)))
    (is (every (lambda (id) (and (stringp id)
                                 (= 37 (length id))
                                 (search "quarantine:" id :end2 11)))
               ids))
    (is (not (string= (first ids) (second ids))))))

(test failed-settlement-closes-worker-without-acknowledging
  (let* ((stream (make-instance 'test-failed-settlement-stream))
         (consumer (make-instance
                    'test-failed-settlement-consumer
                    :name "failed-settlement"
                    :stream stream
                    :fn (lambda (self delivery)
                          (declare (ignore self delivery))
                          (star.consumers:settlement-reject)))))
    ;; Superved restart backoff must not sleep in tests (issue #38).
    (let ((star.consumers::*retry-sleep-function* (nosleep-counting)))
      (star.consumers:run-consumer consumer))
    (is (test-stream-closed-p stream))
    (is (= 1 (test-stream-settlement-attempts stream)))
    (is (= 1 (star.consumers:consumer-unsettled consumer)))
    (is (= 0 (star.consumers:consumer-settlement-count consumer :reject)))))

(test consumer-creation
  (let ((consumer (make-test-consumer :name "test" :workers 2)))
    (is (string= "test" (star.consumers:consumer-name consumer)))
    (is (= 2 (star.consumers:consumer-worker-count consumer)))
    (is (not (null (star.consumers:consumer-lock consumer))))
    (is (eq :created (star.consumers:consumer-state consumer)))))

(test settlement-normalization
  (is (eq :ack
          (star.consumers:consumer-settlement-action
           (star.consumers:normalize-settlement nil))))
  (is (eq :retry
          (star.consumers:consumer-settlement-action
           (star.consumers:normalize-settlement :retry))))
  (signals error
    (star.consumers:normalize-settlement :unknown-action)))

(test accepted-delivery-settles-once
  (let* ((handled nil)
         (consumer
           (make-test-consumer
            :handler-fn
            (lambda (self message)
              (declare (ignore self))
              (setf handled message)
              (star.consumers:settlement-ack "stored"))))
         (stream (star.consumers:consumer-stream consumer))
         (settlement (star.consumers:consume consumer "message")))
    (is (string= "message" handled))
    (is (eq :ack (star.consumers:consumer-settlement-action settlement)))
    (is (= 1 (length (test-stream-settlements stream))))
    (is (= 0 (star.consumers:consumer-in-flight consumer)))
    (is (= 0 (star.consumers:consumer-unsettled consumer)))
    (is (= 1 (star.consumers:consumer-settlement-count consumer :ack)))))

(test filtered-delivery-restores-credit
  (let* ((handled nil)
         (consumer
           (make-test-consumer
            :test-fn (constantly nil)
            :handler-fn
            (lambda (self message)
              (declare (ignore self message))
              (setf handled t))))
         (settlement (star.consumers:consume consumer "ignored")))
    (is (not handled))
    (is (eq :filtered-ack
            (star.consumers:consumer-settlement-action settlement)))
    (is (= 0 (star.consumers:consumer-unsettled consumer)))
    (is (= 1
           (star.consumers:consumer-settlement-count
            consumer
            :filtered-ack)))))

(test handler-failure-uses-configured-settlement
  (let* ((consumer
           (make-instance
            'star.consumers:consumer
            :name "failure-test"
            :workers 1
            :test-fn #'identity
            :on-error :dead-letter
            :fn (lambda (self message)
                  (declare (ignore self message))
                  (error "boom"))
            :stream (make-instance 'test-settlement-stream)))
         (settlement (star.consumers:consume consumer "message")))
    (is (eq :dead-letter
            (star.consumers:consumer-settlement-action settlement)))
    (is (= 1 (star.consumers:consumer-failures consumer)))
    (is (= 1
           (star.consumers:consumer-settlement-count
            consumer
            :dead-letter)))))

(test rabbit-stream-configuration
  (let ((stream (make-mock-rabbit-stream)))
    (is (string= "localhost" (star.consumers:rabbit-stream-host stream)))
    (is (= 5672 (star.consumers:rabbit-stream-port stream)))
    (is (string= "guest" (star.consumers:rabbit-stream-user stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name stream)))
    (is (string= "test-exchange"
                 (star.consumers:rabbit-stream-exchange stream)))
    (is (string= "test.key"
                 (star.consumers:rabbit-stream-routing-key stream)))
    (is (not (star.consumers:rabbit-stream-open-p stream)))))

(test rabbit-workers-own-distinct-streams
  (let* ((configuration
           (make-instance
            'star.consumers:rabbit-consumer
            :name "rabbit-test"
            :workers 2
            :stream (make-mock-rabbit-stream)
            :fn (lambda (self message)
                  (declare (ignore self message))
                  :ack)))
         (left (star.consumers:make-rabbit-worker-consumer configuration 1))
         (right (star.consumers:make-rabbit-worker-consumer configuration 2))
         (left-stream (star.consumers:consumer-stream left))
         (right-stream (star.consumers:consumer-stream right)))
    (is (not (eq left-stream right-stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name left-stream)))
    (is (string= "test-queue"
                 (star.consumers:rabbit-stream-queue-name right-stream)))
    (is (= 1 (star.consumers:consumer-worker-count left)))
    (is (= 1 (star.consumers:consumer-worker-count right)))))

(test wrong-thread-settlement-is-rejected
  (let ((stream (make-mock-rabbit-stream)))
    (signals star.consumers:wrong-stream-owner
      (star.consumers:stream-settle
       stream
       (cons "message" 1)
       (star.consumers:settlement-ack)))))

(test bounded-retry-policy
  (let ((policy
          (star.consumers:make-retry-policy
           :max-retries 2
           :base-delay-ms 100
           :max-delay-ms 1000
           :jitter-ratio 0.0d0)))
    (is (= 100 (star.consumers:retry-delay-ms policy 0 0.5d0)))
    (is (= 200 (star.consumers:retry-delay-ms policy 1 0.5d0)))
    (is (eq :retry
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:transient-delivery-error)
             1)))
    (is (eq :dead-letter
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:transient-delivery-error)
             2)))
    (is (eq :dead-letter
            (star.consumers:retry-action-for
             policy
             (make-condition 'star.consumers:permanent-delivery-error)
             0)))))

(test rabbit-startup-retry-recovers-from-transient-server-error
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 2
            :base-delay-ms 10
            :max-delay-ms 100
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps nil)
         (result
           (star.consumers::call-with-rabbit-startup-retry
            stream
            policy
            (lambda ()
              (incf attempts)
              (if (< attempts 3)
                  (progn
                    (setf (star.consumers:rabbit-stream-owner-thread stream)
                          (bt:current-thread))
                    (error 'test-rabbitmq-server-error))
                  :opened))
            :sleep-fn (lambda (seconds) (push seconds sleeps)))))
    (is (eq :opened result))
    (is (= 3 attempts))
    (is (equal '(0.01d0 0.02d0) (reverse sleeps)))
    (is (null (star.consumers:rabbit-stream-owner-thread stream)))
    (is (null (star.consumers:rabbit-stream-connection stream)))
    (is (not (star.consumers:rabbit-stream-open-p stream)))))

(test rabbit-startup-retry-exhaustion-is-visible
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 2
            :base-delay-ms 1
            :max-delay-ms 10
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps 0))
    (signals test-rabbitmq-server-error
      (star.consumers::call-with-rabbit-startup-retry
       stream
       policy
       (lambda ()
         (incf attempts)
         (error 'test-rabbitmq-server-error))
       :sleep-fn (lambda (seconds)
                   (declare (ignore seconds))
                   (incf sleeps))))
    (is (= 3 attempts))
    (is (= 2 sleeps))))

(test rabbit-startup-retry-does-not-hide-programming-errors
  (let* ((stream (make-mock-retrying-rabbit-stream))
         (policy
           (star.consumers:make-retry-policy
            :max-retries 5
            :base-delay-ms 1
            :max-delay-ms 10
            :jitter-ratio 0.0d0))
         (attempts 0)
         (sleeps 0))
    (signals simple-error
      (star.consumers::call-with-rabbit-startup-retry
       stream
       policy
       (lambda ()
         (incf attempts)
         (error "programming bug"))
       :sleep-fn (lambda (seconds)
                   (declare (ignore seconds))
                   (incf sleeps))))
    (is (= 1 attempts))
    (is (zerop sleeps))))

(test transient-document-predicate
  (let ((transient
          (cons
           "{\"_id\":\"one\",\"dtype\":\"note\",\"transient\":true}"
           1))
        (persistent
          (cons
           "{\"_id\":\"two\",\"dtype\":\"note\",\"transient\":false}"
           2)))
    (is (star.rabbit:transient-p transient))
    (is (not (star.rabbit:transient-p persistent)))))

;;; Reconnect after broker-forced connection loss (issue #37 live incident
;;; 2026-10-30: documents.ingest/targets/update sat at 0 consumers ~25 min
;;; after a RabbitMQ restart because run-consumer returned permanently on
;;; any transport error).

(define-condition test-connection-lost-error (error) ())

(defclass test-reconnect-stream ()
  ((script
    :initarg :script
    :accessor test-stream-script)
   (opens :initform 0 :accessor test-stream-opens)
   (closes :initform 0 :accessor test-stream-closes)
   (settled :initform 0 :accessor test-stream-settled)))

(defmethod star.consumers:open-stream ((stream test-reconnect-stream))
  (incf (test-stream-opens stream))
  stream)

(defmethod star.consumers:close-stream ((stream test-reconnect-stream))
  (incf (test-stream-closes stream))
  stream)

(defmethod star.consumers:stream-read ((stream test-reconnect-stream))
  (let ((next (pop (test-stream-script stream))))
    (cond
      ((null next) (signal 'end-of-file))
      ((eq next :err) (error 'test-connection-lost-error))
      (t next))))

(defmethod star.consumers:stream-settle
    ((stream test-reconnect-stream) delivery settlement)
  (declare (ignore delivery))
  (incf (test-stream-settled stream))
  settlement)

(defun make-reconnect-consumer (script)
  (make-instance
   'star.consumers:consumer
   :name "reconnect-test"
   :workers 1
   :test-fn #'identity
   :fn (lambda (consumer message)
         (declare (ignore consumer message))
         (star.consumers:settlement-ack))
   :stream (make-instance 'test-reconnect-stream :script script)))

(defparameter *reconnect-test-sleeps* 0)

(defun nosleep-counting ()
  (lambda (seconds)
    (declare (ignore seconds))
    (incf *reconnect-test-sleeps*)))

(test transport-error-reconnects-and-resumes
  (let* ((consumer (make-reconnect-consumer '(:err "d1" :err "d2" :eof)))
         (stream (star.consumers:consumer-stream consumer)))
    (let ((star.consumers::*retry-sleep-function* (nosleep-counting)))
      (star.consumers:run-consumer consumer))
    (is (= 3 (test-stream-opens stream))
        "must reopen after each transport loss")
    (is (= 2 (test-stream-settled stream))
        "must keep consuming deliveries after reconnects")
    (is (eq :stopped (star.consumers:consumer-state consumer)))))

(test end-of-file-still-stops-gracefully
  (let* ((consumer (make-reconnect-consumer '("d1" :eof)))
         (stream (star.consumers:consumer-stream consumer)))
    (star.consumers:run-consumer consumer)
    (is (= 1 (test-stream-opens stream))
        "end-of-file must not trigger a reopen")
    (is (= 1 (test-stream-settled stream)))
    (is (eq :stopped (star.consumers:consumer-state consumer)))))

(test stopping-fences-reconnect
  (let* ((consumer (make-reconnect-consumer '(:err "never" :eof)))
         (stream (star.consumers:consumer-stream consumer))
         (stop-during-backoff
           (lambda (seconds)
             (declare (ignore seconds))
             (star.consumers:consumer-update-state consumer :stopping))))
    (let ((star.consumers::*retry-sleep-function* stop-during-backoff))
      (star.consumers:run-consumer consumer))
    (is (= 1 (test-stream-opens stream))
        "no reopen once the consumer is stopping")
    (is (eq :stopped (star.consumers:consumer-state consumer)))))

(defun run-consumer-tests ()
  (run! 'consumer-tests))

;;; Queue topology ownership (issue #385) --------------------------------

(test create-rabbit-consumer-accepts-queue-topology-arguments
  (let ((arguments-accessor
          (find-symbol "RABBIT-STREAM-QUEUE-ARGUMENTS" :star.consumers))
        (dead-letter-exchange-accessor
          (find-symbol "RETRY-STREAM-DEAD-LETTER-EXCHANGE" :star.consumers))
        (dead-letter-queue-accessor
          (find-symbol "RETRY-STREAM-DEAD-LETTER-QUEUE" :star.consumers))
        (consumer
          (ignore-errors
            (star.consumers:create-rabbit-consumer
             :name "topology-arguments-red"
             :queue-name "events"
             :exchange-name "events"
             :routing-key "event.#"
             :handler-fn #'identity
             :queue-arguments
             (list (cons "x-dead-letter-exchange" "events.dead-letter"))
             :dead-letter-exchange "events.dead-letter"
             :dead-letter-routing-key "events.invalid"
             :dead-letter-queue "events.quarantine"))))
    (is-true arguments-accessor
             "rabbit-stream-queue-arguments accessor must exist")
    (is-true dead-letter-exchange-accessor
             "retry-stream-dead-letter-exchange accessor must exist")
    (is-true dead-letter-queue-accessor
             "retry-stream-dead-letter-queue accessor must exist")
    (is-true consumer
             "create-rabbit-consumer must accept queue topology arguments")
    (when (and consumer arguments-accessor)
      (is (equal '(("x-dead-letter-exchange" . "events.dead-letter"))
                 (funcall arguments-accessor
                          (star.consumers:consumer-stream consumer)))
          "supported queue arguments must reach the stream"))
    (when (and consumer dead-letter-exchange-accessor)
      (is (string= "events.dead-letter"
                   (funcall dead-letter-exchange-accessor
                            (star.consumers:consumer-stream consumer)))
          "dead-letter exchange must reach the stream"))
    (when (and consumer dead-letter-queue-accessor)
      (is (string= "events.quarantine"
                   (funcall dead-letter-queue-accessor
                            (star.consumers:consumer-stream consumer)))
          "dead-letter queue must reach the stream"))))

;;; Queue topology reconciliation (issue #385) --------------------------
;;;
;;; The scripted cl-rabbit stubs record into a shared log cell (the CAR of
;;; LOG) so closures created inside and outside the installer see the same
;;; entries.

(defun install-cl-rabbit-open-stubs (log on-declare on-delete)
  "Script the cl-rabbit surface used by open-stream; return a restore thunk.
LOG is a cons cell whose CAR accumulates the recorded calls."
  (let ((saved nil))
    (flet ((stub (symbol function)
             (push (cons symbol (symbol-function symbol)) saved)
             (sb-ext:without-package-locks
               (setf (symbol-function symbol) function)))
           (record (entry)
             (push entry (car log))))
      (stub 'cl-rabbit:new-connection (constantly :test-connection))
      (stub 'cl-rabbit:tcp-socket-new (constantly :test-socket))
      (stub 'cl-rabbit:socket-open
            (lambda (&rest args) (declare (ignore args))))
      (stub 'cl-rabbit:login-sasl-plain
            (lambda (&rest args) (declare (ignore args))))
      (stub 'cl-rabbit:channel-open
            (lambda (connection channel)
              (declare (ignore connection))
              (record (list :channel-open channel))))
      (stub 'cl-rabbit:basic-qos
            (lambda (&rest args) (declare (ignore args))))
      (stub 'cl-rabbit:exchange-declare
            (lambda (connection channel exchange type &key durable)
              (declare (ignore connection channel type durable))
              (record (list :exchange-declare exchange))))
      (stub 'cl-rabbit:queue-declare on-declare)
      (stub 'cl-rabbit:queue-delete on-delete)
      (stub 'cl-rabbit:queue-bind
            (lambda (connection channel &key queue exchange routing-key)
              (declare (ignore connection channel))
              (record (list :queue-bind queue exchange routing-key))))
      (stub 'cl-rabbit:basic-consume
            (lambda (&rest args) (declare (ignore args))))
      (stub 'cl-rabbit:destroy-connection
            (lambda (&rest args) (declare (ignore args)))))
    (lambda ()
      (dolist (binding saved)
        (sb-ext:without-package-locks
          (setf (symbol-function (car binding)) (cdr binding)))))))

(defun topology-probe-stream ()
  (make-instance
   'star.consumers:retrying-rabbit-queue-stream
   :host "localhost"
   :port 5672
   :queue-name "events"
   :exchange-name "events"
   :routing-key "event.#"
   :queue-arguments
   (list (cons "x-dead-letter-exchange" "events.dead-letter")
         (cons "x-dead-letter-routing-key" "events.invalid"))
   :dead-letter-exchange "events.dead-letter"
   :dead-letter-routing-key "events.invalid"
   :dead-letter-queue "events.quarantine"))

(defun topology-declare-recorder (log)
  "Record queue declarations into LOG; signal 406 on the first EVENTS declare."
  (let ((declares 0))
    (lambda (connection channel &key queue durable arguments)
      (declare (ignore connection channel durable))
      (incf declares)
      (push (list :queue-declare queue arguments) (car log))
      (when (and (string= queue "events") (= declares 1))
        (error (make-condition
                'cl-rabbit:rabbitmq-server-error
                :reply-code cl-rabbit:+amqp-precondition-failed+
                :message "inequivalent arg 'x-dead-letter-exchange'"))))))

(defun recorded-declare-for (log queue-name)
  (find queue-name (car log)
        :test #'string=
        :key (lambda (entry)
               (and (eq (first entry) :queue-declare)
                    (second entry)))))

(test create-rabbit-consumer-accepts-queue-topology-arguments
  (let ((arguments-accessor
          (find-symbol "RABBIT-STREAM-QUEUE-ARGUMENTS" :star.consumers))
        (dead-letter-exchange-accessor
          (find-symbol "RETRY-STREAM-DEAD-LETTER-EXCHANGE" :star.consumers))
        (dead-letter-queue-accessor
          (find-symbol "RETRY-STREAM-DEAD-LETTER-QUEUE" :star.consumers))
        (consumer
          (ignore-errors
            (star.consumers:create-rabbit-consumer
             :name "topology-arguments-red"
             :queue-name "events"
             :exchange-name "events"
             :routing-key "event.#"
             :handler-fn #'identity
             :queue-arguments
             (list (cons "x-dead-letter-exchange" "events.dead-letter"))
             :dead-letter-exchange "events.dead-letter"
             :dead-letter-routing-key "events.invalid"
             :dead-letter-queue "events.quarantine"))))
    (is-true arguments-accessor
             "rabbit-stream-queue-arguments accessor must exist")
    (is-true dead-letter-exchange-accessor
             "retry-stream-dead-letter-exchange accessor must exist")
    (is-true dead-letter-queue-accessor
             "retry-stream-dead-letter-queue accessor must exist")
    (is-true consumer
             "create-rabbit-consumer must accept queue topology arguments")
    (when (and consumer arguments-accessor)
      (is (equal '(("x-dead-letter-exchange" . "events.dead-letter")
                  ("x-dead-letter-routing-key" . "events.invalid"))
                 (funcall arguments-accessor
                          (star.consumers:consumer-stream consumer)))
          "supported queue arguments must reach the stream"))
    (when (and consumer dead-letter-exchange-accessor)
      (is (string= "events.dead-letter"
                   (funcall dead-letter-exchange-accessor
                            (star.consumers:consumer-stream consumer)))
          "dead-letter exchange must reach the stream"))
    (when (and consumer dead-letter-queue-accessor)
      (is (string= "events.quarantine"
                   (funcall dead-letter-queue-accessor
                            (star.consumers:consumer-stream consumer)))
          "dead-letter queue must reach the stream"))))

(test active-open-stream-declares-supported-topology
  (let ((log (list nil)))
    (let ((restore
            (install-cl-rabbit-open-stubs
             log
             (lambda (connection channel &key queue durable arguments)
               (declare (ignore connection channel durable))
               (push (list :queue-declare queue arguments) (car log)))
             (lambda (&rest args) (declare (ignore args)))))
          (stream (topology-probe-stream)))
      (unwind-protect
           (star.consumers:open-stream stream)
        (funcall restore)))
    (let ((declaration (recorded-declare-for log "events")))
      (is-true declaration "the events queue must be declared")
      (is (equal '(("x-dead-letter-exchange" . "events.dead-letter")
                  ("x-dead-letter-routing-key" . "events.invalid"))
                (third declaration))
          "the active declaration path must carry the supported arguments"))
    (is (member '(:exchange-declare "events.dead-letter") (car log)
                :test #'equal)
        "the dead-letter exchange must be declared")
    (is (recorded-declare-for log "events.quarantine")
        "the dead-letter queue must be declared")
    (is (member '(:queue-bind "events.quarantine" "events.dead-letter"
                  "events.invalid")
                (car log) :test #'equal)
        "the dead-letter queue must be bound to the dead-letter routing key")))

(test incompatible-empty-queue-is-recreated-non-destructively
  (let ((log (list nil)))
    (let ((on-declare (topology-declare-recorder log)))
      (let ((restore
              (install-cl-rabbit-open-stubs
               log
               on-declare
               (lambda (connection channel queue &key if-empty if-unused)
                 (declare (ignore connection channel))
                 (push (list :queue-delete queue if-empty if-unused)
                       (car log))
                 nil)))
            (stream (topology-probe-stream)))
        (unwind-protect
             (is-true (star.consumers:open-stream stream)
                      "an empty incompatible queue must be recreated")
          (funcall restore)))
      (is (= 2 (count "events" (car log)
                      :test #'string=
                      :key (lambda (entry)
                             (and (eq (first entry) :queue-declare)
                                  (second entry)))))
          "the events queue must be declared again after recreation")
      (is (member '(:queue-delete "events" t t) (car log) :test #'equal)
          "the stale queue must be deleted only when empty and unused")
      (is (equal 2 (count-if (lambda (entry)
                               (eq (first entry) :channel-open))
                             (car log)))
          "the closed channel must be reopened before the retry")
      (is (equal '(("x-dead-letter-exchange" . "events.dead-letter")
                  ("x-dead-letter-routing-key" . "events.invalid"))
                 (third (recorded-declare-for log "events")))
          "the recreated queue must carry the supported arguments"))))

(test incompatible-non-empty-queue-is-actionable-not-destructive
  (let ((log (list nil))
        (deleted nil))
    (let ((restore
            (install-cl-rabbit-open-stubs
             log
             (lambda (connection channel &key queue durable arguments)
               (declare (ignore connection channel durable arguments))
               (push (list :queue-declare queue) (car log))
               (error (make-condition
                       'cl-rabbit:rabbitmq-server-error
                       :reply-code cl-rabbit:+amqp-precondition-failed+
                       :message "inequivalent arg 'x-dead-letter-exchange'")))
             (lambda (connection channel queue &key if-empty if-unused)
               (declare (ignore connection channel if-empty if-unused))
               (push queue deleted)
               (error (make-condition
                       'cl-rabbit:rabbitmq-server-error
                       :reply-code 406
                       :message "queue must be empty")))))
          (stream (topology-probe-stream)))
      (unwind-protect
           (let ((signalled
                   (handler-case (star.consumers:open-stream stream)
                     (star.consumers:queue-topology-mismatch (c) c))))
             (is-true (typep signalled
                             'star.consumers:queue-topology-mismatch)
                      "a non-empty incompatible queue must signal an actionable mismatch")
             (is (string= "events"
                          (star.consumers:queue-topology-mismatch-queue-name
                           signalled))))
        (funcall restore)))
    (is-true (member "events" deleted :test #'string=)
              "recreation must have been attempted non-destructively")))

(test topology-mismatch-is-terminal-not-startup-retryable
  (let ((condition
          (make-condition 'star.consumers:queue-topology-mismatch
                          :queue-name "events"
                          :server-message "queue must be empty")))
    (is-false (star.consumers:rabbit-startup-retryable-p condition)
              "a topology mismatch must never be retried as transient")))

(test worker-stream-clones-carry-queue-topology
  (let* ((consumer
           (star.consumers:create-rabbit-consumer
            :name "topology-clone-test"
            :queue-name "events"
            :exchange-name "events"
            :routing-key "event.#"
            :handler-fn #'identity
            :dead-letter-exchange "events.dead-letter"
            :dead-letter-routing-key "events.invalid"
            :dead-letter-queue "events.quarantine"))
         (parent-stream (star.consumers:consumer-stream consumer))
         (worker
           (star.consumers:make-rabbit-worker-consumer consumer 1))
         (worker-stream (star.consumers:consumer-stream worker)))
    (is (equal (star.consumers:rabbit-stream-queue-arguments parent-stream)
               (star.consumers:rabbit-stream-queue-arguments worker-stream))
        "worker streams must declare the same supported arguments")
    (is (string= "events.dead-letter"
                 (star.consumers:retry-stream-dead-letter-exchange
                  worker-stream))
        "worker streams must carry the dead-letter topology")))
