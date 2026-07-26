(in-package :star.consumers)

(defparameter *retry-sleep-function* #'sleep)

(defstruct (retry-policy
             (:constructor make-retry-policy
                 (&key
                    (max-retries 4)
                    (base-delay-ms 250)
                    (max-delay-ms 30000)
                    (jitter-ratio 0.20d0))))
  (max-retries 4 :type (integer 0 *))
  (base-delay-ms 250 :type (integer 0 *))
  (max-delay-ms 30000 :type (integer 0 *))
  (jitter-ratio 0.20d0 :type (real 0 1)))

(define-condition delivery-processing-error (error)
  ((cause
    :initarg :cause
    :reader delivery-error-cause
    :initform nil)
   (reason
    :initarg :reason
    :reader delivery-error-reason
    :initform "delivery processing failed"))
  (:report
   (lambda (condition stream)
     (format stream "~a" (delivery-error-reason condition)))))

(define-condition transient-delivery-error (delivery-processing-error) ())
(define-condition permanent-delivery-error (delivery-processing-error) ())
(define-condition conflict-delivery-error (permanent-delivery-error) ())
(define-condition unauthorized-delivery-error (permanent-delivery-error) ())
(define-condition schema-invalid-delivery-error (permanent-delivery-error) ())
(define-condition internal-delivery-error (transient-delivery-error) ())

(defun delivery-error-class (condition)
  (etypecase condition
    (schema-invalid-delivery-error :schema-invalid)
    (unauthorized-delivery-error :unauthorized)
    (conflict-delivery-error :conflict)
    (internal-delivery-error :internal)
    (transient-delivery-error :transient)
    (permanent-delivery-error :permanent)
    (delivery-processing-error :internal)))

(defun delivery-error-retryable-p (condition)
  (typep condition 'transient-delivery-error))

(defun condition-class-name (condition)
  (string-upcase
   (symbol-name (class-name (class-of condition)))))

(defun condition-name-contains-p (condition fragment)
  (not (null (search (string-upcase fragment)
                     (condition-class-name condition)))))

(defun classify-delivery-condition (condition)
  "Convert arbitrary handler conditions to the server failure taxonomy."
  (cond
    ((typep condition 'delivery-processing-error)
     condition)
    ((or (condition-name-contains-p condition "UNAUTHORIZED")
         (condition-name-contains-p condition "FORBIDDEN"))
     (make-condition 'unauthorized-delivery-error
                     :cause condition
                     :reason (princ-to-string condition)))
    ((or (condition-name-contains-p condition "MUTATION-CONFLICT")
         (condition-name-contains-p condition "MISSING-DOCUMENT-FOR-UPDATE")
         (condition-name-contains-p condition "HTTP-REQUEST-CONFLICT"))
     (make-condition 'conflict-delivery-error
                     :cause condition
                     :reason (princ-to-string condition)))
    ((or (condition-name-contains-p condition "V09-DOCUMENT-ERROR")
         (condition-name-contains-p condition "DOCUMENT-VALIDATION")
         (condition-name-contains-p condition "SCHEMA"))
     (make-condition 'schema-invalid-delivery-error
                     :cause condition
                     :reason (princ-to-string condition)))
    ((or (condition-name-contains-p condition "TIMEOUT")
         (condition-name-contains-p condition "CONNECTION")
         (condition-name-contains-p condition "RABBITMQ")
         (condition-name-contains-p condition "OUTBOX-STORE-CONFLICT"))
     (make-condition 'transient-delivery-error
                     :cause condition
                     :reason (princ-to-string condition)))
    (t
     (make-condition 'internal-delivery-error
                     :cause condition
                     :reason (princ-to-string condition)))))

(defun rabbit-property (properties key &optional default)
  (let ((entry (assoc key properties :test #'eq)))
    (if entry (cdr entry) default)))

(defun copy-rabbit-properties (properties)
  (loop for (key . value) in properties
        collect (cons key
                      (if (and (eq key :headers) (listp value))
                          (copy-tree value)
                          value))))

(defun rabbit-headers (properties)
  (or (rabbit-property properties :headers) nil))

(defun header-name= (left right)
  (string-equal (string left) (string right)))

(defun rabbit-header (properties name &optional default)
  (let ((entry (assoc name (rabbit-headers properties) :test #'header-name=)))
    (if entry (cdr entry) default)))

(defun set-rabbit-header (properties name value)
  (let* ((copy (copy-rabbit-properties properties))
         (headers (copy-tree (or (rabbit-property copy :headers) nil)))
         (entry (assoc name headers :test #'header-name=)))
    (if entry
        (setf (cdr entry) value)
        (push (cons name value) headers))
    (let ((property-entry (assoc :headers copy :test #'eq)))
      (if property-entry
          (setf (cdr property-entry) headers)
          (push (cons :headers headers) copy)))
    copy))

(defun set-rabbit-property (properties key value)
  (let* ((copy (copy-rabbit-properties properties))
         (entry (assoc key copy :test #'eq)))
    (if entry
        (setf (cdr entry) value)
        (push (cons key value) copy))
    copy))

(defun delivery-attempt (properties)
  (let ((value (rabbit-header properties "x-starintel-attempt" 0)))
    (if (and (integerp value) (not (minusp value))) value 0)))

(defun delivery-trace-id (properties)
  (or (rabbit-header properties "x-starintel-trace-id")
      (rabbit-property properties :correlation-id)
      (cms-ulid:ulid)))

(defun delivery-message-id (properties)
  (or (rabbit-property properties :message-id)
      (cms-ulid:ulid)))

(defun delivery-first-seen-at (properties)
  (or (rabbit-header properties "x-starintel-first-seen-at")
      (spec:utc-now)))

(defun append-attempt-history (properties attempt timestamp)
  (let* ((existing
           (rabbit-header properties "x-starintel-attempt-history" ""))
         (entry (format nil "~d@~a" attempt timestamp))
         (value (if (or (null existing) (string= existing ""))
                    entry
                    (format nil "~a,~a" existing entry))))
    (set-rabbit-header properties "x-starintel-attempt-history" value)))

(defun retry-properties (properties stream next-attempt delay-ms)
  (let* ((timestamp (spec:utc-now))
         (trace-id (delivery-trace-id properties))
         (message-id (delivery-message-id properties))
         (copy (set-rabbit-property properties :delivery-mode 2)))
    (setf copy (set-rabbit-property copy :message-id message-id)
          copy (set-rabbit-property copy :correlation-id trace-id)
          copy (set-rabbit-header copy "x-starintel-attempt" next-attempt)
          copy (set-rabbit-header copy "x-starintel-trace-id" trace-id)
          copy (set-rabbit-header copy "x-starintel-first-seen-at"
                                  (delivery-first-seen-at properties))
          copy (set-rabbit-header copy "x-starintel-original-exchange"
                                  (retry-stream-current-exchange stream))
          copy (set-rabbit-header copy "x-starintel-original-routing-key"
                                  (retry-stream-current-routing-key stream))
          copy (set-rabbit-header copy "x-starintel-retry-delay-ms" delay-ms)
          copy (append-attempt-history copy next-attempt timestamp))
    copy))

(defun retry-delay-ms (policy attempt &optional random-unit)
  "Return capped exponential delay with symmetric jitter.

ATTEMPT is the zero-based attempt that just failed. RANDOM-UNIT may be supplied
by tests and must be between zero and one."
  (let* ((base (retry-policy-base-delay-ms policy))
         (cap (retry-policy-max-delay-ms policy))
         (raw (min cap (* base (expt 2 attempt))))
         (ratio (retry-policy-jitter-ratio policy))
         (unit (or random-unit (random 1.0d0)))
         (factor (+ 1.0d0 (* ratio (- (* 2.0d0 unit) 1.0d0)))))
    (round (max 0 (* raw factor)))))

(defun retry-action-for (policy failure attempt)
  (if (and (delivery-error-retryable-p failure)
           (< attempt (retry-policy-max-retries policy)))
      :retry
      :dead-letter))

(defclass retrying-rabbit-queue-stream (rabbit-queue-stream)
  ((retry-policy
    :initarg :retry-policy
    :accessor retry-stream-policy
    :initform (make-retry-policy))
   (quarantine-fn
    :initarg :quarantine-fn
    :accessor retry-stream-quarantine-fn
    :initform nil)
   (quarantine-exchange
    :initarg :quarantine-exchange
    :accessor retry-stream-quarantine-exchange
    :initform "starintel.quarantine")
   (quarantine-queue
    :initarg :quarantine-queue
    :accessor retry-stream-quarantine-queue
    :initform "starintel-quarantine")
   (current-body
    :accessor retry-stream-current-body
    :initform nil)
   (current-properties
    :accessor retry-stream-current-properties
    :initform nil)
   (current-routing-key
    :accessor retry-stream-current-routing-key
    :initform "")
   (current-exchange
    :accessor retry-stream-current-exchange
    :initform "")
   (current-received-at
    :accessor retry-stream-current-received-at
    :initform nil)))

(defclass retrying-rabbit-consumer (rabbit-consumer)
  ((retry-policy
    :initarg :retry-policy
    :accessor retry-consumer-policy
    :initform (make-retry-policy))
   (quarantine-fn
    :initarg :quarantine-fn
    :accessor retry-consumer-quarantine-fn
    :initform nil)
   (quarantine-exchange
    :initarg :quarantine-exchange
    :accessor retry-consumer-quarantine-exchange
    :initform "starintel.quarantine")
   (quarantine-queue
    :initarg :quarantine-queue
    :accessor retry-consumer-quarantine-queue
    :initform "starintel-quarantine")))

(defmethod open-stream ((stream retrying-rabbit-queue-stream))
  (call-next-method)
  (let ((connection (rabbit-stream-connection stream))
        (channel (rabbit-stream-channel stream))
        (exchange (retry-stream-quarantine-exchange stream))
        (queue (retry-stream-quarantine-queue stream)))
    (cl-rabbit:exchange-declare
     connection channel exchange "topic" :durable t)
    (cl-rabbit:queue-declare
     connection channel :queue queue :durable t)
    (cl-rabbit:queue-bind
     connection channel
     :queue queue
     :exchange exchange
     :routing-key "quarantine.#"))
  stream)

(defmethod consumer-read ((consumer retrying-rabbit-consumer))
  (let* ((stream (consumer-stream consumer))
         (envelope (stream-read stream))
         (message (cl-rabbit:envelope/message envelope))
         (body
           (babel:octets-to-string
            (cl-rabbit:message/body message)
            :encoding :utf-8)))
    (setf (retry-stream-current-body stream) body
          (retry-stream-current-properties stream)
          (cl-rabbit:message/properties message)
          (retry-stream-current-routing-key stream)
          (cl-rabbit:envelope/routing-key envelope)
          (retry-stream-current-exchange stream)
          (cl-rabbit:envelope/exchange envelope)
          (retry-stream-current-received-at stream)
          (spec:utc-now))
    (cons body (cl-rabbit:envelope/delivery-tag envelope))))

(defun current-delivery-attempt (consumer)
  (let ((stream (consumer-stream consumer)))
    (if (typep stream 'retrying-rabbit-queue-stream)
        (delivery-attempt (retry-stream-current-properties stream))
        0)))

(defun configured-failure-settlement (consumer condition)
  (if (typep consumer 'retrying-rabbit-consumer)
      (let* ((failure (classify-delivery-condition condition))
             (attempt (current-delivery-attempt consumer))
             (action
               (retry-action-for
                (retry-consumer-policy consumer)
                failure
                attempt)))
        (make-settlement action
                         :reason (delivery-error-reason failure)
                         :condition failure))
      (make-settlement
       (consumer-failure-action consumer)
       :reason (princ-to-string condition)
       :condition condition)))

(defun json-safe-value (value)
  (typecase value
    (null :null)
    (string value)
    (integer value)
    (float value)
    ((member t) :true)
    (symbol (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun rabbit-properties-json (properties)
  (let ((object (jsown:empty-object)))
    (dolist (entry properties object)
      (let ((key (string-downcase (symbol-name (car entry))))
            (value (cdr entry)))
        (if (eq (car entry) :headers)
            (let ((headers (jsown:empty-object)))
              (dolist (header value)
                (setf (jsown:val headers (string (car header)))
                      (json-safe-value (cdr header))))
              (setf (jsown:val object key) headers))
            (setf (jsown:val object key) (json-safe-value value)))))))

(defun quarantine-record (stream settlement)
  (let* ((properties (retry-stream-current-properties stream))
         (failure
           (classify-delivery-condition
            (or (consumer-settlement-condition settlement)
                (make-condition 'permanent-delivery-error
                                :reason (or (consumer-settlement-reason settlement)
                                            "delivery rejected")))))
         (attempt (delivery-attempt properties))
         (record (jsown:empty-object)))
    (setf (jsown:val record "_id")
          (format nil "quarantine:~a" (cms-ulid:ulid))
          (jsown:val record "type") "_server_quarantine"
          (jsown:val record "status") "quarantined"
          (jsown:val record "failure_class")
          (string-downcase (symbol-name (delivery-error-class failure)))
          (jsown:val record "failure_reason")
          (delivery-error-reason failure)
          (jsown:val record "condition_type")
          (condition-class-name failure)
          (jsown:val record "original_exchange")
          (retry-stream-current-exchange stream)
          (jsown:val record "original_routing_key")
          (retry-stream-current-routing-key stream)
          (jsown:val record "message_id")
          (delivery-message-id properties)
          (jsown:val record "trace_id")
          (delivery-trace-id properties)
          (jsown:val record "attempt_count") attempt
          (jsown:val record "first_seen_at")
          (delivery-first-seen-at properties)
          (jsown:val record "received_at")
          (or (retry-stream-current-received-at stream) (spec:utc-now))
          (jsown:val record "failed_at") (spec:utc-now)
          (jsown:val record "replayed_at") :null
          (jsown:val record "replay_count") 0
          (jsown:val record "original_body")
          (or (retry-stream-current-body stream) "")
          (jsown:val record "original_properties")
          (rabbit-properties-json properties))
    record))

(defun ack-rabbit-delivery (stream delivery)
  (cl-rabbit:basic-ack
   (rabbit-stream-connection stream)
   (rabbit-stream-channel stream)
   (rabbit-delivery-tag delivery)))

(defun republish-retry (stream delivery settlement)
  (declare (ignore settlement))
  (let* ((properties (retry-stream-current-properties stream))
         (attempt (delivery-attempt properties))
         (next-attempt (1+ attempt))
         (delay-ms (retry-delay-ms (retry-stream-policy stream) attempt))
         (next-properties
           (retry-properties properties stream next-attempt delay-ms)))
    (funcall *retry-sleep-function* (/ delay-ms 1000.0d0))
    (cl-rabbit:basic-publish
     (rabbit-stream-connection stream)
     (rabbit-stream-channel stream)
     :exchange (retry-stream-current-exchange stream)
     :routing-key (retry-stream-current-routing-key stream)
     :properties next-properties
     :body (retry-stream-current-body stream))
    (ack-rabbit-delivery stream delivery)))

(defun publish-quarantine-record (stream record)
  (cl-rabbit:basic-publish
   (rabbit-stream-connection stream)
   (rabbit-stream-channel stream)
   :exchange (retry-stream-quarantine-exchange stream)
   :routing-key
   (format nil "quarantine.~a"
           (jsown:val record "failure_class"))
   :properties
   (list (cons :content-type "application/json")
         (cons :delivery-mode 2)
         (cons :message-id (jsown:val record "_id"))
         (cons :correlation-id (jsown:val record "trace_id")))
   :body (jsown:to-json record)))

(defun persist-and-publish-quarantine (stream delivery settlement)
  (let ((record (quarantine-record stream settlement)))
    (when (retry-stream-quarantine-fn stream)
      (funcall (retry-stream-quarantine-fn stream) record))
    (publish-quarantine-record stream record)
    (ack-rabbit-delivery stream delivery)
    record))

(defmethod stream-settle
    ((stream retrying-rabbit-queue-stream) delivery settlement)
  (assert-rabbit-stream-owner stream)
  (ecase (consumer-settlement-action settlement)
    ((:ack :filtered-ack)
     (ack-rabbit-delivery stream delivery))
    (:retry
     (republish-retry stream delivery settlement))
    ((:dead-letter :reject)
     (persist-and-publish-quarantine stream delivery settlement)))
  settlement)

(defun make-rabbit-worker-consumer (consumer worker-number)
  "Clone retry configuration while preserving one stream per owner thread."
  (let ((stream (consumer-stream consumer)))
    (make-instance
     'retrying-rabbit-consumer
     :name (format nil "~a-~d" (consumer-name consumer) worker-number)
     :workers 1
     :stream
     (make-instance
      'retrying-rabbit-queue-stream
      :queue-name (rabbit-stream-queue-name stream)
      :exchange-name (rabbit-stream-exchange stream)
      :exchange-type (rabbit-exchange-type stream)
      :exchange-durable (rabbit-exchange-durable-p stream)
      :routing-key (rabbit-stream-routing-key stream)
      :host (rabbit-stream-host stream)
      :port (rabbit-stream-port stream)
      :user (rabbit-stream-user stream)
      :password (rabbit-stream-password stream)
      :vhost (rabbit-stream-vhost stream)
      :queue-durable (rabbit-stream-queue-durable-p stream)
      :prefetch-count (rabbit-stream-prefetch-count stream)
      :retry-policy (retry-consumer-policy consumer)
      :quarantine-fn (retry-consumer-quarantine-fn consumer)
      :quarantine-exchange (retry-consumer-quarantine-exchange consumer)
      :quarantine-queue (retry-consumer-quarantine-queue consumer))
     :fn (consumer-fn consumer)
     :test-fn (consumer-filter consumer)
     :on-error (consumer-failure-action consumer)
     :on-filter (consumer-filtered-action consumer)
     :retry-policy (retry-consumer-policy consumer)
     :quarantine-fn (retry-consumer-quarantine-fn consumer)
     :quarantine-exchange (retry-consumer-quarantine-exchange consumer)
     :quarantine-queue (retry-consumer-quarantine-queue consumer))))

(defun create-rabbit-consumer
    (&key
       (name (error "Consumer name is required"))
       (n 1)
       (queue-name (error "Queue name is required"))
       (exchange-name "documents")
       (exchange-type "topic")
       (exchange-durable t)
       (routing-key (error "Routing key is required"))
       (host "localhost")
       (port 5672)
       (username "guest")
       (password "guest")
       (vhost "/")
       (queue-durable t)
       (prefetch-count 200)
       (on-error :retry)
       (on-filter :filtered-ack)
       (max-retries 4)
       (retry-base-delay-ms 250)
       (retry-max-delay-ms 30000)
       (retry-jitter-ratio 0.20d0)
       quarantine-fn
       (quarantine-exchange "starintel.quarantine")
       (quarantine-queue "starintel-quarantine")
       (test-fn #'identity)
       (handler-fn (error "Handler function is required")))
  (unless (plusp n)
    (error "Rabbit consumer worker count must be positive"))
  (let ((policy
          (make-retry-policy
           :max-retries max-retries
           :base-delay-ms retry-base-delay-ms
           :max-delay-ms retry-max-delay-ms
           :jitter-ratio retry-jitter-ratio)))
    (make-instance
     'retrying-rabbit-consumer
     :name (string-downcase (string name))
     :stream
     (make-instance
      'retrying-rabbit-queue-stream
      :queue-name queue-name
      :exchange-name exchange-name
      :exchange-type exchange-type
      :exchange-durable exchange-durable
      :routing-key routing-key
      :host host
      :port port
      :user username
      :password password
      :vhost vhost
      :queue-durable queue-durable
      :prefetch-count prefetch-count
      :retry-policy policy
      :quarantine-fn quarantine-fn
      :quarantine-exchange quarantine-exchange
      :quarantine-queue quarantine-queue)
     :workers n
     :fn handler-fn
     :test-fn test-fn
     :on-error on-error
     :on-filter on-filter
     :retry-policy policy
     :quarantine-fn quarantine-fn
     :quarantine-exchange quarantine-exchange
     :quarantine-queue quarantine-queue)))

(defun quarantine-replay-envelope (record &key corrected-body)
  "Return BODY, PROPERTIES, EXCHANGE, and ROUTING-KEY for explicit replay."
  (let* ((now (spec:utc-now))
         (old-trace (jsown:val record "trace_id"))
         (new-trace (cms-ulid:ulid))
         (replay-count (1+ (or (jsown:val-safe record "replay_count") 0)))
         (headers
           (list
            (cons "x-starintel-attempt" 0)
            (cons "x-starintel-trace-id" new-trace)
            (cons "x-starintel-parent-trace-id" old-trace)
            (cons "x-starintel-first-seen-at" now)
            (cons "x-starintel-attempt-history" "")
            (cons "x-starintel-replay-of" (jsown:val record "_id"))
            (cons "x-starintel-replay-count" replay-count)))
         (properties
           (list
            (cons :content-type "application/json")
            (cons :delivery-mode 2)
            (cons :message-id (cms-ulid:ulid))
            (cons :correlation-id new-trace)
            (cons :headers headers))))
    (values
     (or corrected-body (jsown:val record "original_body"))
     properties
     (jsown:val record "original_exchange")
     (jsown:val record "original_routing_key"))))
