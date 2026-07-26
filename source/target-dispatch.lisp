(in-package :star.actors)

(defparameter *target-max-delay-seconds* 31536000)
(defparameter *active-target-schedules* (make-hash-table :test #'equal))

(defstruct (target-destination-handle
             (:constructor make-target-destination-handle
                 (kind name &key component routing-key compatibility-routing-keys)))
  kind
  name
  component
  routing-key
  compatibility-routing-keys)

(defstruct (target-dispatch-envelope
             (:constructor %make-target-dispatch-envelope
                 (record destination schedule-id execution-id attempt trace-id
                  lease-id fencing-token deadline)))
  record
  destination
  schedule-id
  execution-id
  attempt
  trace-id
  lease-id
  fencing-token
  deadline)

(defstruct (target-dispatch-outcome
             (:constructor make-target-dispatch-outcome
                 (status &key reason acceptance-id envelope retryable-p)))
  status
  reason
  acceptance-id
  envelope
  (retryable-p nil))

(define-condition invalid-target-dispatch (error)
  ((reason :initarg :reason :reader invalid-target-dispatch-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid target dispatch: ~a"
             (invalid-target-dispatch-reason condition)))))

(define-condition target-ingress-overloaded (error)
  ((reason :initarg :reason :reader target-ingress-overloaded-reason))
  (:report
   (lambda (condition stream)
     (format stream "Target ingress overloaded: ~a"
             (target-ingress-overloaded-reason condition)))))

(define-condition target-destination-unavailable (error)
  ((reason :initarg :reason :reader target-destination-unavailable-reason))
  (:report
   (lambda (condition stream)
     (format stream "Target destination unavailable: ~a"
             (target-destination-unavailable-reason condition)))))

(defun target-dispatch-digest (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun valid-target-actor-name-p (name)
  (and (target-nonempty-string-p name)
       (cl-ppcre:scan "^[A-Za-z0-9][A-Za-z0-9._:-]*$" name)))

(defun canonical-target-routing-key (actor-name)
  (unless (valid-target-actor-name-p actor-name)
    (error 'invalid-target-dispatch
           :reason (format nil "invalid actor identity ~s" actor-name)))
  (format nil "documents.target.dispatch.~a" (string-downcase actor-name)))

(defun compatibility-target-routing-keys (actor-name)
  (list (format nil "actors.~a.new.target" (string-downcase actor-name))))

(defun resolve-target-destination (actor-name &key (resolver #'get-dest-actor))
  "Resolve ACTOR-NAME into an explicit local or Rabbit component handle."
  (unless (valid-target-actor-name-p actor-name)
    (error 'invalid-target-dispatch
           :reason (format nil "invalid actor identity ~s" actor-name)))
  (let ((component (funcall resolver actor-name)))
    (if component
        (make-target-destination-handle
         :local actor-name :component component)
        (make-target-destination-handle
         :rabbit actor-name
         :routing-key (canonical-target-routing-key actor-name)
         :compatibility-routing-keys
         (compatibility-target-routing-keys actor-name)))))

(defun target-record-deadline (record)
  (target-value (target-record-document record) "deadline" nil))

(defun target-record-schedule-id (record)
  (or (target-value (target-record-document record) "schedule_id" nil)
      (target-record-id record)))

(defun target-record-transient-p (record)
  (star.documents:document-transient-p
   (target-record-document record)))

(defun validate-target-dispatch-record
    (record &key (now (star.documents:utc-now))
                 (max-delay *target-max-delay-seconds*))
  "Validate identity, schedule, recurrence, transient policy, and deadline."
  (unless (target-nonempty-string-p (target-record-id record))
    (error 'invalid-target-dispatch :reason "target id is required"))
  (unless (valid-target-actor-name-p (target-record-actor record))
    (error 'invalid-target-dispatch :reason "actor identity is invalid"))
  (unless (target-nonempty-string-p (target-record-target record))
    (error 'invalid-target-dispatch :reason "target value is required"))
  (unless (and (integerp (target-record-delay record))
               (plusp (target-record-delay record))
               (<= (target-record-delay record) max-delay))
    (error 'invalid-target-dispatch
           :reason (format nil "delay must be between 1 and ~d seconds" max-delay)))
  (unless (target-nonempty-string-p (target-record-schedule-id record))
    (error 'invalid-target-dispatch :reason "schedule id is required"))
  (when (target-record-transient-p record)
    (error 'invalid-target-dispatch
           :reason "transient targets cannot create durable schedules"))
  (let ((deadline (target-record-deadline record)))
    (when deadline
      (unless (target-nonempty-string-p deadline)
        (error 'invalid-target-dispatch
               :reason "deadline must be an ISO timestamp string"))
      (unless (string> deadline now)
        (error 'invalid-target-dispatch :reason "target deadline has expired"))))
  record)

(defun target-execution-id (record schedule-id)
  (format nil "target-execution:~a"
          (target-dispatch-digest
           (format nil "~a|~a|~a"
                   (target-record-id record)
                   (or (target-record-revision record) "unrevisioned")
                   schedule-id))))

(defun make-target-dispatch-envelope
    (record &key destination (attempt 0) trace-id lease-id (fencing-token 1))
  (validate-target-dispatch-record record)
  (let* ((schedule-id (target-record-schedule-id record))
         (resolved
           (or destination
               (resolve-target-destination (target-record-actor record)))))
    (%make-target-dispatch-envelope
     record
     resolved
     schedule-id
     (target-execution-id record schedule-id)
     attempt
     (or trace-id (cms-ulid:ulid))
     (or lease-id (cms-ulid:ulid))
     fencing-token
     (target-record-deadline record))))

(defun target-dispatch-fingerprint (envelope)
  (target-dispatch-digest
   (format nil "~a|~a|~a|~a|~a|~a"
           (target-dispatch-envelope-schedule-id envelope)
           (target-record-id (target-dispatch-envelope-record envelope))
           (or (target-record-revision
                (target-dispatch-envelope-record envelope))
               "unrevisioned")
           (target-destination-handle-kind
            (target-dispatch-envelope-destination envelope))
           (target-destination-handle-name
            (target-dispatch-envelope-destination envelope))
           (target-record-delay
            (target-dispatch-envelope-record envelope)))))

(defun target-acceptance-id (schedule-id)
  (format nil "target-acceptance:~a"
          (target-dispatch-digest schedule-id)))

(defun target-acceptance-document (envelope)
  (let* ((record (target-dispatch-envelope-record envelope))
         (destination (target-dispatch-envelope-destination envelope))
         (document (jsown:empty-object)))
    (setf (jsown:val document "_id")
          (target-acceptance-id
           (target-dispatch-envelope-schedule-id envelope))
          (jsown:val document "type") "_server_target_acceptance"
          (jsown:val document "status") "pending"
          (jsown:val document "fingerprint")
          (target-dispatch-fingerprint envelope)
          (jsown:val document "target_id") (target-record-id record)
          (jsown:val document "target_revision")
          (or (target-record-revision record) :null)
          (jsown:val document "actor") (target-record-actor record)
          (jsown:val document "schedule_id")
          (target-dispatch-envelope-schedule-id envelope)
          (jsown:val document "execution_id")
          (target-dispatch-envelope-execution-id envelope)
          (jsown:val document "attempt")
          (target-dispatch-envelope-attempt envelope)
          (jsown:val document "trace_id")
          (target-dispatch-envelope-trace-id envelope)
          (jsown:val document "lease_id")
          (target-dispatch-envelope-lease-id envelope)
          (jsown:val document "fencing_token")
          (target-dispatch-envelope-fencing-token envelope)
          (jsown:val document "destination_kind")
          (string-downcase
           (symbol-name (target-destination-handle-kind destination)))
          (jsown:val document "routing_key")
          (or (target-destination-handle-routing-key destination) :null)
          (jsown:val document "recurring")
          (if (target-record-recurring-p record) :true :false)
          (jsown:val document "delay") (target-record-delay record)
          (jsown:val document "deadline")
          (or (target-dispatch-envelope-deadline envelope) :null)
          (jsown:val document "accepted_at") :null
          (jsown:val document "last_dispatch_at") :null
          (jsown:val document "created_at") (star.documents:utc-now)
          (jsown:val document "target_document")
          (target-record-document record))
    document))

(defun target-acceptance-equivalent-p (left right)
  (and (string= (jsown:val left "schedule_id")
                (jsown:val right "schedule_id"))
       (string= (jsown:val left "fingerprint")
                (jsown:val right "fingerprint"))))

(defun adopt-target-acceptance-metadata (envelope acceptance)
  "Use durable execution/lease/fencing values when resuming a pending record."
  (setf (target-dispatch-envelope-execution-id envelope)
        (jsown:val acceptance "execution_id")
        (target-dispatch-envelope-trace-id envelope)
        (jsown:val acceptance "trace_id")
        (target-dispatch-envelope-lease-id envelope)
        (jsown:val acceptance "lease_id")
        (target-dispatch-envelope-fencing-token envelope)
        (jsown:val acceptance "fencing_token"))
  envelope)

(defun target-dispatch-document (envelope)
  "Return the target document with dispatch metadata for remote transport."
  (let* ((document
           (star.databases.couchdb::clone-outbox-json
            (target-record-document
             (target-dispatch-envelope-record envelope))))
         (extensions
           (or (star.documents:object-value document "extensions" nil)
               (jsown:empty-object))))
    (setf (jsown:val extensions "target_execution_id")
          (target-dispatch-envelope-execution-id envelope)
          (jsown:val extensions "target_schedule_id")
          (target-dispatch-envelope-schedule-id envelope)
          (jsown:val extensions "target_attempt")
          (target-dispatch-envelope-attempt envelope)
          (jsown:val extensions "target_trace_id")
          (target-dispatch-envelope-trace-id envelope)
          (jsown:val extensions "target_lease_id")
          (target-dispatch-envelope-lease-id envelope)
          (jsown:val extensions "target_fencing_token")
          (target-dispatch-envelope-fencing-token envelope)
          (jsown:val document "extensions") extensions)
    document))

(defun classify-target-dispatch-condition (condition)
  (let ((name
          (string-upcase
           (symbol-name (class-name (class-of condition))))))
    (cond
      ((or (search "OVERLOAD" name) (search "MAILBOX-FULL" name))
       (make-condition 'target-ingress-overloaded
                       :reason (princ-to-string condition)))
      ((or (search "STOPPED" name)
           (search "UNAVAILABLE" name)
           (search "NO-ACTOR" name))
       (make-condition 'target-destination-unavailable
                       :reason (princ-to-string condition)))
      (t condition))))

(defun dispatch-target-envelope-now
    (envelope &key
                (local-send-fn
                  (lambda (component payload)
                    (tell component payload)))
                (remote-send-fn
                  (lambda (routing-key document)
                    (star.rabbit:emit-document
                     "documents" routing-key document))))
  "Dispatch one occurrence through an explicit destination handle."
  (let ((destination (target-dispatch-envelope-destination envelope)))
    (handler-case
        (ecase (target-destination-handle-kind destination)
          (:local
           (unless (target-destination-handle-component destination)
             (error 'target-destination-unavailable
                    :reason "local component handle is missing"))
           (funcall local-send-fn
                    (target-destination-handle-component destination)
                    envelope))
          (:rabbit
           (funcall remote-send-fn
                    (target-destination-handle-routing-key destination)
                    (target-dispatch-document envelope))))
      (error (condition)
        (error (classify-target-dispatch-condition condition)))))
  t)

(defun next-target-occurrence-envelope (envelope)
  (%make-target-dispatch-envelope
   (target-dispatch-envelope-record envelope)
   (target-dispatch-envelope-destination envelope)
   (target-dispatch-envelope-schedule-id envelope)
   (format nil "target-execution:~a" (cms-ulid:ulid))
   0
   (target-dispatch-envelope-trace-id envelope)
   (target-dispatch-envelope-lease-id envelope)
   (target-dispatch-envelope-fencing-token envelope)
   (target-dispatch-envelope-deadline envelope)))

(defun wheel-schedule-target-once (schedule-id delay callback)
  (wt:schedule-once *target-timer* delay callback :sig schedule-id))

(defun wheel-schedule-target-recurring (schedule-id delay callback)
  (wt:schedule-recurring
   *target-timer* delay delay callback schedule-id))

(defun register-target-schedule
    (envelope dispatch-fn schedule-once-fn schedule-recurring-fn)
  "Register one active schedule identity regardless of destination transport."
  (let* ((record (target-dispatch-envelope-record envelope))
         (schedule-id (target-dispatch-envelope-schedule-id envelope)))
    (when (gethash schedule-id *active-target-schedules*)
      (return-from register-target-schedule :duplicate))
    (let ((callback
            (lambda ()
              (funcall dispatch-fn
                       (next-target-occurrence-envelope envelope)))))
      (if (target-record-recurring-p record)
          (funcall schedule-recurring-fn
                   schedule-id (target-record-delay record) callback)
          (funcall schedule-once-fn
                   schedule-id (target-record-delay record) callback)))
    (setf (gethash schedule-id *active-target-schedules*) envelope)
    :scheduled))

(defun target-outcome
    (status envelope &key reason retryable-p)
  (make-target-dispatch-outcome
   status
   :reason reason
   :retryable-p retryable-p
   :envelope envelope
   :acceptance-id
   (target-acceptance-id
    (target-dispatch-envelope-schedule-id envelope))))

(defun process-target-dispatch-envelope
    (envelope persist-fn update-fn
     &key
       (dispatch-fn #'dispatch-target-envelope-now)
       (schedule-once-fn #'wheel-schedule-target-once)
       (schedule-recurring-fn #'wheel-schedule-target-recurring))
  "Persist acceptance, establish schedule, and return a structured outcome."
  (handler-case
      (let ((desired (target-acceptance-document envelope)))
        (multiple-value-bind (acceptance disposition)
            (funcall persist-fn desired #'target-acceptance-equivalent-p)
          (when (eq disposition :conflict)
            (return-from process-target-dispatch-envelope
              (target-outcome
               :invalid envelope
               :reason "duplicate active schedule identity has different content")))
          (adopt-target-acceptance-metadata envelope acceptance)
          (when (eq disposition :duplicate)
            (return-from process-target-dispatch-envelope
              (target-outcome :duplicate envelope)))
          (let ((schedule-result
                  (register-target-schedule
                   envelope dispatch-fn schedule-once-fn
                   schedule-recurring-fn)))
            (funcall update-fn
                     (jsown:val acceptance "_id")
                     (lambda (document)
                       (setf (jsown:val document "status") "scheduled"
                             (jsown:val document "accepted_at") (star.documents:utc-now))
                       document))
            (target-outcome
             (if (eq schedule-result :duplicate) :duplicate :accepted)
             envelope))))
    (invalid-target-dispatch (condition)
      (target-outcome
       :invalid envelope
       :reason (invalid-target-dispatch-reason condition)))
    (target-ingress-overloaded (condition)
      (target-outcome
       :overloaded envelope
       :reason (target-ingress-overloaded-reason condition)
       :retryable-p t))
    (target-destination-unavailable (condition)
      (target-outcome
       :unavailable envelope
       :reason (target-destination-unavailable-reason condition)
       :retryable-p t))
    (error (condition)
      (target-outcome
       :failed envelope
       :reason (princ-to-string condition)
       :retryable-p t))))

(defun accept-target-record
    (record &key (attempt 0) trace-id destination
                 persist-fn update-fn dispatch-fn
                 schedule-once-fn schedule-recurring-fn)
  "Synchronously durably accept a target and return a structured outcome."
  (handler-case
      (let ((envelope
              (make-target-dispatch-envelope
               record
               :attempt attempt
               :trace-id trace-id
               :destination destination)))
        (if persist-fn
            (process-target-dispatch-envelope
             envelope persist-fn update-fn
             :dispatch-fn (or dispatch-fn #'dispatch-target-envelope-now)
             :schedule-once-fn
             (or schedule-once-fn #'wheel-schedule-target-once)
             :schedule-recurring-fn
             (or schedule-recurring-fn #'wheel-schedule-target-recurring))
            (anypool:with-connection
                (client star.databases.couchdb:*couchdb-pool*)
              (process-target-dispatch-envelope
               envelope
               (lambda (desired duplicate-predicate)
                 (star.databases.couchdb:couchdb-persist-target-acceptance
                  client star:*couchdb-default-database*
                  desired duplicate-predicate))
               (lambda (acceptance-id updater)
                 (star.databases.couchdb:couchdb-update-target-acceptance
                  client star:*couchdb-default-database*
                  acceptance-id updater))
               :dispatch-fn (or dispatch-fn #'dispatch-target-envelope-now)
               :schedule-once-fn
               (or schedule-once-fn #'wheel-schedule-target-once)
               :schedule-recurring-fn
               (or schedule-recurring-fn #'wheel-schedule-target-recurring)))))
    (invalid-target-dispatch (condition)
      (make-target-dispatch-outcome
       :invalid :reason (invalid-target-dispatch-reason condition)))))

(defun target-delivery-context (consumer)
  (let ((stream (and consumer (consumer-stream consumer))))
    (if (typep stream 'star.consumers:retrying-rabbit-queue-stream)
        (values
         (star.consumers:delivery-attempt
          (star.consumers:retry-stream-current-properties stream))
         (star.consumers:delivery-trace-id
          (star.consumers:retry-stream-current-properties stream)))
        (values 0 nil))))

(defun accept-target-delivery (consumer document)
  (handler-case
      (let ((record (parse-target-record document)))
        (multiple-value-bind (attempt trace-id)
            (target-delivery-context consumer)
          (accept-target-record
           record :attempt attempt :trace-id trace-id)))
    (error (condition)
      (make-target-dispatch-outcome
       :invalid :reason (princ-to-string condition)))))

(defun target-outcome-success-p (outcome)
  (member (target-dispatch-outcome-status outcome)
          '(:accepted :duplicate)
          :test #'eq))

(defun submit-target (target &key (first-time-p t) (recovered-p nil))
  "Compatibility submission API backed by durable synchronous acceptance."
  (declare (ignore first-time-p recovered-p))
  (let* ((record
           (etypecase target
             (target-record target)
             (list (parse-target-record target))))
         (outcome (accept-target-record record)))
    (case (target-dispatch-outcome-status outcome)
      ((:accepted :duplicate) outcome)
      (:invalid
       (error 'invalid-target-dispatch
              :reason (target-dispatch-outcome-reason outcome)))
      (:overloaded
       (error 'target-ingress-overloaded
              :reason (target-dispatch-outcome-reason outcome)))
      (t
       (error 'target-destination-unavailable
              :reason (target-dispatch-outcome-reason outcome))))))


(defun sumbit-target (target &optional (first-time t))
  "Compatibility alias for the historical misspelling."
  (submit-target target :first-time-p first-time))

(defun start-target-actor (system)
  "Start a typed compatibility ingress actor over the durable coordinator."
  (setf *targets*
        (actor-of
         system
         :name "*targets*"
         :receive
         (lambda (message)
           (etypecase message
             (target-command
              (submit-target
               (target-command-record message)
               :first-time-p (target-command-first-time-p message)
               :recovered-p (target-command-recovered-p message)))
             (target-record (submit-target message))
             (list (submit-target message)))))))

(defun make-remote-target-consumer
    (actor-name handler-fn
     &key
       (queue-name
         (format nil "actor-~a-targets" (string-downcase actor-name)))
       (n 1))
  "Create a consumer bound to the canonical remote target route."
  (star.consumers:create-rabbit-consumer
   :name queue-name
   :n n
   :queue-name queue-name
   :exchange-name "documents"
   :routing-key (canonical-target-routing-key actor-name)
   :host star:*rabbit-address*
   :port star:*rabbit-port*
   :username star:*rabbit-user*
   :password star:*rabbit-password*
   :test-fn #'identity
   :handler-fn
   (lambda (consumer message)
     (declare (ignore consumer))
     (let ((document
             (star.documents:ensure-document (car message))))
       (star.consumers:normalize-settlement
        (funcall handler-fn document))))))
