(in-package :star.actors)

(defun target-delivery-context (consumer)
  (let ((stream
          (and consumer
               (star.consumers:consumer-stream consumer))))
    (if (typep stream 'star.consumers:retrying-rabbit-queue-stream)
        (values
         (star.consumers:delivery-attempt
          (star.consumers:retry-stream-current-properties stream))
         (star.consumers:delivery-trace-id
          (star.consumers:retry-stream-current-properties stream)))
        (values 0 nil))))

(defun make-target-dispatch-envelope
    (record &key destination (attempt 0) trace-id lease-id fencing-token)
  "Build a target dispatch envelope only from explicit lease authority.

Compatibility callers must not fabricate a lease id or fencing token. Until an
ingress path resolves a current trusted lease record, target execution fails
closed before persistence, scheduling, or publication."
  (validate-target-dispatch-record record)
  (unless (and (target-nonempty-string-p lease-id)
               (integerp fencing-token)
               (plusp fencing-token))
    (error 'invalid-target-dispatch
           :reason "authoritative lease id and positive fencing token are required"))
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
     lease-id
     fencing-token
     (target-record-deadline record))))

(defun target-record-matches-lease-identity-p (record identity)
  "Fail closed unless lease authority names this exact target and actor."
  (and (typep record 'target-record)
       (typep identity 'star.leases:lease-identity)
       (string= (target-record-id record)
                (star.leases:lease-identity-target-id identity))
       (string= (string-downcase (target-record-actor record))
                (star.leases:lease-identity-actor-name identity))))

(defun accept-target-record-with-authority
    (record service context lease-id fencing-token
     &key (attempt 0) trace-id destination
       persist-fn update-fn dispatch-fn schedule-once-fn schedule-recurring-fn
       (deadline (+ (* 1000 (- (get-universal-time) 2208988800)) 5000))
       (request-id (format nil "target-dispatch:~a" (cms-ulid:ulid)))
       (authority-fn #'star.authorization:current-target-lease-authority)
       (commit-fn #'star.leases:commit-fenced-intent))
  "Resolve caller-supplied lease locators to trusted authority before acceptance.

Adapters must supply a trusted request context plus the caller's lease id and
fencing token. The caller-provided locator is never copied into a dispatch
envelope directly: CURRENT-TARGET-LEASE-AUTHORITY must first return the active
server-owned lease record, which is then committed atomically by the fencing
backend before persistence, scheduling, or publication."
  (let ((authority
          (funcall authority-fn service context lease-id fencing-token)))
    (unless (eq :found
                (star.authorization:target-lease-service-result-code authority))
      (return-from accept-target-record-with-authority
        (make-target-dispatch-outcome
         :invalid
         :reason
         (format nil "target lease authority rejected dispatch: ~a"
                 (star.authorization:target-lease-service-result-code authority)))))
    (accept-target-record-with-lease
     record
     (star.authorization:target-lease-service-store service)
     (star.authorization:target-lease-service-result-lease authority)
     :attempt attempt
     :trace-id trace-id
     :destination destination
     :persist-fn persist-fn
     :update-fn update-fn
     :dispatch-fn dispatch-fn
     :schedule-once-fn schedule-once-fn
     :schedule-recurring-fn schedule-recurring-fn
     :deadline deadline
     :request-id request-id
     :commit-fn commit-fn)))

(defun accept-target-record-with-lease
    (record store lease-record
     &key (attempt 0) trace-id destination
       persist-fn update-fn dispatch-fn schedule-once-fn schedule-recurring-fn
       (deadline (+ (* 1000 (- (get-universal-time) 2208988800)) 5000))
       (request-id (format nil "target-dispatch:~a" (cms-ulid:ulid)))
       (commit-fn #'star.leases:commit-fenced-intent))
  "Accept RECORD only after the lease backend commits an immutable dispatch intent.

The fenced intent is the authorization linearization point. A stale/expired lease
must fail before CouchDB acceptance, scheduling, or Rabbit/local dispatch can run.
The immutable intent also gives recovery code a durable key from which to finish a
commit after a crash between lease authorization and downstream persistence."
  (unless (and (typep lease-record 'star.leases:lease-record)
               (target-record-matches-lease-identity-p
                record (star.leases:lease-record-identity lease-record)))
    (return-from accept-target-record-with-lease
      (make-target-dispatch-outcome
       :invalid :reason "target record does not match authoritative lease identity")))
  (let* ((envelope
           (make-target-dispatch-envelope
            record
            :attempt attempt
            :trace-id trace-id
            :destination destination
            :lease-id (star.leases:lease-record-lease-id lease-record)
            :fencing-token (star.leases:lease-record-fencing-token lease-record)))
         (intent-id
           (format nil "target-dispatch-intent:~a"
                   (target-dispatch-envelope-execution-id envelope)))
         (intent-value (target-dispatch-fingerprint envelope))
         (commit-result
           (funcall commit-fn
                    store
                    (star.leases:lease-record-identity lease-record)
                    lease-record
                    intent-id
                    intent-value
                    :deadline deadline
                    :request-id request-id)))
    (unless (eq commit-result :committed)
      (return-from accept-target-record-with-lease
        (target-outcome
         :invalid envelope
         :reason (format nil "lease authority rejected target dispatch: ~a"
                         commit-result))))
    (if persist-fn
        (process-target-dispatch-envelope
         envelope persist-fn update-fn
         :dispatch-fn (or dispatch-fn #'dispatch-target-envelope-now)
         :schedule-once-fn (or schedule-once-fn #'wheel-schedule-target-once)
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
           :schedule-once-fn (or schedule-once-fn #'wheel-schedule-target-once)
           :schedule-recurring-fn
           (or schedule-recurring-fn #'wheel-schedule-target-recurring))))))