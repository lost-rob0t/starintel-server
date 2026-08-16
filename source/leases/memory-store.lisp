(in-package :star.leases)

(defclass memory-lease-store (lease-store)
  ((lock
    :initform (bt:make-lock "memory-lease-store")
    :reader memory-store-lock)
   (records
    :initform (make-hash-table :test #'equal)
    :reader memory-store-records)
   (counters
    :initform (make-hash-table :test #'equal)
    :reader memory-store-counters)
   (idempotency
    :initform (make-hash-table :test #'equal)
    :reader memory-store-idempotency)
   (clock
    :initarg :clock
    :reader memory-store-clock)
   (id-generator
    :initarg :id-generator
    :reader memory-store-id-generator)
   (audit-hook
    :initarg :audit-hook
    :reader memory-store-audit-hook)
   (metrics-hook
    :initarg :metrics-hook
    :reader memory-store-metrics-hook)
   (closed-p
    :initform nil
    :accessor memory-store-closed-p)))

(defun unix-time-milliseconds ()
  (* 1000 (- (get-universal-time) 2208988800)))

(defun make-memory-lease-store
    (&key (clock #'unix-time-milliseconds)
       (id-generator #'cms-ulid:ulid) audit-hook metrics-hook)
  "Create the deterministic test backend; production code must use an owned adapter."
  (make-instance
   'memory-lease-store
   :clock clock
   :id-generator id-generator
   :audit-hook audit-hook
   :metrics-hook metrics-hook))

(defun outcome (code &key lease leases retryable-p detail)
  (make-lease-outcome
   :code code
   :lease lease
   :leases leases
   :retryable-p (if retryable-p t (retryable-lease-outcome-code-p code))
   :detail detail))

(defun snapshot-record (record)
  (when record
    (let ((copy (copy-lease-record record)))
      (setf (lease-record-metadata copy)
            (copy-tree (lease-record-metadata record)))
      copy)))

(defun snapshot-outcome (result)
  (let ((copy (copy-lease-outcome result)))
    (setf (lease-outcome-lease copy)
          (snapshot-record (lease-outcome-lease result))
          (lease-outcome-leases copy)
          (mapcar #'snapshot-record (lease-outcome-leases result)))
    copy))

(defun emit-operation-hooks (store operation request-id result)
  (let ((event
          (list
           :operation operation
           :request-id request-id
           :code (lease-outcome-code result)
           :retryable-p (lease-outcome-retryable-p result))))
    (when (memory-store-audit-hook store)
      (funcall (memory-store-audit-hook store) event))
    (when (memory-store-metrics-hook store)
      (funcall (memory-store-metrics-hook store) event)))
  result)

(defun valid-request-shape-p (deadline request-id)
  (and (integerp deadline)
       (valid-lease-identifier-p request-id)))

(defun call-memory-operation (store operation deadline request-id function)
  (let ((result
          (if (not (valid-request-shape-p deadline request-id))
              (outcome :invalid-request)
              (bt:with-lock-held ((memory-store-lock store))
                (let ((now (funcall (memory-store-clock store))))
                  (cond
                    ((memory-store-closed-p store)
                     (outcome :closed))
                    ((<= deadline now)
                     (outcome :timeout :retryable-p t))
                    (t
                     (funcall function now))))))))
    (emit-operation-hooks store operation request-id result)))

(defun record-for-key (store key now)
  (let ((record (gethash key (memory-store-records store))))
    (if (and record (<= (lease-record-expires-at record) now))
        (progn
          (remhash key (memory-store-records store))
          (let ((expired (snapshot-record record)))
            (setf (lease-record-state expired) :expired)
            (values nil expired)))
        (values record nil))))

(defun idempotency-key (operation key owner request-id)
  (list operation key owner request-id))

(defun idempotent-result (store key digest)
  (let ((entry (gethash key (memory-store-idempotency store))))
    (when entry
      (if (equal digest (car entry))
          (snapshot-outcome (cdr entry))
          (outcome :idempotency-conflict)))))

(defun remember-result (store key digest result)
  (setf (gethash key (memory-store-idempotency store))
        (cons (copy-tree digest) (snapshot-outcome result)))
  result)

(defun valid-positive-integer-p (value)
  (and (integerp value) (plusp value)))

(defmethod acquire-lease
    ((store memory-lease-store) identity
     &key owner-principal-id owner-client-id owner-credential-id
       service-instance-id ttl-ms maximum-lifetime-ms execution-id job-id
       trace-id metadata deadline request-id)
  (call-memory-operation
   store :acquire deadline request-id
   (lambda (now)
     (let* ((key (canonical-target-lock-key identity))
            (id-key
              (idempotency-key :acquire key owner-principal-id request-id))
            (digest
              (list owner-client-id owner-credential-id service-instance-id
                    ttl-ms maximum-lifetime-ms execution-id job-id trace-id
                    metadata))
            (prior (idempotent-result store id-key digest)))
        (cond
          (prior prior)
          ((not
            (and (every #'valid-lease-identifier-p
                        (list owner-principal-id owner-client-id
                              owner-credential-id service-instance-id
                              execution-id job-id trace-id))
                 (valid-lease-metadata-p metadata)
                 (valid-positive-integer-p ttl-ms)
                 (valid-positive-integer-p maximum-lifetime-ms)
                 (<= ttl-ms maximum-lifetime-ms)))
           (outcome :invalid-request))
         (t
          (multiple-value-bind (active expired)
              (record-for-key store key now)
            (declare (ignore expired))
            (if active
                (remember-result
                 store id-key digest (outcome :conflict :retryable-p t))
                (let* ((token
                         (1+ (gethash key (memory-store-counters store) 0)))
                       (lease-id (funcall (memory-store-id-generator store)))
                       (record
                         (make-lease-record
                          :lock-key key
                          :identity identity
                          :lease-id lease-id
                          :owner-principal-id owner-principal-id
                          :owner-client-id owner-client-id
                          :owner-credential-id owner-credential-id
                          :service-instance-id service-instance-id
                          :fencing-token token
                          :acquired-at now
                          :renewed-at now
                          :expires-at (+ now ttl-ms)
                          :ttl-ms ttl-ms
                          :maximum-lifetime-ms maximum-lifetime-ms
                          :execution-id execution-id
                          :job-id job-id
                          :trace-id trace-id
                          :request-id request-id
                          :metadata (copy-tree metadata)
                          :state :active)))
                  (setf (gethash key (memory-store-counters store)) token
                        (gethash key (memory-store-records store)) record)
                  (remember-result
                   store id-key digest
                   (outcome
                    :acquired :lease (snapshot-record record))))))))))))

(defun ownership-outcome (record lease-id owner instance token)
  (cond
    ((< token (lease-record-fencing-token record))
     (outcome :stale-token))
    ((not (string= lease-id (lease-record-lease-id record)))
     (outcome :stale-token))
    ((or (not (string= owner (lease-record-owner-principal-id record)))
         (not (string= instance (lease-record-service-instance-id record))))
     (outcome :not-owner))
    ((/= token (lease-record-fencing-token record))
     (outcome :stale-token))
    (t nil)))

(defmethod renew-lease
    ((store memory-lease-store) identity
     &key lease-id owner-principal-id service-instance-id fencing-token ttl-ms
       deadline request-id)
  (call-memory-operation
   store :renew deadline request-id
   (lambda (now)
     (let* ((key (canonical-target-lock-key identity))
            (id-key
              (idempotency-key :renew key owner-principal-id request-id))
            (digest
              (list lease-id service-instance-id fencing-token ttl-ms))
            (prior (idempotent-result store id-key digest)))
        (cond
          (prior prior)
          ((not
            (and (every #'valid-lease-identifier-p
                        (list lease-id owner-principal-id service-instance-id))
                 (valid-positive-integer-p fencing-token)
                 (valid-positive-integer-p ttl-ms)))
           (outcome :invalid-request))
          (t
           (multiple-value-bind (record expired)
               (record-for-key store key now)
             (let ((result
                     (cond
                       (expired (outcome :expired :lease expired))
                       ((null record) (outcome :expired))
                       ((ownership-outcome
                         record lease-id owner-principal-id service-instance-id
                         fencing-token))
                       (t
                        (let ((new-expiry
                                (min (+ now ttl-ms)
                                     (+ (lease-record-acquired-at record)
                                        (lease-record-maximum-lifetime-ms
                                         record)))))
                          (if (<= new-expiry now)
                              (outcome :expired)
                              (progn
                                (setf (lease-record-renewed-at record) now
                                      (lease-record-expires-at record) new-expiry
                                      (lease-record-ttl-ms record) ttl-ms)
                                (outcome
                                 :renewed
                                 :lease (snapshot-record record)))))))))
               (remember-result store id-key digest result)))))))))

(defmethod release-lease
    ((store memory-lease-store) identity
     &key lease-id owner-principal-id service-instance-id fencing-token
       deadline request-id)
  (call-memory-operation
   store :release deadline request-id
   (lambda (now)
     (let* ((key (canonical-target-lock-key identity))
            (id-key
              (idempotency-key :release key owner-principal-id request-id))
            (digest (list lease-id service-instance-id fencing-token))
            (prior (idempotent-result store id-key digest)))
        (cond
          (prior prior)
          ((not
            (and (every #'valid-lease-identifier-p
                        (list lease-id owner-principal-id service-instance-id))
                 (valid-positive-integer-p fencing-token)))
           (outcome :invalid-request))
          (t
           (multiple-value-bind (record expired)
               (record-for-key store key now)
             (let ((result
                     (cond
                       (expired (outcome :expired :lease expired))
                       ((null record) (outcome :expired))
                       ((ownership-outcome
                         record lease-id owner-principal-id service-instance-id
                         fencing-token))
                       (t
                        (remhash key (memory-store-records store))
                        (let ((released (snapshot-record record)))
                          (setf (lease-record-state released) :released)
                          (outcome :released :lease released))))))
               (remember-result store id-key digest result)))))))))

(defmethod get-lease
    ((store memory-lease-store) identity &key deadline request-id)
  (call-memory-operation
   store :get deadline request-id
   (lambda (now)
     (multiple-value-bind (record expired)
         (record-for-key
          store (canonical-target-lock-key identity) now)
       (cond
         (record (outcome :found :lease (snapshot-record record)))
         (expired (outcome :expired :lease expired))
         (t (outcome :not-found)))))))

(defun record-matches-filters-p
    (record owner-principal-id target-id program-id)
  (and
   (or (null owner-principal-id)
       (string= owner-principal-id
                (lease-record-owner-principal-id record)))
   (or (null target-id)
       (string= (normalize-identity-component "target-id" target-id)
                (lease-identity-target-id (lease-record-identity record))))
   (or (null program-id)
       (string= (normalize-identity-component "program-id" program-id)
                (lease-identity-program-id (lease-record-identity record))))))

(defmethod list-leases
    ((store memory-lease-store)
     &key owner-principal-id target-id program-id deadline request-id)
  (call-memory-operation
   store :list deadline request-id
   (lambda (now)
     (if (not (and (valid-lease-filter-p owner-principal-id)
                   (valid-lease-component-filter-p target-id)
                   (valid-lease-component-filter-p program-id)))
         (outcome :invalid-request)
         (let ((records nil))
           (dolist (key
                     (loop for key being the hash-keys
                             of (memory-store-records store)
                           collect key))
             (multiple-value-bind (record expired)
                 (record-for-key store key now)
               (declare (ignore expired))
               (when (and record
                          (record-matches-filters-p
                           record owner-principal-id target-id program-id))
                 (push (snapshot-record record) records))))
           (outcome
            :listed
            :leases
            (sort records #'string< :key #'lease-record-lock-key)))))))

(defmethod revoke-lease
    ((store memory-lease-store) identity
     &key lease-id fencing-token reason deadline request-id)
  (call-memory-operation
   store :revoke deadline request-id
   (lambda (now)
     (let* ((key (canonical-target-lock-key identity))
            (id-key (idempotency-key :revoke key "administrator" request-id))
            (digest (list lease-id fencing-token reason))
            (prior (idempotent-result store id-key digest)))
        (cond
          (prior prior)
          ((not
            (and (valid-lease-identifier-p lease-id)
                 (valid-positive-integer-p fencing-token)
                 (valid-lease-reason-p reason)))
           (outcome :invalid-request))
         (t
          (multiple-value-bind (record expired)
              (record-for-key store key now)
            (let ((result
                    (cond
                      (expired (outcome :expired :lease expired))
                      ((null record) (outcome :not-found))
                      ((or (not (string= lease-id
                                         (lease-record-lease-id record)))
                           (/= fencing-token
                               (lease-record-fencing-token record)))
                       (outcome :stale-token))
                      (t
                       (remhash key (memory-store-records store))
                       (let ((revoked (snapshot-record record)))
                         (setf (lease-record-state revoked) :revoked)
                         (outcome :revoked :lease revoked))))))
              (remember-result store id-key digest result)))))))))

(defmethod backend-health
    ((store memory-lease-store) &key deadline request-id)
  (call-memory-operation
   store :health deadline request-id
   (lambda (now)
     (declare (ignore now))
     (outcome :healthy))))

(defmethod close-lease-store
    ((store memory-lease-store) &key deadline request-id)
  (let ((result
          (if (not (valid-request-shape-p deadline request-id))
              (outcome :invalid-request)
              (bt:with-lock-held ((memory-store-lock store))
                (let ((now (funcall (memory-store-clock store))))
                  (if (<= deadline now)
                      (outcome :timeout :retryable-p t)
                      (progn
                        (setf (memory-store-closed-p store) t)
                        (clrhash (memory-store-records store))
                        (clrhash (memory-store-idempotency store))
                        (outcome :closed))))))))
    (emit-operation-hooks store :close request-id result)))
