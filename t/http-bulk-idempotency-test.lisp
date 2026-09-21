(in-package :star-server-tests)

(def-suite http-bulk-idempotency-tests
  :description "Durable bulk idempotency, concurrency, and fail-closed tests")

(in-suite http-bulk-idempotency-tests)

(defvar *idempotency-test-store* nil)
(defvar *idempotency-test-store-lock* nil)
(defvar *idempotency-test-revision* 0)

(defun idem-clone (value)
  (star.frontends.http-api::clone-json-value value))

(defun idem-test-load (record-id)
  (bt:with-lock-held (*idempotency-test-store-lock*)
    (let ((record (gethash record-id *idempotency-test-store*)))
      (and record (idem-clone record)))))

(defun idem-test-save (record)
  "Tiny in-memory CouchDB CAS model used by unit tests."
  (bt:with-lock-held (*idempotency-test-store-lock*)
    (let* ((id (jsown:val record "_id"))
           (existing (gethash id *idempotency-test-store*))
           (provided-rev (jsown:val-safe record "_rev"))
           (existing-rev (and existing (jsown:val-safe existing "_rev"))))
      (when (or (and existing (not (equal provided-rev existing-rev)))
                (and (null existing) provided-rev))
        (error 'star.frontends.http-api::bulk-idempotency-store-conflict))
      (let ((saved (idem-clone record)))
        (incf *idempotency-test-revision*)
        (setf (jsown:val saved "_rev")
              (format nil "~d-test" *idempotency-test-revision*))
        (setf (gethash id *idempotency-test-store*) (idem-clone saved))
        saved))))

(defmacro with-idempotency-test-store (&body body)
  `(let ((*idempotency-test-store* (make-hash-table :test #'equal))
         (*idempotency-test-store-lock* (bt:make-lock "idem-test-store"))
         (*idempotency-test-revision* 0)
         (star.frontends.http-api::*bulk-idempotency-load-fn* #'idem-test-load)
         (star.frontends.http-api::*bulk-idempotency-save-fn* #'idem-test-save)
         (star.frontends.http-api::*bulk-idempotency-now-fn* (lambda () 1000))
         (star.frontends.http-api::*http-correlation-id* "corr-idem-test"))
     ,@body))

(defun idem-doc (&key (id "idem-doc") (tenant "tenant-a") (value 1))
  (jsown:new-js
    ("_id" id)
    ("tenant_id" tenant)
    ("value" value)))

(defun capture-idempotency-http-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.frontends.http-api::http-input-error (condition)
      condition)))

(test canonical-object-order-hashes-identically
  (let* ((left (jsown:new-js ("z" 1) ("a" 2)))
         (right (jsown:new-js ("a" 2) ("z" 1)))
         (left-hash
           (star.frontends.http-api::bulk-request-fingerprint (list left)))
         (right-hash
           (star.frontends.http-api::bulk-request-fingerprint (list right))))
    (is (string= left-hash right-hash))))

(test same-key-and-payload-replays-one-reservation
  (with-idempotency-test-store
    (let ((documents (list (idem-doc))))
      (multiple-value-bind (first first-state)
          (star.frontends.http-api::reserve-bulk-idempotency
           documents "request-1" "principal-a" :inline)
        (multiple-value-bind (second second-state)
            (star.frontends.http-api::reserve-bulk-idempotency
             documents "request-1" "principal-a" :inline)
          (is (eq :owner first-state))
          (is (eq :replay second-state))
          (is (string= (jsown:val first "job_id")
                       (jsown:val second "job_id")))
          (is (= 1 (hash-table-count *idempotency-test-store*))))))))

(test same-key-different-payload-is-409
  (with-idempotency-test-store
    (star.frontends.http-api::reserve-bulk-idempotency
     (list (idem-doc :value 1)) "request-2" "principal-a" :inline)
    (let ((condition
            (capture-idempotency-http-error
             (lambda ()
               (star.frontends.http-api::reserve-bulk-idempotency
                (list (idem-doc :value 2))
                "request-2" "principal-a" :inline)))))
      (is condition)
      (is (= 409
             (star.frontends.http-api::http-input-error-status condition)))
      (is (string= "idempotency_key_reused"
                   (star.frontends.http-api::http-input-error-code condition))))))

(test idempotency-key-is-scoped-by-principal
  (with-idempotency-test-store
    (let ((documents (list (idem-doc))))
      (multiple-value-bind (a state-a)
          (star.frontends.http-api::reserve-bulk-idempotency
           documents "shared-key" "principal-a" :inline)
        (multiple-value-bind (b state-b)
            (star.frontends.http-api::reserve-bulk-idempotency
             documents "shared-key" "principal-b" :inline)
          (is (eq :owner state-a))
          (is (eq :owner state-b))
          (is-false (string= (jsown:val a "job_id")
                             (jsown:val b "job_id"))))))))

(test idempotency-key-is-scoped-by-tenant-set
  (with-idempotency-test-store
    (multiple-value-bind (a state-a)
        (star.frontends.http-api::reserve-bulk-idempotency
         (list (idem-doc :tenant "tenant-a"))
         "shared-key" "principal-a" :inline)
      (multiple-value-bind (b state-b)
          (star.frontends.http-api::reserve-bulk-idempotency
           (list (idem-doc :tenant "tenant-b"))
           "shared-key" "principal-a" :inline)
        (is (eq :owner state-a))
        (is (eq :owner state-b))
        (is-false (string= (jsown:val a "job_id")
                           (jsown:val b "job_id")))))))

(test completed-expired-key-can-be-reused
  (with-idempotency-test-store
    (let ((documents (list (idem-doc))))
      (multiple-value-bind (record state)
          (star.frontends.http-api::reserve-bulk-idempotency
           documents "expiry-key" "principal-a" :inline)
        (is (eq :owner state))
        (star.frontends.http-api::mark-bulk-idempotency-status
         (jsown:val record "job_id") "completed"
         :succeeded 1 :failed 0)
        (let ((star.frontends.http-api::*bulk-idempotency-now-fn*
                (lambda () (+ 1000
                              star.frontends.http-api::*bulk-idempotency-ttl-seconds*
                              1))))
          (multiple-value-bind (replacement replacement-state)
              (star.frontends.http-api::reserve-bulk-idempotency
               documents "expiry-key" "principal-a" :inline)
            (declare (ignore replacement))
            (is (eq :owner replacement-state))))))))

(test unresolved-expired-key-remains-a-no-retry-fence
  (with-idempotency-test-store
    (let ((documents (list (idem-doc))))
      (star.frontends.http-api::reserve-bulk-idempotency
       documents "uncertain-key" "principal-a" :inline)
      (let ((star.frontends.http-api::*bulk-idempotency-now-fn*
              (lambda () (+ 1000
                            star.frontends.http-api::*bulk-idempotency-ttl-seconds*
                            1))))
        (multiple-value-bind (record state)
            (star.frontends.http-api::reserve-bulk-idempotency
             documents "uncertain-key" "principal-a" :inline)
          (declare (ignore record))
          (is (eq :replay state)))))))

(test concurrent-duplicates-have-exactly-one-owner
  (with-idempotency-test-store
    (let* ((documents (list (idem-doc)))
           (result-lock (bt:make-lock "idem-results"))
           (states nil)
           (threads
             (loop repeat 8
                   collect
                   (bt:make-thread
                    (lambda ()
                      (multiple-value-bind (record state)
                          (star.frontends.http-api::reserve-bulk-idempotency
                           documents "race-key" "principal-a" :inline)
                        (declare (ignore record))
                        (bt:with-lock-held (result-lock)
                          (push state states))))))))
      (dolist (thread threads)
        (bt:join-thread thread))
      (is (= 1 (count :owner states)))
      (is (= 7 (count :replay states)))
      (is (= 1 (hash-table-count *idempotency-test-store*))))))

(test persisted-record-survives-in-memory-job-loss
  (with-idempotency-test-store
    (multiple-value-bind (record state)
        (star.frontends.http-api::reserve-bulk-idempotency
         (list (idem-doc)) "restart-key" "principal-a" :async)
      (is (eq :owner state))
      (let ((job-id (jsown:val record "job_id"))
            (star.frontends.http-api::*bulk-ingest-jobs*
              (make-hash-table :test #'equal)))
        (star.frontends.http-api::mark-bulk-idempotency-status
         job-id "accepted")
        (let ((loaded
                (star.frontends.http-api::load-bulk-idempotency-by-job-id
                 job-id)))
          (is loaded)
          (is (string= "accepted" (jsown:val loaded "status")))
          (is (string= "principal-a" (jsown:val loaded "principal_id"))))))))

(test worker-publishes-keyed-job-once-and-persists-result
  (with-idempotency-test-store
    (let* ((documents (list (idem-doc :id "one") (idem-doc :id "two")))
           (published 0))
      (multiple-value-bind (record state)
          (star.frontends.http-api::reserve-bulk-idempotency
           documents "worker-key" "principal-a" :inline)
        (is (eq :owner state))
        (let* ((job-id (jsown:val record "job_id"))
               (job
                 (star.frontends.http-api::make-bulk-ingest-job
                  :id job-id
                  :principal "principal-a"
                  :documents documents
                  :correlation-id "corr-worker"
                  :submitted-at 1000)))
          (star.frontends.http-api::execute-bulk-job
           job
           :publish-fn
           (lambda (document)
             (declare (ignore document))
             (incf published)))
          (is (= 2 published))
          (let ((persisted
                  (star.frontends.http-api::load-bulk-idempotency-by-job-id
                   job-id)))
            (is (string= "completed" (jsown:val persisted "status")))
            (is (= 2 (jsown:val persisted "succeeded")))
            (is (= 0 (jsown:val persisted "failed"))))
          (multiple-value-bind (replay replay-state)
              (star.frontends.http-api::reserve-bulk-idempotency
               documents "worker-key" "principal-a" :inline)
            (declare (ignore replay))
            (is (eq :replay replay-state)))
          (is (= 2 published)))))))

(test persistence-failure-before-worker-run-publishes-nothing
  (let* ((documents (list (idem-doc)))
         (published 0)
         (job-id
           (star.frontends.http-api::idempotency-job-id
            "principal-a" '("tenant-a") "unavailable-key"))
         (job
           (star.frontends.http-api::make-bulk-ingest-job
            :id job-id
            :principal "principal-a"
            :documents documents
            :correlation-id "corr-fail"
            :submitted-at 1000))
         (star.frontends.http-api::*bulk-idempotency-load-fn*
           (lambda (record-id)
             (declare (ignore record-id))
             (error "store unavailable"))))
    (star.frontends.http-api::execute-bulk-job
     job
     :publish-fn
     (lambda (document)
       (declare (ignore document))
       (incf published)))
    (is (= 0 published))
    (is (eq :failed (star.frontends.http-api::bulk-ingest-job-status job)))))
