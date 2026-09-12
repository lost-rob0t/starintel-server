(in-package :star.frontends.http-api)

(defvar *execute-bulk-job-without-idempotency*
  (symbol-function 'execute-bulk-job))

(defun idempotent-bulk-job-p (job)
  (idempotency-job-id-p (bulk-ingest-job-id job)))

(defun idempotency-job-status-string (job)
  (string-downcase (symbol-name (bulk-ingest-job-status job))))

(defun execute-bulk-job (job &key (publish-fn #'publish-document))
  "Execute JOB, persisting keyed job state before and after Rabbit side effects."
  (if (not (idempotent-bulk-job-p job))
      (funcall *execute-bulk-job-without-idempotency*
               job :publish-fn publish-fn)
      (handler-case
          (progn
            ;; If durable status cannot be advanced, publish nothing. A retry
            ;; will see the existing reservation instead of duplicating work.
            (mark-bulk-idempotency-status
             (bulk-ingest-job-id job) "running")
            (let ((result
                    (funcall *execute-bulk-job-without-idempotency*
                             job :publish-fn publish-fn)))
              (handler-case
                  (mark-bulk-idempotency-status
                   (bulk-ingest-job-id result)
                   (idempotency-job-status-string result)
                   :succeeded (bulk-ingest-job-succeeded result)
                   :failed (bulk-ingest-job-failed result)
                   :error-code (bulk-ingest-job-error-code result))
                (error (condition)
                  ;; Publication already happened. Never retry it merely because
                  ;; status persistence failed; leave the durable record running.
                  (log:error
                   "Bulk idempotency completion persistence failed job=~a: ~a"
                   (bulk-ingest-job-id result) condition)))
              result))
        (error (condition)
          (log:error "Keyed bulk job refused before publish job=~a: ~a"
                     (bulk-ingest-job-id job) condition)
          (setf (bulk-ingest-job-status job) :failed
                (bulk-ingest-job-error-code job)
                "bulk_idempotency_unavailable")
          job))))

(defun submit-idempotent-bulk-ingest-job
    (documents principal-id job-id
     &key
       (tell-fn #'sento.actor:tell)
       (ensure-workers-fn #'start-bulk-ingest-workers))
  "Queue DOCUMENTS under caller-owned deterministic JOB-ID."
  (unless (funcall ensure-workers-fn)
    (signal-http-input-error
     503 "bulk_service_unavailable" "Bulk ingest service is not available"))
  (let ((job
          (make-bulk-ingest-job
           :id job-id
           :principal principal-id
           :documents documents
           :correlation-id (current-correlation-id)
           :service-context (star.auth:current-service-call-context)
           :submitted-at (get-universal-time))))
    (bt:with-lock-held (*bulk-ingest-lock*)
      (when (gethash job-id *bulk-ingest-jobs*)
        ;; The durable reservation should make this path a replay before queue
        ;; submission. Treat an in-process duplicate as an invariant violation.
        (signal-http-input-error
         409 "bulk_job_already_queued" "Bulk ingest job is already queued"))
      (when (>= *bulk-pending-jobs* +bulk-max-pending-jobs+)
        (signal-http-input-error
         429 "bulk_queue_full" "Bulk ingest queue is full"))
      (let ((principal-pending
              (gethash principal-id *bulk-pending-by-principal* 0)))
        (when (>= principal-pending +bulk-max-pending-per-principal+)
          (signal-http-input-error
           429
           "principal_bulk_quota_exceeded"
           "Principal has too many pending bulk jobs"))
        (incf *bulk-pending-jobs*)
        (setf (gethash principal-id *bulk-pending-by-principal*)
              (1+ principal-pending)))
      (setf (gethash job-id *bulk-ingest-jobs*) job)
      (let ((worker
              (nth (mod *bulk-ingest-worker-index*
                        (length *bulk-ingest-workers*))
                   *bulk-ingest-workers*)))
        (incf *bulk-ingest-worker-index*)
        (handler-case
            (funcall tell-fn worker job)
          (error (condition)
            (decf *bulk-pending-jobs*)
            (let ((current
                    (gethash principal-id *bulk-pending-by-principal* 0)))
              (if (> current 1)
                  (setf (gethash principal-id *bulk-pending-by-principal*)
                        (1- current))
                  (remhash principal-id *bulk-pending-by-principal*)))
            (remhash job-id *bulk-ingest-jobs*)
            (log:error "Failed to enqueue keyed bulk job correlation=~a: ~a"
                       (current-correlation-id) condition)
            (signal-http-input-error
             503 "bulk_enqueue_failed" "Bulk ingest job could not be queued")))))
    job))

(defun authenticated-principal-id ()
  (or (star.auth:current-principal-id)
      (signal-http-input-error
       401 "authentication_required" "Authentication is required")))

(defun accepted-bulk-response (job-id document-count)
  (jsown:to-json
   (jsown:new-js
     ("status" "accepted")
     ("job_id" job-id)
     ("total" document-count)
     ("status_url" (bulk-idempotency-status-url job-id))
     ("correlation_id" (current-correlation-id)))))

(defun dispatch-unkeyed-bulk (documents principal-id)
  (if (eq :inline (bulk-request-mode (length documents)))
      (process-inline-bulk documents)
      (let ((job (submit-bulk-ingest-job documents principal-id)))
        (setf (lack.response:response-status *response*) 202)
        (accepted-bulk-response (bulk-ingest-job-id job) (length documents)))))

(defun safe-pre-enqueue-rejection-p (condition)
  (and (typep condition 'http-input-error)
       (member (http-input-error-code condition)
               '("bulk_service_unavailable"
                 "bulk_queue_full"
                 "principal_bulk_quota_exceeded")
               :test #'string=)))

(defun dispatch-keyed-inline-bulk (documents record)
  (let ((job-id (jsown:val record "job_id")))
    (handler-case
        (progn
          (mark-bulk-idempotency-status job-id "running")
          (store-bulk-idempotency-inline-result
           job-id
           (process-inline-bulk documents)))
      (error (condition)
        ;; A timeout can happen after a prefix was published. Preserve a hard
        ;; no-retry fence rather than guessing whether the batch was accepted.
        (ignore-errors
          (mark-bulk-idempotency-status job-id "indeterminate"))
        (error condition)))))

(defun dispatch-keyed-async-bulk (documents principal-id record)
  (let* ((job-id (jsown:val record "job_id"))
         (document-count (length documents)))
    (handler-case
        (progn
          (submit-idempotent-bulk-ingest-job documents principal-id job-id)
          (let ((body (accepted-bulk-response job-id document-count)))
            (mark-bulk-idempotency-status job-id "accepted")
            (store-bulk-idempotency-initial-response job-id 202 body)
            (setf (lack.response:response-status *response*) 202)
            body))
      (http-input-error (condition)
        (if (safe-pre-enqueue-rejection-p condition)
            (ignore-errors (reject-bulk-idempotency-reservation job-id))
            (ignore-errors
              (mark-bulk-idempotency-status job-id "indeterminate")))
        (error condition))
      (error (condition)
        (ignore-errors
          (mark-bulk-idempotency-status job-id "indeterminate"))
        (error condition)))))

(defun dispatch-keyed-bulk (documents key principal-id)
  (let ((mode (bulk-request-mode (length documents))))
    (multiple-value-bind (record disposition)
        (reserve-bulk-idempotency documents key principal-id mode)
      (ecase disposition
        (:replay
         (replay-bulk-idempotency-record record))
        (:owner
         (if (eq mode :inline)
             (dispatch-keyed-inline-bulk documents record)
             (dispatch-keyed-async-bulk documents principal-id record)))))))

(defun handle-idempotent-authorized-bulk-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((documents (require-json-array (parse-json-request)))
           (document-count (length documents))
           (metadata (route-policy-metadata "/documents/bulk" "POST"))
           (principal-id (authenticated-principal-id)))
      (when (> document-count star:*bulk-max-documents*)
        (signal-http-input-error
         413
         "bulk_document_limit_exceeded"
         "Bulk request exceeds the configured document limit"
         (jsown:new-js ("requested" document-count)
                       ("maximum" star:*bulk-max-documents*))))
      (loop for document in documents
            for index from 0
            do (validate-document-input document :index index))
      (star.authorization:authorize-bulk-documents!
       documents
       :principal (current-policy-principal)
       :metadata metadata)
      (let ((key (request-bulk-idempotency-key)))
        (if key
            (dispatch-keyed-bulk documents key principal-id)
            (dispatch-unkeyed-bulk documents principal-id))))))

(defun authorized-bulk-job-p (principal-id owner-id)
  (or (star.auth:administrator-principal-p)
      (and (stringp principal-id)
           (stringp owner-id)
           (string= principal-id owner-id))))

(defun handle-durable-bulk-status-route (params)
  (with-http-boundary ()
    (let* ((job-id (query-value params "job-id"))
           (principal-id (authenticated-principal-id))
           (job
             (and job-id
                  (bt:with-lock-held (*bulk-ingest-lock*)
                    (gethash job-id *bulk-ingest-jobs*)))))
      (cond
        (job
         (unless (authorized-bulk-job-p
                  principal-id (bulk-ingest-job-principal job))
           (signal-http-input-error
            404 "bulk_job_not_found" "Bulk ingest job was not found"))
         (jsown:to-json (bulk-job-info-json job)))
        ((idempotency-job-id-p job-id)
         (let ((record (load-bulk-idempotency-by-job-id job-id)))
           (unless (and record
                        (authorized-bulk-job-p
                         principal-id
                         (jsown:val-safe record "principal_id")))
             (signal-http-input-error
              404 "bulk_job_not_found" "Bulk ingest job was not found"))
           (jsown:to-json (bulk-idempotency-status-json record))))
        (t
         (signal-http-input-error
          404 "bulk_job_not_found" "Bulk ingest job was not found"))))))

;; Loaded after the historical/authenticated routes: these are the final
;; production bindings for bulk submission and status lookup.
(setf (ningle:route *app* "/documents/bulk" :method :post)
      #'handle-idempotent-authorized-bulk-route)
(setf (ningle:route *app* "/documents/bulk/:job-id" :method :get)
      #'handle-durable-bulk-status-route)
