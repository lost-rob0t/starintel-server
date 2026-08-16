(in-package :star.frontends.http-api)

(defparameter +bulk-inline-document-limit+ 10)
(defparameter +bulk-inline-deadline-seconds+ 2)
(defparameter +bulk-max-pending-jobs+ 32)
(defparameter +bulk-max-pending-per-principal+ 4)
(defparameter +bulk-worker-count+ 4)

(defvar *bulk-ingest-workers* nil)
(defvar *bulk-ingest-worker-system* nil)
(defvar *bulk-ingest-worker-index* 0)
(defvar *bulk-ingest-jobs* (make-hash-table :test #'equal))
(defvar *bulk-pending-jobs* 0)
(defvar *bulk-pending-by-principal* (make-hash-table :test #'equal))
(defvar *bulk-ingest-lock* (bt:make-lock "bulk-ingest-state"))
(defvar *service-call-context* nil)

(defstruct (bulk-ingest-job
            (:constructor make-bulk-ingest-job
                (&key id principal documents correlation-id service-context
                      submitted-at (status :queued) (succeeded 0) (failed 0)
                      error-code)))
  id
  principal
  documents
  correlation-id
  service-context
  submitted-at
  status
  succeeded
  failed
  error-code)

(defun bulk-job-info-json (job)
  (jsown:new-js
    ("job_id" (bulk-ingest-job-id job))
    ("status" (string-downcase
                (symbol-name (bulk-ingest-job-status job))))
    ("total" (length (bulk-ingest-job-documents job)))
    ("succeeded" (bulk-ingest-job-succeeded job))
    ("failed" (bulk-ingest-job-failed job))
    ("correlation_id" (bulk-ingest-job-correlation-id job))))

(defun release-bulk-job-slot (job)
  (bt:with-lock-held (*bulk-ingest-lock*)
    (decf *bulk-pending-jobs*)
    (let* ((principal (bulk-ingest-job-principal job))
           (count (gethash principal *bulk-pending-by-principal* 0)))
      (if (> count 1)
          (setf (gethash principal *bulk-pending-by-principal*)
                (1- count))
          (remhash principal *bulk-pending-by-principal*)))))

(defun comma-separated-scopes (scopes)
  (format nil "~{~a~^,~}" scopes))

(defun service-context-properties
    (dtype context &optional star.authorization:*current-authorization-decision*)
  "Build server-owned Rabbit properties. Caller-supplied document fields are ignored."
  (let ((properties (list (cons :type dtype)))
        (headers nil))
    (when context
      (push (cons :correlation-id
                  (star.auth:service-call-context-correlation-id context))
            properties)
      (setf headers
            (list
             (cons "x-star-principal-id"
                   (star.auth:service-call-context-principal-id context))
             (cons "x-star-principal-type"
                   (star.auth:service-call-context-principal-type context))
             (cons "x-star-credential-id"
                   (star.auth:service-call-context-credential-id context))
             (cons "x-star-scopes"
                   (comma-separated-scopes
                    (star.auth:service-call-context-scopes context)))
             (cons "x-star-deadline"
                   (princ-to-string
                    (star.auth:service-call-context-deadline context))))))
    (when star.authorization:*current-authorization-decision*
      (setf headers
            (append headers
                    (star.authorization:decision-rabbit-headers
                     star.authorization:*current-authorization-decision*))))
    (when headers
      (push (cons :headers headers) properties))
    (nreverse properties)))

(defun current-publish-service-context ()
  (or *service-call-context*
      (star.auth:current-service-call-context)))

(defun publish-document-unchecked (document)
  (let* ((dtype (jsown:val document "dtype"))
         (routing-key (format nil star.rabbit:+ingest-fmt-key+ dtype))
         (context (current-publish-service-context)))
    (star.actors:publish
     star.actors:*producer-agent*
     :body (jsown:to-json document)
     :routing-key routing-key
     :properties
     (service-context-properties
      dtype
      context
      star.authorization:*current-authorization-decision*))))

(defun publish-document (document)
  "Authorize at the embedded publish boundary before any Rabbit side effect."
  (star.authorization:authorized-publish-document
   document
   #'publish-document-unchecked
   :principal (current-publish-service-context)
   :metadata
   (list :route "internal:rabbit-publish"
         :method "PUBLISH"
         :correlation-id (current-correlation-id))))

(defun execute-bulk-job (job &key (publish-fn #'publish-document))
  (setf (bulk-ingest-job-status job) :running)
  (let ((*service-call-context* (bulk-ingest-job-service-context job)))
    (handler-case
        (progn
          (loop for document in (bulk-ingest-job-documents job)
                do (handler-case
                       (progn
                         (funcall publish-fn document)
                         (incf (bulk-ingest-job-succeeded job)))
                     (error (condition)
                       (log:error
                        "Bulk publish failed job=~a correlation=~a: ~a"
                        (bulk-ingest-job-id job)
                        (bulk-ingest-job-correlation-id job)
                        condition)
                       (incf (bulk-ingest-job-failed job)))))
          (setf (bulk-ingest-job-status job)
                (if (zerop (bulk-ingest-job-failed job))
                    :completed
                    :completed-with-errors)))
      (error (condition)
        (log:error "Bulk job failed job=~a correlation=~a: ~a"
                   (bulk-ingest-job-id job)
                   (bulk-ingest-job-correlation-id job)
                   condition)
        (setf (bulk-ingest-job-status job) :failed
              (bulk-ingest-job-error-code job) "bulk_job_failed"))))
  job)

(defun bulk-worker-handler (job)
  (unwind-protect
       (execute-bulk-job job)
    (release-bulk-job-slot job)))

(defun make-bulk-worker (system index dispatcher)
  (sento.actor-context:actor-of
   system
   :name (format nil "bulk-ingest-worker-~d" index)
   :dispatcher dispatcher
   :receive #'bulk-worker-handler))

(defun start-bulk-ingest-workers (&optional (system star.actors:*sys*))
  (unless system
    (return-from start-bulk-ingest-workers nil))
  (bt:with-lock-held (*bulk-ingest-lock*)
    (unless (and *bulk-ingest-workers*
                 (eq *bulk-ingest-worker-system* system))
      (setf *bulk-ingest-workers*
            (loop for index below +bulk-worker-count+
                  collect
                  (handler-case
                      (make-bulk-worker system index :pinned)
                    (error ()
                      (make-bulk-worker system index :shared))))
            *bulk-ingest-worker-system* system
            *bulk-ingest-worker-index* 0)))
  *bulk-ingest-workers*)

(defun start-bulk-ingest-workers-hook ()
  (start-bulk-ingest-workers star.actors:*sys*))

(nhooks:add-hook star:*actors-start-hook* #'start-bulk-ingest-workers-hook)

(defun submit-bulk-ingest-job (documents principal
                               &key
                                 (tell-fn #'sento.actor:tell)
                                 (ensure-workers-fn
                                   #'start-bulk-ingest-workers))
  "Reserve bounded queue capacity and enqueue one authenticated job."
  (unless (funcall ensure-workers-fn)
    (signal-http-input-error
     503
     "bulk_service_unavailable"
     "Bulk ingest service is not available"))
  (let ((job
          (make-bulk-ingest-job
           :id (cms-ulid:ulid)
           :principal principal
           :documents documents
           :correlation-id (current-correlation-id)
           :service-context (star.auth:current-service-call-context)
           :submitted-at (get-universal-time))))
    (bt:with-lock-held (*bulk-ingest-lock*)
      (when (>= *bulk-pending-jobs* +bulk-max-pending-jobs+)
        (signal-http-input-error
         429
         "bulk_queue_full"
         "Bulk ingest queue is full"))
      (let ((principal-pending
              (gethash principal *bulk-pending-by-principal* 0)))
        (when (>= principal-pending +bulk-max-pending-per-principal+)
          (signal-http-input-error
           429
           "principal_bulk_quota_exceeded"
           "Principal has too many pending bulk jobs"))
        (incf *bulk-pending-jobs*)
        (setf (gethash principal *bulk-pending-by-principal*)
              (1+ principal-pending)))
      (setf (gethash (bulk-ingest-job-id job) *bulk-ingest-jobs*) job)
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
                    (gethash principal *bulk-pending-by-principal* 0)))
              (if (> current 1)
                  (setf (gethash principal *bulk-pending-by-principal*)
                        (1- current))
                  (remhash principal *bulk-pending-by-principal*)))
            (remhash (bulk-ingest-job-id job) *bulk-ingest-jobs*)
            (log:error "Failed to enqueue bulk job correlation=~a: ~a"
                       (current-correlation-id)
                       condition)
            (signal-http-input-error
             503
             "bulk_enqueue_failed"
             "Bulk ingest job could not be queued")))))
    job))

(defun bulk-request-mode (document-count)
  (if (<= document-count +bulk-inline-document-limit+)
      :inline
      :async))

(defun process-inline-bulk (documents)
  (let ((succeeded 0)
        (failed 0))
    (bt:with-timeout (+bulk-inline-deadline-seconds+)
      (loop for document in documents
            do (handler-case
                   (progn
                     (publish-document document)
                     (incf succeeded))
                 (star.authorization:authorization-error (condition)
                   (error condition))
                 (error (condition)
                   (log:error "Inline bulk publish failed correlation=~a: ~a"
                              (current-correlation-id)
                              condition)
                   (incf failed)))))
    (jsown:to-json
     (jsown:new-js
       ("total" (length documents))
       ("succeeded" succeeded)
       ("failed" failed)
       ("correlation_id" (current-correlation-id))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bulk-request-mode
            execute-bulk-job
            submit-bulk-ingest-job
            bulk-ingest-job
            bulk-ingest-job-principal
            bulk-ingest-job-service-context
            bulk-ingest-job-status
            bulk-ingest-job-succeeded
            bulk-ingest-job-failed
            service-context-properties
            publish-document)
          :star.frontends.http-api))
