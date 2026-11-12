(in-package :star-server-tests)

(def-suite http-boundary-tests
  :description "HTTP boundary validation, safe errors, and bulk backpressure tests")

(in-suite http-boundary-tests)

(defun make-boundary-document (&key (dtype "host")
                                    (schema-version starintel.legacy:+starintel-doc-version+)
                                    (version 1)
                                    (id "boundary-doc-1"))
  (let ((document
          (starintel.legacy:encode
           (starintel.legacy:new-host
            "boundary-tests"
            :ip "192.0.2.10"
            :os "linux"))))
    (setf (jsown:val document "_id") id
          (jsown:val document "dtype") dtype
          (jsown:val document "schema_version") schema-version
          (jsown:val document "version") version)
    document))

(defun capture-http-input-error (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.frontends.http-api:http-input-error (condition)
      condition)))

(test jsown-object-is-not-a-bulk-array
  (let ((object (jsown:new-js ("dtype" "host")))
        (array (list (jsown:new-js ("dtype" "host")))))
    (is-true (star.frontends.http-api:json-object-p object))
    (is-false (star.frontends.http-api:json-array-p object))
    (is-true (star.frontends.http-api:json-array-p array))))

(test malformed-json-is-a-400-client-error
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:parse-json-octets
              (babel:string-to-octets "{" :encoding :utf-8)
              "application/json")))))
    (is-true condition)
    (is (= 400
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "malformed_json"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test non-json-content-type-is-rejected
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:parse-json-octets
              (babel:string-to-octets "{}" :encoding :utf-8)
              "text/plain")))))
    (is (= 415
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "unsupported_media_type"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test oversized-request-body-is-rejected-before-parsing
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:parse-json-octets
              (make-array 17
                          :element-type '(unsigned-byte 8)
                          :initial-element 32)
              "application/json"
              :max-bytes 16)))))
    (is (= 413
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "request_body_too_large"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test missing-and-mismatched-dtype-return-422
  (let* ((missing (make-boundary-document))
         (mismatch (make-boundary-document :dtype "email")))
    (jsown:remkey missing "dtype")
    (let ((missing-condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input
                missing
                :path-dtype "host"))))
          (mismatch-condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input
                mismatch
                :path-dtype "host")))))
      (is (= 422
             (star.frontends.http-api:http-input-error-status
              missing-condition)))
      (is (= 422
             (star.frontends.http-api:http-input-error-status
              mismatch-condition)))
      (is (string= "dtype_mismatch"
                    (star.frontends.http-api:http-input-error-code
                     mismatch-condition))))))

(test canonical-v09-document-passes-strict-validation
  (let ((document (make-boundary-document)))
    (is (eq document
            (star.frontends.http-api:validate-document-input
             document
             :path-dtype "host")))))

(test document-revision-is-independent-from-schema-revision
  (let ((document (make-boundary-document :version 7)))
    (is (eq document
            (star.frontends.http-api:validate-document-input
             document
             :path-dtype "host")))
    (is (= 7 (jsown:val document "version")))
    (is (string= "0.9.0" (jsown:val document "schema_version")))))

(test version-string-does-not-substitute-for-schema-version
  (let ((document (make-boundary-document)))
    (jsown:remkey document "schema_version")
    (setf (jsown:val document "version") "0.9.0")
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input
                document
                :path-dtype "host")))))
      (is (= 422
             (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "schema_version_required"
                   (star.frontends.http-api:http-input-error-code condition))))))

(test unsupported-schema-version-is-rejected
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:validate-document-input
              (make-boundary-document :schema-version "999.0")
              :path-dtype "host")))))
    (is (= 422
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "unsupported_schema_version"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test undeclared-top-level-field-is-rejected
  (let ((document (make-boundary-document)))
    (setf (jsown:val document "legacy_flat_field") "not-v09")
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input
                document
                :path-dtype "host")))))
      (is (= 422
             (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "invalid_document_schema"
                   (star.frontends.http-api:http-input-error-code condition))))))

(test dtype-data-schema-mismatch-is-rejected
  (let ((document (make-boundary-document :dtype "email")))
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input document)))))
      (is (= 422
             (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "invalid_document_schema"
                   (star.frontends.http-api:http-input-error-code condition))))))

(test target-boundary-folds-legacy-envelope-and-validates-strictly
  (let* ((document
           (jsown:new-js
             ("_id" "legacy-target-1")
             ("dataset" "star-intel")
             ("target" "example.invalid")
             ("delay" 30)
             ("recurring" :false)))
         (normalized
           (star.frontends.http-api::normalize-legacy-target-document
            document "nmap")))
    (is (eq document normalized))
    (is (eq document
            (star.frontends.http-api:validate-document-input
             document :path-dtype "target")))
    (let ((data (jsown:val document "data")))
      (is (string= "nmap" (jsown:val data "actor")))
      (is (string= "example.invalid" (jsown:val data "target")))
      (is (= 30 (jsown:val data "delay")))
      (is (eq :false (jsown:val data "recurring"))))
    ;; The historical top-level fields are gone from the closed envelope.
    (dolist (key '("actor" "target" "delay" "recurring" "options"))
      (is-false (jsown:keyp document key)))
    ;; Missing envelope fields were defaulted to schema-valid values.
    (is (string= "0.9.0" (jsown:val document "schema_version")))
    (is (= 1 (jsown:val document "version")))
    (is (stringp (jsown:val document "date_added")))
    (is (stringp (jsown:val document "date_updated")))
    (is (vectorp (jsown:val document "sources")))
    (is (vectorp (jsown:val document "evidence")))
    (is (string= "target" (jsown:val document "dtype")))))

(test target-boundary-preserves-caller-provided-data
  (let* ((document
           (jsown:new-js
             ("_id" "legacy-target-2")
             ("dataset" "star-intel")
             ("target" "caller-top-level.invalid")
             ("data" (jsown:new-js ("target" "caller-data.invalid")
                                   ("options" #("opt-a")))))))
    (star.frontends.http-api::normalize-legacy-target-document
     document "subfinder")
    ;; Explicit data wins over the historical top-level field; the URL
    ;; actor remains authoritative.
    (let ((data (jsown:val document "data")))
      (is (string= "caller-data.invalid" (jsown:val data "target")))
      (is (string= "subfinder" (jsown:val data "actor")))
      (is (= 1 (length (jsown:val data "options")))))
    (is-false (jsown:keyp document "target"))))

(test target-boundary-rejects-undeclared-legacy-fields
  (let ((document
          (jsown:new-js
            ("_id" "legacy-target-3")
            ("dataset" "star-intel")
            ("target" "example.invalid")
            ("legacy_flat_field" "not-v09"))))
    (star.frontends.http-api::normalize-legacy-target-document
     document "nmap")
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:validate-document-input
                document :path-dtype "target")))))
      (is (= 422
             (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "invalid_document_schema"
                   (star.frontends.http-api:http-input-error-code condition))))))

(test put-document-route-requires-write-capability
  (is (string= "documents:write"
               (star.frontends.http-api::route-action
                :put "/document/example"))))

(test client-status-envelopes-never-expose-tracebacks
  (let* ((star.frontends.http-api::*http-correlation-id* "corr-test")
         (body
           (star.frontends.http-api::status-msg
            "Bad Request"
            'error
            :code "invalid_request"
            :traceback "password=super-secret internal stack"))
         (parsed (jsown:parse body)))
    (is-false (jsown:keyp parsed "trace"))
    (is (null (search "super-secret" body :test #'char-equal)))
    (is (string= "corr-test" (jsown:val parsed "correlation_id")))))

(test numeric-query-validation-is-bounded
  (dolist (raw '("not-a-number" "0" "101"))
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:bounded-query-integer
                (list (cons "limit" raw))
                "limit"
                :minimum 1
                :maximum 100)))))
      (is (= 400
             (star.frontends.http-api:http-input-error-status condition)))))
  (is (= 25
         (star.frontends.http-api:bounded-query-integer
          nil
          "limit"
          :default 25
          :minimum 1
          :maximum 100))))

(test oversized-bulk-selects-asynchronous-dispatch
  (is (eq :inline
          (star.frontends.http-api:bulk-request-mode 10)))
  (is (eq :async
          (star.frontends.http-api:bulk-request-mode 11))))

(defmacro with-isolated-boundary-bulk-jobs (&body body)
  `(let* ((star.actors:*sys* (list :boundary-bulk-test-system))
          (star.frontends.http-api::*bulk-ingest-workers* (list :fake-worker))
          (star.frontends.http-api::*bulk-ingest-worker-system* star.actors:*sys*)
          (star.frontends.http-api::*bulk-ingest-worker-index* 0)
          (star.frontends.http-api::*bulk-ingest-jobs*
            (make-hash-table :test #'equal))
          (star.frontends.http-api::*bulk-pending-jobs* 0)
          (star.frontends.http-api::*bulk-pending-by-principal*
            (make-hash-table :test #'equal))
          (star.frontends.http-api::*bulk-ingest-lock*
            (bt:make-lock "http-boundary-retention-test-lock"))
          (star.frontends.http-api::*http-correlation-id* "corr-retention")
          (star.auth:*request-security-context*
            (star.auth::%make-request-security-context
             :principal (star.auth::%make-request-principal
                         :id "retention-owner" :type "api_client"
                         :scopes '("documents:read"))
             :correlation-id "corr-retention")))
     ;; Do not intern a candidate-only special when compiling the RED baseline.
     (let ((terminal-ids
             (find-symbol "*BULK-TERMINAL-JOB-IDS*" :star.frontends.http-api)))
       (progv (if terminal-ids (list terminal-ids) nil)
              (if terminal-ids (list nil) nil)
         ,@body))))

(defun submit-boundary-bulk-job ()
  (let* ((captured nil)
         (job
           (star.frontends.http-api:submit-bulk-ingest-job
            (list (make-boundary-document))
            "retention-owner"
            :ensure-workers-fn
            (lambda () star.frontends.http-api::*bulk-ingest-workers*)
            :tell-fn
            (lambda (worker queued-job)
              (declare (ignore worker))
              (setf captured queued-job)))))
    (is (eq job captured))
    job))

(defun finish-boundary-bulk-job (job &optional (scenario :completed) publish-fn)
  (when (eq scenario :failed)
    (setf (star.frontends.http-api::bulk-ingest-job-documents job)
          (cons (first (star.frontends.http-api::bulk-ingest-job-documents job))
                :invalid-tail)))
  ;; Submission has returned: neither execution nor release runs under enqueue's lock.
  (unwind-protect
       (star.frontends.http-api:execute-bulk-job
        job :publish-fn
        (or publish-fn
            (lambda (document)
              (declare (ignore document))
              (when (eq scenario :completed-with-errors)
                (error "Synthetic per-document publish failure")))))
    (star.frontends.http-api::release-bulk-job-slot job)))

(defun poll-boundary-bulk-job (job principal-id &optional administrator-p)
  (let* ((star.auth:*request-security-context*
           (star.auth::%make-request-security-context
            :principal (star.auth::%make-request-principal
                        :id principal-id
                        :type (if administrator-p "administrator" "api_client")
                        :scopes '("documents:read"))))
         (response
           (lack.component:call
            star.frontends.http-api:*app*
            (list :request-method :get
                  :path-info (format nil "/documents/bulk/~a"
                                     (star.frontends.http-api::bulk-ingest-job-id job))
                  :url-scheme "http" :server-name "127.0.0.1"
                  :server-port 5000 :remote-addr "127.0.0.1"
                  :headers (make-hash-table :test #'equal))))
         (body (third response)))
    (values (first response)
            (jsown:parse (if (consp body)
                             (apply #'concatenate 'string body)
                             body)))))

(test terminal-bulk-jobs-release-request-payloads-but-remain-pollable
  (with-isolated-boundary-bulk-jobs
    (dolist (scenario '(:completed :completed-with-errors :failed))
      (let* ((documents
               (list (make-boundary-document :id "terminal-doc-1")
                     (make-boundary-document :id "terminal-doc-2")))
             (job
               (star.frontends.http-api::make-bulk-ingest-job
                :id "terminal-job"
                :principal "terminal-principal"
                :documents documents
                :correlation-id "terminal-correlation"
                :service-context '(:request-only))))
        (when (eq scenario :failed)
          ;; A malformed sequence exercises the outer job failure handler.
          (setf (star.frontends.http-api::bulk-ingest-job-documents job)
                (cons (first documents) :invalid-tail)))
        (star.frontends.http-api:execute-bulk-job
         job
         :publish-fn
         (lambda (document)
           (declare (ignore document))
           (when (eq scenario :completed-with-errors)
             (error "Synthetic per-document publish failure"))))
        (let ((info (star.frontends.http-api::bulk-job-info-json job)))
          (is (eq scenario
                  (star.frontends.http-api::bulk-ingest-job-status job)))
          (is (= 2 (jsown:val info "total")))
          (is (string= "terminal-job" (jsown:val info "job_id")))
          (is (string= "terminal-correlation"
                       (jsown:val info "correlation_id")))
          (is (string= "terminal-principal"
                       (star.frontends.http-api::bulk-ingest-job-principal job)))
          (is (= (case scenario
                   (:completed 2)
                   (:failed 1)
                   (otherwise 0))
                 (jsown:val info "succeeded")))
          (is (= (if (eq scenario :completed-with-errors) 2 0)
                 (jsown:val info "failed")))
          (is (null (star.frontends.http-api::bulk-ingest-job-documents job)))
          (is (null (star.frontends.http-api::bulk-ingest-job-service-context
                     job))))))))

(test asynchronous-bulk-submission-does-not-wait-for-worker
  (let* ((fake-system (list :test-system))
         (sent-worker nil)
         (sent-job nil)
         (star.actors:*sys* fake-system)
         (star.frontends.http-api::*bulk-ingest-workers*
           (list :fake-worker))
         (star.frontends.http-api::*bulk-ingest-worker-system* fake-system)
         (star.frontends.http-api::*bulk-ingest-worker-index* 0)
         (star.frontends.http-api::*bulk-ingest-jobs*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-pending-jobs* 0)
         (star.frontends.http-api::*bulk-pending-by-principal*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-ingest-lock*
           (bt:make-lock "http-boundary-test-lock"))
         (star.frontends.http-api::*http-correlation-id* "corr-async")
         (documents
           (loop for index below 11
                 collect
                 (make-boundary-document
                  :id (format nil "async-doc-~d" index))))
         (started (get-internal-real-time))
         (job
           (star.frontends.http-api:submit-bulk-ingest-job
            documents
            "principal-test"
            :ensure-workers-fn
            (lambda ()
              star.frontends.http-api::*bulk-ingest-workers*)
            :tell-fn
            (lambda (worker queued-job)
              (setf sent-worker worker
                    sent-job queued-job))))
         (elapsed
           (/ (- (get-internal-real-time) started)
              internal-time-units-per-second)))
    (is-true job)
    (is (< elapsed 0.5))
    (is (eq :fake-worker sent-worker))
    (is (eq job sent-job))
    (is (eq :queued
            (star.frontends.http-api:bulk-ingest-job-status job)))))

(test per-principal-bulk-quota-is-enforced
  (let* ((fake-system (list :test-system))
         (star.actors:*sys* fake-system)
         (star.frontends.http-api::*bulk-ingest-workers*
           (list :fake-worker))
         (star.frontends.http-api::*bulk-ingest-worker-system* fake-system)
         (star.frontends.http-api::*bulk-ingest-worker-index* 0)
         (star.frontends.http-api::*bulk-ingest-jobs*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-pending-jobs* 0)
         (star.frontends.http-api::*bulk-pending-by-principal*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-ingest-lock*
           (bt:make-lock "http-boundary-quota-lock"))
         (star.frontends.http-api::*http-correlation-id* "corr-quota")
         (documents (list (make-boundary-document)))
         (ensure-workers
           (lambda ()
             star.frontends.http-api::*bulk-ingest-workers*))
         (tell-noop
           (lambda (worker job)
             (declare (ignore worker job)))))
    (dotimes (index 4)
      (declare (ignore index))
      (star.frontends.http-api:submit-bulk-ingest-job
       documents
       "quota-principal"
       :ensure-workers-fn ensure-workers
       :tell-fn tell-noop))
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api:submit-bulk-ingest-job
                documents
                "quota-principal"
                :ensure-workers-fn ensure-workers
                :tell-fn tell-noop)))))
      (is (= 429
             (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "principal_bulk_quota_exceeded"
                    (star.frontends.http-api:http-input-error-code
                     condition))))))

(test terminal-bulk-jobs-are-evicted-after-bounded-retention
  (with-isolated-boundary-bulk-jobs
    (let ((finished nil)
          (oldest-by-status nil)
          (recent-by-status nil))
      (labels ((complete-wave (count)
               (dotimes (index count)
                 (let* ((scenario (case index
                                    ((0 285) :completed-with-errors)
                                    ((1 286) :failed)
                                    (otherwise :completed)))
                        (job (submit-boundary-bulk-job)))
                   (finish-boundary-bulk-job job scenario)
                   (push job finished)
                   (unless (getf oldest-by-status scenario)
                     (setf (getf oldest-by-status scenario) job))
                   (setf (getf recent-by-status scenario) job))))
               (assert-retention-window ()
                 (let ((registry star.frontends.http-api::*bulk-ingest-jobs*))
                   (is (= 256 (hash-table-count registry)))
                   (is (every
                        (lambda (job)
                          (eq job (gethash
                                   (star.frontends.http-api::bulk-ingest-job-id job)
                                   registry)))
                        (subseq finished 0 256)))
                   (is (every
                        (lambda (job)
                          (null (gethash
                                 (star.frontends.http-api::bulk-ingest-job-id job)
                                 registry)))
                        (nthcdr 256 finished))))
                 (is (every
                      (lambda (job)
                        (and (null (star.frontends.http-api::bulk-ingest-job-documents
                                    job))
                             (null (star.frontends.http-api::bulk-ingest-job-service-context
                                    job))))
                      finished))
                 (is (zerop star.frontends.http-api::*bulk-pending-jobs*))
                 (is (zerop (hash-table-count
                             star.frontends.http-api::*bulk-pending-by-principal*)))))
        (complete-wave 288)
        (assert-retention-window)
        (dolist (scenario '(:completed :completed-with-errors :failed))
          (let ((oldest (getf oldest-by-status scenario))
                (recent (getf recent-by-status scenario)))
            (is (null (gethash
                       (star.frontends.http-api::bulk-ingest-job-id oldest)
                       star.frontends.http-api::*bulk-ingest-jobs*)))
            (is (eq scenario (star.frontends.http-api:bulk-ingest-job-status recent)))
            (multiple-value-bind (status info)
                (poll-boundary-bulk-job recent "retention-owner")
              (is (= 200 status))
              (is (string= (string-downcase (symbol-name scenario))
                           (jsown:val info "status")))
              (is (= 1 (jsown:val info "total")))
              (is (= (if (eq scenario :completed-with-errors) 0 1)
                     (jsown:val info "succeeded")))
              (is (= (if (eq scenario :completed-with-errors) 1 0)
                     (jsown:val info "failed"))))))
        (complete-wave 64)
        (assert-retention-window)))))

(test terminal-bulk-retention-preserves-queued-and-running-jobs
  (with-isolated-boundary-bulk-jobs
    (let ((queued (submit-boundary-bulk-job))
          (running (submit-boundary-bulk-job))
          (published nil))
      (finish-boundary-bulk-job
       running :completed
       (lambda (document)
         (declare (ignore document))
         (setf published t)
         (is (eq :running (star.frontends.http-api:bulk-ingest-job-status running)))
         ;; Other jobs finish while this registered job is inside its publish callback.
         (dotimes (index 288)
           (declare (ignore index))
           (finish-boundary-bulk-job (submit-boundary-bulk-job)))
         (let ((registry star.frontends.http-api::*bulk-ingest-jobs*))
           (is (= 258 (hash-table-count registry)))
           (is (eq queued
                   (gethash (star.frontends.http-api::bulk-ingest-job-id queued)
                            registry)))
           (is (eq running
                   (gethash (star.frontends.http-api::bulk-ingest-job-id running)
                            registry))))
         (is (eq :queued (star.frontends.http-api:bulk-ingest-job-status queued)))
         (is (= 2 star.frontends.http-api::*bulk-pending-jobs*))))
      (is-true published)
      (is (eq :completed (star.frontends.http-api:bulk-ingest-job-status running)))
      (is (= 257 (hash-table-count star.frontends.http-api::*bulk-ingest-jobs*)))
      (finish-boundary-bulk-job queued)
      (is (= 256 (hash-table-count star.frontends.http-api::*bulk-ingest-jobs*)))
      (is (zerop star.frontends.http-api::*bulk-pending-jobs*)))))

(test retained-bulk-status-polling-preserves-ownership
  (with-isolated-boundary-bulk-jobs
    (let ((oldest (submit-boundary-bulk-job))
          (newest nil))
      (finish-boundary-bulk-job oldest)
      (dotimes (index 256)
        (declare (ignore index))
        (setf newest (submit-boundary-bulk-job))
        (finish-boundary-bulk-job newest))
      (dolist (reader '(("retention-owner" nil 200)
                        ("other-principal" nil 404)
                        ("retention-admin" t 200)))
        (destructuring-bind (principal administrator-p expected) reader
          (multiple-value-bind (status info)
              (poll-boundary-bulk-job newest principal administrator-p)
            (is (= expected status))
            (if (= expected 200)
                (progn
                  (is (string= (star.frontends.http-api::bulk-ingest-job-id newest)
                               (jsown:val info "job_id")))
                  (is (string= "completed" (jsown:val info "status")))
                  (is (string= "corr-retention" (jsown:val info "correlation_id"))))
                (is (string= "bulk_job_not_found" (jsown:val info "code")))))
          (multiple-value-bind (status info)
              (poll-boundary-bulk-job oldest principal administrator-p)
            (is (= 404 status))
            (is (string= "bulk_job_not_found" (jsown:val info "code")))))))))

(test terminal-bulk-finalization-is-idempotent
  (with-isolated-boundary-bulk-jobs
    (let ((oldest (submit-boundary-bulk-job)))
      (finish-boundary-bulk-job oldest)
      (dotimes (index 255)
        (declare (ignore index))
        (finish-boundary-bulk-job (submit-boundary-bulk-job)))
      ;; Re-delivery must neither publish again nor refresh completion order.
      (star.frontends.http-api:execute-bulk-job
       oldest :publish-fn (lambda (document)
                            (declare (ignore document))
                            (error "A terminal job must not execute again")))
      (is (eq :completed (star.frontends.http-api:bulk-ingest-job-status oldest)))
      (is (= 1 (star.frontends.http-api::bulk-ingest-job-succeeded oldest)))
      (finish-boundary-bulk-job (submit-boundary-bulk-job))
      (is (= 256 (hash-table-count star.frontends.http-api::*bulk-ingest-jobs*)))
      (is (null (gethash (star.frontends.http-api::bulk-ingest-job-id oldest)
                        star.frontends.http-api::*bulk-ingest-jobs*))))))
