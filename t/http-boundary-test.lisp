(in-package :star-server-tests)

(def-suite http-boundary-tests
  :description "HTTP boundary validation, safe errors, and bulk backpressure tests")

(in-suite http-boundary-tests)

(defun make-boundary-document (&key (dtype "host")
                                    (version starintel:+starintel-doc-version+)
                                    (id "boundary-doc-1"))
  (jsown:new-js
    ("_id" id)
    ("dataset" "boundary-tests")
    ("dtype" dtype)
    ("version" version)))

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
              (babel:string-to-octets "{broken" :encoding :utf-8)
              "application/json")))))
    (is condition)
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

(test unsupported-schema-version-is-rejected
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:validate-document-input
              (make-boundary-document :version "999.0")
              :path-dtype "host")))))
    (is (= 422
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "unsupported_schema_version"
                 (star.frontends.http-api:http-input-error-code condition)))))

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

(test asynchronous-bulk-submission-does-not-wait-for-worker
  (let* ((system (sento.actor-system:make-actor-system))
         (worker
           (sento.actor-context:actor-of
            system
            :name "http-boundary-slow-worker"
            :receive (lambda (job)
                       (declare (ignore job))
                       (sleep 1))))
         (star.actors:*sys* system)
         (star.frontends.http-api::*bulk-ingest-workers* (list worker))
         (star.frontends.http-api::*bulk-ingest-worker-system* system)
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
         (started (get-internal-real-time)))
    (unwind-protect
         (let* ((job
                  (star.frontends.http-api:submit-bulk-ingest-job
                   documents
                   "principal-test"))
                (elapsed
                  (/ (- (get-internal-real-time) started)
                     internal-time-units-per-second)))
           (is job)
           (is (< elapsed 0.5))
           (is (eq :queued
                   (star.frontends.http-api:bulk-ingest-job-status job))))
      (sento.actor-context:shutdown system))))

(test per-principal-bulk-quota-is-enforced
  (let* ((system (sento.actor-system:make-actor-system))
         (worker
           (sento.actor-context:actor-of
            system
            :name "http-boundary-quota-worker"
            :receive (lambda (job)
                       (declare (ignore job))
                       (sleep 2))))
         (star.actors:*sys* system)
         (star.frontends.http-api::*bulk-ingest-workers* (list worker))
         (star.frontends.http-api::*bulk-ingest-worker-system* system)
         (star.frontends.http-api::*bulk-ingest-worker-index* 0)
         (star.frontends.http-api::*bulk-ingest-jobs*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-pending-jobs* 0)
         (star.frontends.http-api::*bulk-pending-by-principal*
           (make-hash-table :test #'equal))
         (star.frontends.http-api::*bulk-ingest-lock*
           (bt:make-lock "http-boundary-quota-lock"))
         (star.frontends.http-api::*http-correlation-id* "corr-quota")
         (documents (list (make-boundary-document))))
    (unwind-protect
         (progn
           (dotimes (index 4)
             (declare (ignore index))
             (star.frontends.http-api:submit-bulk-ingest-job
              documents
              "quota-principal"))
           (let ((condition
                   (capture-http-input-error
                    (lambda ()
                      (star.frontends.http-api:submit-bulk-ingest-job
                       documents
                       "quota-principal")))))
             (is (= 429
                    (star.frontends.http-api:http-input-error-status
                     condition)))
             (is (string=
                  "principal_bulk_quota_exceeded"
                  (star.frontends.http-api:http-input-error-code
                   condition)))))
      (sento.actor-context:shutdown system))))
