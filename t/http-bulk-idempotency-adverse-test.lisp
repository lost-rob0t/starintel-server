(in-package :star-server-tests)

(in-suite http-bulk-idempotency-tests)

(test post-publish-exception-becomes-indeterminate-and-never-reowns-key
  (with-idempotency-test-store
    (let* ((documents (list (idem-doc :id "accepted-before-error")))
           (published 0))
      (multiple-value-bind (record state)
          (star.frontends.http-api::reserve-bulk-idempotency
           documents "post-publish-error" "principal-a" :async)
        (is (eq :owner state))
        (let* ((job-id (jsown:val record "job_id"))
               (job
                 (star.frontends.http-api::make-bulk-ingest-job
                  :id job-id
                  :principal "principal-a"
                  :documents documents
                  :correlation-id "corr-post-publish"
                  :submitted-at 1000))
               (star.frontends.http-api::*execute-bulk-job-without-idempotency*
                 (lambda (worker-job &key publish-fn)
                   ;; Model the hard case: Rabbit accepted a document and then
                   ;; the worker lost certainty before it could return a result.
                   (funcall publish-fn
                            (first
                             (star.frontends.http-api::bulk-ingest-job-documents
                              worker-job)))
                   (error "response lost after acceptance"))))
          (star.frontends.http-api::execute-bulk-job
           job
           :publish-fn
           (lambda (document)
             (declare (ignore document))
             (incf published)))
          (is (= 1 published))
          (let ((persisted
                  (star.frontends.http-api::load-bulk-idempotency-by-job-id
                   job-id)))
            (is (string= "indeterminate" (jsown:val persisted "status"))))
          ;; A retry with the same key/payload must replay the durable fence.
          ;; It must never become owner again and therefore cannot republish.
          (multiple-value-bind (replay replay-state)
              (star.frontends.http-api::reserve-bulk-idempotency
               documents "post-publish-error" "principal-a" :async)
            (declare (ignore replay))
            (is (eq :replay replay-state)))
          (is (= 1 published)))))))
