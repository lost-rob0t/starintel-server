(in-package :star.frontends.http-api)

(defun handle-authenticated-bulk-status-route (params)
  (with-http-boundary ()
    (let* ((job-id (query-value params "job-id"))
           (job
             (and job-id
                  (bt:with-lock-held (*bulk-ingest-lock*)
                    (gethash job-id *bulk-ingest-jobs*))))
           (principal-id (star.auth:current-principal-id)))
      (unless (and job
                   (or (star.auth:administrator-principal-p)
                       (string= principal-id
                                (bulk-ingest-job-principal job))))
        (signal-http-input-error
         404
         "bulk_job_not_found"
         "Bulk ingest job was not found"))
      (jsown:to-json (bulk-job-info-json job)))))

(setf (ningle:route *app* "/documents/bulk/:job-id" :method :get)
      #'handle-authenticated-bulk-status-route)
