(in-package :star.frontends.http-api)

(defun handle-new-document-route (params)
  (with-http-boundary ()
    (let* ((path-dtype (query-value params "dtype"))
           (document (require-json-object (parse-json-request))))
      (unless (non-empty-string-p path-dtype)
        (signal-http-input-error
         400
         "missing_path_parameter"
         "Route dtype is required"))
      (validate-document-input document :path-dtype path-dtype)
      (publish-document document)
      (jsown:to-json document))))

(defun handle-new-target-route (params)
  (with-http-boundary ()
    (let* ((actor (query-value params "actor"))
           (document (require-json-object (parse-json-request))))
      (unless (non-empty-string-p actor)
        (signal-http-input-error
         400
         "missing_path_parameter"
         "Route actor is required"))
      (setf (jsown:val document "dtype") "target"
            (jsown:val document "actor") actor)
      ;; This route is the historical target compatibility adapter. Canonical
      ;; document and bulk routes remain strict by default.
      (validate-document-input
       document
       :path-dtype "target"
       :strict-schema-p nil)
      (publish-target-document-unchecked document)
      (jsown:to-json document))))

(defun handle-bulk-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((documents (require-json-array (parse-json-request)))
           (document-count (length documents)))
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
      (if (eq :inline (bulk-request-mode document-count))
          (process-inline-bulk documents)
          (let ((job (submit-bulk-ingest-job
                      documents
                      (request-principal))))
            (setf (lack.response:response-status *response*) 202)
            (jsown:to-json
             (jsown:new-js
               ("status" "accepted")
               ("job_id" (bulk-ingest-job-id job))
               ("total" document-count)
               ("status_url"
                (format nil "/documents/bulk/~a"
                        (bulk-ingest-job-id job)))
               ("correlation_id" (current-correlation-id)))))))))

(defun handle-bulk-status-route (params)
  (with-http-boundary ()
    (let* ((job-id (query-value params "job-id"))
           (job (and job-id
                     (bt:with-lock-held (*bulk-ingest-lock*)
                       (gethash job-id *bulk-ingest-jobs*)))))
      (unless job
        (signal-http-input-error
         404
         "bulk_job_not_found"
         "Bulk ingest job was not found"))
      (jsown:to-json (bulk-job-info-json job)))))

(defun handle-search-route (params)
  (with-http-boundary ()
    (let ((q (require-query-string params "q"))
          (limit (bounded-query-integer
                  params "limit" :default 25 :minimum 1)))
      (couchdb-handler (client *couchdb-pool*)
        (let* ((db star:*couchdb-default-database*)
               (bookmark (query-value params "bookmark"))
               (sort (query-value params "sort"))
               (query (jsown:new-js
                        ("q" q)
                        ("limit" limit)
                        ("include_docs" t))))
          (when sort
            (setf (jsown:val query "sort") sort))
          (when bookmark
            (setf (jsown:val query "bookmark") bookmark))
          (cl-couch:fts-search client
                               (jsown:to-json query)
                               db
                               "search"
                               "fts"))))))

(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'handle-new-document-route)

(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'handle-new-target-route)

(setf (ningle:route *app* "/documents/bulk" :method :post)
      #'handle-bulk-route)

(setf (ningle:route *app* "/documents/bulk/:job-id" :method :get)
      #'handle-bulk-status-route)

(setf (ningle:route *app* "/search" :method :get)
      #'handle-search-route)
