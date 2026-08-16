(in-package :star.frontends.http-api)

(defun handle-tenant-scoped-dataset-size-route (params)
  (with-http-boundary ()
    (let* ((dataset (require-query-string params "dataset"))
           (tenant (or (query-value params "tenant") "default"))
           (metadata (route-policy-metadata "/dataset-size" "GET")))
      (star.authorization:authorize!
       "views:read"
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :tenant-id tenant
        :dataset-id dataset)
       :metadata metadata)
      (couchdb-handler (client *couchdb-pool*)
        (jsown:to-json
         (query-view
          client
          star:*couchdb-default-database*
          "authorization"
          "dataset_size"
          :key (list tenant dataset)
          :include-docs nil
          :reduce t
          :update nil))))))

(setf (ningle:route *app* "/dataset-size" :method :get)
      #'handle-tenant-scoped-dataset-size-route)
