(in-package :star.frontends.http-api)

(defvar *actor-manifest-document-loader*
  (lambda ()
    (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
      (star.actors::query-actor-manifest-documents
       :client client :database star:*couchdb-default-database*)))
  "Persisted manifest reader; tests may replace the storage effect port.")

(defun handle-actor-discovery-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (set-cache-control "no-store")
    (jsown:to-json
     (star.actors:actor-registry-document
      (funcall *actor-manifest-document-loader*)))))

(mount-http-operation "actors.list" #'handle-actor-discovery-route)
