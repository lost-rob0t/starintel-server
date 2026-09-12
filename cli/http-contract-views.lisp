(in-package :star.http.contract)

(upsert-http-operation
 (make-http-operation
  :id "views.scoped.get"
  :client-name "scoped-view"
  :method :get
  :path "/api/v1/views/:design/:view"
  :path-parameters '("design" "view")
  :summary "Query an approved scope-aware CouchDB projection with bounded range controls"
  :tags '("views" "data")
  :authority :authenticated
  :scopes '("views:read")
  :responses
  (append
   (list
    (response 200 "Bounded scoped view result." (generic-object-schema))
    (response 400 "Invalid or unsupported view query." +error-schema+)
    (response 403 "View scope is not authorized." +error-schema+)
    (response 404 "View is not exposed through the scoped API." +error-schema+))
   (standard-errors))))
