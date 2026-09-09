(in-package :star.http.contract)

(defparameter +actor-discovery-response-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "data" (generic-object-schema)))
   :required '("status" "data")
   :additional-properties nil
   :description "Runtime actor deployment discovery response."))

(upsert-http-operation
 (make-http-operation
  :id "actors.list"
  :client-name "actor-list"
  :method :get
  :path "/api/v1/actors"
  :summary "List local and remote actor deployments visible to this server"
  :tags '("actors")
  :authority :authenticated
  :scopes '("actors:read")
  :responses
  (append
   (list
    (response 200 "Actor deployments discovered."
              +actor-discovery-response-schema+))
   (standard-errors))))
