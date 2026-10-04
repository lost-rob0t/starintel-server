(in-package :star.http.contract)

(defparameter +actor-registry-entry-schema+
  (object-schema
   (list
    (cons "resourceUri" (string-schema :min-length 1))
    (cons "resourceKind" (string-schema :min-length 1))
    (cons "semantic" (generic-object-schema))
    (cons "accepts" (generic-object-schema))
    (cons "produces" (generic-object-schema))
    (cons "capabilities" (array-schema (string-schema)))
    (cons "operatorVisible" (boolean-schema))
    (cons "provenance" (generic-object-schema))
    (cons "status" (string-schema :min-length 1))
    (cons "ready" (boolean-schema))
    (cons "observedAt" (string-schema)))
   :required '("resourceUri" "resourceKind" "semantic" "accepts" "produces"
               "capabilities" "operatorVisible" "provenance" "status" "ready")
   :additional-properties nil
   :description "Safe operator-visible actor or service registry entry."))

(defparameter +actor-discovery-response-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "data"
          (object-schema
           (list
            (cons "schema" (string-schema))
            (cons "actors" (array-schema +actor-registry-entry-schema+))
            (cons "count" (integer-schema :minimum 0)))
           :required '("schema" "actors" "count")
           :additional-properties nil)))
   :required '("status" "data")
   :additional-properties nil
   :description "Safe StarIntel actor/service registry discovery response."))

(upsert-http-operation
 (make-http-operation
  :id "actors.list"
  :client-name "actor-list"
  :method :get
  :path "/v1/actors"
  :summary "List registered operator-visible actors and services"
  :tags '("actors")
  :authority :authenticated
  :scopes '("actors:read")
  :responses
  (append
   (list
    (response 200 "Actor deployments discovered."
              +actor-discovery-response-schema+))
   (standard-errors))))
