(in-package :star.http.contract)

(defparameter +geo-bbox-query-parameters+
  (list
   (list :name "bbox" :required t
         :schema
         (string-schema
          :min-length 7
          :description
          "Bounding box as west,south,east,north decimal coordinates."))
   (list :name "dataset" :required t
         :schema
         (string-schema
          :min-length 1
          :description "Authorized StarIntel dataset to search."))
   (list :name "tenant"
         :schema
         (string-schema
          :min-length 1
          :description "Tenant scope; defaults to default."))
   (list :name "limit"
         :schema
         (integer-schema
          :minimum 1
          :maximum 100
          :description "Maximum number of matching documents."))))

(upsert-http-operation
 (make-http-operation
  :id "geo.bbox"
  :client-name "geo-bbox"
  :method :get
  :path "/api/v1/geo/bbox"
  :summary "Search CouchDB JSON documents by geographic bounding box"
  :tags '("geo" "search")
  :authority :authenticated
  :scopes '("search:read")
  :query-parameters +geo-bbox-query-parameters+
  :responses
  (append
   (list
    (response
     200
     "CouchDB search response containing documents inside the bounding box."
     (generic-object-schema)))
   (standard-errors))))

(dolist (operation *http-operations*)
  (normalize-schema-json-values (http-operation-request-schema operation))
  (dolist (response (http-operation-responses operation))
    (normalize-schema-json-values (getf response :schema))))
