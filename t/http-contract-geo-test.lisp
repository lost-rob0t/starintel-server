(in-package :star-server-tests)

(def-suite http-contract-geo-tests
  :description "Versioned geographic search operation")

(in-suite http-contract-geo-tests)

(test geo-bbox-operation-is-advertised
  (let ((operation (star.http.contract:find-http-operation "geo.bbox")))
    (is (eq :get (star.http.contract:http-operation-method operation)))
    (is (string= "/api/v1/geo/bbox"
                 (star.http.contract:http-operation-path operation)))
    (is (eq :authenticated
            (star.http.contract:http-operation-authority operation)))
    (is (equal '("search:read")
               (star.http.contract:http-operation-scopes operation)))
    (let ((parameters
            (star.http.contract:http-operation-query-parameters operation)))
      (is (= 4 (length parameters)))
      (is (string= "bbox" (getf (first parameters) :name)))
      (is (getf (first parameters) :required))
      (is (find "dataset" parameters
                :key (lambda (parameter) (getf parameter :name))
                :test #'string=)))))

(test geo-bbox-openapi-and-client-manifest-are-generated
  (let* ((openapi
           (jsown:with-injective-reader
             (jsown:parse (star.http.contract:openapi-json))))
         (paths (jsown:val openapi "paths"))
         (geo (jsown:val-safe paths "/api/v1/geo/bbox")))
    (is-true geo)
    (is (string= "geo.bbox"
                 (jsown:val
                  (jsown:val geo "get")
                  "x-starintel-operation-id"))))
  (let* ((manifest
           (jsown:with-injective-reader
             (jsown:parse (star.http.contract:client-manifest-json))))
         (operation
           (find "geo.bbox"
                 (jsown:val manifest "operations")
                 :key (lambda (entry)
                        (jsown:val entry "operation_id"))
                 :test #'string=)))
    (is-true operation)
    (is (string= "/api/v1/geo/bbox"
                 (jsown:val operation "path")))))

(test geo-bbox-capability-and-route-policy-are-visible
  (let* ((document (star.frontends.http-api::capabilities-document))
         (data (jsown:val document "data"))
         (features (jsown:val data "features"))
         (endpoints (jsown:val data "endpoints"))
         (endpoint
           (find "geo_bbox_v1"
                 endpoints
                 :key (lambda (entry) (jsown:val entry "id"))
                 :test #'string=)))
    (is (eq :true (jsown:val features "geo_bbox")))
    (is-true endpoint)
    (is (string= "/api/v1/geo/bbox" (jsown:val endpoint "path")))
    (is (equal '("search:read") (jsown:val endpoint "scopes")))
    (is (string=
         "search:read"
         (star.frontends.http-api::route-action
          :get "/api/v1/geo/bbox")))))
