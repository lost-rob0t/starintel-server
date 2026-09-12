(in-package :star-server-tests)

(in-suite http-boundary-tests)

(defparameter *expected-v2-view-categories*
  '("analytics" "research" "targets" "graph" "geo" "operations" "migrations"))

(test v2-view-registry-is-complete-and-backed-by-design-docs
  (is-true (star.databases.couchdb:validate-view-registry))
  (let ((catalog (star.frontends.http-api::view-v2-catalog-document))
        (v2-count 0))
    (is (>= (jsown:val catalog "count") 82))
    (dolist (category *expected-v2-view-categories*)
      (let ((category-catalog
              (star.frontends.http-api::view-v2-catalog-document category)))
        (incf v2-count (jsown:val category-catalog "count"))
        (is (> (jsown:val category-catalog "count") 0))))
    (is (= 82 v2-count))))

(test v2-view-name-resolution-is-registry-only
  (is (eq 'star.databases.couchdb::analytics-count-by-dataset-dtype
          (star.frontends.http-api::view-v2-resolve-name
           "count-by-dataset-dtype" "analytics")))
  (is (eq 'star.databases.couchdb::graph-predicate-counts
          (star.frontends.http-api::view-v2-resolve-name
           "predicate_counts" "graph")))
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api::view-v2-resolve-name
              "javascript=emit-everything" "analytics")))))
    (is (= 404 (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "view_not_found"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test v2-view-query-contract-parses-couchdb-options
  (let* ((params '(("limit" . "250")
                   ("skip" . "10")
                   ("descending" . "true")
                   ("reduce" . "false")
                   ("include_docs" . "true")
                   ("key" . "\"person\"")
                   ("update" . "lazy")))
         (arguments (star.frontends.http-api::view-v2-query-arguments params)))
    (is (= 250 (getf arguments :limit)))
    (is (= 10 (getf arguments :skip)))
    (is-true (getf arguments :descending))
    (is-false (getf arguments :reduce))
    (is-true (getf arguments :include-docs))
    (is (string= "person" (getf arguments :key)))
    (is (eq :lazy (getf arguments :update)))))

(test v2-view-query-contract-rejects-invalid-values
  (let ((boolean-condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api::view-v2-query-arguments
              '(("reduce" . "maybe"))))))
        (limit-condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api::view-v2-query-arguments
              '(("limit" . "999999")))))))
    (is (= 400 (star.frontends.http-api:http-input-error-status
                boolean-condition)))
    (is (= 400 (star.frontends.http-api:http-input-error-status
                limit-condition)))))

(test v2-view-api-routes-live-under-api-v1
  (dolist (entry star.frontends.http-api::*http-view-v2-route-matrix*)
    (is (uiop:string-prefix-p "/api/v1/" (first entry))))
  (dolist (path '("/api/v1/views"
                  "/api/v1/views/analytics/count-by-dataset-dtype"
                  "/api/v1/analytics"
                  "/api/v1/analytics/count-by-dataset-dtype"
                  "/api/v1/research/node-status"
                  "/api/v1/graph/predicate-counts"
                  "/api/v1/geo/by-country"
                  "/api/v1/operations/status"
                  "/api/v1/migrations/promote-next"))
    (is (string= "views:read"
                 (star.frontends.http-api::route-action :get path)))))

(test v2-migration-views-carry-the-explicit-promotion-chain
  (let* ((documents (star.databases.couchdb::checked-in-design-document-map))
         (migration-document (gethash "migrations_v2" documents))
         (views (jsown:val migration-document "views"))
         (promote (jsown:val (jsown:val views "promote_next") "map"))
         (outdated (jsown:val (jsown:val views "outdated_by_version") "map")))
    (is (search "'0.7.3':'0.8.0'" promote))
    (is (search "'0.8.0':'0.9.0'" promote))
    (is (search "'0.9.0':'0.9.1'" promote))
    (is (search "'0.4.5':'0.4.6'" promote))
    (is (search "'0.7.0':'0.7.3'" promote))
    (is (search "legacy-booker" promote))
    (is (search "emit([v,n" promote))
    (is (search "emit([v,doc.dtype" outdated))))

(test v2-map-reduce-covers-research-target-geo-graph-and-operations
  (dolist (name
           '(star.databases.couchdb::research-node-cost
             star.databases.couchdb::research-node-dependencies
             star.databases.couchdb::targets-v2-by-root
             star.databases.couchdb::targets-v2-depth
             star.databases.couchdb::geo-by-geohash-prefix
             star.databases.couchdb::geo-contained-by
             star.databases.couchdb::graph-lineage-edges
             star.databases.couchdb::graph-claim-evidence-edges
             star.databases.couchdb::operations-phase-dependencies
             star.databases.couchdb::operations-capability-gaps))
    (is (not (null (star.databases.couchdb:registered-view-spec name))))))

(test v2-view-api-is-described-by-the-shared-http-contract
  (dolist (operation-id '("views.v2.catalog"
                          "views.v2.execute"
                          "views.v2.execute-category"
                          "analytics.v2.default"
                          "analytics.v2.execute"
                          "research.v2.execute"
                          "graph.v2.execute"
                          "geo.v2.execute"
                          "operations.v2.execute"
                          "migrations.v2.execute"))
    (let ((operation (star.http.contract:find-http-operation operation-id)))
      (is (not (null operation)))
      (is (eq :get (star.http.contract:http-operation-method operation)))
      (is (eq :authenticated
              (star.http.contract:http-operation-authority operation)))
      (is (equal '("views:read")
                 (star.http.contract:http-operation-scopes operation)))
      (is (uiop:string-prefix-p "/api/v1/"
           (star.http.contract:http-operation-path operation))))))
