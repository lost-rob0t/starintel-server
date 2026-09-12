(in-package :star.http.contract)

(defun register-view-v2-http-operation
    (id client-name path summary tags)
  (upsert-http-operation
   (make-http-operation
    :id id
    :client-name client-name
    :method :get
    :path path
    :summary summary
    :tags tags
    :authority :authenticated
    :scopes '("views:read")
    :responses
    (append
     (list
      (response 200 "Registered Map/Reduce view response."
                (generic-object-schema)))
     (standard-errors)))))

(register-view-v2-http-operation
 "views.v2.catalog" "views-v2-catalog" "/api/v1/views"
 "List registered server-owned Map/Reduce views" '("views" "analytics"))
(register-view-v2-http-operation
 "views.v2.execute" "views-v2-execute" "/api/v1/views/:view"
 "Execute a registered server-owned Map/Reduce view" '("views"))
(register-view-v2-http-operation
 "views.v2.execute-category" "views-v2-execute-category"
 "/api/v1/views/:category/:view"
 "Execute a registered view within a v2 view category" '("views"))
(register-view-v2-http-operation
 "analytics.v2.default" "analytics-v2-default" "/api/v1/analytics"
 "Return the default aggregate StarIntel analytics dashboard"
 '("views" "analytics"))

(dolist (entry
         '(("analytics.v2.execute" "analytics-v2-execute"
            "/api/v1/analytics/:view" "analytics")
           ("research.v2.execute" "research-v2-execute"
            "/api/v1/research/:view" "research")
           ("graph.v2.execute" "graph-v2-execute"
            "/api/v1/graph/:view" "graph")
           ("geo.v2.execute" "geo-v2-execute"
            "/api/v1/geo/:view" "geo")
           ("operations.v2.execute" "operations-v2-execute"
            "/api/v1/operations/:view" "operations")
           ("migrations.v2.execute" "migrations-v2-execute"
            "/api/v1/migrations/:view" "migrations")))
  (register-view-v2-http-operation
   (first entry) (second entry) (third entry)
   (format nil "Execute a registered ~a Map/Reduce view" (fourth entry))
   (list "views" (fourth entry))))
