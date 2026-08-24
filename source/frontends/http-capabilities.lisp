(in-package :star.frontends.http-api)

(defparameter +api-v1-version+ "v1")
(defparameter +capabilities-path+ "/api/v1/capabilities")

(defun json-boolean (value)
  (if value :true :false))

(defun capability-endpoint
    (id method path &key legacy (authority "authenticated") scopes)
  (jsown:new-js
    ("id" id)
    ("method" method)
    ("path" path)
    ("legacy" (json-boolean legacy))
    ("authority" authority)
    ("scopes" (or scopes nil))))

(defun configured-auth-modes ()
  (let ((mode (string-downcase (or star:*auth-mode* ""))))
    (cond
      ((string= mode "api-key") (list "api-key"))
      ((string= mode "disabled") (list "disabled"))
      (t (list "configured")))))

(defun capabilities-data ()
  (jsown:new-js
    ("build"
     (jsown:new-js
       ("service" "starintel-gserver")
       ("version" star:*star-server-version*)))
    ("schema_revisions"
     (jsown:new-js
       ("api" +api-v1-version+)
       ("document" starintel:+starintel-doc-version+)))
    ("transports" (list "http"))
    ("authentication"
     (jsown:new-js
       ("modes" (configured-auth-modes))
       ("capabilities_endpoint_requires_auth" :false)))
    ("features"
     (jsown:new-js
       ("documents" :true)
       ("bulk_ingest" :true)
       ("search" :true)
       ("stats" :true)
       ("targets" :true)
       ("views"
        (jsown:new-js
          ("available" :true)
          ("registry" :true)
          ("query" :true)))
       ("queue_ingest" :true)
       ("target_leases" :false)
       ("streams" :false)
       ("openapi" :true)))
    ("limits"
     (jsown:new-js
       ("bulk_documents" star:*bulk-max-documents*)
       ("public_search_results" 50)
       ("default_request_timeout_ms"
        star:*auth-default-request-timeout-ms*)
       ("max_request_timeout_ms"
        star:*auth-max-request-timeout-ms*)))
    ("endpoints"
     (list
      (capability-endpoint
       "capabilities" "GET" +capabilities-path+
       :authority "public")
      (capability-endpoint
       "public_search" "GET" "/api/v1/search"
       :authority "public")
      (capability-endpoint
       "stats" "GET" "/api/v1/stats"
       :authority "public")
      (capability-endpoint
       "document_create" "POST" "/new/document/:dtype"
       :legacy t :scopes '("documents:write"))
      (capability-endpoint
       "document_read" "GET" "/document/:id"
       :legacy t :scopes '("documents:read"))
      (capability-endpoint
       "document_bulk" "POST" "/documents/bulk"
       :legacy t :scopes '("documents:bulk"))
      (capability-endpoint
       "search" "GET" "/search"
       :legacy t :scopes '("search:read"))
      (capability-endpoint
       "target_create" "POST" "/new/target/:actor"
       :legacy t :scopes '("targets:dispatch"))
      (capability-endpoint
       "targets_by_actor" "GET" "/targets/:actor"
       :legacy t :scopes '("targets:read"))))
    ("compatibility"
     (jsown:new-js
       ("legacy_routes" :true)
       ("legacy_routes_deprecated" :false)))))

(defun capabilities-document ()
  (jsown:new-js
    ("status" "ok")
    ("data" (capabilities-data))))

(defun capabilities-json ()
  (jsown:to-json (capabilities-document)))

(pushnew +capabilities-path+ star:*auth-public-paths* :test #'string=)

(setf (ningle:route *app* +capabilities-path+ :method :get)
      (lambda (params)
        (declare (ignore params))
        (set-default-headers)
        (capabilities-json)))
