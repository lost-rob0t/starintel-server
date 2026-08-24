(in-package :star.frontends.http-api)

(dolist (path '("/openapi.json" "/client-manifest.json"))
  (pushnew path star:*auth-public-paths* :test #'string=))

(defun mount-http-operation (operation-id handler)
  "Mount HANDLER using the canonical method/path for OPERATION-ID."
  (let ((operation (star.http.contract:find-http-operation operation-id)))
    (setf (ningle:route *app*
                        (star.http.contract:http-operation-path operation)
                        :method (star.http.contract:http-operation-method operation))
          handler)
    operation))

(defun handle-contracted-health-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (status-msg "OK" 'info)))

(defun handle-contracted-server-info-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (jsown:to-json
     (jsown:new-js
       ("doc_spec_version" starintel:+starintel-doc-version+)
       ("default-dataset" star:*couchdb-default-database*)
       ("event_log" star:*couchdb-event-log-database*)
       ("server" "starintel-gserver")
       ("version" star:*star-server-version*)
       ("openapi" "/openapi.json")
       ("client_manifest" "/client-manifest.json")))))

(defun handle-openapi-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:openapi-json)))

(defun handle-client-manifest-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:client-manifest-json)))

;; Re-mount the first contracted surface from operation IDs. Some legacy route
;; declarations still exist in their historical source files; these final
;; mounts are authoritative and prevent method/path drift for the contracted
;; client surface while the remaining legacy API is migrated incrementally.
(mount-http-operation "health.get" #'handle-contracted-health-route)
(mount-http-operation "server.get" #'handle-contracted-server-info-route)
(mount-http-operation "schema.openapi.get" #'handle-openapi-route)
(mount-http-operation "schema.client-manifest.get" #'handle-client-manifest-route)

(mount-http-operation "auth.login" #'handle-auth-login-route)
(mount-http-operation "auth.bootstrap" #'handle-auth-bootstrap-route)
(mount-http-operation "auth.context.get" #'handle-auth-context-route)
(mount-http-operation "auth.users.create" #'handle-auth-create-user-route)
(mount-http-operation "auth.users.list" #'handle-auth-list-users-route)
(mount-http-operation "auth.users.password.reset"
                      #'handle-auth-reset-user-password-route)
(mount-http-operation "auth.password.change" #'handle-auth-change-password-route)
(mount-http-operation "auth.credentials.create" #'handle-auth-create-route)
(mount-http-operation "auth.credentials.list" #'handle-auth-list-route)
(mount-http-operation "auth.credentials.rotate" #'handle-auth-rotate-route)
(mount-http-operation "auth.credentials.revoke" #'handle-auth-revoke-route)
(mount-http-operation "auth.credentials.disable" #'handle-auth-disable-route)
