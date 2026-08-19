(in-package :star.auth)

(defun ensure-initial-user (&key (store *credential-store*))
  "Create the configured first human administrator once.

When no initial password is explicitly configured, leave the human-user store
empty. The one-time API-key bootstrap flow remains available and can be used to
create/reset human users without shipping a known administrator password."
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (let ((username (normalize-username star:*auth-initial-username*)))
    (cond
      ((plusp (user-store-count store))
       (user-store-get store username))
      ((null star:*auth-initial-password*)
       (log:info
        "No initial human administrator password configured; skipping first-run user creation")
       nil)
      (t
       ;; Retain explicit first-run compatibility for existing deployments.
       ;; Normal create/change operations still enforce the configured minimum.
       (create-user
        star:*auth-initial-username*
        star:*auth-initial-password*
        "administrator"
        (list "admin")
        :must-change-password t
        :allow-weak-password t
        :store store)))))

(defun administrator-principal-p (&optional (principal (current-request-principal)))
  "Treat explicit admin scope, not descriptive principal type, as authority."
  (and principal
       (scope-granted-p "admin" principal)))

(in-package :star.frontends.http-api)

(defun route-action (method path)
  "Resolve the capability required for an HTTP route.

Human-user administration was added after the original centralized mapping.
Keep it fail-closed while allowing authenticated users to reach their own
password-change handler, which re-verifies the current password."
  (cond
    ((or (public-auth-path-p path)
         (eq method :options))
     :public)
    ((and (eq method :post) (string= path "/auth/password"))
     :authenticated)
    ((string= path "/auth/users")
     (case method
       ((:get :post) "principals:manage")
       (otherwise nil)))
    ((and (eq method :post)
          (path-prefix-p "/auth/users/" path)
          (path-suffix-p "/password" path))
     "principals:manage")
    ((and (eq method :get) (string= path "/auth/context"))
     "identity:read")
    ((string= path "/auth/credentials")
     (case method
       (:get "credentials:read")
       (:post "credentials:create")
       (otherwise nil)))
    ((and (path-prefix-p "/auth/credentials/" path)
          (eq method :post))
     (cond
       ((path-suffix-p "/rotate" path) "credentials:rotate")
       ((path-suffix-p "/revoke" path) "credentials:revoke")
       ((path-suffix-p "/disable" path) "credentials:disable")
       (t nil)))
    ((path-prefix-p "/document/" path)
     (case method
       (:get "documents:read")
       (:delete "documents:delete")
       (otherwise nil)))
    ((and (eq method :post)
          (path-prefix-p "/new/document/" path))
     "documents:write")
    ((path-prefix-p "/documents/bulk" path)
     "documents:bulk")
    ((and (eq method :get) (string= path "/search"))
     "search:read")
    ((and (eq method :post)
          (path-prefix-p "/new/target/" path))
     "targets:dispatch")
    ((path-prefix-p "/targets/" path)
     (cond
       ((and (eq method :post)
             (path-suffix-p "/force-release" path))
        "targets:force-release")
       ((and (eq method :post)
             (path-suffix-p "/lease" path))
        "targets:lease")
       ((eq method :get) "targets:read")
       (t nil)))
    ((and (eq method :post)
          (path-prefix-p "/new/event/" path))
     "events:write")
    ((and (eq method :post)
          (path-prefix-p "/events/" path)
          (path-suffix-p "/replay" path))
     "events:replay")
    ((and (eq method :get)
          (path-prefix-p "/views/" path))
     "views:read")
    ((and (eq method :get)
          (string= path "/dataset-size"))
     "views:read")
    ((and (eq method :get)
          (path-prefix-p "/documents/" path))
     "views:read")
    (t nil)))

(defun authorize-http-route! (method path correlation-id)
  (let ((action (route-action method path)))
    (cond
      ((or (eq action :public)
           (eq action :authenticated))
       nil)
      ((null action)
       (star.authorization:authorize!
        "unmapped:http-route"
        :metadata (request-policy-metadata method path correlation-id)))
      ((legacy-unscoped-view-path-p path)
       (star.authorization:authorize!
        action
        :resource
        (star.authorization:make-authorization-resource
         :tenant-id "default"
         :dataset-id "__unscoped_legacy_view__")
        :metadata (request-policy-metadata method path correlation-id)))
      (t
       (star.authorization:authorize!
        action
        :metadata (request-policy-metadata method path correlation-id))))))

(defun restriction-scope-prefix (scope)
  (loop for prefix in star.authorization::+scope-prefixes+
        when (and (> (length scope) (length prefix))
                  (string= prefix scope :end2 (length prefix)))
          return prefix))

(defun delegated-scope-covered-p (requested-scope caller-scopes)
  "Return true when CALLER-SCOPES contains REQUESTED-SCOPE or a matching wildcard."
  (or (member requested-scope caller-scopes :test #'string=)
      (let ((prefix (restriction-scope-prefix requested-scope)))
        (and prefix
             (member (concatenate 'string prefix "*")
                     caller-scopes
                     :test #'string=)))))

(defun admin-scoped-principal-p (principal)
  (and principal
       (member "admin"
               (star.auth:request-principal-scopes principal)
               :test #'string=)))

(defun credential-identity-delegable-p (owner principal-type principal)
  "Non-admin issuers may only mint or manage credentials for their own identity."
  (and principal
       (or (admin-scoped-principal-p principal)
           (and (string= owner
                         (star.auth:request-principal-id principal))
                (string-equal principal-type
                              (star.auth:request-principal-type principal))))))

(defun credential-grant-delegable-p (principal-type scopes principal)
  "Prevent a credential issuer from granting authority it does not possess."
  (when principal
    (let ((caller-scopes (star.auth:request-principal-scopes principal)))
      (or (member "admin" caller-scopes :test #'string=)
          (and (not (string-equal principal-type "administrator"))
               (not (member "admin" scopes :test #'string=))
               (every (lambda (scope)
                        (delegated-scope-covered-p scope caller-scopes))
                      scopes))))))

(defun require-delegable-credential-grant (owner principal-type scopes)
  (let ((principal (star.auth:current-request-principal)))
    (unless (and (credential-identity-delegable-p
                  owner principal-type principal)
                 (credential-grant-delegable-p
                  principal-type scopes principal))
      (signal-http-input-error
       403
       "access_denied"
       "Access denied")))
  t)

(defun credential-record-delegable-p (record principal)
  (and record
       (credential-identity-delegable-p
        (star.auth:api-key-record-owner record)
        (star.auth:api-key-record-principal-type record)
        principal)
       (credential-grant-delegable-p
        (star.auth:api-key-record-principal-type record)
        (star.auth:api-key-record-scopes record)
        principal)))

(defun require-delegable-stored-credential (credential-id)
  "Deny delegated lifecycle operations outside the caller's identity or authority."
  (let ((record
          (and star.auth:*credential-store*
               (star.auth:credential-store-get
                star.auth:*credential-store*
                credential-id))))
    ;; Preserve the lifecycle API's normal not-found behavior. Existing records
    ;; must be owned by the caller and no more authoritative, unless the caller
    ;; is explicitly admin-scoped.
    (when (and record
               (not (credential-record-delegable-p
                     record
                     (star.auth:current-request-principal))))
      (signal-http-input-error
       403
       "access_denied"
       "Access denied"))
    record))

(defun handle-hardened-auth-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((body (require-json-object (parse-json-request)))
             (owner (require-auth-string body "owner"))
             (principal-type
               (require-auth-string body "principal_type"))
             (scopes (require-scope-array body))
             (expires-in-seconds
               (optional-positive-integer body "expires_in_seconds")))
        (require-delegable-credential-grant owner principal-type scopes)
        (multiple-value-bind (record raw-key)
            (star.auth:create-api-key
             owner
             principal-type
             scopes
             :expires-in-seconds expires-in-seconds)
          (setf (lack.response:response-status *response*) 201)
          (credential-secret-response record raw-key))))))

(defun handle-hardened-auth-rotate-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((credential-id (credential-id-param params))
             (body (require-json-object (parse-json-request)))
             (overlap-seconds
               (or (jsown:val-safe body "overlap_seconds") 0)))
        (unless (and (integerp overlap-seconds)
                     (not (minusp overlap-seconds)))
          (signal-http-input-error
           422
           "invalid_auth_request"
           "Field overlap_seconds must be a non-negative integer"))
        (require-delegable-stored-credential credential-id)
        (multiple-value-bind (record raw-key)
            (star.auth:rotate-api-key credential-id overlap-seconds)
          (setf (lack.response:response-status *response*) 201)
          (credential-secret-response record raw-key))))))

(defun handle-hardened-auth-revoke-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let ((credential-id (credential-id-param params)))
        (require-delegable-stored-credential credential-id)
        (lifecycle-status-response
         (star.auth:revoke-api-key credential-id)
         "Credential revoked")))))

(defun handle-hardened-auth-disable-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let ((credential-id (credential-id-param params)))
        (require-delegable-stored-credential credential-id)
        (lifecycle-status-response
         (star.auth:disable-api-key credential-id)
         "Credential disabled")))))

;; Re-register after http-authorization-routes so delegated lifecycle endpoints
;; cannot retain the pre-attenuation function objects captured by those routes.
(setf (ningle:route *app* "/auth/credentials" :method :post)
      (credential-action-handler
       "credentials:create" #'handle-hardened-auth-create-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/rotate" :method :post)
      (credential-action-handler
       "credentials:rotate" #'handle-hardened-auth-rotate-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/revoke" :method :post)
      (credential-action-handler
       "credentials:revoke" #'handle-hardened-auth-revoke-route))
(setf (ningle:route *app* "/auth/credentials/:credential-id/disable" :method :post)
      (credential-action-handler
       "credentials:disable" #'handle-hardened-auth-disable-route))

(defparameter *security-response-headers*
  (list
   :x-content-type-options "nosniff"
   :x-frame-options "DENY"
   :content-security-policy
   "default-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'none'"
   :referrer-policy "no-referrer"
   :permissions-policy "camera=(), geolocation=(), microphone=()"))

(defun security-headers-middleware (app)
  "Attach browser hardening headers to every application response."
  (lambda (env)
    (append-response-headers
     (lack.component:call app env)
     *security-response-headers*)))

;; This is intentionally the final HTTP middleware assembly. Security headers
;; wrap the existing CORS, authentication, and authorization boundaries without
;; weakening or replacing any of them.
(setf *server*
      (lack:builder
       :accesslog
       (security-headers-middleware
        (cors-middleware
         (authentication-middleware
          (authorization-middleware *app*))))))
