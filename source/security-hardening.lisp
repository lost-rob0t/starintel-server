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

(in-package :star.frontends.http-api)

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

;; This is intentionally the final HTTP middleware assembly. The older
;; definitions in http-api.lisp remain load-order compatibility code, while the
;; effective server uses strict CORS, authentication, and these security headers.
(setf *server*
      (lack:builder
       :accesslog
       (security-headers-middleware
        (cors-middleware
         (authentication-middleware *app*)))))
