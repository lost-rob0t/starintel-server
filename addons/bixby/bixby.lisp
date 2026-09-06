(in-package :star.addons.bixby)

(defvar *public-base-url* nil)
(defvar *redirect-uri* nil)
(defvar *read-scopes* '("documents:read" "search:read"))
(defvar *operations-scopes* '("targets:dispatch"))

(defun normalize-base-url (value)
  (unless (and (stringp value)
               (plusp (length value)))
    (error "Bixby public base URL must be a non-empty HTTPS URL"))
  (let ((uri (ignore-errors (quri:uri value))))
    (unless (and uri
                 (string-equal "https" (quri:uri-scheme uri))
                 (quri:uri-host uri)
                 (null (quri:uri-fragment uri)))
      (error "Bixby public base URL must be an absolute HTTPS URL without a fragment")))
  (string-right-trim "/" value))

(defun normalize-redirect-uri (value)
  (unless (star.auth:valid-https-redirect-uri-p value)
    (error "Bixby redirect URI must be an absolute HTTPS callback URI"))
  value)

(defun configure-bixby (&key public-base-url redirect-uri read-scopes operations-scopes)
  "Configure Samsung-specific adapter metadata without changing core OAuth.

This function is intended for init.lisp. It records only Bixby integration
settings. OAuth users, clients, codes, tokens, scopes, and authorization remain
owned by STAR.AUTH."
  (when public-base-url
    (setf *public-base-url* (normalize-base-url public-base-url)))
  (when redirect-uri
    (setf *redirect-uri* (normalize-redirect-uri redirect-uri)))
  (when read-scopes
    (setf *read-scopes* (star.auth:normalize-oauth-scopes read-scopes)))
  (when operations-scopes
    (setf *operations-scopes*
          (star.auth:normalize-oauth-scopes operations-scopes)))
  (bixby-oauth-settings))

(defun require-bixby-config ()
  (unless *public-base-url*
    (error "Bixby add-on requires PUBLIC-BASE-URL configuration"))
  (unless *redirect-uri*
    (error "Bixby add-on requires REDIRECT-URI configuration"))
  t)

(defun bixby-oauth-settings ()
  "Return the provider values consumed by Samsung capsule configuration."
  (require-bixby-config)
  (list :authorize-endpoint
        (format nil "~a/oauth/authorize" *public-base-url*)
        :token-endpoint
        (format nil "~a/oauth/token" *public-base-url*)
        :redirect-uri *redirect-uri*
        :read-scopes (copy-list *read-scopes*)
        :operations-scopes (copy-list *operations-scopes*)))

(defun create-bixby-oauth-client (&key include-operations
                                       (store star.auth:*credential-store*))
  "Create one standard core OAuth client for the configured Bixby callback.

The returned secret is intentionally one-time output from STAR.AUTH and must be
placed in Samsung Developer Center or equivalent secret storage. This function
is explicit and is never called automatically during add-on load/reload."
  (require-bixby-config)
  (star.auth:create-oauth-client
   (list *redirect-uri*)
   (remove-duplicates
    (append (copy-list *read-scopes*)
            (when include-operations
              (copy-list *operations-scopes*)))
    :test #'string=)
   :store store))

(defun start-bixby-addon ()
  ;; Loading the add-on must be side-effect-light and reload-safe. Client
  ;; registration is explicit because it creates secret material.
  (log:info "StarIntel Bixby add-on loaded; core OAuth remains authoritative")
  t)

(defun stop-bixby-addon ()
  (log:info "StarIntel Bixby add-on stopped")
  t)

(star:register-addon
 :starintel-bixby
 :system :starintel-bixby
 :start #'start-bixby-addon
 :stop #'stop-bixby-addon)