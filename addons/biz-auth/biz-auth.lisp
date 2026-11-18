;;;; biz-auth: identity lifecycle in the canonical StarIntel auth system.
;;;; starintel-server #415.
;;;;
;;;; Exposes create/ban/unban/status for identities in the ZITADEL
;;;; Management API to service principals holding `biz-auth:admin`.
;;;; Fail-closed: without STAR_ZITADEL_TOKEN every operation answers
;;;; 503 biz_auth_unavailable and never contacts the identity backend.

(in-package :star.biz-auth)

(defparameter +create-path+ "/api/v1/biz-auth/users")
(defparameter +ban-path+ "/api/v1/biz-auth/users/ban")
(defparameter +unban-path+ "/api/v1/biz-auth/users/unban")
(defparameter +status-path+ "/api/v1/biz-auth/users/status")

(defparameter +internal-email-domain+ "@starintel.actor")

(defvar *active* nil)

;; Test seam: bind to a string to force a credential, to :unset to force
;; absence. NIL reads STAR_ZITADEL_TOKEN from the environment.
(defvar *token-override* nil)

(defun effective-token ()
  (etypecase *token-override*
    (null (let ((environment (uiop:getenv "STAR_ZITADEL_TOKEN")))
            (and environment (plusp (length environment)) environment)))
    (string *token-override*)
    ((eql :unset) nil)))

(defun configured-p ()
  "True when a service credential is available; identity operations fail
closed without one."
  (and (effective-token) t))

(defun identity-url ()
  (or (uiop:getenv "STAR_ZITADEL_URL") "https://auth.starintel.actor"))

(defun normalize-biz-username (username)
  "Canonicalize through the server's username contract, then enforce the
ASCII subset required for identity-backend usernames and internal-domain
email local-parts (local users may be Unicode-tolerant; ZITADEL
identities are not)."
  (let ((normalized (star.auth:normalize-username username)))
    (unless (every (lambda (character)
                     (or (char<= #\a character #\z)
                         (char<= #\0 character #\9)
                         (find character "-_.")))
                   normalized)
      (error "username must be ASCII letters, digits, '-', '_', or '.'"))
    normalized))

(defun generate-initial-password ()
  "Random 20-character password over an alphabet that satisfies the
identity backend's complexity policy (upper, lower, digit, symbol) by
construction."
  (let ((alphabet
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^*")
        (password (make-string 20)))
    (loop for index below (length password)
          do (setf (char password index)
                   (char alphabet (random (length alphabet)))))
    ;; Pin one character of every required class into fixed positions.
    ;; The symbol run starts at 62 and runs to the end of the alphabet.
    (setf (char password 0) (char alphabet (random 26))
          (char password 1) (char alphabet (+ 26 (random 26)))
          (char password 2) (char alphabet (+ 52 (random 10)))
          (char password 3)
          (char alphabet (+ 62 (random (- (length alphabet) 62)))))
    password))

(defun user-state-keyword (state)
  "Map a ZITADEL user state string onto the server's status vocabulary."
  (cond
    ((not (stringp state)) :unknown)
    ((search "USER_STATE_ACTIVE" state) :active)
    ((search "USER_STATE_INACTIVE" state) :inactive)
    ((search "USER_STATE_INITIAL" state) :initial)
    (t :unknown)))

(defun import-payload (username first-name last-name password)
  "Verified provisioning recipe (live 2026-11-13): the import endpoint
accepts a plain-string password, and the nested email object's address
key is `email` — with a pre-verified internal-domain address no mail
ever leaves the fleet."
  (jsown:new-js
    ("userName" username)
    ("profile" (jsown:new-js
                ("firstName" (or first-name username))
                ("lastName" (or last-name username))))
    ("email" (jsown:new-js
              ("email" (concatenate 'string username +internal-email-domain+))
              ("isEmailVerified" t)))
    ("password" password)
    ("passwordChangeRequired" t)))

(defun search-payload (username)
  (jsown:new-js
    ("query" (jsown:new-js ("limit" 1)))
    ("queries"
     (list (jsown:new-js
             ("userNameQuery"
              (jsown:new-js
                ("userName" username)
                ("method" "TEXT_QUERY_METHOD_EQUALS"))))))))

(define-condition identity-backend-error (error)
  ((status :initarg :status :reader identity-backend-error-status)
   (code :initarg :code :reader identity-backend-error-code)
   (message :initarg :message :reader identity-backend-error-message)))

(defun identity-post (path payload)
  "POST JSON to the identity backend with the service credential.
Returns the parsed response body. Signals identity-backend-error."
  (let ((token (effective-token)))
    (unless token
      (error 'identity-backend-error
             :status 503 :code "biz_auth_unavailable"
             :message "identity service credential is not configured"))
    (handler-case
        (jsown:parse
         (dexador:request
          (concatenate 'string (identity-url) path)
          :method :post
          :headers (list (cons "content-type" "application/json")
                         (cons "authorization"
                               (concatenate 'string "Bearer " token)))
          :content (jsown:to-json payload)))
      (dexador:http-request-failed ()
        (error 'identity-backend-error
               :status 502 :code "identity_backend_error"
               :message "identity backend rejected the request"))
      (error ()
        (error 'identity-backend-error
               :status 502 :code "identity_backend_unavailable"
               :message "identity backend is unreachable")))))

(defun find-user (username)
  "Look a user up by exact username. Returns (values user-id state) or
(values nil nil). Signals identity-backend-error on failure."
  (let ((response (identity-post "/management/v1/users/_search"
                                 (search-payload username))))
    (let ((results (jsown:val-safe response "result")))
      (if (and (listp results) results)
          (values (jsown:val (first results) "userId")
                  (jsown:val-safe (first results) "state"))
          (values nil nil)))))

(defun create-identity (username &key first-name last-name)
  (let* ((password (generate-initial-password))
         (response (identity-post "/management/v1/users/human/_import"
                                  (import-payload username first-name
                                                  last-name password))))
    (values (jsown:val response "userId") password)))

(defun set-user-state (user-id transition)
  (identity-post
   (format nil "/management/v1/users/~a/~a" user-id transition) nil))

;;; --- HTTP surface -------------------------------------------------------

(defun service-principal-p ()
  (let ((principal (star.auth:current-request-principal)))
    (and principal
         (string= "service" (star.auth:request-principal-type principal))
         principal)))

(defun require-biz-auth-principal ()
  (star.frontends.http-api::with-http-boundary ()
    (unless *active*
      (star.frontends.http-api::signal-http-input-error
       503 "biz_auth_unavailable" "biz-auth is not active"))
    (unless (service-principal-p)
      (star.frontends.http-api::signal-http-input-error
       403 "access_denied" "Access denied"))
    (handler-case
        (star.authorization:authorize!
         "biz-auth:admin"
         :principal (star.auth:current-request-principal)
         :resource (star.authorization:make-authorization-resource)
         :metadata (list :route +create-path+ :method "POST"))
      (star.authorization:authorization-error ()
        (star.frontends.http-api::signal-http-input-error
         403 "access_denied" "Access denied")))))

(defun request-username ()
  (let* ((request (star.frontends.http-api::require-json-object
                   (star.frontends.http-api::parse-json-request)))
         (username (jsown:val-safe request "username")))
    (handler-case
        (normalize-biz-username username)
      (error ()
        (star.frontends.http-api::signal-http-input-error
         400 "invalid_username" "Invalid username")))))

(defun identity-error-response (condition)
  (star.frontends.http-api::signal-http-input-error
   (identity-backend-error-status condition)
   (identity-backend-error-code condition)
   (identity-backend-error-message condition)))

(defun user-response (username user-id state)
  (jsown:to-json
   (jsown:new-js
     ("username" username)
     ("user_id" user-id)
     ("status" (string-downcase
                (symbol-name (user-state-keyword (or state "USER_STATE_INITIAL"))))))))

(defun handle-create (params)
  (declare (ignore params))
  (require-biz-auth-principal)
  (let* ((request (star.frontends.http-api::require-json-object
                   (star.frontends.http-api::parse-json-request)))
         (username (handler-case
                       (normalize-biz-username (jsown:val-safe request "username"))
                     (error ()
                       (star.frontends.http-api::signal-http-input-error
                        400 "invalid_username" "Invalid username"))))
         (first-name (jsown:val-safe request "first_name"))
         (last-name (jsown:val-safe request "last_name")))
    (handler-case
        (multiple-value-bind (user-id password) (create-identity
                                                 username
                                                 :first-name first-name
                                                 :last-name last-name)
          (jsown:to-json
           (jsown:new-js
             ("username" username)
             ("user_id" user-id)
             ("initial_password" password)
             ("status" "initial"))))
      (identity-backend-error (condition)
        (identity-error-response condition)))))

(defun handle-state-transition (params transition)
  (declare (ignore params))
  (require-biz-auth-principal)
  (let ((username (request-username)))
    (handler-case
        (multiple-value-bind (user-id state) (find-user username)
          (unless user-id
            (star.frontends.http-api::signal-http-input-error
             404 "unknown_user" "No such user"))
          (set-user-state user-id transition)
          (user-response username user-id
                         (if (string= transition "_deactivate")
                             "USER_STATE_INACTIVE"
                             "USER_STATE_ACTIVE")))
      (identity-backend-error (condition)
        (identity-error-response condition)))))

(defun handle-status (params)
  (declare (ignore params))
  (require-biz-auth-principal)
  (let ((username (request-username)))
    (handler-case
        (multiple-value-bind (user-id state) (find-user username)
          (unless user-id
            (star.frontends.http-api::signal-http-input-error
             404 "unknown_user" "No such user"))
          (user-response username user-id state))
      (identity-backend-error (condition)
        (identity-error-response condition)))))

(defun username-schema ()
  (star.http.contract:string-schema :min-length 1))

(defun start ()
  ;; One shared operation shape: username-keyed JSON in, identity state out.
  (star.http.contract:upsert-http-operation
     (star.http.contract:make-http-operation
      :id "biz-auth.users.create"
      :client-name "biz-auth-users-create"
      :method :post
      :path +create-path+
      :summary "Create an identity in the canonical auth system"
      :tags '("identity")
      :authority :authenticated
      :scopes '("biz-auth:admin")
      :request-schema
      (star.http.contract:object-schema
       (list (cons "username" (username-schema))
             (cons "first_name" (star.http.contract:string-schema))
             (cons "last_name" (star.http.contract:string-schema)))
       :required '("username")
       :additional-properties nil)
      :responses (list (star.http.contract:response
                        200 "Created identity; initial_password is reveal-once."
                        (star.http.contract:object-schema
                         (list (cons "username" (star.http.contract:string-schema))
                               (cons "user_id" (star.http.contract:string-schema))
                               (cons "initial_password"
                                     (star.http.contract:string-schema :secret t))
                               (cons "status" (star.http.contract:string-schema)))
                         :required '("username" "user_id" "initial_password"
                                     "status")
                         :additional-properties nil))
                       (star.http.contract:standard-errors))))
    (dolist (spec (list (list "biz-auth.users.ban" +ban-path+ "_deactivate"
                              "Ban (deactivate) an identity")
                        (list "biz-auth.users.unban" +unban-path+ "_reactivate"
                              "Unban (reactivate) an identity")
                        (list "biz-auth.users.status" +status-path+ nil
                              "Look up an identity's state")))
      (destructuring-bind (id path transition summary) spec
        (declare (ignore transition))
        (star.http.contract:upsert-http-operation
         (star.http.contract:make-http-operation
          :id id
          :client-name (substitute #\- #\. id)
          :method :post
          :path path
          :summary summary
          :tags '("identity")
          :authority :authenticated
          :scopes '("biz-auth:admin")
          :request-schema
          (star.http.contract:object-schema
           (list (cons "username" (username-schema)))
           :required '("username")
           :additional-properties nil)
          :responses (append
                      (list (star.http.contract:response
                             200 "Identity state as JSON."
                             (star.http.contract:object-schema
                              (list (cons "username"
                                          (star.http.contract:string-schema))
                                    (cons "user_id"
                                          (star.http.contract:string-schema))
                                    (cons "status"
                                          (star.http.contract:string-schema)))
                              :required '("username" "user_id" "status")
                              :additional-properties nil)))
                      (star.http.contract:standard-errors))))))
    (star.frontends.http-api:mount-http-operation
     "biz-auth.users.create" #'handle-create)
    (star.frontends.http-api:mount-http-operation
     "biz-auth.users.ban"
     (lambda (params) (handle-state-transition params "_deactivate")))
    (star.frontends.http-api:mount-http-operation
     "biz-auth.users.unban"
     (lambda (params) (handle-state-transition params "_reactivate")))
    (star.frontends.http-api:mount-http-operation
     "biz-auth.users.status" #'handle-status)
    (setf *active* t))

(defun stop ()
  (setf *active* nil))

(star:register-addon "biz-auth"
                     :system :starintel-biz-auth
                     :start #'start
                     :stop #'stop)
