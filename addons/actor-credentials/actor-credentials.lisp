(in-package :star.actor-credentials)

(defparameter +resolve-path+ "/api/v1/actor-credentials/resolve")
(defvar *accounts* (make-hash-table :test #'equal))
(defvar *accounts-lock* (bt:make-lock "actor-credential-accounts"))
(defvar *active* nil)

(defun account-name-p (value)
  (and (stringp value)
       (<= 1 (length value) 64)
       (every (lambda (character)
                (or (and (char<= #\a character) (char<= character #\z))
                    (and (char<= #\0 character) (char<= character #\9))
                    (find character "-_.")))
              value)))

(defun credential-key (actor account credential-type)
  (unless (and (account-name-p actor) (account-name-p account)
               (account-name-p credential-type))
    (error "Invalid actor credential identity"))
  (list actor account credential-type))

(defun register-credential (actor account credential-type provider)
  "Bind ACTOR/ACCOUNT/CREDENTIAL-TYPE to a provider in trusted Lisp config.
PROVIDER is called only after actor, account, and type authorization."
  (unless (functionp provider)
    (error "Actor credential provider must be a function"))
  (let ((key (credential-key actor account credential-type)))
    (bt:with-lock-held (*accounts-lock*)
      (setf (gethash key *accounts*) provider)))
  credential-type)

(defun unregister-credential (actor account credential-type)
  (bt:with-lock-held (*accounts-lock*)
    (remhash (credential-key actor account credential-type) *accounts*)))

(defun resolve-credential (actor account credential-type)
  "Return one configured JSON credential. Never expose provider error text."
  (let ((provider
          (bt:with-lock-held (*accounts-lock*)
            (gethash (credential-key actor account credential-type) *accounts*))))
    (unless provider
      (error "Actor account is unavailable"))
    (handler-case
        (let ((credential (funcall provider)))
          (unless credential
            (error "Empty credential"))
          (jsown:to-json credential)
          credential)
      (error ()
        (error "Actor account is unavailable")))))

(defun resolve-request (params)
  (declare (ignore params))
  (star.frontends.http-api::with-http-boundary ()
    (unless *active*
      (star.frontends.http-api::signal-http-input-error
       503 "actor_credentials_unavailable" "Actor credentials are unavailable"))
    (let* ((request (star.frontends.http-api::require-json-object
                     (star.frontends.http-api::parse-json-request)))
           (actor (jsown:val-safe request "actor"))
           (account (jsown:val-safe request "account"))
           (credential-type (jsown:val-safe request "credential_type"))
           (principal (star.auth:current-request-principal)))
      (unless (and (account-name-p actor) (account-name-p account)
                   (account-name-p credential-type))
        (star.frontends.http-api::signal-http-input-error
         400 "invalid_actor_credential" "Invalid actor credential identity"))
      (unless (and principal
                   (string= "service" (star.auth:request-principal-type principal)))
        (star.frontends.http-api::signal-http-input-error
         403 "access_denied" "Access denied"))
      (handler-case
          (star.authorization:authorize!
           "actor-credentials:read"
           :principal principal
           :resource (star.authorization:make-authorization-resource
                      :actor-name actor :account-id account
                      :credential-type credential-type)
           :metadata (list :route +resolve-path+ :method "POST"))
        (star.authorization:authorization-error ()
          (star.frontends.http-api::signal-http-input-error
           403 "access_denied" "Access denied")))
      (let ((credential
              (handler-case (resolve-credential actor account credential-type)
                (error ()
                  (star.frontends.http-api::signal-http-input-error
                   503 "actor_credentials_unavailable"
                   "Actor credentials are unavailable")))))
        (setf (lack.response:response-headers star.frontends.http-api::*response*)
              (append (lack.response:response-headers
                       star.frontends.http-api::*response*)
                      (list :cache-control "no-store")))
        (jsown:to-json
         (jsown:new-js
           ("actor" actor)
           ("account" account)
           ("credential_type" credential-type)
           ("credential" credential)))))))

(defun start ()
  (star.http.contract:upsert-http-operation
   (star.http.contract:make-http-operation
    :id "actor-credentials.resolve"
    :client-name "actor-credential-resolve"
    :method :post
    :path +resolve-path+
    :summary "Resolve an actor account credential for an authorized service"
    :tags '("actors" "credentials")
    :authority :authenticated
    :scopes '("actor-credentials:read")
    :request-schema
    (star.http.contract:object-schema
     (list (cons "actor" (star.http.contract:string-schema :min-length 1))
           (cons "account" (star.http.contract:string-schema :min-length 1))
           (cons "credential_type" (star.http.contract:string-schema :min-length 1)))
     :required '("actor" "account" "credential_type")
     :additional-properties nil)
    :responses
    (append
     (list
      (star.http.contract:response
       200 "Credential for the authorized actor account."
       (star.http.contract:object-schema
        (list
         (cons "actor" (star.http.contract:string-schema))
         (cons "account" (star.http.contract:string-schema))
         (cons "credential_type" (star.http.contract:string-schema))
         (cons "credential" (jsown:new-js ("x-starintel-secret" t))))
        :required '("actor" "account" "credential_type" "credential")
        :additional-properties nil)))
     (star.http.contract:standard-errors))))
  (star.frontends.http-api:mount-http-operation
   "actor-credentials.resolve" #'resolve-request)
  (setf *active* t))

(defun stop ()
  (setf *active* nil))

(star:register-addon "actor-credentials"
                     :system :starintel-actor-credentials
                     :start #'start
                     :stop #'stop)
