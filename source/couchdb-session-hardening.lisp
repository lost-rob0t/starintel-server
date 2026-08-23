(in-package :star.databases.couchdb)

(defun couchdb-session-username (response)
  "Return the authenticated CouchDB username from a /_session RESPONSE."
  (handler-case
      (let* ((document (if (stringp response)
                           (jsown:parse response)
                           response))
             (context (and document (jsown:val-safe document "userCtx")))
             (username (and context (jsown:val-safe context "name"))))
        (and (stringp username) username))
    (error () nil)))

(defun couchdb-session-response (client)
  "Fetch CLIENT's CouchDB /_session response using its live cookie jar."
  (dexador:request
   (quri:merge-uris
    (quri:make-uri :path "/_session")
    (cl-couch:couchdb-url client))
   :method :get
   :headers (cl-couch:couchdb-headers client)
   :cookie-jar (cl-couch:couchdb-cookie client)
   :keep-alive t))

(defun couchdb-client-session-valid-p
    (client
     &key
       (username star:*couchdb-user*)
       (request-fn #'couchdb-session-response))
  "Return true only when CLIENT still has an authenticated CouchDB session."
  (handler-case
      (let ((actual-username
              (couchdb-session-username
               (funcall request-fn client))))
        (and actual-username
             (string= username actual-username)))
    (error () nil)))

(defun make-session-aware-couchdb-pool
    (&key
       name
       connector
       authenticator
       session-valid-p
       disconnector
       max-open-count
       max-idle-count)
  "Create an AnyPool that replaces clients whose CouchDB session is stale."
  (unless connector
    (error "CouchDB pool requires a connector."))
  (unless authenticator
    (error "CouchDB pool requires an authenticator."))
  (unless session-valid-p
    (error "CouchDB pool requires a session validator."))
  (anypool:make-pool
   :name name
   :connector
   (lambda ()
     (let ((client (funcall connector)))
       (funcall authenticator client)
       client))
   :ping session-valid-p
   :disconnector disconnector
   :max-open-count max-open-count
   :max-idle-count max-idle-count))

(defun make-star-couchdb-pool
    (&key
       name
       max-open-count
       max-idle-count
       (connector
         (lambda ()
           (cl-couch:new-couchdb
            star:*couchdb-host*
            star:*couchdb-port*
            :scheme star:*couchdb-scheme*)))
       (authenticator
         (lambda (client)
           (cl-couch:password-auth
            client
            star:*couchdb-user*
            star:*couchdb-password*)))
       (session-valid-p
         (lambda (client)
           (couchdb-client-session-valid-p client)))
       (disconnector
         (lambda (client)
           (cl-couch:remove-auth client))))
  "Create a StarIntel CouchDB pool that renews expired AuthSession clients."
  (make-session-aware-couchdb-pool
   :name name
   :connector connector
   :authenticator authenticator
   :session-valid-p session-valid-p
   :disconnector disconnector
   :max-open-count max-open-count
   :max-idle-count max-idle-count))

(defun install-couchdb-session-hardening ()
  "Replace the main application pool with session-aware CouchDB clients."
  (setf *couchdb-pool*
        (make-star-couchdb-pool
         :name "couchdb-connections"
         :max-open-count 20
         :max-idle-count 10))
  t)

(defun star.auth::make-auth-couchdb-pool ()
  "Create the authorization store pool with the same session renewal policy."
  (make-star-couchdb-pool
   :name "starintel-auth-couchdb-connections"
   :max-open-count 10
   :max-idle-count 5))

(install-couchdb-session-hardening)
