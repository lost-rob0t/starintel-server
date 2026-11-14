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

(defvar *session-validity-cache*
  (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key)
  "Per-client cached /_session probe results, trusted for a bounded TTL.

Keys are weak so discarded pool clients drop their entries.")

(defvar *session-validity-cache-lock*
  (bt2:make-lock :name "couchdb-session-validity-cache")
  "Guards concurrent probe-result bookkeeping across pooled clients.")

(defun session-cache-ttl-internal ()
  "Cache TTL in internal time units, or NIL when caching is disabled."
  (let ((seconds star:*couchdb-session-cache-seconds*))
    (and (numberp seconds)
         (plusp seconds)
         (* seconds internal-time-units-per-second))))

(defun couchdb-client-session-valid-p
    (client
     &key
       (username star:*couchdb-user*)
       (request-fn #'couchdb-session-response))
  "Return true only when CLIENT still has an authenticated CouchDB session.

AnyPool invokes this ping on every checkout.  To keep ingest from issuing
one =GET /_session= per operation, a successful probe is trusted for
=star:*couchdb-session-cache-seconds*=.  Caching applies only to the real
probe; test-supplied request functions always run."
  (let ((ttl (session-cache-ttl-internal)))
    (flet ((probe ()
             (handler-case
                 (let ((actual-username
                         (couchdb-session-username
                          (funcall request-fn client))))
                   (and actual-username
                        (string= username actual-username)))
               (error () nil))))
      (if (or (null ttl) (not (eq request-fn #'couchdb-session-response)))
          (probe)
          (let ((now (get-internal-real-time)))
            (bt2:with-lock-held (*session-validity-cache-lock*)
              (let ((entry (gethash client *session-validity-cache*)))
                (when (and entry
                           (getf entry :valid)
                           (< (- now (getf entry :checked-at)) ttl)
                           (string= username (getf entry :username)))
                  (return-from couchdb-client-session-valid-p t))))
            (let ((valid (probe)))
              (bt2:with-lock-held (*session-validity-cache-lock*)
                (setf (gethash client *session-validity-cache*)
                      (list :valid valid
                            :checked-at now
                            :username username)))
              valid))))))

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
       (session-valid-p #'couchdb-client-session-valid-p)
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

(defun star.auth::make-auth-couchdb-pool
    (&key
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
       (session-valid-p #'couchdb-client-session-valid-p)
       (disconnector
         (lambda (client)
           (cl-couch:remove-auth client))))
  "Create the authorization store pool with the shared session renewal policy."
  (make-star-couchdb-pool
   :name "starintel-auth-couchdb-connections"
   :max-open-count 10
   :max-idle-count 5
   :connector connector
   :authenticator authenticator
   :session-valid-p session-valid-p
   :disconnector disconnector))

(defun install-couchdb-session-hardening ()
  "Install checkout validation on the already-constructed live HTTP pool.

Preserve the pool object itself: tests and runtime components may retain its
identity.  Its existing connector already creates and authenticates fresh
clients, so adding the shared session probe is sufficient to make AnyPool
discard an expired idle client and invoke that connector for a replacement."
  (setf (anypool::pool-ping star.frontends.http-api::*couchdb-pool*)
        #'couchdb-client-session-valid-p)
  t)

(install-couchdb-session-hardening)
