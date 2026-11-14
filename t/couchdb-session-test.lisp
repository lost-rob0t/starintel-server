(in-package :star-server-tests)

(def-suite couchdb-session-tests
  :description "Hermetic CouchDB AuthSession renewal tests")

(in-suite couchdb-session-tests)

(test couchdb-session-probe-requires-authenticated-user
  (is (string=
       "admin"
       (star.databases.couchdb::couchdb-session-username
        "{\"ok\":true,\"userCtx\":{\"name\":\"admin\",\"roles\":[\"_admin\"]}}")))
  (is-false
   (star.databases.couchdb::couchdb-session-username
    "{\"ok\":true,\"userCtx\":{\"name\":null,\"roles\":[]}}"))
  (is
   (star.databases.couchdb::couchdb-client-session-valid-p
    :fake-client
    :username "admin"
    :request-fn
    (lambda (client)
      (declare (ignore client))
      "{\"ok\":true,\"userCtx\":{\"name\":\"admin\",\"roles\":[\"_admin\"]}}")))
  (is-false
   (star.databases.couchdb::couchdb-client-session-valid-p
    :fake-client
    :username "admin"
    :request-fn
    (lambda (client)
      (declare (ignore client))
      "{\"ok\":true,\"userCtx\":{\"name\":null,\"roles\":[]}}")))
  ;; A probe that fails (timeout, network error) counts as expired: the
  ;; boundary must fall back to renewal, never use the stale client.
  (is-false
   (star.databases.couchdb::couchdb-client-session-valid-p
    :fake-client
    :username "admin"
    :request-fn
    (lambda (client)
      (declare (ignore client))
      (error "simulated probe timeout")))))

(test expired-session-is-renewed-at-checkout-boundary
  (let ((next-id 0)
        (reauthenticated nil)
        (validated nil))
    (flet ((new-client ()
             (list :id (incf next-id)))
           (session-valid-p (client)
             (push (getf client :id) validated)
             (getf client :valid))
           (reauthenticate (client)
             (push (getf client :id) reauthenticated)
             (setf (getf client :valid) t)))
      (let ((pool
              (star.databases.couchdb::make-star-couchdb-pool
               :name "session-renewal-boundary-test"
               :connector #'new-client
               :disconnector (constantly nil)
               :max-open-count 1
               :max-idle-count 1)))
        ;; One idle client whose session has expired.
        (let ((stale (anypool:fetch pool)))
          (setf (getf stale :valid) nil)
          (anypool:putback stale pool))
        (star.databases.couchdb:with-couchdb-client
            (client pool
                    :session-valid-p #'session-valid-p
                    :authenticator #'reauthenticate)
          (is (= 1 (getf client :id)))
          (is (getf client :valid)
              "renewed client is session-valid inside the body"))
        (is (equal '(1) reauthenticated)
           "the expired session was renewed before the body ran")
        (is (= 0 (anypool:pool-active-count pool))
            "no active-count leak after the renewal checkout")
        ;; A still-valid session is validated but never reauthenticated.
        (star.databases.couchdb:with-couchdb-client
            (client pool
                    :session-valid-p #'session-valid-p
                    :authenticator #'reauthenticate)
          (is (getf client :valid)))
        (is (equal '(1) reauthenticated)
            "valid sessions are not reauthenticated")))))

(test pools-perform-no-session-work-under-the-pool-lock
  "The #221 policy lives at with-couchdb-client; pool-level ping must not
perform session validation and connectors must not authenticate (#433)."
  (is-true (funcall (anypool::pool-ping star.frontends.http-api::*couchdb-pool*)
                    :never-a-client)
           "live pool ping accepts any object without probing sessions")
  (let ((auth-pool (star.auth::make-auth-couchdb-pool)))
    (is-true (funcall (anypool::pool-ping auth-pool) :never-a-client)
             "auth pool ping accepts any object without probing sessions"))
  (is (eq star.databases.couchdb::*couchdb-session-validator*
          #'star.databases.couchdb::couchdb-client-session-valid-p)
      "the checkout boundary uses the shared /_session validator")
  (is (eq star.databases.couchdb::*couchdb-session-reauth*
          #'star.databases.couchdb::reauthenticate-couchdb-client)
      "the checkout boundary renews with the configured credentials"))

(test authenticator-failure-returns-client-and-propagates
  (let ((next-id 0))
    (flet ((new-client ()
             (list :id (incf next-id)))
           (failing-reauth (client)
             (error "renewal unavailable for client ~a"
                    (getf client :id))))
      (let ((pool
              (star.databases.couchdb::make-star-couchdb-pool
               :name "auth-failure-cleanup-test"
               :connector #'new-client
               :disconnector (constantly nil)
               :max-open-count 1
               :max-idle-count 1)))
        (signals error
          (star.databases.couchdb:with-couchdb-client
              (client pool
                      :session-valid-p (constantly nil)
                      :authenticator #'failing-reauth)
            (is-false t "body must not run when renewal fails")))
        (is (= 0 (anypool:pool-active-count pool))
            "client was returned to the pool despite renewal failure")
        (is (= 1 (anypool:pool-idle-count pool))
            "the returned client stays pooled for later renewal")))))

(test checkout-independence-while-session-probe-parked
  "A parked session validation on one pooled client must not block an
unrelated checkout and return of a different client (#433)."
  (let ((next-id 0)
        (parked-client-id nil)
        (b-client-id nil)
        (a-in-probe (bt:make-semaphore :name "a-in-probe"))
        (release-a (bt:make-semaphore :name "release-a"))
        (b-done (bt:make-semaphore :name "b-done")))
    (flet ((new-client ()
             (list :id (incf next-id)))
           (park-first-validation (client)
             (unless parked-client-id
               (setf parked-client-id (getf client :id))
               (bt:signal-semaphore a-in-probe)
               (bt:wait-on-semaphore release-a :timeout 30))
             t))
      (let ((pool
              (star.databases.couchdb::make-star-couchdb-pool
               :name "checkout-independence-test"
               :connector #'new-client
               :disconnector (constantly nil)
               :max-open-count 2
               :max-idle-count 2)))
        ;; Two distinct idle clients; connectors never validate, so the
        ;; parking probe is still armed.
        (let ((warm-a (anypool:fetch pool))
              (warm-b (anypool:fetch pool)))
          (anypool:putback warm-b pool)
          (anypool:putback warm-a pool))
        (let ((thread-a
                (bt:make-thread
                 (lambda ()
                   (star.databases.couchdb:with-couchdb-client
                       (client pool :session-valid-p #'park-first-validation)))
                 :name "parked-probe-thread"))
              (thread-b nil))
          (unwind-protect
               (progn
                 (is-true (bt:wait-on-semaphore a-in-probe :timeout 10)
                          "fixture: the session probe never started")
                 (setf thread-b
                       (bt:make-thread
                        (lambda ()
                          (star.databases.couchdb:with-couchdb-client
                              (client pool
                                      :session-valid-p (constantly t)
                                      :authenticator (constantly t))
                            (setf b-client-id (getf client :id)))
                          (bt:signal-semaphore b-done))
                        :name "independent-checkout-thread"))
                 (is-true (bt:wait-on-semaphore b-done :timeout 2)
                          "unrelated checkout and return must complete while another client's session probe is parked")
                 (is (and b-client-id
                          parked-client-id
                          (/= b-client-id parked-client-id))
                     "thread B must use the distinct idle client, not wait behind thread A's probe"))
            (bt:signal-semaphore release-a)
            (bt:join-thread thread-a)
            (when thread-b (bt:join-thread thread-b))
            (is (= 0 (anypool:pool-active-count pool))
                "no active-count leak after both checkouts returned")
            (is (= 2 (anypool:pool-idle-count pool))
                "both clients idle after both returns")))))))

(test application-and-auth-pools-share-session-validator
  ;; Since the #433 pool-lock fix the pools wrap the validator in a lambda
  ;; (validation happens outside the pool lock), so identity of the raw
  ;; ping function is no longer asserted. The contract that remains: both
  ;; pools are built from the same *couchdb-session-validator*, which is
  ;; the cached session probe.
  (is (eq #'star.databases.couchdb::couchdb-client-session-valid-p
          star.databases.couchdb::*couchdb-session-validator*))
  (is (functionp (anypool::pool-ping star.frontends.http-api::*couchdb-pool*)))
  (let ((auth-pool (star.auth::make-auth-couchdb-pool)))
    (is (functionp (anypool::pool-ping auth-pool)))))

(test pooled-session-probe-result-is-cached-per-client
  ;; A fresh valid cache entry answers the ping without an HTTP round trip;
  ;; the real probe would fail on this sentinel client.
  (let ((client (list :sentinel))
        (star:*couchdb-session-cache-seconds* 60))
    (bt2:with-lock-held (star.databases.couchdb::*session-validity-cache-lock*)
      (setf (gethash client star.databases.couchdb::*session-validity-cache*)
            (list :valid t
                  :checked-at (get-internal-real-time)
                  :username "admin")))
    (is-true
     (star.databases.couchdb::couchdb-client-session-valid-p
      client
      :username "admin"))))

(test stale-session-probe-cache-entry-is-reprobed
  ;; Expired entries must not mask a dead session.
  (let ((client (list :sentinel))
        (star:*couchdb-session-cache-seconds* 60))
    (bt2:with-lock-held (star.databases.couchdb::*session-validity-cache-lock*)
      (setf (gethash client star.databases.couchdb::*session-validity-cache*)
            (list :valid t
                  :checked-at
                  (- (get-internal-real-time)
                     (* 120 internal-time-units-per-second))
                  :username "admin")))
    (is-false
     (star.databases.couchdb::couchdb-client-session-valid-p
      client
      :username "admin"))))

(test session-probe-caching-disabled-at-zero-ttl
  (let ((client (list :sentinel))
        (star:*couchdb-session-cache-seconds* 0))
    (bt2:with-lock-held (star.databases.couchdb::*session-validity-cache-lock*)
      (setf (gethash client star.databases.couchdb::*session-validity-cache*)
            (list :valid t
                  :checked-at (get-internal-real-time)
                  :username "admin")))
    (is-false
     (star.databases.couchdb::couchdb-client-session-valid-p
      client
      :username "admin"))))
origin/master
