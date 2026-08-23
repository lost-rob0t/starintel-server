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
      "{\"ok\":true,\"userCtx\":{\"name\":null,\"roles\":[]}}"))))

(test expired-pooled-session-is-discarded-and-reauthenticated
  (let ((next-id 0)
        (authenticated-ids nil)
        (disconnected-ids nil)
        (stale-id nil))
    (flet ((new-client ()
             (list :id (incf next-id)))
           (authenticate (client)
             (push (getf client :id) authenticated-ids))
           (session-valid-p (client)
             (not (eql stale-id (getf client :id))))
           (disconnect (client)
             (push (getf client :id) disconnected-ids)))
      (let ((pool
              (star.databases.couchdb::make-session-aware-couchdb-pool
               :name "session-renewal-test"
               :connector #'new-client
               :authenticator #'authenticate
               :session-valid-p #'session-valid-p
               :disconnector #'disconnect
               :max-open-count 1
               :max-idle-count 1)))
        (let ((first (anypool:fetch pool)))
          (is (= 1 (getf first :id)))
          (is (equal '(1) authenticated-ids))
          (anypool:putback first pool)
          (setf stale-id 1)
          (let ((replacement (anypool:fetch pool)))
            (unwind-protect
                 (progn
                   (is (= 2 (getf replacement :id)))
                   (is (equal '(2 1) authenticated-ids))
                   (is (equal '(1) disconnected-ids)))
              (anypool:putback replacement pool))))))))

(test star-couchdb-pool-uses-session-renewal-policy
  (let ((connect-count 0)
        (auth-count 0)
        (stale-p nil)
        (disconnect-count 0))
    (let ((pool
            (star.databases.couchdb::make-star-couchdb-pool
             :name "star-session-policy-test"
             :max-open-count 1
             :max-idle-count 1
             :connector
             (lambda ()
               (incf connect-count)
               (list :generation connect-count))
             :authenticator
             (lambda (client)
               (declare (ignore client))
               (incf auth-count))
             :session-valid-p
             (lambda (client)
               (declare (ignore client))
               (not stale-p))
             :disconnector
             (lambda (client)
               (declare (ignore client))
               (incf disconnect-count)))))
      (let ((first (anypool:fetch pool)))
        (anypool:putback first pool)
        (setf stale-p t)
        (let ((replacement (anypool:fetch pool)))
          (unwind-protect
               (progn
                 (is (= 2 connect-count))
                 (is (= 2 auth-count))
                 (is (= 1 disconnect-count))
                 (is (= 2 (getf replacement :generation))))
            (anypool:putback replacement pool)))))))
