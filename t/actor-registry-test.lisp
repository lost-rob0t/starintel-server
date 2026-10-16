(in-package :star-server-tests)

(def-suite actor-registry-tests)
(in-suite actor-registry-tests)

(defun actor-registry-test-manifest
    (uri &key (kind "actor") (digest "sha256:test")
              (visible :true) (name "username-hunt"))
  (jsown:new-js
    ("resourceUri" uri)
    ("resourceKind" kind)
    ("semantic"
     (jsown:new-js
       ("name" name)
       ("version" "1.0.0")
       ("digest" digest)))
    ("accepts"
     (jsown:new-js
       ("targets" (list "username"))
       ("documents" nil)
       ("messages" nil)))
    ("produces"
     (jsown:new-js
       ("targets" nil)
       ("documents" (list "account" "relation" "evidence"))
       ("messages" nil)))
    ("capabilities" (list "run"))
    ("operatorVisible" visible)
    ("provenance"
     (jsown:new-js
       ("sourcePackage" "starintel-pro-actors")))))

(test registry-rejects-malformed-and-mismatched-star-identities
  (signals star.actors::invalid-actor-manifest
    (star.actors::build-actor-registry
     (list (actor-registry-test-manifest "not-a-star-uri"))))
  (signals star.actors::invalid-actor-manifest
    (star.actors::build-actor-registry
     (list (actor-registry-test-manifest
            "star://local/service/user-hunt" :kind "actor"))))
  (signals star.actors::invalid-actor-manifest
    (star.actors::build-actor-registry
     (list (actor-registry-test-manifest
            "star://local/actor/user-hunt" :kind "worker")))))

(test registry-rejects-missing-semantic-identity
  (let ((manifest
          (actor-registry-test-manifest
           "star://local/actor/user-hunt")))
    (setf (jsown:val (jsown:val manifest "semantic") "digest") "")
    (signals star.actors::invalid-actor-manifest
      (star.actors::build-actor-registry (list manifest)))))

(test registry-rejects-duplicate-and-conflicting-resource-claims
  (let ((manifest
          (actor-registry-test-manifest
           "star://local/actor/user-hunt")))
    (signals star.actors::actor-registry-conflict
      (star.actors::build-actor-registry (list manifest manifest))))
  (signals star.actors::actor-registry-conflict
    (star.actors::build-actor-registry
     (list
      (actor-registry-test-manifest
       "star://local/actor/user-hunt" :digest "sha256:first")
      (actor-registry-test-manifest
       "star://local/actor/user-hunt" :digest "sha256:second")))))

(test runtime-observation-does-not-create-catalog-entries
  (let ((registry (star.actors::build-actor-registry nil))
        (observations (make-hash-table :test #'equal)))
    (star.actors::record-actor-runtime-status
     "star://local/actor/internal-worker" "online"
     :observations observations)
    (is (null
         (star.actors::actor-registry-public-entries
          registry observations)))))

(test manifest-replacement-discards-stale-runtime-state
  (let ((star.actors::*actor-registry* (make-hash-table :test #'equal))
        (star.actors::*actor-runtime-observations*
          (make-hash-table :test #'equal))
        (star.actors::*actor-runtime-bindings*
          (make-hash-table :test #'equal)))
    (star.actors:install-actor-manifests
     (list (actor-registry-test-manifest
            "star://local/actor/first")))
    (star.actors:record-actor-runtime-status
     "star://local/actor/first" "online" :ready-p t)
    (setf (gethash "star://local/actor/first"
                   star.actors::*actor-runtime-bindings*)
          :stale-binding)
    (star.actors:install-actor-manifests
     (list (actor-registry-test-manifest
            "star://local/actor/second")))
    (is (zerop (hash-table-count
                star.actors::*actor-runtime-observations*)))
    (is (zerop (hash-table-count
                star.actors::*actor-runtime-bindings*)))
    (let ((entry (first (star.actors:actor-registry-public-entries))))
      (is (string= "star://local/actor/second"
                   (jsown:val entry "resourceUri")))
      (is (string= "unavailable" (jsown:val entry "status"))))))

(test public-catalog-is-visible-safe-sorted-and-liveness-aware
  (let* ((registry
           (star.actors::build-actor-registry
            (list
             (actor-registry-test-manifest
              "star://local/actor/zeta" :name "zeta")
             (actor-registry-test-manifest
              "star://local/actor/hidden" :name "hidden" :visible :false)
             (actor-registry-test-manifest
              "star://local/actor/alpha" :name "alpha"))))
         (observations (make-hash-table :test #'equal)))
    (star.actors::record-actor-runtime-status
     "star://local/actor/alpha" "online"
     :ready-p t
     :observed-at "2026-10-03T12:00:00Z"
     :observations observations)
    (let* ((entries
             (star.actors::actor-registry-public-entries
              registry observations))
           (alpha (first entries))
           (zeta (second entries)))
      (is (= 2 (length entries)))
      (is (string= "star://local/actor/alpha"
                   (jsown:val alpha "resourceUri")))
      (is (string= "online" (jsown:val alpha "status")))
      (is (eq :true (jsown:val alpha "ready")))
      (is (string= "unavailable" (jsown:val zeta "status")))
      (is (eq :false (jsown:val zeta "ready")))
      (is-false (jsown:keyp zeta "observedAt"))
      (is-false (jsown:keyp alpha "runtimePath"))
      (is-false (jsown:keyp alpha "queue"))
      (is-false (jsown:keyp alpha "routingKey")))))

(test local-runtime-binding-reports-a-live-sento-actor
  (let* ((manifest
           (actor-registry-test-manifest
            "star://local/actor/live-test" :name "live-test"))
         (star.actors::*actor-registry*
           (star.actors::build-actor-registry (list manifest)))
         (star.actors::*actor-runtime-observations*
           (make-hash-table :test #'equal))
         (star.actors::*actor-runtime-bindings*
           (make-hash-table :test #'equal))
         (system (make-actor-system))
         (actor nil))
    (unwind-protect
         (progn
           (setf actor
                 (actor-of system :name "live-test" :receive #'identity))
           (star.actors:bind-local-actor-runtime
            "star://local/actor/live-test" actor)
           (let ((entry
                   (first (star.actors:actor-registry-public-entries))))
             (is (string= "online" (jsown:val entry "status")))
             (is (eq :true (jsown:val entry "ready")))))
      (sento.actor-context:shutdown system))))

(test actor-list-contract-is-canonical-authenticated-and-advertised
  (let ((operation (star.http.contract:find-http-operation "actors.list")))
    (is (eq :get (star.http.contract:http-operation-method operation)))
    (is (string= "/v1/actors"
                 (star.http.contract:http-operation-path operation)))
    (is (eq :authenticated
            (star.http.contract:http-operation-authority operation)))
    (is (equal '("actors:read")
               (star.http.contract:http-operation-scopes operation))))
  (is (string= "actors:read"
               (star.frontends.http-api::route-action :get "/v1/actors")))
  (let* ((document (star.frontends.http-api::capabilities-document))
         (data (jsown:val document "data"))
         (endpoints (jsown:val data "endpoints"))
         (endpoint
           (find "actors" endpoints
                 :key (lambda (item) (jsown:val item "id"))
                 :test #'string=)))
    (is (not (null endpoint)))
    (is (string= "/v1/actors" (jsown:val endpoint "path")))
    (is (equal '("actors:read") (jsown:val endpoint "scopes")))))

(test actor-list-scope-can-be-issued
  (is (equal '("actors:read")
             (star.authorization:validate-grant-scopes '("actors:read"))))
  (signals star.auth:credential-lifecycle-error
    (star.authorization:validate-grant-scopes '("actors:unknown"))))

(test issued-actor-scope-authorizes-live-discovery-only
  (let* ((pepper "isolated-actor-discovery-test-pepper")
         (star:*auth-pepper* pepper)
         (store (star.auth:make-memory-credential-store))
         (registry (star.actors::build-actor-registry nil))
         (observations (make-hash-table :test #'equal))
         (bindings (make-hash-table :test #'equal))
         (server nil)
         (base "http://127.0.0.1:18537"))
    (multiple-value-bind (admin-record admin-key)
        (star.auth:create-api-key "test-admin" "administrator" '("admin")
                                  :store store)
      (declare (ignore admin-record))
      (labels ((request (path &key key (method :get) content)
                 (format t "~&Actor discovery request: ~a ~a~%" method path)
                 (handler-case
                     (dexador:request
                      (concatenate 'string base path)
                      :method method :content content :keep-alive nil
                      :headers (append
                                (when key
                                  (list (cons "Authorization"
                                              (format nil "Bearer ~a" key))))
                                (when content
                                  '(("Content-Type" . "application/json")))))
                   (dexador:http-request-failed (condition)
                     (values (dexador:response-body condition)
                             (dexador:response-status condition)))))
               (issue (scope)
                 (request "/auth/credentials" :key admin-key :method :post
                          :content (jsown:to-json
                                    (jsown:new-js
                                      ("owner" "discovery-test")
                                      ("principal_type" "api_client")
                                      ("scopes" (list scope)))))))
        (unwind-protect
             (progn
               (setf server
                     (clack:clackup
                      (lambda (env)
                        (format t "~&Actor discovery received: ~a ~a~%"
                                (getf env :request-method) (getf env :path-info))
                        ;; Clack workers need these bindings on each request.
                        (let ((star:*auth-mode* "api-key")
                              (star:*auth-dev-bypass* nil)
                              (star:*auth-pepper* pepper)
                              (star.auth:*credential-store* store)
                              (star.actors::*actor-registry* registry)
                              (star.actors::*actor-runtime-observations* observations)
                              (star.actors::*actor-runtime-bindings* bindings))
                          (lack.component:call star.frontends.http-api::*server* env)))
                      :server :hunchentoot :address "127.0.0.1"
                      :port 18537 :silent t :debug nil))
               ;; CLACKUP starts its listener asynchronously. Wait for it before
               ;; sending the first request, without retrying application failures.
               (loop repeat 50
                     when (handler-case
                              (let ((socket (usocket:socket-connect
                                             "127.0.0.1" 18537 :timeout 0.2)))
                                (usocket:socket-close socket)
                                t)
                            (usocket:socket-error () nil))
                       return t
                     do (sleep 0.1)
                     finally (error "Actor discovery test listener did not start"))
               (multiple-value-bind (body status) (issue "actors:read")
                 (is (= 201 status))
                 (when (= 201 status)
                   (let ((key (jsown:val (jsown:parse body) "api_key")))
                     (multiple-value-bind (catalog code headers)
                         (request "/v1/actors" :key key)
                       (is (= 200 code))
                       (is (string= "no-store" (gethash "cache-control" headers)))
                       (is (string= "starintel-actor-registry-v1"
                                    (jsown:val (jsown:val (jsown:parse catalog) "data")
                                               "schema"))))
                     (is (= 403 (nth-value 1
                                  (request "/new/target/test" :key key :method :post)))))))
               (multiple-value-bind (body status) (issue "actors:unknown")
                 (is (= 400 status))
                 (is (string= "invalid_scope" (jsown:val (jsown:parse body) "code"))))
               (is (= 401 (nth-value 1 (request "/v1/actors"))))
               (is (= 401 (nth-value 1 (request "/v1/actors" :key "invalid"))))
               (multiple-value-bind (body status) (issue "documents:read")
                 (is (= 201 status))
                 (when (= 201 status)
                   (is (= 403 (nth-value 1
                                (request "/v1/actors"
                                         :key (jsown:val (jsown:parse body) "api_key"))))))))
          (when server (clack:stop server)))))))
