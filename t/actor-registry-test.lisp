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
