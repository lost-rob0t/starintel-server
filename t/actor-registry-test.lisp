(in-package :star-server-tests)

(def-suite actor-registry-tests
  :description "Pure StarLang manifest registry validation and discovery")

(in-suite actor-registry-tests)

(defun registry-test-digest (character)
  (format nil "sha256:~a" (make-string 64 :initial-element character)))

(defun registry-test-actor (name &key (accepts #()) (produces #())
                                       (capabilities #("run")))
  (list
   (cons "name" name)
   (cons "runtime" "external")
   (cons "protocol" "rabbitmq")
   (cons "endpoint" "secret.internal:5672")
   (cons "accepts" accepts)
   (cons "produces" produces)
   (cons "capabilities" capabilities)
   (cons "serviceUri" (format nil "star://pro-actors:localhost:~a" name))))

(defun registry-test-manifest
    (name actor &optional (digest (registry-test-digest #\a)))
  (list
   (cons "wireVersion" 1)
   (cons "library"
         (list (cons "name" name)
               (cons "version" "0.9.1.2")
               (cons "digest" digest)))
   (cons "imports" #())
   (cons "types" #())
   (cons "predicates" #())
   (cons "messages" #())
   (cons "actors" (vector actor))))

(defun registry-error-code (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.actor-registry:actor-registry-error (condition)
      (star.actor-registry:actor-registry-error-code condition))))

(test actor-registry-api-is-present
  (is-true (find-package :star.actor-registry))
  (is-true (fboundp 'star.actor-registry:build-actor-registry)))

(test manifest-order-does-not-change-safe-registry-projection
  (let* ((mastodon
           (registry-test-manifest
            "mastodon"
            (registry-test-actor
             "mastodon"
             :accepts #("starintel/profile@1")
             :produces #("starintel/post@1"))))
         (fediwatch
           (registry-test-manifest
            "fediwatch"
            (registry-test-actor
             "fediwatch"
             :accepts #("starintel/actor-config@0.9.2")
             :produces #("starintel/post@1"))
            (registry-test-digest #\b)))
         (left (star.actor-registry:build-actor-registry
                (vector mastodon fediwatch)))
         (right (star.actor-registry:build-actor-registry
                 (vector fediwatch mastodon)))
         (projection (star.actor-registry:actor-registry-list left)))
    (is (= 2 (star.actor-registry:actor-registry-count left)))
    (is (equalp projection
                (star.actor-registry:actor-registry-list right)))
    (is (string< (getf (aref projection 0) :service-uri)
                 (getf (aref projection 1) :service-uri)))
    (is-false (getf (aref projection 0) :endpoint))
    (is (string= "legacy_star_service_uri_v1"
                 (getf (aref projection 0) :identity-status)))))

(test lookup-and-bounded-queries-are-exact
  (let* ((manifest
           (registry-test-manifest
            "mastodon"
            (registry-test-actor
             "mastodon"
             :accepts #("starintel/profile@1")
             :produces #("starintel/post@1")
             :capabilities #("run" "preview"))))
         (registry (star.actor-registry:build-actor-registry
                    (vector manifest)))
         (uri "star://pro-actors:localhost:mastodon"))
    (is (string= "mastodon"
                 (getf (star.actor-registry:actor-registry-find registry uri)
                       :name)))
    (is-false (star.actor-registry:actor-registry-find
               registry "star://pro-actors:localhost:masto"))
    (is (= 1 (length (star.actor-registry:query-actor-registry
                      registry :capability "preview"))))
    (is (= 1 (length (star.actor-registry:query-actor-registry
                      registry :accepts "starintel/profile@1"))))
    (is (= 0 (length (star.actor-registry:query-actor-registry
                      registry :produces "starintel/account@1"))))
    (is (= 0 (length (star.actor-registry:actor-registry-list
                      registry :limit 0))))))

(test malformed-and-conflicting-manifests-fail-closed
  (let* ((actor (registry-test-actor "mastodon"))
         (manifest (registry-test-manifest "mastodon" actor))
         (conflict (registry-test-manifest
                    "mastodon" actor (registry-test-digest #\b))))
    (is (string= "duplicate_service_uri"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector manifest manifest))))))
    (is (string= "semantic_digest_conflict"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector manifest conflict))))))
    (is (string= "invalid_semantic_digest"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector
                      (registry-test-manifest
                       "bad" (registry-test-actor "bad")
                       "sha256:ABC")))))))
    (is (string= "unknown_field"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector '(("schema_version" . "1.0")
                               ("actor_id" . "mastodon"))))))))))

(test service-uri-name-mismatch-and-runtime-values-are-rejected
  (let ((mismatched (registry-test-actor "mastodon"))
        (runtime-value (registry-test-actor "fediwatch")))
    (setf (cdr (assoc "serviceUri" mismatched :test #'string=))
          "star://pro-actors:localhost:other")
    (setf (cdr (assoc "endpoint" runtime-value :test #'string=))
          (lambda () (error "must never execute")))
    (is (string= "service_uri_name_mismatch"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector
                      (registry-test-manifest "bad" mismatched)))))))
    (is (string= "non_data_manifest"
                 (registry-error-code
                  (lambda ()
                    (star.actor-registry:build-actor-registry
                     (vector
                      (registry-test-manifest "bad" runtime-value)))))))))
