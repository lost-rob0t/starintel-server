(in-package :star-server-tests)

(def-suite actor-manifest-options-tests
  :description "Generic actor-manifest target option contract")

(in-suite actor-manifest-options-tests)

(defun make-option-manifest (actor specs)
  (jsown:new-js
    ("_id" (format nil "manifest:~a" actor))
    ("dataset" "test")
    ("dtype" "actor-manifest")
    ("schema_version" starintel:+starintel-doc-version+)
    ("version" 1)
    ("date_added" "2026-09-12T00:00:00Z")
    ("date_updated" "2026-09-12T00:00:00Z")
    ("sources" #())
    ("evidence" #())
    ("data"
     (jsown:new-js
       ("actor" actor)
       ("consumer_path" (format nil "queue.~a" actor))
       ("target_options" specs)))))

(test actor-manifest-options-read-both-target-option-shapes
  (let ((options
          (vector
           (jsown:new-js ("max_username_rate_per_second" 2.5))
           (jsown:new-js ("key" "depth") ("value" 4)))))
    (is (= 2.5
           (star.actor-manifest-options:target-option-value
            options "max_username_rate_per_second")))
    (is (= 4
           (star.actor-manifest-options:target-option-value
            options "depth")))
    (is (string= "fallback"
                 (star.actor-manifest-options:target-option-value
                  options "missing" "fallback")))))

(test actor-manifest-options-merge-defaults-without-overriding-explicit-values
  (let* ((manifest
           (make-option-manifest
            "username-targets"
            (vector
             (jsown:new-js
               ("key" "max_username_rate_per_second")
               ("default" 1.0))
             (jsown:new-js
               ("key" "target_options")
               ("default" #())))))
         (explicit
           (vector
            (jsown:new-js ("max_username_rate_per_second" 5.0))))
         (merged
           (star.actor-manifest-options:merge-target-options-with-manifest
            manifest explicit)))
    (is (string= "username-targets"
                 (star.actor-manifest-options:actor-manifest-actor manifest)))
    (is (= 5.0
           (star.actor-manifest-options:target-option-value
            merged "max_username_rate_per_second")))
    (is (vectorp
         (star.actor-manifest-options:target-option-value
          merged "target_options")))))

(test actor-manifest-options-enforces-required-options
  (let ((manifest
          (make-option-manifest
           "username-targets"
           (vector
            (jsown:new-js
              ("key" "target_actor")
              ("required" :true))))))
    (signals star.actor-manifest-options::missing-required-target-option
      (star.actor-manifest-options:merge-target-options-with-manifest
       manifest #()))
    (let ((merged
            (star.actor-manifest-options:merge-target-options-with-manifest
             manifest
             (vector (jsown:new-js ("target_actor" "userhunt"))))))
      (is (string= "userhunt"
                   (star.actor-manifest-options:target-option-value
                    merged "target_actor"))))))

(test actor-manifest-options-rejects-non-manifest-documents
  (let ((document
          (jsown:new-js
            ("dtype" "target")
            ("data" (jsown:new-js ("actor" "username-targets"))))))
    (signals star.actor-manifest-options::invalid-actor-manifest
      (star.actor-manifest-options:actor-manifest-actor document))))

(test external-actor-contract-is-publicly-discoverable
  (let* ((document (star.frontends.http-api::capabilities-document))
         (data (jsown:val document "data"))
         (features (jsown:val data "features"))
         (contracts (jsown:val data "actor_contracts")))
    (is (eq :true (jsown:val features "actor_manifests")))
    (is (eq :true (jsown:val features "actor_target_options")))
    (is (eq :true (jsown:val features "remote_actor_dispatch")))
    (is (string= "actor-manifest"
                 (jsown:val contracts "manifest_dtype")))
    (is (string= "data.target_options"
                 (jsown:val contracts "manifest_target_options_field")))
    (is (string= "documents.ingest.target"
                 (jsown:val contracts "canonical_target_ingest_key")))
    (is (string= "documents"
                 (jsown:val contracts "remote_target_exchange")))
    (is (string= "documents.target.dispatch.<actor>"
                 (jsown:val contracts "remote_target_routing_key_template")))))
