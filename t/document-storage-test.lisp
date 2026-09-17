(in-package :star-server-tests)

(def-suite document-storage-tests
  :description "Tenant document lifecycle, tier policy, and storage backend semantics")

(in-suite document-storage-tests)

(defun make-storage-test-document (&key (id "doc-storage-1")
                                        (tenant "tenant-a")
                                        (dataset "dataset-a"))
  (jsown:new-js
    ("_id" id)
    ("_rev" "1-test")
    ("dataset" dataset)
    ("dtype" "note")
    ("schema_version" "0.9.0")
    ("date_added" "2026-09-16T00:00:00Z")
    ("tenant_id" tenant)
    ("data" (jsown:new-js ("body" "payload")))
    ("extensions" (jsown:empty-object))))

(test storage-tier-validation-is-closed
  (dolist (tier '("hot" "warm" "cold" "archive"))
    (is (string= tier (star.storage:normalize-storage-tier tier))))
  (signals error
    (star.storage:normalize-storage-tier "glacier-deep-magic")))

(test tenant-storage-policy-is-server-owned
  (let ((star::*document-storage-default-tier* "hot")
        (star::*document-storage-tenant-tiers*
          "tenant-a=cold,tenant-b=warm"))
    (is (string= "cold"
                 (star.storage:tenant-default-storage-tier "tenant-a")))
    (is (string= "warm"
                 (star.storage:tenant-default-storage-tier "tenant-b")))
    (is (string= "hot"
                 (star.storage:tenant-default-storage-tier "tenant-c")))))

(test tier-backend-map-is-server-owned
  (let ((star::*document-storage-tier-backends*
          "hot=couchdb,warm=memory,cold=s3,archive=s3"))
    (is (string= "couchdb" (star.storage:tier-backend-name "hot")))
    (is (string= "memory" (star.storage:tier-backend-name "warm")))
    (is (string= "s3" (star.storage:tier-backend-name "cold")))))

(test memory-storage-backend-round-trips
  (let ((backend (star.storage:make-memory-storage-backend :name "test-memory")))
    (star.storage:storage-put backend "a/b.json" "{\"ok\":true}")
    (is (string= "{\"ok\":true}"
                 (star.storage:storage-get backend "a/b.json")))
    (is (= (length "{\"ok\":true}")
           (getf (star.storage:storage-head backend "a/b.json") :size)))
    (is-true (star.storage:storage-delete backend "a/b.json"))
    (signals star.storage:storage-backend-error
      (star.storage:storage-get backend "a/b.json"))))

(test storage-object-key-is-tenant-dataset-and-content-scoped
  (let* ((document
           (make-storage-test-document
            :id "doc/unsafe"
            :tenant "tenant/a"
            :dataset "dataset/a"))
         (logical-key (star.storage:storage-object-key document))
         (immutable-key (star.storage::storage-object-key document "abcd")))
    (is (string= "tenants/tenant_2F_a/datasets/dataset_2F_a/documents/doc_2F_unsafe.json"
                 logical-key))
    (is (string= "tenants/tenant_2F_a/datasets/dataset_2F_a/documents/doc_2F_unsafe.abcd.json"
                 immutable-key))))

(test cold-storage-stub-retains-authorization-envelope-only
  (let* ((document (make-storage-test-document))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "cold")
             ("backend" "s3")
             ("state" "offloaded")
             ("object_key" "tenants/tenant-a/datasets/dataset-a/documents/doc-storage-1.hash.json")))
         (stub (star.storage::storage-document-stub document metadata)))
    (is (string= "doc-storage-1" (jsown:val stub "_id")))
    (is (string= "tenant-a" (jsown:val stub "tenant_id")))
    (is (string= "dataset-a" (jsown:val stub "dataset")))
    (is (string= "note" (jsown:val stub "dtype")))
    (is-false (jsown:keyp stub "data"))
    (is (string= "cold" (star.storage:document-storage-tier stub)))
    (is (string= "s3" (star.storage:document-storage-backend-name stub)))))

(test canonical-object-payload-removes-couchdb-and-server-private-state
  (let* ((document (make-storage-test-document))
         (extensions (jsown:val document "extensions"))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "warm")
             ("backend" "memory")
             ("state" "mirrored")
             ("object_key" "object.json"))))
    (setf (jsown:val extensions star.databases.couchdb::+outbox-extension-key+)
          (vector (jsown:new-js ("status" "pending")))
          (jsown:val extensions star.databases.couchdb::+mutation-ledger-extension-key+)
          (jsown:new-js ("mutation-a" "hash-a")))
    (star.storage::set-storage-metadata! document metadata)
    (let* ((payload (star.storage::canonical-storage-payload document))
           (parsed (jsown:parse payload))
           (public-extensions (jsown:val parsed "extensions")))
      (is-false (jsown:keyp parsed "_rev"))
      (is-false
       (jsown:keyp public-extensions
                   star.storage:+document-storage-extension-key+))
      (is-false
       (jsown:keyp public-extensions
                   star.databases.couchdb::+outbox-extension-key+))
      (is-false
       (jsown:keyp public-extensions
                   star.databases.couchdb::+mutation-ledger-extension-key+))
      (is (string= "payload"
                   (jsown:val (jsown:val parsed "data") "body"))))))

(test rabbit-ingest-discards-actor-supplied-placement-state
  (let* ((document (make-storage-test-document))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "archive")
             ("backend" "evil-backend")
             ("state" "offloaded")
             ("object_key" "attacker-controlled"))))
    (star.storage::set-storage-metadata! document metadata)
    (let ((incoming (star.rabbit::public-incoming-storage-document document)))
      (is-false (star.storage::document-storage-metadata-present-p incoming))
      (is (string= "payload"
                   (jsown:val (jsown:val incoming "data") "body"))))))

(test ordinary-document-egress-redacts-placement-and-injected-tenant
  (let* ((document (make-storage-test-document))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "archive")
             ("backend" "s3")
             ("state" "offloaded")
             ("object_key" "secret/object/key.json"))))
    (star.storage::set-storage-metadata! document metadata)
    (star.frontends.http-api:strip-server-tenant-fields document)
    (is-false (jsown:keyp document "tenant_id"))
    (is-false
     (jsown:keyp (jsown:val document "extensions")
                 star.storage:+document-storage-extension-key+))))

(test lifecycle-envelope-exposes-placement-explicitly
  (let* ((document (make-storage-test-document))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "cold")
             ("backend" "s3")
             ("state" "offloaded")
             ("object_key" "tenants/tenant-a/datasets/dataset-a/documents/doc-storage-1.abcd.json")
             ("content_sha256" "abcd")
             ("updated_at" "2026-09-16T00:00:00Z"))))
    (star.storage::set-storage-metadata! document metadata)
    (let ((lifecycle (star.storage:document-storage-lifecycle-json document)))
      (is (string= "doc-storage-1" (jsown:val lifecycle "document_id")))
      (is (string= "tenant-a" (jsown:val lifecycle "tenant")))
      (is (string= "cold" (jsown:val lifecycle "tier")))
      (is (string= "s3" (jsown:val lifecycle "backend")))
      (is (string= "offloaded" (jsown:val lifecycle "state"))))))

(test explicit-hot-placement-survives-default-tier-changes
  (let* ((document (make-storage-test-document))
         (metadata
           (jsown:new-js
             ("version" 1)
             ("tier" "hot")
             ("backend" "couchdb")
             ("state" "resident")
             ("object_key" :null))))
    (star.storage::set-storage-metadata! document metadata)
    (is-true (star.storage::document-storage-metadata-present-p document))
    (is (string= "hot" (star.storage:document-storage-tier document)))))

(test lifecycle-http-contract-is-versioned-and-tenant-authorized
  (dolist (case
           '(("documents.lifecycle.get" :get "documents:read")
             ("documents.lifecycle.update" :put "documents:write")))
    (destructuring-bind (id method scope) case
      (let ((operation (star.http.contract:find-http-operation id)))
        (is (eq method (star.http.contract:http-operation-method operation)))
        (is (string= "/api/v1/documents/:id/lifecycle"
                     (star.http.contract:http-operation-path operation)))
        (is (equal (list scope)
                   (star.http.contract:http-operation-scopes operation)))
        (is (equal '("id")
                   (star.http.contract:http-operation-path-parameters operation)))))))

(test lifecycle-routes-inherit-document-capabilities
  (is (string= "documents:read"
               (star.frontends.http-api::route-action
                :get "/api/v1/documents/doc-1/lifecycle")))
  (is (string= "documents:write"
               (star.frontends.http-api::route-action
                :put "/api/v1/documents/doc-1/lifecycle"))))

(test s3-backend-is-disabled-when-credentials-are-incomplete
  (let ((star::*s3-bucket* nil)
        (star::*s3-access-key-id* nil)
        (star::*s3-secret-access-key* nil))
    (is (null
         (star.storage:make-s3-storage-backend-from-settings :errorp nil)))))
