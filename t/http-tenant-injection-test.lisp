(in-package :star-server-tests)

(def-suite http-tenant-injection-tests
  :description "Server-side tenant injection before persistence and egress redaction")

(in-suite http-tenant-injection-tests)

(defun injection-document (&key (dataset "dataset-a") tenant)
  (let ((document
          (jsown:new-js
            ("_id" "doc-injection-1")
            ("dataset" dataset)
            ("dtype" "note")
            ("schema_version" "0.9.0")
            ("version" 1)
            ("date_added" "2026-09-14T00:00:00Z")
            ("date_updated" "2026-09-14T00:00:00Z")
            ("sources")
            ("evidence")
            ("data" (jsown:new-js)))))
    (when tenant
      (setf (jsown:val document "tenant_id") tenant))
    document))

(test stamp-injects-resolved-tenant-when-unconfigured
  (let ((star:*tenant-fallback* nil)
        (star:*tenant-dataset-map* nil))
    (let ((document (injection-document)))
      (star.frontends.http-api:stamp-server-tenant! document)
      (is (string= "default" (jsown:val document "tenant_id"))))))

(test stamp-injects-fallback-and-dataset-map-tenants
  (let ((star:*tenant-fallback* "ci")
        (star:*tenant-dataset-map* '(("dataset-a" . "llm"))))
    (let ((mapped (injection-document :dataset "dataset-a"))
          (unmapped (injection-document :dataset "dataset-b")))
      (star.frontends.http-api:stamp-server-tenant! mapped)
      (star.frontends.http-api:stamp-server-tenant! unmapped)
      (is (string= "llm" (jsown:val mapped "tenant_id")))
      (is (string= "ci" (jsown:val unmapped "tenant_id"))))))

(test stamp-never-rewrites-a-declared-tenant
  (let ((star:*tenant-fallback* "ci")
        (star:*tenant-dataset-map* '(("dataset-a" . "llm"))))
    (let ((document (injection-document :tenant "default")))
      (star.frontends.http-api:stamp-server-tenant! document)
      (is (string= "default" (jsown:val document "tenant_id"))))))

(test strip-removes-injected-tenant-and-nothing-else
  (let ((star:*tenant-fallback* "ci"))
    (let ((document (injection-document)))
      (star.frontends.http-api:stamp-server-tenant! document)
      (is (string= "ci" (jsown:val document "tenant_id")))
      (star.frontends.http-api:strip-server-tenant-fields document)
      (is (null (jsown:keyp document "tenant_id")))
      (is (string= "doc-injection-1" (jsown:val document "_id")))
      (is (string= "dataset-a" (jsown:val document "dataset")))
      (is (string= "note" (jsown:val document "dtype"))))))

(test strip-leaves-tenantless-documents-untouched
  (let ((document (injection-document)))
    (star.frontends.http-api:strip-server-tenant-fields document)
    (is (string= "doc-injection-1" (jsown:val document "_id")))
    (is (null (jsown:keyp document "tenant_id")))))

(test strip-from-rows-handles-list-and-vector-rows
  (let* ((doc-a (star.frontends.http-api:stamp-server-tenant!
                 (injection-document)))
         (doc-b (star.frontends.http-api:stamp-server-tenant!
                 (injection-document)))
         (list-response
          (jsown:new-js
            ("rows" (list (jsown:new-js ("id" "a") ("doc" doc-a))
                         (jsown:new-js ("id" "b"))))))
         (vector-response
          (jsown:new-js
            ("rows" (vector (jsown:new-js ("id" "c") ("doc" doc-b)))))))
    (star.frontends.http-api:strip-server-tenant-from-rows list-response)
    (star.frontends.http-api:strip-server-tenant-from-rows vector-response)
    (dolist (row (jsown:val list-response "rows"))
      (when (jsown:keyp row "doc")
        (is (null (jsown:keyp (jsown:val row "doc") "tenant_id")))))
    (loop for row across (jsown:val vector-response "rows")
          do (is (null (jsown:keyp (jsown:val row "doc") "tenant_id"))))))

(test strip-from-search-body-roundtrips-json-and-redacts
  (let* ((stored (star.frontends.http-api:stamp-server-tenant!
                  (injection-document)))
         (body (jsown:to-json
                (jsown:new-js
                  ("total" 1)
                  ("rows" (list (jsown:new-js
                                 ("id" "doc-injection-1")
                                 ("doc" stored)))))))
         (stripped (star.frontends.http-api:strip-server-tenant-from-search-body
                    body))
         (parsed (jsown:parse stripped)))
    (is (= 1 (jsown:val parsed "total")))
    (let ((doc (jsown:val (first (jsown:val parsed "rows")) "doc")))
      (is (null (jsown:keyp doc "tenant_id")))
      (is (string= "doc-injection-1" (jsown:val doc "_id"))))))

(test stamp-then-strip-roundtrips-the-client-contract
  (let ((star:*tenant-fallback* "ci")
        (star:*tenant-dataset-map* nil))
    (let* ((stored (star.frontends.http-api:stamp-server-tenant!
                    (injection-document))))
      (is (string= "ci" (jsown:val stored "tenant_id")))
      (let ((returned (star.frontends.http-api:strip-server-tenant-fields
                       stored)))
        (is (null (jsown:keyp returned "tenant_id")))
        (dolist (key '("_id" "dataset" "dtype" "version"))
          (is (equal (jsown:val (injection-document) key)
                     (jsown:val returned key))))))))

(test consumer-decode-exempts-injected-tenant-and-restores-it
  (let ((star:*tenant-fallback* "ci"))
    (let* ((stamped (star.frontends.http-api:stamp-server-tenant!
                     (injection-document :dataset "testing-debug")))
           (message (cons (jsown:to-json stamped) 1))
           (decoded (star.rabbit:decode-rabbit-document message)))
      (is (string= "ci" (jsown:val decoded "tenant_id")))
      (is (string= "doc-injection-1" (jsown:val decoded "_id")))
      (is (string= "note" (jsown:val decoded "dtype"))))))

(test consumer-decode-still-rejects-schema-invalid-documents
  (let ((message
          (cons
           (jsown:to-json
            (jsown:new-js
              ("_id" "bad-doc")
              ("dataset" "testing-debug")
              ("dtype" "note")
              ("schema_version" "0.9.0")
              ("version" 1)
              ("undeclared_top_level_field" "schema violation")
              ("date_added" "2026-09-14T00:00:00Z")
              ("date_updated" "2026-09-14T00:00:00Z")
              ("sources")
              ("evidence")
              ("data")))
           1)))
    (signals star.consumers:schema-invalid-delivery-error
      (star.rabbit:decode-rabbit-document message))))
