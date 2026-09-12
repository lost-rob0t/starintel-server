(in-package :star-server-tests)

(def-suite http-target-tenant-tests
  :description "Tenant-scoped target-list regression and adversarial isolation")

(in-suite http-target-tenant-tests)

(defun make-target-list-principal (id scopes)
  (star.auth::%make-request-principal
   :id id
   :type "api_client"
   :scopes scopes
   :credential-id (format nil "credential-~a" id)))

(defun make-tenant-target-document (id tenant actor &key (dataset "dataset-a"))
  (let ((document
          (jsown:new-js
            ("_id" id)
            ("tenant_id" tenant)
            ("dtype" "target")
            ("version" starintel:+starintel-doc-version+)
            ("data"
             (jsown:new-js
               ("actor" actor)
               ("target" (format nil "target-value-~a" id)))))))
    (when dataset
      (setf (jsown:val document "dataset") dataset))
    document))

(defun target-view-response (&rest documents)
  (jsown:new-js
    ("rows"
     (loop for document in documents
           collect (jsown:new-js ("doc" document))))))

(test target-list-regression-honors-explicit-tenant-and-composite-view-key
  (let* ((principal
           (make-target-list-principal
            "agent-zero-reader"
            '("targets:read"
              "tenant:agent-zero"
              "actor:*")))
         (document
           (make-tenant-target-document
            "target-a" "agent-zero" "test-actor"))
         (query-calls 0)
         (captured-arguments nil)
         (result
           (star.frontends.http-api::query-authorized-target-documents
            nil
            "records"
            "test-actor"
            "agent-zero"
            principal
            '(:route "/targets/:actor" :method "GET")
            :query-fn
            (lambda (client database design view &rest arguments)
              (declare (ignore client))
              (incf query-calls)
              (is (string= "records" database))
              (is (string= "targets" design))
              (is (string= "by_tenant_actor" view))
              (setf captured-arguments arguments)
              (target-view-response document)))))
    ;; This is the exact regression credential from #169: no dataset:* or
    ;; target:* grant is needed for the tenant+actor target-list contract.
    (is (= 1 query-calls))
    (is (equal '("agent-zero" "test-actor")
               (getf captured-arguments :key)))
    (is-true (getf captured-arguments :include-docs))
    (is (= 1 (length result)))
    (is (string= "target-a" (jsown:val (first result) "_id")))))

(test target-list-denies-ungranted-tenant-before-backend-io
  (let ((principal
          (make-target-list-principal
           "agent-zero-reader"
           '("targets:read"
             "tenant:agent-zero"
             "actor:*")))
        (query-calls 0))
    (signals star.authorization:authorization-error
      (star.frontends.http-api::query-authorized-target-documents
       nil
       "records"
       "test-actor"
       "other-tenant"
       principal
       nil
       :query-fn
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (incf query-calls)
         (error "unauthorized target list reached backend I/O"))))
    (is (zerop query-calls))))

(test target-list-drops-poisoned-cross-tenant-and-cross-actor-rows
  (let* ((principal
           (make-target-list-principal
            "wildcard-reader"
            '("targets:read"
              "tenant:*"
              "actor:*")))
         (allowed
           (make-tenant-target-document
            "allowed" "agent-zero" "test-actor"))
         (wrong-tenant
           (make-tenant-target-document
            "wrong-tenant" "other-tenant" "test-actor"))
         (wrong-actor
           (make-tenant-target-document
            "wrong-actor" "agent-zero" "other-actor"))
         (result
           (star.frontends.http-api::query-authorized-target-documents
            nil
            "records"
            "test-actor"
            "agent-zero"
            principal
            nil
            :query-fn
            (lambda (&rest arguments)
              (declare (ignore arguments))
              ;; Simulate stale/misindexed/poisoned backend output. Wildcard
              ;; authorization must not turn these rows into response leakage.
              (target-view-response allowed wrong-tenant wrong-actor)))))
    (is (= 1 (length result)))
    (is (string= "allowed" (jsown:val (first result) "_id")))))

(test target-list-preserves-present-dataset-restrictions
  (let* ((principal
           (make-target-list-principal
            "dataset-reader"
            '("targets:read"
              "tenant:agent-zero"
              "actor:test-actor"
              "dataset:dataset-a"
              "target:*")))
         (allowed
           (make-tenant-target-document
            "target-a" "agent-zero" "test-actor"
            :dataset "dataset-a"))
         (wrong-dataset
           (make-tenant-target-document
            "target-b" "agent-zero" "test-actor"
            :dataset "dataset-b"))
         (missing-dataset
           (make-tenant-target-document
            "target-c" "agent-zero" "test-actor"
            :dataset nil))
         (result
           (star.frontends.http-api::query-authorized-target-documents
            nil
            "records"
            "test-actor"
            "agent-zero"
            principal
            nil
            :query-fn
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (target-view-response
               allowed wrong-dataset missing-dataset)))))
    (is (= 1 (length result)))
    (is (string= "target-a" (jsown:val (first result) "_id")))))

(test target-list-preserves-present-target-restrictions
  (let* ((principal
           (make-target-list-principal
            "target-reader"
            '("targets:read"
              "tenant:agent-zero"
              "actor:test-actor"
              "target:target-a")))
         (allowed
           (make-tenant-target-document
            "target-a" "agent-zero" "test-actor"))
         (wrong-target
           (make-tenant-target-document
            "target-b" "agent-zero" "test-actor"))
         (result
           (star.frontends.http-api::query-authorized-target-documents
            nil
            "records"
            "test-actor"
            "agent-zero"
            principal
            nil
            :query-fn
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (target-view-response allowed wrong-target)))))
    (is (= 1 (length result)))
    (is (string= "target-a" (jsown:val (first result) "_id")))))

(test target-list-missing-tenant-default-is-explicit-and-empty-is-rejected
  (is (string= "default"
               (star.frontends.http-api::target-list-tenant
                '(("actor" . "test-actor")))))
  (is (string= "agent-zero"
               (star.frontends.http-api::target-list-tenant
                '(("tenant" . "agent-zero")))))
  (signals star.frontends.http-api::http-input-error
    (star.frontends.http-api::target-list-tenant
     '(("tenant" . "")))))

(test tenant-scoped-target-view-is-checked-in
  (let* ((design-documents
           (star.databases.couchdb::checked-in-design-document-map))
         (targets (gethash "targets" design-documents))
         (views (and targets (jsown:val-safe targets "views")))
         (view (and views (jsown:val-safe views "by_tenant_actor")))
         (map-source (and view (jsown:val-safe view "map"))))
    (is-true view)
    (is (search "tenant_id" map-source))
    (is (search "emit([tenant, v('actor')]" map-source))))
