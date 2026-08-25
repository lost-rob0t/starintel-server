(in-package :star-server-tests)

(def-suite authorization-policy-tests
  :description "Default-deny capability and resource authorization")

(in-suite authorization-policy-tests)

(defun make-policy-principal (id scopes &optional (type "api_client"))
  (star.auth::%make-request-principal
   :id id
   :type type
   :scopes scopes
   :credential-id (format nil "credential-~a" id)))

(defun policy-resource (&key (tenant "default") dataset actor target
                          namespace program (id "resource-1")
                          (dtype "note"))
  (star.authorization:make-authorization-resource
   :tenant-id tenant
   :dataset-id dataset
   :actor-name actor
   :target-id target
   :target-namespace namespace
   :program-id program
   :resource-id id
   :dtype dtype))

(defun policy-document (&key (id "doc-1") (dataset "dataset-a")
                          (tenant "default") (dtype "note") actor
                          namespace program)
  (let ((document
          (jsown:new-js
            ("_id" id)
            ("dataset" dataset)
            ("tenant_id" tenant)
            ("dtype" dtype)
            ("version" starintel:+starintel-doc-version+))))
    (when actor
      (setf (jsown:val document "actor") actor))
    (when namespace
      (setf (jsown:val document "target_namespace") namespace))
    (when program
      (setf (jsown:val document "program_id") program))
    document))

(test read-only-principal-cannot-write-delete-dispatch-or-lease
  (let* ((principal
           (make-policy-principal
            "reader"
            '("documents:read"
              "tenant:default"
              "dataset:dataset-a")))
         (resource (policy-resource :dataset "dataset-a")))
    (is-true
     (star.authorization:authorize!
      "documents:read" :principal principal :resource resource))
    (dolist (action
             '("documents:write"
               "documents:delete"
               "targets:dispatch"
               "targets:lease"))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         action :principal principal :resource resource)))))

(test dataset-and-tenant-scopes-deny-cross-boundary-access
  (let ((principal
          (make-policy-principal
           "dataset-a-reader"
           '("documents:read"
             "search:read"
             "views:read"
             "tenant:default"
             "dataset:dataset-a"))))
    (is-true
     (star.authorization:authorize!
      "documents:read"
      :principal principal
      :resource (policy-resource :dataset "dataset-a")))
    (signals star.authorization:authorization-error
      (star.authorization:authorize!
       "documents:read"
       :principal principal
       :resource (policy-resource :dataset "dataset-b")))
    (signals star.authorization:authorization-error
      (star.authorization:authorize!
       "documents:read"
       :principal principal
       :resource
       (policy-resource :tenant "tenant-b" :dataset "dataset-a")))))

(test direct-lookup-service-enforces-dataset-policy
  (let ((principal
          (make-policy-principal
           "direct-reader"
           '("documents:read"
             "tenant:default"
             "dataset:dataset-a")))
        (fetches 0))
    (is-true
     (star.authorization:authorized-fetch-document
      "doc-a"
      (lambda (id)
        (declare (ignore id))
        (incf fetches)
        (policy-document :dataset "dataset-a"))
      :principal principal))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-fetch-document
       "doc-b"
       (lambda (id)
         (declare (ignore id))
         (incf fetches)
         (policy-document :dataset "dataset-b"))
       :principal principal))
    (is (= 2 fetches))))

(test delete-service-authorizes-before-side-effect
  (let ((reader
          (make-policy-principal
           "reader"
           '("documents:read"
             "tenant:default"
             "dataset:dataset-a")))
        (deleter
          (make-policy-principal
           "deleter"
           '("documents:delete"
             "tenant:default"
             "dataset:dataset-a")))
        (deletes 0))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-delete-document
       "doc-a"
       (lambda (id)
         (declare (ignore id))
         (let ((document (policy-document :dataset "dataset-a")))
           (setf (jsown:val document "_rev") "1-test")
           document))
       (lambda (id revision)
         (declare (ignore id revision))
         (incf deletes))
       :principal reader))
    (is (zerop deletes))
    (star.authorization:authorized-delete-document
     "doc-a"
     (lambda (id)
       (declare (ignore id))
       (let ((document (policy-document :dataset "dataset-a")))
         (setf (jsown:val document "_rev") "1-test")
         document))
     (lambda (id revision)
       (declare (ignore id revision))
       (incf deletes)
       :deleted)
     :principal deleter)
    (is (= 1 deletes))))

(test search-query-is-backend-scoped-and-cross-dataset-search-denies
  (let ((principal
          (make-policy-principal
           "searcher"
           '("search:read"
             "tenant:default"
             "dataset:dataset-a"))))
    (let ((query
            (star.authorization:authorized-search-query
             "content:fixture"
             :principal principal)))
      (is (search "dataset:\"dataset-a\"" query))
      (is (search "tenant_id:\"default\"" query))
      (is (null (search "dataset-b" query))))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-search-query
       "content:fixture"
       :principal principal
       :requested-dataset "dataset-b"))))

(test scoped-view-removes-cross-dataset-rows-and-replaces-counts
  (let* ((principal
           (make-policy-principal
            "view-reader"
            '("views:read"
              "tenant:default"
              "dataset:dataset-a")))
         (row-a
           (jsown:new-js
             ("id" "doc-a")
             ("doc" (policy-document
                     :id "doc-a" :dataset "dataset-a"))))
         (row-b
           (jsown:new-js
             ("id" "doc-b")
             ("doc" (policy-document
                     :id "doc-b" :dataset "dataset-b"))))
         (response
           (jsown:new-js
             ("total_rows" 2)
             ("offset" 0)
             ("rows" (list row-a row-b))))
         (filtered
           (star.authorization:authorized-view-response
            response
            :principal principal
            :requested-dataset "dataset-a")))
    (is (= 1 (jsown:val filtered "total_rows")))
    (is (= 1 (length (jsown:val filtered "rows"))))
    (is (string= "doc-a"
                 (jsown:val (first (jsown:val filtered "rows")) "id")))))

(test bulk-denial-happens-before-any-publish
  (let ((principal
          (make-policy-principal
           "bulk-writer"
           '("documents:bulk"
             "documents:write"
             "tenant:default"
             "dataset:dataset-a")))
        (published 0)
        (documents
          (list (policy-document :id "a" :dataset "dataset-a")
                (policy-document :id "b" :dataset "dataset-b"))))
    (signals star.authorization:authorization-error
      (star.authorization:authorize-bulk-documents!
       documents :principal principal))
    (dolist (document documents)
      (when nil
        (star.authorization:authorized-publish-document
         document
         (lambda (value)
           (declare (ignore value))
           (incf published))
         :principal principal)))
    (is (zerop published))))

(test actor-target-namespace-and-program-scopes-are-all-required
  (let ((principal
          (make-policy-principal
           "actor-worker"
           '("targets:dispatch"
             "targets:lease"
             "tenant:default"
             "dataset:dataset-a"
             "actor:usernamegen"
             "target:target-a"
             "target-namespace:people"
             "program:program-a")
           "actor_component")))
    (is-true
     (star.authorization:authorize!
      "targets:dispatch"
      :principal principal
      :resource
      (policy-resource
       :dataset "dataset-a"
       :actor "usernamegen"
       :target "target-a"
       :namespace "people"
       :program "program-a")))
    (dolist (resource
             (list
              (policy-resource :dataset "dataset-a" :actor "other"
                               :target "target-a" :namespace "people"
                               :program "program-a")
              (policy-resource :dataset "dataset-a" :actor "usernamegen"
                               :target "target-a" :namespace "other"
                               :program "program-a")
              (policy-resource :dataset "dataset-a" :actor "usernamegen"
                               :target "target-a" :namespace "people"
                               :program "other")))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         "targets:dispatch"
         :principal principal
         :resource resource)))))

(test ordinary-target-operator-cannot-force-release-or-administer-credentials
  (let ((principal
          (make-policy-principal
           "target-operator"
           '("targets:read"
             "targets:dispatch"
             "targets:lease"
             "tenant:default"
             "dataset:dataset-a"
             "actor:actor-a"
             "target:target-a"))))
    (dolist (action
             '("targets:force-release"
               "credentials:create"
               "credentials:rotate"
               "credentials:revoke"
               "events:replay"))
      (signals star.authorization:authorization-error
        (star.authorization:authorize!
         action
         :principal principal
         :resource
         (policy-resource
          :dataset "dataset-a" :actor "actor-a" :target "target-a"))))))

(test internal-callers-require-explicit-principal-or-bound-trusted-context
  (signals star.authorization:authorization-error
    (star.authorization:authorize! "documents:read"))
  (let ((trusted
          (star.authorization:make-trusted-authorization-context
           :id "migration-service"
           :principal-type "service_instance"
           :scopes '("documents:read"
                     "tenant:default"
                     "dataset:dataset-a"))))
    (signals star.authorization:authorization-error
      (star.authorization:authorize!
       "documents:read"
       :principal trusted
       :resource (policy-resource :dataset "dataset-a")))
    (star.authorization:with-trusted-authorization-context (trusted)
      (is-true
       (star.authorization:authorize!
        "documents:read"
        :resource (policy-resource :dataset "dataset-a"))))))

(test rabbit-provenance-is-server-generated-and-does-not-trust-document-fields
  (let* ((principal
           (make-policy-principal
            "publisher"
            '("documents:write"
              "tenant:default"
              "dataset:dataset-a")))
         (service-context
           (star.auth::%make-service-call-context
            :principal-id "publisher"
            :principal-type "api_client"
            :credential-id "credential-publisher"
            :scopes '("documents:write"
                      "tenant:default"
                      "dataset:dataset-a")
            :correlation-id "corr-policy"
            :deadline 9999))
         (document
           (policy-document :dataset "dataset-a"))
         (decision
           (star.authorization:authorize!
            "documents:write"
            :principal principal
            :resource
            (star.authorization:resource-from-document document))))
    (setf (jsown:val document "principal_id") "caller-forged"
          (jsown:val document "authorization_action") "admin")
    (let* ((star.authorization:*current-authorization-decision* decision)
           (properties
             (star.frontends.http-api:service-context-properties
              "note" service-context decision))
           (headers (cdr (assoc :headers properties))))
      (is (string= "publisher"
                   (cdr (assoc "x-star-principal-id" headers
                               :test #'string=))))
      (is (string= "documents:write"
                   (cdr (assoc "x-star-authorization-action" headers
                               :test #'string=))))
      (is (null (search "caller-forged"
                        (prin1-to-string properties))))
      (is (null (search "star_sk_"
                        (prin1-to-string properties)))))))

(test authorization-audit-is-structured-and-redacted
  (let* ((principal
           (make-policy-principal
            "audit-reader"
            '("documents:read"
              "tenant:default"
              "dataset:dataset-a")))
         (captured nil)
         (star.authorization:*authorization-audit-sink*
           (lambda (event allowed-p)
             (setf captured (list event allowed-p)))))
    (star.authorization:authorize!
     "documents:read"
     :principal principal
     :resource (policy-resource :dataset "dataset-a")
     :metadata '(:route "/document/:id"
                 :method "GET"
                 :correlation-id "corr-audit"))
    (let ((serialized (jsown:to-json (first captured))))
      (is-true (second captured))
      (is (search "authorization_decision" serialized))
      (is (search "documents:read" serialized))
      (is (search "corr-audit" serialized))
      (is (null (search "star_sk_" serialized)))
      (is (null (search "verifier" serialized :test #'char-equal))))))

(test scope-vocabulary-is-closed-and-unmapped-routes-deny
  (signals star.auth:credential-lifecycle-error
    (star.authorization:validate-grant-scopes
     '("documents:read" "unknown:grant")))
  (let ((principal
          (make-policy-principal "admin" '("admin") "administrator")))
    (signals star.authorization:authorization-error
      (star.authorization:authorize!
       "unmapped:http-route" :principal principal)))
   (is (null
        (star.frontends.http-api::route-action
         :post "/not/a/registered/route"))))

(test document-update-route-requires-write-capability
  (is (string= "documents:write"
               (star.frontends.http-api::route-action
                :put "/document/document-1"))))

(test update-service-authorizes-before-side-effect
  (let* ((document (policy-document :dataset "dataset-a"))
         (patch (jsown:new-js ("data" (jsown:new-js ("value" "updated")))))
         (updated nil)
         (reader
           (make-policy-principal
            "update-reader"
            '("documents:read" "tenant:default" "dataset:dataset-a")))
         (writer
           (make-policy-principal
            "update-writer"
            '("documents:write" "tenant:default" "dataset:dataset-a"))))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-update-document
       "doc-1"
       patch
       (lambda (document-id)
         (declare (ignore document-id))
         document)
       (lambda (candidate)
         (declare (ignore candidate))
         (setf updated t))
       :principal reader))
    (is-false updated)
    (is-true
     (star.authorization:authorized-update-document
      "doc-1"
      patch
      (lambda (document-id)
        (declare (ignore document-id))
        document)
      (lambda (candidate)
        (setf updated candidate))
      :principal writer))
    (is-true updated)))
