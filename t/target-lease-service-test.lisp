(in-package :star-server-tests)

(def-suite target-lease-service-tests
  :description "Shared authenticated/authorized target lease application service")

(in-suite target-lease-service-tests)

(defun lease-test-principal (id scopes)
  (star.auth::%make-request-principal
   :id id
   :type "api_client"
   :scopes scopes
   :credential-id (format nil "credential-~a" id)))

(defun lease-test-scopes (&key (dataset "dataset-a") force-release-p)
  (append
   (list "targets:lease"
         "tenant:tenant-a"
         (format nil "dataset:~a" dataset)
         "program:program-a"
         "target-namespace:domain"
         "target:example.com"
         "actor:worker")
   (when force-release-p (list "targets:force-release"))))

(defun make-lease-test-service (&key (now 1800000000000))
  (let ((clock now)
        (counter 0))
    (values
     (star.authorization:make-target-lease-service
      (star.leases:make-memory-lease-store
       :clock (lambda () clock)
       :id-generator
       (lambda () (format nil "lease-test-~d" (incf counter))))
      :service-instance-id "server-test")
     (lambda (&optional value)
       (when value (setf clock value))
       clock))))

(defun lease-test-context
    (principal request-id &key (dataset "dataset-a") ttl-ms
                           maximum-lifetime-ms)
  (star.authorization:make-target-lease-request-context
   :principal principal
   :tenant-id "tenant-a"
   :dataset-id dataset
   :program-id "program-a"
   :target-namespace "domain"
   :target-id "example.com"
   :actor-name "worker"
   :workflow-name "default"
   :operation-class "default"
   :request-id request-id
   :deadline 1800000060000
   :ttl-ms ttl-ms
   :maximum-lifetime-ms maximum-lifetime-ms
   :metadata (jsown:new-js ("purpose" "test"))))

(defun lease-result-code (result)
  (star.authorization:target-lease-service-result-code result))

(test target-lease-acquire-is-logically-idempotent
  (multiple-value-bind (service clock) (make-lease-test-service)
    (declare (ignore clock))
    (let* ((principal
             (lease-test-principal "worker-a" (lease-test-scopes)))
           (context
             (lease-test-context principal "acquire-1" :ttl-ms 30000))
           (first (star.authorization:acquire-target-lease service context))
           (retry (star.authorization:acquire-target-lease service context))
           (conflicting
             (star.authorization:acquire-target-lease
              service
              (lease-test-context principal "acquire-1" :ttl-ms 20000))))
      (is (eq :acquired (lease-result-code first)))
      (is (eq :acquired (lease-result-code retry)))
      (is (string=
           (star.leases:lease-record-lease-id
            (star.authorization:target-lease-service-result-lease first))
           (star.leases:lease-record-lease-id
            (star.authorization:target-lease-service-result-lease retry))))
      (is (=
           (star.leases:lease-record-fencing-token
            (star.authorization:target-lease-service-result-lease first))
           (star.leases:lease-record-fencing-token
            (star.authorization:target-lease-service-result-lease retry))))
      (is (eq :idempotency-conflict (lease-result-code conflicting))))))

(test target-lease-renew-and-release-retries-are-deterministic
  (multiple-value-bind (service clock) (make-lease-test-service)
    (declare (ignore clock))
    (let* ((principal
             (lease-test-principal "worker-a" (lease-test-scopes)))
           (acquire
             (star.authorization:acquire-target-lease
              service (lease-test-context principal "acquire-2")))
           (lease (star.authorization:target-lease-service-result-lease acquire))
           (lease-id (star.leases:lease-record-lease-id lease))
           (token (star.leases:lease-record-fencing-token lease))
           (renew-context (lease-test-context principal "renew-2"))
           (renew-1
             (star.authorization:renew-target-lease
              service renew-context lease-id token))
           (renew-2
             (star.authorization:renew-target-lease
              service renew-context lease-id token))
           (release-context (lease-test-context principal "release-2"))
           (release-1
             (star.authorization:release-target-lease
              service release-context lease-id token))
           (release-2
             (star.authorization:release-target-lease
              service release-context lease-id token)))
      (is (eq :renewed (lease-result-code renew-1)))
      (is (eq :renewed (lease-result-code renew-2)))
      (is (=
           (star.leases:lease-record-expires-at
            (star.authorization:target-lease-service-result-lease renew-1))
           (star.leases:lease-record-expires-at
            (star.authorization:target-lease-service-result-lease renew-2))))
      (is (eq :released (lease-result-code release-1)))
      (is (eq :released (lease-result-code release-2))))))

(test target-lease-authorization-fails-closed
  (multiple-value-bind (service clock) (make-lease-test-service)
    (declare (ignore clock))
    (let* ((allowed
             (lease-test-principal "worker-a" (lease-test-scopes)))
           (missing-capability
             (lease-test-principal
              "worker-no-lease"
              '("tenant:tenant-a" "dataset:dataset-a" "program:program-a"
                "target-namespace:domain" "target:example.com" "actor:worker")))
           (other-dataset
             (lease-test-principal
              "worker-b" (lease-test-scopes :dataset "dataset-b")))
           (acquire
             (star.authorization:acquire-target-lease
              service (lease-test-context allowed "acquire-auth"))))
      (is (eq :acquired (lease-result-code acquire)))
      (is (eq :unauthenticated
              (lease-result-code
               (star.authorization:get-target-lease
                service (lease-test-context nil "anon-get")))))
      (is (eq :unauthorized
              (lease-result-code
               (star.authorization:get-target-lease
                service
                (lease-test-context missing-capability "denied-get")))))
      ;; The caller is authorized for dataset-b but the stored lease is bound
      ;; to dataset-a. Inspection hides the existence of the other scope.
      (is (eq :not-found
              (lease-result-code
               (star.authorization:get-target-lease
                service
                (lease-test-context
                 other-dataset "hidden-get" :dataset "dataset-b"))))))))

(test target-lease-list-does-not-disclose-hidden-records
  (multiple-value-bind (service clock) (make-lease-test-service)
    (declare (ignore clock))
    (let* ((owner-a
             (lease-test-principal "worker-a" (lease-test-scopes)))
           (reader-b
             (lease-test-principal
              "worker-b" (lease-test-scopes :dataset "dataset-b"))))
      (is (eq :acquired
              (lease-result-code
               (star.authorization:acquire-target-lease
                service (lease-test-context owner-a "acquire-list")))))
      (let ((result
              (star.authorization:list-target-leases
               service
               (star.authorization:make-target-lease-request-context
                :principal reader-b
                :dataset-id "dataset-b"
                :request-id "list-hidden"
                :deadline 1800000060000))))
        (is (eq :listed (lease-result-code result)))
        (is (null
             (star.authorization:target-lease-service-result-leases result)))))))

(test target-lease-revoke-requires-elevated-authority
  (multiple-value-bind (service clock) (make-lease-test-service)
    (declare (ignore clock))
    (let* ((owner
             (lease-test-principal "worker-a" (lease-test-scopes)))
           (operator
             (lease-test-principal
              "operator-a" (lease-test-scopes :force-release-p t)))
           (acquire
             (star.authorization:acquire-target-lease
              service (lease-test-context owner "acquire-revoke")))
           (lease (star.authorization:target-lease-service-result-lease acquire))
           (lease-id (star.leases:lease-record-lease-id lease))
           (token (star.leases:lease-record-fencing-token lease)))
      (is (eq :unauthorized
              (lease-result-code
               (star.authorization:revoke-target-lease
                service
                (lease-test-context owner "revoke-denied")
                lease-id token "test-revoke"))))
      (is (eq :revoked
              (lease-result-code
               (star.authorization:revoke-target-lease
                service
                (lease-test-context operator "revoke-allowed")
                lease-id token "test-revoke")))))))
