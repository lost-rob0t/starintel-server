(in-package :star-server-tests)

(def-suite database-runtime-tests
  :description "Backend-neutral StarIntel database runtime contract.")

(in-suite database-runtime-tests)

(defun database-test-principal (&rest scopes)
  (star.auth::%make-service-call-context
   :principal-id "db-test"
   :principal-type "service"
   :credential-id "cred-db-test"
   :scopes scopes
   :correlation-id "corr-db-test"
   :deadline nil))

(defun database-test-context (&rest scopes)
  (star.databases.runtime:make-database-call-context
   :principal (apply #'database-test-principal scopes)
   :tenant-id "tenant-a"
   :dataset-id "dataset-a"
   :correlation-id "corr-db-test"))

(defun setup-database-runtime-fixture ()
  (star.databases.runtime:clear-database-runtime)
  (let ((backend
          (star.databases.runtime:make-fake-database-backend "fake")))
    (star.databases.runtime:register-database-backend "fake" backend)
    (star.databases.runtime:register-database-profile
     "intel" "fake" :options '(:fixture t))
    (star.databases.runtime:register-database-operation
     "person-by-id" "intel" :read
     '(:kind :lookup :entity :person)
     :result-limit 1
     :timeout-ms 1000)
    (star.databases.runtime:register-database-operation
     "store-person" "intel" :write
     '(:kind :upsert :entity :person)
     :timeout-ms 1000)
    (star.databases.runtime:register-database-operation
     "watch-people" "intel" :subscribe
     '(:kind :changes :entity :person)
     :result-limit 100)
    backend))

(defun teardown-database-runtime-fixture ()
  (star.databases.runtime:clear-database-runtime))

(test database-read-is-authorized-before-adapter
  (let ((backend (setup-database-runtime-fixture)))
    (unwind-protect
         (let* ((context
                  (database-test-context
                   "database:read"
                   "tenant:tenant-a"
                   "dataset:dataset-a"
                   "database:intel"))
                (result
                  (star.databases.runtime:execute-database-request
                   (star.databases.runtime:make-database-request
                    :operation "person-by-id"
                    :bindings '(:id "person-1")
                    :context context))))
           (is (eq :success
                   (star.databases.runtime:database-result-status result)))
           (is (eq :read
                   (getf
                    (first
                     (star.databases.runtime:fake-database-backend-calls
                      backend))
                    :access))))
      (teardown-database-runtime-fixture))))

(test database-write-with-read-only-principal-never-hits-adapter
  (let ((backend (setup-database-runtime-fixture)))
    (unwind-protect
         (let* ((context
                  (database-test-context
                   "database:read"
                   "tenant:tenant-a"
                   "dataset:dataset-a"
                   "database:intel"))
                (result
                  (star.databases.runtime:execute-database-request
                   (star.databases.runtime:make-database-request
                    :operation "store-person"
                    :bindings '(:id "person-1" :name "Alice")
                    :idempotency-key "write-1"
                    :context context))))
           (is (eq :error
                   (star.databases.runtime:database-result-status result)))
           (is (eq :capability-denied
                   (star.databases.runtime:database-result-error-code result)))
           (is (null
                (star.databases.runtime:fake-database-backend-calls
                 backend))))
      (teardown-database-runtime-fixture))))

(test write-requires-idempotency-before-side-effects
  (let ((backend (setup-database-runtime-fixture)))
    (unwind-protect
         (let* ((context
                  (database-test-context
                   "database:write"
                   "tenant:tenant-a"
                   "dataset:dataset-a"
                   "database:intel"))
                (result
                  (star.databases.runtime:execute-database-request
                   (star.databases.runtime:make-database-request
                    :operation "store-person"
                    :bindings '(:id "person-1")
                    :context context))))
           (is (eq :error
                   (star.databases.runtime:database-result-status result)))
           (is (eq :invalid-request
                   (star.databases.runtime:database-result-error-code result)))
           (is (null
                (star.databases.runtime:fake-database-backend-calls
                 backend))))
      (teardown-database-runtime-fixture))))

(test logical-database-scope-is-enforced
  (let ((backend (setup-database-runtime-fixture)))
    (unwind-protect
         (let* ((context
                  (database-test-context
                   "database:read"
                   "tenant:tenant-a"
                   "dataset:dataset-a"
                   "database:other"))
                (result
                  (star.databases.runtime:execute-database-request
                   (star.databases.runtime:make-database-request
                    :operation "person-by-id"
                    :bindings '(:id "person-1")
                    :context context))))
           (is (eq :capability-denied
                   (star.databases.runtime:database-result-error-code result)))
           (is (null
                (star.databases.runtime:fake-database-backend-calls
                 backend))))
      (teardown-database-runtime-fixture))))

(test unhealthy-backend-is-retryable-unavailable
  (star.databases.runtime:clear-database-runtime)
  (let ((backend
          (star.databases.runtime:make-fake-database-backend
           "fake" :healthy-p nil)))
    (unwind-protect
         (progn
           (star.databases.runtime:register-database-backend "fake" backend)
           (star.databases.runtime:register-database-profile "intel" "fake")
           (star.databases.runtime:register-database-operation
            "person-by-id" "intel" :read '(:kind :lookup))
           (let ((result
                   (star.databases.runtime:execute-database-request
                    (star.databases.runtime:make-database-request
                     :operation "person-by-id"
                     :bindings '(:id "person-1")
                     :context
                     (database-test-context
                      "database:read"
                      "tenant:tenant-a"
                      "dataset:dataset-a"
                      "database:intel")))))
             (is (eq :database-unavailable
                     (star.databases.runtime:database-result-error-code result)))
             (is (star.databases.runtime:database-result-retryable-p result))
             (is (null
                  (star.databases.runtime:fake-database-backend-calls
                   backend)))))
      (teardown-database-runtime-fixture))))

(test sento-database-actor-executes-same-runtime-boundary
  (let ((backend (setup-database-runtime-fixture))
        (system
          (make-actor-system
           '(:dispatchers
             (:pinned (:workers 1 :strategy :random))
             :timeout-timer (:resolution 50 :max-size 100)))))
    (unwind-protect
         (let* ((actor
                  (star.databases.runtime:start-database-runtime-actor
                   system :name "database-runtime-test"))
                (request
                  (star.databases.runtime:make-database-request
                   :operation "person-by-id"
                   :bindings '(:id "person-2")
                   :context
                   (database-test-context
                    "database:read"
                    "tenant:tenant-a"
                    "dataset:dataset-a"
                    "database:intel")))
                (result
                  (sento.actor:ask-s
                   actor
                   (star.databases.runtime:make-database-runtime-command
                    :request request)
                   :time-out 2)))
           (is (eq :success
                   (star.databases.runtime:database-result-status result)))
           (is (= 1
                  (length
                   (star.databases.runtime:fake-database-backend-calls
                    backend)))))
      (sento.actor-system:shutdown system)
      (teardown-database-runtime-fixture))))
