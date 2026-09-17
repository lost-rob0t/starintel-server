(in-package :star-server-tests)

(def-suite migration-tests
  :description "Prolog migration candidate invariants and optimistic apply path")

(in-suite migration-tests)

(defun migration-test-current-document ()
  (let ((document
          (starintel:encode
           (starintel:new-host
            "migration-tests"
            :ip "192.0.2.44"
            :os "linux"))))
    (setf (jsown:val document "_rev") "7-migration-fixture"
          (jsown:val document "tenant_id") "tenant-a"
          (jsown:val document "schema_version") "0.8.0")
    document))

(defun migration-test-candidate (current)
  (let ((candidate
          (jsown:with-injective-reader
            (jsown:parse (jsown:to-json current)))))
    (setf (jsown:val candidate "schema_version") "0.9.0")
    (let ((lineage (or (jsown:val-safe candidate "lineage")
                       (jsown:empty-object))))
      (setf (jsown:val lineage "migration_from") "0.8.0"
            (jsown:val candidate "lineage") lineage))
    candidate))

(defun migration-error-code (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.migrations:migration-candidate-error (condition)
      (star.migrations:migration-candidate-error-code condition))))

(test migration-target-is-immutable-schema-not-release
  (let* ((current (migration-test-current-document))
         (candidate (migration-test-candidate current)))
    (setf (jsown:val candidate "schema_version") "0.9.1")
    (is
     (string=
      "unsupported_target_schema"
      (migration-error-code
       (lambda ()
         (star.migrations:prepare-migration-candidate
          current candidate)))))))

(test migration-rejects-stale-revision
  (let* ((current (migration-test-current-document))
         (candidate (migration-test-candidate current)))
    (setf (jsown:val candidate "_rev") "8-raced")
    (is
     (string=
      "stale_revision"
      (migration-error-code
       (lambda ()
         (star.migrations:prepare-migration-candidate
          current candidate)))))))

(test migration-rejects-tenant-switch
  (let* ((current (migration-test-current-document))
         (candidate (migration-test-candidate current)))
    (setf (jsown:val candidate "tenant_id") "tenant-b")
    (is
     (string=
      "tenant_changed"
      (migration-error-code
       (lambda ()
         (star.migrations:prepare-migration-candidate
          current candidate)))))))

(test migration-dry-run-validates-without-saving
  (let* ((current (migration-test-current-document))
         (candidate (migration-test-candidate current))
         (saved nil)
         (authorized nil)
         (outcome
           (star.databases.couchdb:apply-migration-candidate
            (lambda (id)
              (is (string= id (jsown:val current "_id")))
              current)
            (lambda (prepared)
              (setf saved prepared)
              prepared)
            candidate
            :write-p nil
            :authorize-fn
            (lambda (fresh)
              (is (eq fresh current))
              (setf authorized t)))))
    (is-true authorized)
    (is-false saved)
    (is (eq :ready
            (star.databases.couchdb:migration-outcome-status outcome)))))

(test migration-authorization-precedes-save
  (let* ((current (migration-test-current-document))
         (candidate (migration-test-candidate current))
         (saved nil))
    (signals simple-error
      (star.databases.couchdb:apply-migration-candidate
       (lambda (id)
         (declare (ignore id))
         current)
       (lambda (prepared)
         (setf saved prepared)
         prepared)
       candidate
       :authorize-fn
       (lambda (fresh)
         (declare (ignore fresh))
         (error "denied"))))
    (is-false saved)))

(test migration-candidate-scope-check-is-exact
  (let ((candidate
          (migration-test-candidate
           (migration-test-current-document))))
    (is-true
     (star.frontends.http-api::migration-document-scope-p
      candidate "tenant-a" "migration-tests"))
    (is-false
     (star.frontends.http-api::migration-document-scope-p
      candidate "tenant-b" "migration-tests"))
    (is-false
     (star.frontends.http-api::migration-document-scope-p
      candidate "tenant-a" "other-dataset"))))

(test migration-route-actions-use-existing-capabilities
  (is
   (string=
    "views:read"
    (star.frontends.http-api::route-action
     :get "/api/v1/migrations/preview")))
  (is
   (string=
    "documents:write"
    (star.frontends.http-api::route-action
     :post "/api/v1/migrations/apply"))))

(defun run-migration-tests ()
  (run! 'migration-tests))
