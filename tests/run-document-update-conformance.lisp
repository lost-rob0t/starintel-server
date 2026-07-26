(in-package :cl-user)

(defun update-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun update-test-document
    (id &key (revision "2-current") (target "example.org"))
  (let ((document (jsown:empty-object))
        (data (jsown:empty-object)))
    (setf (jsown:val data "actor") "scanner"
          (jsown:val data "target") target
          (jsown:val data "delay") 0
          (jsown:val data "recurring") :false
          (jsown:val data "options") #()
          (jsown:val document "_id") id
          (jsown:val document "_rev") revision
          (jsown:val document "dataset") "default"
          (jsown:val document "dtype") "target"
          (jsown:val document "schema_version") "0.9.0"
          (jsown:val document "version") 1
          (jsown:val document "date_added") "2026-07-26T00:00:00Z"
          (jsown:val document "date_updated") "2026-07-26T00:00:00Z"
          (jsown:val document "sources") #()
          (jsown:val document "evidence") #()
          (jsown:val document "data") data)
    document))

(defun update-test-patch (&key revision target id dtype)
  (let ((patch (jsown:empty-object)))
    (when revision
      (setf (jsown:val patch "_rev") revision))
    (when id
      (setf (jsown:val patch "_id") id))
    (when dtype
      (setf (jsown:val patch "dtype") dtype))
    (when target
      (let ((data (jsown:empty-object)))
        (setf (jsown:val data "target") target
              (jsown:val patch "data") data)))
    patch))

(defun test-stale-client-revision-refetches-and-updates ()
  (let* ((existing (update-test-document "target:stale"))
         (patch (update-test-patch
                 :revision "1-stale" :target "updated.example"))
         (existing-before (jsown:to-json existing))
         (patch-before (jsown:to-json patch))
         (saved nil)
         (outcome
           (star.databases.couchdb:upsert-document-update
            (lambda (id)
              (declare (ignore id))
              (star.databases.couchdb::clone-document-update-json existing))
            (lambda (candidate)
              (setf saved
                    (star.databases.couchdb::clone-document-update-json
                     candidate))
              candidate)
            "target:stale"
            patch)))
    (update-check
     (eq :updated
         (star.databases.couchdb:document-update-outcome-status outcome))
     "Stale revision outcome was ~s"
     (star.databases.couchdb:document-update-outcome-status outcome))
    (update-check
     (string= "2-current" (jsown:val saved "_rev"))
     "Stale revision overwrote latest revision: ~s"
     (jsown:val saved "_rev"))
    (update-check
     (string= "updated.example"
              (jsown:val (jsown:val saved "data") "target"))
     "Patch data was not merged")
    (update-check (string= existing-before (jsown:to-json existing))
                  "Existing document was mutated")
    (update-check (string= patch-before (jsown:to-json patch))
                  "Patch was mutated")))

(defun test-missing-document-strips_stale_revision_before_create ()
  (let* ((patch (update-test-document
                 "target:create" :revision "9-stale-create"))
         (patch-before (jsown:to-json patch))
         (saved nil)
         (outcome
           (star.databases.couchdb:upsert-document-update
            (lambda (id) (declare (ignore id)) nil)
            (lambda (candidate)
              (setf saved
                    (star.databases.couchdb::clone-document-update-json
                     candidate))
              candidate)
            "target:create"
            patch)))
    (update-check
     (eq :created
         (star.databases.couchdb:document-update-outcome-status outcome))
     "Missing document outcome was not created")
    (update-check
     (not (star.databases.couchdb::outbox-object-has-key-p saved "_rev"))
     "New insert retained caller revision")
    (update-check (string= patch-before (jsown:to-json patch))
                  "Insert patch was mutated")))

(defun test-patch-cannot_change_identity_or_schema ()
  (dolist (patch
           (list
            (update-test-patch :id "target:other")
            (update-test-patch :dtype "person")))
    (let ((outcome
            (star.databases.couchdb:upsert-document-update
             (lambda (id)
               (declare (ignore id))
               (update-test-document "target:protected"))
             (lambda (candidate)
               (declare (ignore candidate))
               (error "Invalid patch reached save"))
             "target:protected"
             patch)))
      (update-check
       (eq :validation-failed
           (star.databases.couchdb:document-update-outcome-status outcome))
       "Protected field patch returned ~s"
       (star.databases.couchdb:document-update-outcome-status outcome)))))

(defun test-conflict_retries_are_bounded_and_refetch_latest_revision ()
  (let ((loads 0)
        (saves 0)
        (seen-revisions nil)
        (patch (update-test-patch
                :revision "1-stale" :target "retry.example")))
    (let ((outcome
            (star.databases.couchdb:upsert-document-update
             (lambda (id)
               (declare (ignore id))
               (incf loads)
               (update-test-document
                "target:retry"
                :revision (format nil "~d-current" loads)))
             (lambda (candidate)
               (incf saves)
               (push (jsown:val candidate "_rev") seen-revisions)
               (error 'star.databases.couchdb:document-update-store-conflict))
             "target:retry"
             patch
             :max-attempts 3)))
      (update-check
       (eq :conflict-exhausted
           (star.databases.couchdb:document-update-outcome-status outcome))
       "Retry exhaustion returned ~s"
       (star.databases.couchdb:document-update-outcome-status outcome))
      (update-check (= 3 loads) "Expected 3 refetches, got ~d" loads)
      (update-check (= 3 saves) "Expected 3 saves, got ~d" saves)
      (update-check
       (equal (reverse seen-revisions)
              '("1-current" "2-current" "3-current"))
       "Retries reused a stale revision: ~s"
       (reverse seen-revisions)))))

(defun test_revision_only_patch_is_duplicate ()
  (let ((saved nil)
        (existing (update-test-document "target:duplicate")))
    (let ((outcome
            (star.databases.couchdb:upsert-document-update
             (lambda (id) (declare (ignore id)) existing)
             (lambda (candidate)
               (declare (ignore candidate))
               (setf saved t))
             "target:duplicate"
             (update-test-patch :revision "1-stale"))))
      (update-check
       (eq :duplicate
           (star.databases.couchdb:document-update-outcome-status outcome))
       "Revision-only patch was not duplicate")
      (update-check (not saved) "Duplicate patch reached save"))))

(defun test-server_private_extensions_are_preserved ()
  (let* ((existing (update-test-document "target:private"))
         (extensions (jsown:empty-object))
         (server-state (jsown:empty-object))
         (patch (jsown:empty-object))
         (patch-extensions (jsown:empty-object))
         (forged-state (jsown:empty-object)))
    (setf (jsown:val server-state "status") "pending"
          (jsown:val extensions "_server_outbox") server-state
          (jsown:val existing "extensions") extensions
          (jsown:val forged-state "status") "forged"
          (jsown:val patch-extensions "_server_outbox") forged-state
          (jsown:val patch-extensions "client_note") "allowed"
          (jsown:val patch "extensions") patch-extensions)
    (let ((merged
            (star.databases.couchdb:merge-document-update
             "target:private" existing patch)))
      (update-check
       (string= "pending"
                (jsown:val
                 (jsown:val
                  (jsown:val merged "extensions") "_server_outbox")
                 "status"))
       "Patch overwrote server-private extension")
      (update-check
       (string= "allowed"
                (jsown:val
                 (jsown:val merged "extensions") "client_note"))
       "Client extension was not merged"))))

(defun run-document-update-conformance-tests ()
  (format t "~&Running revision-safe document update tests~%")
  (test-stale-client-revision-refetches-and-updates)
  (test-missing-document-strips_stale_revision_before_create)
  (test-patch-cannot_change_identity_or_schema)
  (test-conflict_retries_are_bounded_and_refetch_latest_revision)
  (test_revision_only_patch_is_duplicate)
  (test-server_private_extensions_are_preserved)
  (format t "~&Revision-safe document update tests passed~%")
  t)

(run-document-update-conformance-tests)
