(in-package :star-server-tests)

(def-suite couchdb-outbox-recovery-tests
  :description "Crash-safe bounded CouchDB durable-outbox recovery")

(in-suite couchdb-outbox-recovery-tests)

(defun make-outbox-ingest-test-document ()
  (star.documents:ensure-document
   (jsown:new-js ("id" "outbox:ingest") ("dataset" "outbox-tests")
                 ("dtype" "person") ("schemaVersion" "0.10.1")
                 ("fname" "Ada"))))

(test uncontended-ingest-reuses-saved-revision-and-deduplicates
  (let ((state nil) (loads 0) (saves 0) (published 0)
        (document (make-outbox-ingest-test-document)))
    (labels ((load-document (id)
               (declare (ignore id))
               (incf loads)
               (star.databases.couchdb::clone-outbox-json state))
             (save-document (candidate)
               (is (equal (and state (jsown:val state "_rev"))
                          (jsown:val-safe candidate "_rev")))
               (setf state (star.databases.couchdb::clone-outbox-json candidate)
                     (jsown:val state "_rev") (format nil "~d-saved" (incf saves)))
               (star.databases.couchdb::clone-outbox-json state))
             (publish (routing payload event-id)
               (declare (ignore routing payload event-id))
               (is (= 1 saves) "Pending event must be durable before publication")
               (incf published)))
      (let ((result (star.databases.couchdb:process-outbox-mutation
                     #'load-document #'save-document #'publish document :new)))
        (is (= 1 loads) "The saved revision avoids the marker GET")
        (is (= 2 saves) "Both durable writes remain mandatory")
        (is (= 1 published))
        (is (string= "2-saved" (jsown:val result "_rev")))
        (is-true (star.databases.couchdb:outbox-entry-published-p
                  (first (star.databases.couchdb:document-outbox-entries result)))))
      (star.databases.couchdb:process-outbox-mutation
       #'load-document #'save-document #'publish document :new)
      (is (= 2 loads) "Published redelivery only loads the existing state")
      (is (= 2 saves))
      (is (= 1 published)))))

(test ingest-marker-conflict-reloads-and-preserves-intervening-mutation
  (let ((state nil) (loads 0) (saves 0) (conflicts 0) (published 0))
    (labels ((load-document (id)
               (declare (ignore id))
               (incf loads)
               (star.databases.couchdb::clone-outbox-json state))
             (save-document (candidate)
               (incf saves)
               (unless (equal (and state (jsown:val state "_rev"))
                              (jsown:val-safe candidate "_rev"))
                 (incf conflicts)
                 (error 'star.databases.couchdb::outbox-store-conflict))
               (setf state (star.databases.couchdb::clone-outbox-json candidate)
                     (jsown:val state "_rev") (format nil "~d-saved" saves))
               (star.databases.couchdb::clone-outbox-json state))
             (publish (routing payload event-id)
               (declare (ignore routing payload event-id))
               (incf published)
               (let ((incoming (make-outbox-ingest-test-document)))
                 (setf (jsown:val incoming "fname") "Grace")
                 (setf state (star.databases.couchdb::prepare-outbox-mutation
                              state incoming :updated)
                       (jsown:val state "_rev") "2-concurrent"))))
      (let* ((result (star.databases.couchdb:process-outbox-mutation
                      #'load-document #'save-document #'publish
                      (make-outbox-ingest-test-document) :new))
             (entries (star.databases.couchdb:document-outbox-entries result)))
        (is (= 1 conflicts) "Stale saved state must lose CAS")
        (is (= 2 loads) "A conflict must reload the current document")
        (is (= 3 saves))
        (is (= 1 published) "Marker retries must not republish")
        (is (string= "Grace" (jsown:val result "fname")))
        (is (= 2 (length entries)))
        (is (= 2 (length (cdr (star.databases.couchdb::document-mutation-ledger result)))))
        (is-true (star.databases.couchdb:outbox-entry-published-p (first entries)))
        (is-false (star.databases.couchdb:outbox-entry-published-p (second entries)))))))

(test ingest-marker-conflicts-exhaust-bounded-budget
  (let ((state nil) (loads 0) (saves 0) (published 0))
    (labels ((load-document (id)
               (declare (ignore id))
               (incf loads)
               state)
             (save-document (candidate)
               (incf saves)
               (if (= saves 1)
                   (setf state candidate
                         (jsown:val state "_rev") "1-pending")
                   (error 'star.databases.couchdb::outbox-store-conflict))
               state)
             (publish (routing payload event-id)
               (declare (ignore routing payload event-id))
               (incf published)))
      (signals error
        (star.databases.couchdb:process-outbox-mutation
         #'load-document #'save-document #'publish
         (make-outbox-ingest-test-document) :new))
      (is (= 8 loads) "Initial load plus seven conflict reloads")
      (is (= 9 saves) "Pending write plus eight marker CAS attempts")
      (is (= 1 published))
      (is-false (star.databases.couchdb:outbox-entry-published-p
                 (first (star.databases.couchdb:document-outbox-entries state)))))))

(test ingest-publication-failure-leaves-durable-pending-entry
  (let ((state nil) (saves 0) (event-ids nil))
    (labels ((load-document (id) (declare (ignore id)) state)
             (save-document (candidate)
               (incf saves)
               (setf state candidate
                     (jsown:val state "_rev") (format nil "~d-saved" saves))
               state)
             (publish (routing payload event-id)
               (declare (ignore routing payload))
               (push event-id event-ids)
               (when (= 1 (length event-ids))
                 (error "simulated publication failure"))))
      (signals error
        (star.databases.couchdb:process-outbox-mutation
         #'load-document #'save-document #'publish
         (make-outbox-ingest-test-document) :new))
      (is (= 1 saves))
      (is-false (star.databases.couchdb:outbox-entry-published-p
                 (first (star.databases.couchdb:document-outbox-entries state))))
      (star.databases.couchdb:recover-outbox-documents
       #'load-document #'save-document #'publish (list state))
      (is (= 2 saves))
      (is (= 2 (length event-ids)))
      (is (string= (first event-ids) (second event-ids)))
      (is-true (star.databases.couchdb:outbox-entry-published-p
                (first (star.databases.couchdb:document-outbox-entries state)))))))

(test ingest-save-failures-do-not-report-completion
  (dolist (failing-save '(1 2))
    (let ((state nil) (saves 0) (event-ids nil))
      (labels ((load-document (id) (declare (ignore id)) state)
               (save-document (candidate)
                 (when (= failing-save (incf saves))
                   (error "simulated persistence failure"))
                 (setf state candidate
                       (jsown:val state "_rev") (format nil "~d-saved" saves))
                 state)
               (publish (routing payload event-id)
                 (declare (ignore routing payload))
                 (push event-id event-ids)))
        (signals error
          (star.databases.couchdb:process-outbox-mutation
           #'load-document #'save-document #'publish
           (make-outbox-ingest-test-document) :new))
        (if (= failing-save 1)
            (progn
              (is-false state)
              (is-false event-ids "Failed persistence must prevent publication"))
            (progn
              (is (= 1 (length event-ids)))
              (is-false (star.databases.couchdb:outbox-entry-published-p
                         (first (star.databases.couchdb:document-outbox-entries state))))
              (star.databases.couchdb:recover-outbox-documents
               #'load-document #'save-document #'publish (list state))
              (is (= 2 (length event-ids)))
              (is (string= (first event-ids) (second event-ids)))
              (is-true (star.databases.couchdb:outbox-entry-published-p
                        (first (star.databases.couchdb:document-outbox-entries state))))))))))

(defun make-outbox-recovery-test-document (document-id)
  (let ((document (jsown:empty-object))
        (extensions (jsown:empty-object))
        (entry (jsown:empty-object))
        (payload (jsown:empty-object)))
    (setf (jsown:val payload "id") document-id
          (jsown:val entry "event_id") (format nil "event-~a" document-id)
          (jsown:val entry "mutation_id") (format nil "mutation-~a" document-id)
          (jsown:val entry "sequence") 1
          (jsown:val entry "status") "pending"
          (jsown:val entry "routing_key") "documents.updated.person"
          (jsown:val entry "payload") payload
          (jsown:val extensions "_server_outbox") (vector entry)
          (jsown:val document "_id") document-id
          (jsown:val document "extensions") extensions)
    document))

(test recovery-drains-more-than-one-bounded-view-page
  (let ((remaining 20001)
        (passes 0))
    (is-true
     (star.databases.couchdb::recover-outbox-until-empty
      (lambda ()
        (when (plusp remaining)
          (make-list (min remaining 10000) :initial-element :pending)))
      (lambda (documents)
        (incf passes)
        (decf remaining (length documents)))
      :max-passes 4))
    (is (zerop remaining))
    (is (= 3 passes))))

(test recovery-observes-work-arriving-between-batches
  (let ((remaining 10000)
        (injected nil)
        (passes 0))
    (is-true
     (star.databases.couchdb::recover-outbox-until-empty
      (lambda ()
        (when (plusp remaining)
          (make-list (min remaining 10000) :initial-element :pending)))
      (lambda (documents)
        (incf passes)
        (decf remaining (length documents))
        (unless injected
          (setf injected t)
          (incf remaining)))
      :max-passes 3))
    (is (zerop remaining))
    (is (= 2 passes))))

(test recovery-budget-exhaustion-fails-closed
  (let ((passes 0))
    (signals error
      (star.databases.couchdb::recover-outbox-until-empty
       (lambda () '(:still-pending))
       (lambda (documents)
         (declare (ignore documents))
         (incf passes))
       :max-passes 2))
    (is (= 2 passes))))

(test crash-after-publish-before-marker-replays-same-event
  (let* ((state (make-outbox-recovery-test-document "doc-1"))
         (published '())
         (fail-marker t))
    (labels ((load-document (document-id)
               (declare (ignore document-id))
               state)
             (save-document (updated)
               (if fail-marker
                   (progn
                     (setf fail-marker nil)
                     (error "simulated crash before publication marker"))
                   (setf state updated)))
             (publish (routing-key payload event-id)
               (declare (ignore routing-key payload))
               (push event-id published)))
      (signals error
        (star.databases.couchdb:recover-outbox-documents
         #'load-document #'save-document #'publish (list state)))
      (is (= 1 (length published)))
      (star.databases.couchdb:recover-outbox-documents
       #'load-document #'save-document #'publish (list state))
      (is (= 2 (length published)))
      (is (string= (first published) (second published)))
      (is-true
       (star.databases.couchdb:outbox-entry-published-p
        (first (star.databases.couchdb:document-outbox-entries state)))))))

(test publication-marker-conflict-retries-without-republishing
  (let* ((state (make-outbox-recovery-test-document "doc-2"))
         (published 0)
         (save-attempts 0))
    (labels ((load-document (document-id)
               (declare (ignore document-id))
               state)
             (save-document (updated)
               (incf save-attempts)
               (if (= save-attempts 1)
                   (error 'star.databases.couchdb::outbox-store-conflict)
                   (setf state updated)))
             (publish (routing-key payload event-id)
               (declare (ignore routing-key payload event-id))
               (incf published)))
      (star.databases.couchdb:recover-outbox-documents
       #'load-document #'save-document #'publish (list state))
      (is (= 1 published))
      (is (= 2 save-attempts))
      (is-true
       (star.databases.couchdb:outbox-entry-published-p
        (first (star.databases.couchdb:document-outbox-entries state)))))))
(test recovery-can-drain-exactly-at-pass-budget
  ;; There must be one last empty read after the final permitted mutation pass.
  (let ((remaining 2)
        (queries 0)
        (passes 0))
    (is-true
     (star.databases.couchdb::recover-outbox-until-empty
      (lambda ()
        (incf queries)
        (when (plusp remaining) '(:pending)))
      (lambda (documents)
        (incf passes)
        (decf remaining (length documents)))
      :max-passes 2))
    (is (zerop remaining))
    (is (= 2 passes))
    (is (= 3 queries))))

;;;; Conflict-driven ingest fast path (perf/ingest-hot-path)

(defun make-outbox-ingest-document (document-id)
  (let ((document (jsown:empty-object))
        (extensions (jsown:empty-object)))
    (setf (jsown:val extensions "mutation_id")
          (format nil "mutation-~a" document-id)
          (jsown:val document "_id")
          document-id
          (jsown:val document "dtype")
          "person"
          (jsown:val document "extensions")
          extensions)
    document))

(defun make-duplicate-outbox-document (incoming &key (operation :new))
  "Build the stored revision INCOMING idempotently collides with."
  (let* ((public (star.databases.couchdb::public-document-copy incoming))
         (mutation-id
           (star.databases.couchdb::document-mutation-id operation public))
         (content-hash
           (star.databases.couchdb::mutation-content-hash operation public))
         (entry (jsown:empty-object))
         (ledger (jsown:empty-object))
         (extensions (jsown:empty-object)))
    (setf (jsown:val entry "mutation_id") mutation-id
          (jsown:val entry "sequence") 1
          (jsown:val entry "status") "pending"
          (jsown:val ledger mutation-id) content-hash
          (jsown:val extensions "_server_outbox") (vector entry)
          (jsown:val extensions "_server_mutations") ledger
          (jsown:val public "extensions") extensions
          (jsown:val public "_rev") "1-abc")
    public))

(test blind-new-ingest-creates-without-point-lookups
  ;; Happy path: no existence GET before the write and no reload before the
  ;; publication marker; exactly one save-create and one save-marker.
  (let ((incoming (make-outbox-ingest-document "blind-doc-create"))
        (loads 0)
        (saves 0)
        (published 0)
        (saved-state nil))
    (let ((star:*outbox-blind-new-ingest-p* t))
      (star.databases.couchdb::process-outbox-mutation
       (lambda (document-id)
         (declare (ignore document-id))
         (incf loads)
         nil)
       (lambda (state)
         (incf saves)
         (setf saved-state state))
       (lambda (routing-key payload event-id)
         (declare (ignore routing-key payload event-id))
         (incf published))
       incoming
       :new))
    (is (= 0 loads))
    (is (= 2 saves))
    (is (= 1 published))
    (is-true
     (star.databases.couchdb::outbox-entry-published-p
      (first (star.databases.couchdb::document-outbox-entries saved-state))))))

(test blind-new-conflict-falls-back-to-duplicate-detection
  ;; The blind save 409s against an existing document; the retry must reload
  ;; and resolve through the mutation ledger exactly like the old path.
  (let* ((incoming (make-outbox-ingest-document "blind-doc-duplicate"))
         (existing (make-duplicate-outbox-document incoming))
         (loads 0)
         (saves 0))
    (let ((star:*outbox-blind-new-ingest-p* t))
      (multiple-value-bind (state entry disposition)
          (star.databases.couchdb::persist-outbox-mutation
           (lambda (document-id)
             (declare (ignore document-id))
             (incf loads)
             existing)
           (lambda (state)
             (declare (ignore state))
             (incf saves)
             (error 'star.databases.couchdb::outbox-store-conflict))
           incoming
           :new)
        (is (eq :duplicate disposition))
        (is (eq existing state))
        (is (string= "mutation-blind-doc-duplicate"
                     (star.databases.couchdb::outbox-entry-mutation-id entry)))
        (is (= 1 loads))
        (is (= 1 saves))))))

(test blind-new-opt-out-restores-existence-check
  ;; STAR_OUTBOX_BLIND_NEW_INGEST=false must keep the load-first behavior.
  (let ((incoming (make-outbox-ingest-document "blind-doc-optout"))
        (loads 0))
    (star.databases.couchdb::persist-outbox-mutation
     (lambda (document-id)
       (declare (ignore document-id))
       (incf loads)
       nil)
     (lambda (state) state)
     incoming
     :new
     :blind-new-p nil)
    (is (= 1 loads))))

(test updated-mutations-never-skip-the-existence-load
  ;; :updated requires the stored revision, fast path must not apply.
  (let ((incoming (make-outbox-ingest-document "blind-doc-update"))
        (loads 0))
    (let ((star:*outbox-blind-new-ingest-p* t))
      (star.databases.couchdb::persist-outbox-mutation
       (lambda (document-id)
         (declare (ignore document-id))
         (incf loads)
         (make-duplicate-outbox-document
          (make-outbox-ingest-document "blind-doc-update")
          :operation :updated))
       (lambda (state) state)
       incoming
       :updated))
    (is (= 1 loads))))
