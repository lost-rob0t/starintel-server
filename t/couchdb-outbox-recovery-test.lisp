(in-package :star-server-tests)

(def-suite couchdb-outbox-recovery-tests
  :description "Crash-safe bounded CouchDB durable-outbox recovery")

(in-suite couchdb-outbox-recovery-tests)

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
