(in-package :star-server-tests)

(in-suite couchdb-view-integration-tests)

(test real-couchdb-view-recovery-preserves-canonical-literals
  (let* ((client *view-integration-client*)
         (database "starintel-view-literal-recovery-test")
         (id "target:view-canonical-literals")
         (options (jsown:new-js ("false" :false) ("true" :true) ("null" :null)
                                ("array" #()) ("object" (jsown:empty-object))
                                ("nested" (vector :false :null #() (jsown:empty-object)))))
         (wire (jsown:new-js ("id" id) ("dataset" "view-tests") ("dtype" "target")
                             ("schemaVersion" "0.10.1") ("actor" "subfinder")
                             ("target" "example.org") ("delay" 1) ("recurring" :false)
                             ("options" options)))
         (stored (star.documents:ensure-document
                  (star.documents:clone-document-object wire)))
         (published nil)
         (quarantines 0))
    (when (cl-couch:database-exists-p client database)
      (cl-couch:delete-database client database))
    (cl-couch:create-database client database)
    (unwind-protect
      (progn
    ;; Install the production views in the disposable integration database.
    (dolist (view '("targets" "outbox"))
      (cl-couch:create-document
       client database
       (jsown:to-json
        (or (gethash view (star.databases.couchdb::checked-in-design-document-map))
            (error "Missing production design document ~a" view)))))
    ;; Persist through the actual durable-outbox path; interrupt before publish.
    (signals error
      (star.databases.couchdb:couchdb-process-outbox-mutation
       client database
       (lambda (&rest args) (declare (ignore args)) (error "simulate pre-publish crash"))
       stored :new))
    (multiple-value-bind (records invalid)
        (star.actors:load-persisted-target-records
         client database :actors '("subfinder")
         :quarantine-fn (lambda (&rest args) (declare (ignore args)) (incf quarantines)))
      (is (= 0 invalid quarantines))
      (is (= 1 (length records)))
      (when records
        (is (string= id (star.actors:target-record-id (first records))))
        (is-false (star.actors:target-record-recurring-p (first records)))
        (is (string= (jsown:to-json options)
                     (jsown:to-json (star.actors:target-record-options (first records)))))))
    (star.databases.couchdb:recover-couchdb-outbox
     client database
     (lambda (routing-key payload event-id)
       (is (string= "documents.new.target" routing-key))
       (is (stringp event-id))
       (is (eq :false (jsown:val payload "recurring")))
       (push (jsown:to-json (jsown:val payload "options")) published)))
    (is (equal (list (jsown:to-json options)) published))
    (is (null (star.databases.couchdb::couchdb-pending-outbox-documents client database)))
      )
      (cl-couch:delete-database client database))))

;; issue #360: durable acceptance must publish a standalone dtype "target"
;; document (so targets views index it) and boot recovery must re-arm the
;; recurring wheel from it after a simulated restart.
(test real-couchdb-durable-acceptance-publishes-and-rehydrates-recurring-target
  (let* ((client *view-integration-client*)
         (database "starintel-target-rehydration-test")
         (request
           (jsown:new-js
             ("actor" "subfinder")
             ("target" "rehydrate.example.org")
             ("dataset" "view-tests")
             ("delay" 60)
             ("recurring" :true)
             ("options" (jsown:empty-object))
             ("idempotency_key" "rehydrate-key-360")))
         (document
           (star.frontends.http-api::target-v1-document-from-request
            request "human:alice"))
         (record (star.actors:parse-target-record document))
         (target-id (star.actors:target-record-id record))
         (recurring-calls 0)
         (star.actors::*active-target-schedules* (make-hash-table :test #'equal))
         (star.actors::*recovered-target-fingerprints* (make-hash-table :test #'equal)))
    (when (cl-couch:database-exists-p client database)
      (cl-couch:delete-database client database))
    (cl-couch:create-database client database)
    (unwind-protect
         (progn
           (dolist (view '("targets"))
             (cl-couch:create-document
              client database
              (jsown:to-json
               (or (gethash view (star.databases.couchdb::checked-in-design-document-map))
                   (error "Missing production design document ~a" view)))))
           (flet ((accept ()
                    (star.actors:accept-target-record
                     record
                     :destination
                     (star.actors::make-target-destination-handle
                      :rabbit "subfinder"
                      :routing-key "documents.target.dispatch.subfinder")
                     :persist-fn
                     (lambda (desired equivalent-p)
                       (star.databases.couchdb:couchdb-persist-target-acceptance
                        client database desired equivalent-p))
                     :update-fn
                     (lambda (acceptance-id updater)
                       (star.databases.couchdb:couchdb-update-target-acceptance
                        client database acceptance-id updater))
                     :dispatch-fn (lambda (envelope)
                                    (declare (ignore envelope)) t)
                     :schedule-once-fn
                     (lambda (&rest args)
                       (declare (ignore args)) t)
                     :schedule-recurring-fn
                     (lambda (&rest args)
                       (declare (ignore args))
                       (incf recurring-calls)))))
             ;; Initial durable acceptance arms the wheel once.
             (is (eq :accepted
                     (star.actors:target-dispatch-outcome-status (accept))))
             (is (= 1 recurring-calls))
             ;; The canonical target document is queryable standalone.
             (let ((standalone
                     (jsown:with-injective-reader
                       (jsown:parse
                        (cl-couch:get-document client database target-id)))))
               (is (string= "target" (jsown:val standalone "dtype")))
               (is (string= "subfinder" (jsown:val standalone "actor")))
               (is (eq :true (jsown:val standalone "recurring"))))
             (multiple-value-bind (records invalid)
                 (star.actors:load-persisted-target-records
                  client database :actors '("subfinder")
                  :quarantine-fn
                  (lambda (&rest args) (declare (ignore args)) t))
               (is (zerop invalid))
               (is (= 1 (length records)))
               (is (star.actors:target-record-recurring-p (first records))))
             ;; Simulated restart: schedule memory gone, durable state kept.
             (setf star.actors::*active-target-schedules*
                     (make-hash-table :test #'equal)
                   star.actors::*recovered-target-fingerprints*
                     (make-hash-table :test #'equal))
             (is (eq :duplicate
                     (star.actors:target-dispatch-outcome-status (accept))))
             (is (= 2 recurring-calls)
                 "recurring dispatch must resume after restart")
             (is-true (gethash (star.actors::target-record-schedule-id record)
                               star.actors::*active-target-schedules*))
             ;; The standalone document was ensured, not duplicated.
             (let ((standalone
                     (jsown:with-injective-reader
                       (jsown:parse
                        (cl-couch:get-document client database target-id)))))
               (is (string= "1-" (subseq (jsown:val standalone "_rev") 0 2))
                   "standalone target document must stay at revision 1"))))
      (cl-couch:delete-database client database))))

