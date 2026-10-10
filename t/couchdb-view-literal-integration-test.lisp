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

