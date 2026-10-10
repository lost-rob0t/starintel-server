(in-package :star-server-tests)
(in-suite couchdb-view-integration-tests)

(test real-couchdb-exact-number-boundary-and-outbox-readback
  (let ((client *view-integration-client*)
        (database "starintel-exact-number-test")
        (token "0.12345678901234567890123456789"))
    (when (cl-couch:database-exists-p client database)
      (cl-couch:delete-database client database))
    (cl-couch:create-database client database)
    (unwind-protect
         (progn
           ;; Discriminate database normalization from server codec loss.
           ;; Send raw bytes, and inspect raw GET with the pinned exact codec.
           (cl-couch:create-document
            client database
            (format nil "{\"_id\":\"raw-number-probe\",\"exact\":~a}" token))
           (let* ((raw (cl-couch:get-document client database "raw-number-probe"))
                  (parsed (starintel:parse-json raw))
                  (actual (starintel:stringify-json (gethash "exact" parsed))))
             (format t "~&RAW_COUCHDB_EXACT_NUMBER expected=~a actual=~a preserved=~s~%"
                     token actual (string= token actual))
             (is (nth-value 1 (gethash "exact" parsed))))
           (cl-couch:create-document
            client database
            (jsown:to-json (gethash "outbox"
                                   (star.databases.couchdb::checked-in-design-document-map))))
           (let* ((id "canonical:exact-persisted")
                  (incoming (star.rabbit:decode-rabbit-document
                             (cons (exact-number-wire token id) 1)))
                  (published nil))
             (signals error
               (star.databases.couchdb:couchdb-process-outbox-mutation
                client database
                (lambda (&rest args) (declare (ignore args)) (error "pre-publish crash"))
                incoming :new))
             (flet ((assert-readback ()
                      (let ((wire (star.documents:parse-document-object
                                   (star.frontends.http-api:strip-server-tenant-fields
                                    (cl-couch:get-document client database id)))))
                        (assert-exact-number-document wire token))))
               (assert-readback)
               (dolist (document (star.databases.couchdb::couchdb-pending-outbox-documents
                                  client database))
                 (assert-exact-number-document
                  (star.frontends.http-api:strip-server-tenant-fields document) token))
               (star.databases.couchdb:recover-couchdb-outbox
                client database
                (lambda (key payload event-id)
                  (declare (ignore key event-id))
                  (assert-exact-number-document payload token)
                  (push payload published)))
               (is (= 1 (length published)))
               (assert-readback))))
      (cl-couch:delete-database client database))))
