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
             (is (not (null (nth-value 1 (gethash "exact" parsed))))))
           (cl-couch:create-document
            client database
            (jsown:to-json (gethash "outbox"
                                   (star.databases.couchdb::checked-in-design-document-map))))
           (dolist (token '("0.12345678901234567890123456789" "-0" "1.00e+0"
                            "900719925474099312345678901234567890"))
           (let* ((id (format nil "canonical:exact-persisted:~a" token))
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
               (assert-readback)
               ;; A second recovery cannot republish a confirmed entry.
               (star.databases.couchdb:recover-couchdb-outbox
                client database
                (lambda (&rest args) (declare (ignore args))
                  (error "published entry replayed")))
               (let* ((current (star.databases.couchdb::couchdb-load-outbox-document client database id))
                      (updated (star.documents:clone-json-value current)))
                 (setf (jsown:val (jsown:val updated "extensions") "exact")
                       (star.documents:parse-json-value "2.500e0"))
                 (star.databases.couchdb::couchdb-save-outbox-document client database updated)
                 (let ((readback (star.databases.couchdb::couchdb-load-outbox-document client database id)))
                   (assert-exact-number-document readback "2.500e0")
                   (jsown:remkey (jsown:val readback "extensions") "exact")
                   (star.databases.couchdb::couchdb-save-outbox-document client database readback)
                   (is (not (jsown:keyp
                             (jsown:val (star.databases.couchdb::couchdb-load-outbox-document client database id)
                                        "extensions") "exact")))))))))
      (cl-couch:delete-database client database))))
