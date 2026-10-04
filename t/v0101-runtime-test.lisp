(in-package :star-server-tests)

(def-suite v0101-runtime-tests :description "StarLang-generated 0.10.1 ingress and storage boundary")
(in-suite v0101-runtime-tests)

(defun v0101-person (&optional (id "canonical:person"))
  (jsown:new-js ("id" id) ("dataset" "canonical-tests") ("dtype" "person")
                ("schemaVersion" "0.10.1") ("fname" "Ada")))

(test canonical-flat-document-passes-http-ingress
  (let ((document (v0101-person)))
    (is (eq document (star.frontends.http-api:validate-document-input document :path-dtype "person")))
    (is-false (jsown:keyp document "_id"))
    (is (string= "canonical:person" (star.documents:document-id document)))))

(test canonical-http-rejects-unknown-fields-and-referenced-enums
  (let ((document (v0101-person)))
    (setf (jsown:val document "_id") "canonical:person")
    (let ((condition (capture-http-input-error
                      (lambda () (star.frontends.http-api:validate-document-input document)))))
      (is (= 422 (star.frontends.http-api:http-input-error-status condition)))))
  (let ((document (jsown:new-js ("id" "wireless:test") ("dataset" "canonical-tests")
                               ("dtype" "wireless-network") ("schemaVersion" "0.10.1")
                               ("bssid" "aa:bb:cc:dd:ee:ff") ("security" "wpa4"))))
    (let ((condition (capture-http-input-error
                      (lambda () (star.frontends.http-api:validate-document-input document)))))
      (is (= 422 (star.frontends.http-api:http-input-error-status condition))))))

(test canonical-rabbit-ingress-validates-before-storage-identity
  (let* ((wire (v0101-person))
         (stored (star.rabbit:decode-rabbit-document (cons (jsown:to-json wire) 1))))
    (is (string= "canonical:person" (jsown:val stored "_id")))
    (is (string= "canonical:person" (jsown:val stored "id")))
    (is (string= "0.10.1" (jsown:val stored "schemaVersion")))
    (is-false (jsown:keyp stored "schema_version"))))

(test canonical-invalid-rabbit-document-cannot-reach-persistence
  (let ((persisted nil) (document (v0101-person)))
    (setf (jsown:val document "createdAt") -1)
    (signals star.consumers:schema-invalid-delivery-error
      (star.rabbit::process-rabbit-document-mutation
       (cons (jsown:to-json document) 1) :new
       :persist-fn (lambda (&rest args) (declare (ignore args)) (setf persisted t))))
    (is-false persisted)))

(test canonical-internal-transport-roundtrip-preserves-server-tenancy
  (let ((document (star.documents:ensure-document (v0101-person))))
    (setf (jsown:val document "tenant_id") "canonical-tenant")
    (let* ((body (star.documents:document-json document))
           (wire (jsown:parse body))
           (decoded (star.rabbit:decode-rabbit-document (cons body 1))))
      (is-false (jsown:keyp wire "_id"))
      (is (string= "canonical:person" (jsown:val decoded "_id")))
      (is (string= "canonical-tenant" (jsown:val decoded "tenant_id"))))))

(test canonical-storage-egress-keeps-only-canonical-envelope
  (let ((stored (star.documents:ensure-document (v0101-person))))
    (setf (jsown:val stored "_rev") "2-server"
          (jsown:val stored "tenant_id") "private-tenant")
    (let ((wire (star.frontends.http-api::strip-server-tenant-fields stored)))
      (is (string= "2-server" (jsown:val wire "rev")))
      (is-false (jsown:keyp wire "_id"))
      (is-false (jsown:keyp wire "_rev"))
      (is-false (jsown:keyp wire "tenant_id"))
      (is (eq wire (star.documents:validate-document wire))))))

(test canonical-update-preserves-identity-and-validates-merged-state
  (let* ((existing (star.documents:ensure-document (v0101-person)))
         (saved nil)
         (outcome (star.databases.couchdb::upsert-document-update
                   (lambda (id) (declare (ignore id)) existing)
                   (lambda (document) (setf saved document))
                   "canonical:person" (jsown:new-js ("lname" "Lovelace")))))
    (is (eq :updated (star.databases.couchdb:document-update-outcome-status outcome)))
    (is (string= "Lovelace" (jsown:val saved "lname")))
    (is (string= "canonical:person" (jsown:val saved "id")))
    (is (eq saved (star.documents:validate-stored-document saved)))
    (let ((wire (jsown:val (star.databases.couchdb:document-update-outcome-json outcome) "document")))
      (is-false (jsown:keyp wire "_id"))
      (is (eq wire (star.documents:validate-document wire))))))

(test canonical-update-cannot-change-version-or-id
  (dolist (patch (list (jsown:new-js ("schemaVersion" "0.10.2"))
                      (jsown:new-js ("id" "different:person"))))
    (let ((outcome (star.databases.couchdb::upsert-document-update
                    (lambda (id) (declare (ignore id))
                      (star.documents:ensure-document (v0101-person)))
                    (lambda (document) (declare (ignore document)) (error "must not persist"))
                    "canonical:person" patch)))
      (is (eq :validation-failed (star.databases.couchdb:document-update-outcome-status outcome))))))

(test canonical-search-rows-project-storage-fields-and-outbox-payloads
  (dolist (vector-p '(nil t))
    (let* ((stored (star.documents:ensure-document (v0101-person)))
           (payload (star.documents:clone-document-object stored)))
      (setf (jsown:val stored "_rev") "2-search"
            (jsown:val stored "tenant_id") "private-tenant"
            (jsown:val payload "tenant_id") "private-tenant"
            (jsown:val stored "extensions")
            (jsown:new-js ("_server_outbox"
                           (vector (jsown:new-js ("payload" payload))))))
      (let* ((row (jsown:new-js ("doc" stored)
                               ("fields" (star.documents:clone-document-object stored))))
             (response (jsown:new-js ("rows" (if vector-p (vector row) (list row))))))
        (star.frontends.http-api:strip-server-tenant-from-rows response)
        (let ((result (elt (jsown:val response "rows") 0)))
          (dolist (key '("doc" "fields"))
            (let ((wire (jsown:val result key)))
              (is-false (jsown:keyp wire "_id"))
              (is-false (jsown:keyp wire "_rev"))
              (is-false (jsown:keyp wire "tenant_id"))
              (is (string= "2-search" (jsown:val wire "rev")))
              (is (eq wire (star.documents:validate-document wire)))))
          (let* ((extensions (jsown:val (jsown:val result "doc") "extensions"))
                 (outbox (elt (jsown:val extensions "_server_outbox") 0))
                 (wire (jsown:val outbox "payload")))
            (is-false (jsown:keyp wire "tenant_id"))
            (is-false (jsown:keyp wire "_id"))
            (is (eq wire (star.documents:validate-document wire)))))))))
