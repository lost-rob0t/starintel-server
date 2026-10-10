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

(test canonical-message-cannot-enter-historical-url-extractor
  (let* ((calls 0)
         (symbols '(starintel.legacy:new-url starintel.legacy:new-relation
                    star.databases.couchdb:as-json star.actors:publish))
         (originals (mapcar #'symbol-function symbols)))
    (unwind-protect
         (progn
           (dolist (symbol symbols)
             (setf (symbol-function symbol)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (incf calls)
                     (error "Historical side effect must not run."))))
           (dolist (document
                    (list
                     (jsown:new-js ("id" "canonical:message") ("dataset" "canonical-tests")
                                   ("dtype" "message") ("schemaVersion" "0.10.1")
                                   ("message" "https://example.org/") ("platform" "test"))
                     (jsown:new-js ("id" "canonical:message") ("_id" "canonical:message")
                                   ("schemaVersion" "0.10.1") ("content" "https://example.org/"))
                     (jsown:new-js ("schemaVersion" nil) ("content" "https://example.org/"))
                     (jsown:new-js ("id" nil) ("content" "https://example.org/"))
                     (jsown:new-js ("id" "") ("content" "https://example.org/"))))
             (let ((condition
                     (handler-case
                         (progn (star.actors.matcher::process-legacy-url-extractor-message document) nil)
                       (error (value) value))))
               (is (typep condition 'simple-error))
               (is (search "Canonical URL extraction is unsupported" (princ-to-string condition)))
               (is (= 0 calls))
               (is-false (jsown:keyp document "schema_version")))))
      (loop for symbol in symbols for original in originals
            do (setf (symbol-function symbol) original)))))


(defun issue319-fingerprint-options (&optional reordered)
  (jsown:with-injective-reader
    (jsown:parse
     (if reordered
         "{\"z\":{\"CamelKey\":false,\"a\":[null,{},[],\"first\",\"second\"]},\"A\":1}"
         "{\"A\":1,\"z\":{\"a\":[null,{},[],\"first\",\"second\"],\"CamelKey\":false}}"))))

(defun issue319-fingerprint-document (&optional reordered)
  (jsown:new-js
    ("id" "canonical:target-fingerprint") ("_id" "canonical:target-fingerprint")
    ("rev" "1-original") ("_rev" "1-original") ("dtype" "target")
    ("schemaVersion" "0.10.1") ("dataset" "dataset-a") ("tenant_id" "tenant-a")
    ("actor" "subfinder") ("target" "example.org") ("delay" 60) ("recurring" :false)
    ("options" (issue319-fingerprint-options reordered))
    ("extensions" (jsown:new-js ("schedule_id" "issue319:schedule")))))

(defun issue319-fingerprint-envelope (document &key (kind :rabbit) routing-key)
  (let* ((record (star.actors::parse-target-record document))
         (actor (star.actors::target-record-actor record)))
    (star.actors::make-target-dispatch-envelope
     record :destination
     (star.actors::make-target-destination-handle
      kind actor :component (when (eq kind :local) :test-component)
      :routing-key (when (eq kind :rabbit)
                     (or routing-key (star.actors::canonical-target-routing-key actor)))))))

(defun issue319-precanonical-fingerprint (envelope)
  "Independent pre-#319 fingerprint for valid JSON fixtures."
  (let ((record (star.actors::target-dispatch-envelope-record envelope))
        (destination (star.actors::target-dispatch-envelope-destination envelope)))
    (star.actors::target-dispatch-digest
     (format nil "~a|~a|~a|~a|~a|~a|~a|~a|~a|~a|~a"
             (star.actors::target-dispatch-envelope-schedule-id envelope)
             (star.actors::target-record-id record)
             (or (star.actors::target-record-revision record) "unrevisioned")
             (star.actors::target-destination-handle-kind destination)
             (star.actors::target-destination-handle-name destination)
             (star.actors::target-record-actor record)
             (star.actors::target-record-target record)
             (star.actors::target-record-delay record)
             (if (star.actors::target-record-recurring-p record) "true" "false")
             (jsown:to-json (star.actors::target-record-options record))
             (or (star.actors::target-record-deadline record) "no-deadline")))))

(defun issue319-old-acceptance (envelope &optional (status "scheduled"))
  (let ((acceptance (star.actors::target-acceptance-document envelope)))
    (setf (jsown:val acceptance "fingerprint") (issue319-precanonical-fingerprint envelope)
          (jsown:val acceptance "status") status
          (jsown:val acceptance "execution_id") "saved-execution"
          (jsown:val acceptance "trace_id") "saved-trace"
          (jsown:val acceptance "lease_id") "saved-lease"
          (jsown:val acceptance "fencing_token") 7)
    acceptance))

(defun issue319-retry-acceptance (existing envelope)
  (let ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
        (updates 0) (schedules 0) (dispatches 0))
    (let ((outcome
            (star.actors::process-target-dispatch-envelope
             envelope
             (lambda (desired equivalent-p)
               (values existing
                       (cond ((not (funcall equivalent-p existing desired)) :conflict)
                             ((equal "pending" (jsown:val existing "status")) :resumed)
                             (t :duplicate))))
             (lambda (id updater)
               (is (equal id (jsown:val existing "_id")))
               (incf updates) (funcall updater existing))
             :dispatch-fn (lambda (&rest args) (declare (ignore args)) (incf dispatches))
             :schedule-once-fn (lambda (&rest args) (declare (ignore args)) (incf schedules))
             :schedule-recurring-fn (lambda (&rest args) (declare (ignore args)) (incf schedules)))))
      (values outcome updates schedules dispatches))))

(defun issue319-assert-acceptance-conflict (existing envelope)
  (let ((before (jsown:to-json existing)))
    (multiple-value-bind (outcome updates schedules dispatches)
        (issue319-retry-acceptance existing envelope)
      (is (eq :invalid (star.actors::target-dispatch-outcome-status outcome)))
      (is (= 0 updates schedules dispatches))
      (is (string= before (jsown:to-json existing))))))

(test canonical-target-json-is-recursive-injective-and-fail-closed
  (is (string= "{\"A\":1,\"z\":{\"CamelKey\":false,\"a\":[null,{},[],\"first\",\"second\"]}}"
               (star.actors::canonical-target-json (issue319-fingerprint-options))))
  (is (string= (star.actors::canonical-target-json (issue319-fingerprint-options))
               (star.actors::canonical-target-json (issue319-fingerprint-options t))))
  (let ((encodings (mapcar #'star.actors::canonical-target-json
                          (list :false :null #() (jsown:empty-object)))))
    (is (= 4 (length (remove-duplicates encodings :test #'string=)))))
  (dolist (value (list :unsupported 1/2 (make-hash-table)
                      (list :obj (cons "same" 1) (cons "same" 2))))
    (signals star.actors:invalid-target-dispatch (star.actors::canonical-target-json value)))
  (let ((cycle (make-array 1)))
    (setf (aref cycle 0) cycle)
    (signals star.actors:invalid-target-dispatch (star.actors::canonical-target-json cycle))))

(test canonical-reordered-retries-reuse-old-acceptance-without-rewriting-metadata
  (dolist (status '("scheduled" "pending"))
    (let* ((first (issue319-fingerprint-envelope (issue319-fingerprint-document)))
           (retry (issue319-fingerprint-envelope (issue319-fingerprint-document t)))
           (existing (issue319-old-acceptance first status))
           (old-fingerprint (jsown:val existing "fingerprint")))
      (is (string= (star.actors::target-dispatch-fingerprint first)
                   (star.actors::target-dispatch-fingerprint retry)))
      (is (not (string= old-fingerprint (star.actors::target-dispatch-fingerprint retry))))
      (multiple-value-bind (outcome updates schedules dispatches)
          (issue319-retry-acceptance existing retry)
        (is (eq (if (string= status "pending") :accepted :duplicate)
                (star.actors::target-dispatch-outcome-status outcome)))
        (is (= (if (string= status "pending") 1 0) updates schedules))
        (is (= 0 dispatches))
        (is (string= "saved-execution" (star.actors::target-dispatch-envelope-execution-id retry)))
        (is (string= "saved-trace" (star.actors::target-dispatch-envelope-trace-id retry)))
        (is (string= "saved-lease" (star.actors::target-dispatch-envelope-lease-id retry)))
        (is (= 7 (star.actors::target-dispatch-envelope-fencing-token retry)))
        (is (string= old-fingerprint (jsown:val existing "fingerprint")))
        (is (string= "saved-execution" (jsown:val existing "execution_id")))
        (is (string= "saved-trace" (jsown:val existing "trace_id")))
        (is (string= "saved-lease" (jsown:val existing "lease_id")))
        (is (= 7 (jsown:val existing "fencing_token")))))))

(test canonical-dataset-change-conflicts-even-when-old-digests-collide
  (let* ((first (issue319-fingerprint-envelope (issue319-fingerprint-document)))
         (changed (issue319-fingerprint-document)))
    (setf (jsown:val changed "dataset") "dataset-b")
    (let ((retry (issue319-fingerprint-envelope changed)))
      (is (string= (issue319-precanonical-fingerprint first)
                   (issue319-precanonical-fingerprint retry)))
      (is (not (string= (star.actors::target-dispatch-fingerprint first)
                        (star.actors::target-dispatch-fingerprint retry))))
      (issue319-assert-acceptance-conflict (issue319-old-acceptance first) retry))))

(test canonical-target-semantic-type-and-identity-changes-conflict
  (dolist (mutate
            (list
             (lambda (doc) (setf (jsown:val doc "actor") "other-actor"))
             (lambda (doc) (setf (jsown:val doc "target") "example.net"))
             (lambda (doc) (setf (jsown:val doc "delay") 61))
             (lambda (doc) (setf (jsown:val doc "recurring") :true))
             (lambda (doc) (setf (jsown:val doc "deadline") 4102444800))
             (lambda (doc) (setf (jsown:val doc "tenant_id") "tenant-b"))
             (lambda (doc) (jsown:remkey doc "tenant_id"))
             (lambda (doc) (setf (jsown:val doc "id") "canonical:other"
                                 (jsown:val doc "_id") "canonical:other"))
             (lambda (doc) (setf (jsown:val doc "rev") "2-new"
                                 (jsown:val doc "_rev") "2-new"))
             (lambda (doc) (setf (jsown:val (jsown:val doc "extensions") "schedule_id")
                                 "issue319:other-schedule"))))
    (let* ((original (issue319-fingerprint-envelope (issue319-fingerprint-document)))
           (changed (issue319-fingerprint-document)))
      (funcall mutate changed)
      (issue319-assert-acceptance-conflict
       (issue319-old-acceptance original) (issue319-fingerprint-envelope changed))))
  (let* ((original (issue319-fingerprint-envelope (issue319-fingerprint-document)))
         (existing (issue319-old-acceptance original)))
    (issue319-assert-acceptance-conflict
     existing (issue319-fingerprint-envelope (issue319-fingerprint-document) :kind :local))
    (issue319-assert-acceptance-conflict
     existing (issue319-fingerprint-envelope (issue319-fingerprint-document)
                                             :routing-key "other.routing.key"))))

(test canonical-acceptance-missing-mixed-or-inconsistent-metadata-conflicts
  (let* ((envelope (issue319-fingerprint-envelope (issue319-fingerprint-document)))
         (existing (issue319-old-acceptance envelope)))
    (dolist (key '("_id" "type" "status" "fingerprint" "target_document"
                   "target_id" "target_revision" "actor" "schedule_id"
                   "execution_id" "attempt" "trace_id" "lease_id" "fencing_token"
                   "destination_kind" "routing_key" "recurring" "delay" "deadline"))
      (let ((broken (star.documents:clone-document-object existing)))
        (jsown:remkey broken key)
        (issue319-assert-acceptance-conflict broken envelope)))
    (dolist (change '(("_id" . "target-acceptance:wrong") ("target_id" . "wrong")
                      ("target_revision" . "2-wrong") ("actor" . "wrong")
                      ("schedule_id" . "wrong") ("delay" . 61) ("recurring" . :true)
                      ("deadline" . 4102444800) ("destination_kind" . "unknown")
                      ("routing_key" . :null) ("fencing_token" . 0)
                      ("attempt" . -1) ("execution_id" . :null)
                      ("trace_id" . "") ("lease_id" . :null)))
      (let ((broken (star.documents:clone-document-object existing)))
        (setf (jsown:val broken (car change)) (cdr change))
        (issue319-assert-acceptance-conflict broken envelope)))
    (dolist (mutate
              (list
               (lambda (doc) (jsown:remkey doc "schemaVersion"))
               (lambda (doc) (jsown:remkey doc "id"))
               (lambda (doc) (setf (jsown:val doc "schema_version") "0.9.0"))
               (lambda (doc) (setf (jsown:val doc "_id") "canonical:wrong"))
               (lambda (doc) (setf (jsown:val doc "_rev") "2-wrong"))
               (lambda (doc) (setf (jsown:val doc "options") #()))
               (lambda (doc) (setf (jsown:val (jsown:val doc "extensions") "target_execution_id")
                                   "inconsistent-execution"))))
      (let ((broken (star.documents:clone-document-object existing)))
        (funcall mutate (jsown:val broken "target_document"))
        (issue319-assert-acceptance-conflict broken envelope)))))

(test legacy-target-fingerprint-and-equality-remain-byte-compatible
  (let* ((*print-case* :upcase)
         (document (jsown:new-js
                     ("_id" "legacy:target") ("_rev" "3-test") ("dtype" "target")
                     ("actor" "subfinder") ("target" "example.org")
                     ("delay" 60) ("recurring" :false) ("options" #())
                     ("schedule_id" "legacy:schedule")))
         (envelope (issue319-fingerprint-envelope document)))
    (is (string=
         (star.actors::target-dispatch-digest
          "legacy:schedule|legacy:target|3-test|RABBIT|subfinder|subfinder|example.org|60|false|[]|no-deadline")
         (star.actors::target-dispatch-fingerprint envelope)))
    (let ((left (jsown:new-js ("schedule_id" "legacy:schedule") ("fingerprint" "old-digest")))
          (right (jsown:new-js ("schedule_id" "legacy:schedule") ("fingerprint" "old-digest"))))
      (is (star.actors::target-acceptance-equivalent-p left right))
      (setf (jsown:val right "fingerprint") "changed-digest")
      (is-false (star.actors::target-acceptance-equivalent-p left right)))))

(test canonical-target-options-preserve-each-json-distinction
  (let ((options
          (list (jsown:new-js ("value" :false))
                (jsown:new-js ("value" :null))
                (jsown:new-js ("value" #()))
                (jsown:new-js ("value" (jsown:empty-object)))
                (jsown:new-js ("Case" 1))
                (jsown:new-js ("case" 1))
                (jsown:new-js ("order" (vector "first" "second")))
                (jsown:new-js ("order" (vector "second" "first"))))))
    (loop for tail on options do
      (dolist (other (cdr tail))
        (let ((left-doc (issue319-fingerprint-document))
              (right-doc (issue319-fingerprint-document)))
          (setf (jsown:val left-doc "options") (car tail)
                (jsown:val right-doc "options") other)
          (let ((left (issue319-fingerprint-envelope left-doc))
                (right (issue319-fingerprint-envelope right-doc)))
            (is (not (string= (star.actors::target-dispatch-fingerprint left)
                              (star.actors::target-dispatch-fingerprint right))))
            (issue319-assert-acceptance-conflict
             (issue319-old-acceptance left) right)))))))

(test canonical-target-fingerprint-agrees-for-wire-and-storage-identities
  (let* ((stored (issue319-fingerprint-document))
         (wire (star.documents:clone-document-object stored)))
    (jsown:remkey wire "_id")
    (jsown:remkey wire "_rev")
    (let ((left (issue319-fingerprint-envelope stored))
          (right (issue319-fingerprint-envelope wire)))
      (is (string= (star.actors::target-dispatch-fingerprint left)
                   (star.actors::target-dispatch-fingerprint right)))
      (is (star.actors::target-acceptance-equivalent-p
           (issue319-old-acceptance left)
           (star.actors::target-acceptance-document right))))))

(test canonical-target-fingerprint-rejects-inconsistent-typed-record
  (let ((envelope (issue319-fingerprint-envelope (issue319-fingerprint-document))))
    (setf (star.actors::target-record-target
           (star.actors::target-dispatch-envelope-record envelope))
          "changed-only-in-the-record")
    (signals star.actors:invalid-target-dispatch
      (star.actors::target-dispatch-fingerprint envelope))))
