(in-package :star-server-tests)
(in-suite v0101-runtime-tests)

(defun exact-number-wire (token &optional (id "canonical:exact-number"))
  (format nil "{\"id\":~a,\"dataset\":\"canonical-tests\",\"dtype\":\"person\",\"schemaVersion\":\"0.10.1\",\"fname\":\"Ada\",\"extensions\":{\"exact\":~a,\"nested\":[false,null,[],{}]}}"
          (jsown:to-json id) token))

(defun assert-exact-number-document (document token)
  (let ((extensions (jsown:val document "extensions")))
    (is (string= token (jsown:to-json (jsown:val extensions "exact"))))
    (is (string= "[false,null,[],{}]" (jsown:to-json (jsown:val extensions "nested"))))))

(test canonical-exact-numbers-survive-ingress-clone-validation-and-wire
  (dolist (token '("0.12345678901234567890123456789" "-0" "1e-200"
                   "900719925474099312345678901234567890"))
    (let* ((raw (exact-number-wire token))
           (http (star.frontends.http-api:parse-json-octets
                  (babel:string-to-octets raw :encoding :utf-8) "application/json"))
           (broker (star.rabbit:decode-rabbit-document (cons raw 1))))
      (dolist (document (list http broker
                             (star.documents:clone-document-object broker)
                             (star.documents:validate-document http)))
        (assert-exact-number-document document token))
      (assert-exact-number-document
       (star.documents:parse-document-object (star.documents:document-json broker)) token))))

(test canonical-exact-numbers-survive-outbox-and-public-projection
  (let* ((token "0.12345678901234567890123456789")
         (incoming (star.rabbit:decode-rabbit-document (cons (exact-number-wire token) 1))))
    (multiple-value-bind (state entry)
        (star.databases.couchdb:prepare-outbox-mutation nil incoming :new)
      (assert-exact-number-document (jsown:val entry "payload") token)
      (assert-exact-number-document
       (star.frontends.http-api:strip-server-tenant-fields state) token)
      (assert-exact-number-document
       (star.documents:parse-document-object
        (star.frontends.http-api:strip-server-tenant-fields (jsown:to-json state))) token))))

(test canonical-exact-numbers-survive-raw-view-transport
  (let* ((token "0.12345678901234567890123456789")
         (raw (format nil "{\"rows\":[{\"key\":[\"exact\"],\"doc\":~a}]}" (exact-number-wire token)))
         (star.databases.couchdb:*couchdb-view-transport*
           (lambda (client request) (declare (ignore client request)) raw))
         (response (star.databases.couchdb:query-view
                    (test-view-client) "canonical-tests" "fixture" "by_key" :include-docs t)))
    (is (listp (jsown:val response "rows")))
    (assert-exact-number-document (jsown:val (first (jsown:val response "rows")) "doc") token)))

(test canonical-target-json-accepts-exact-authority-numbers
  (let ((value (star.documents:parse-document-object "{\"exact\":0.12345678901234567890123456789}")))
    (is (string= "{\"exact\":0.12345678901234567890123456789}"
                 (star.actors::canonical-target-json value)))))

(test exact-storage-evidence-preserves-numeric-types-and-token-spelling
  (dolist (token '("0.12345678901234567890123456789" "-0" "1.00e+0"
                   "900719925474099312345678901234567890"))
    (let* ((document (star.documents:parse-document-object (exact-number-wire token)))
           (stored (star.databases.couchdb::prepare-exact-storage-document document))
           (bytes (jsown:to-json stored))
           (restored (star.databases.couchdb::restore-exact-storage-document
                      (star.documents:parse-document-object bytes))))
      (is (search (format nil "\"exact\":~a" token) bytes))
      (assert-exact-number-document restored token)
      (is (not (jsown:keyp (jsown:val restored "extensions") "_server_exact_numbers")))
      (is (string= bytes (jsown:to-json stored))))))

(test exact-storage-evidence-verifies-couchdb-projection-and-rejects-stale-state
  (let* ((token "0.12345678901234567890123456789")
         (document (star.documents:parse-document-object (exact-number-wire token)))
         (stored (star.databases.couchdb::prepare-exact-storage-document document)))
    (setf (jsown:val (jsown:val stored "extensions") "exact")
          (star.documents:parse-json-value "0.12345678901234568"))
    (assert-exact-number-document
     (star.databases.couchdb::restore-exact-storage-document stored) token)
    (setf (jsown:val stored "_rev") "2-couchdb-assigned")
    (assert-exact-number-document
     (star.databases.couchdb::restore-exact-storage-document stored) token)
    (let ((changed (star.documents:clone-json-value stored)))
      (setf (jsown:val changed "fname") "Changed")
      (signals error (star.databases.couchdb::restore-exact-storage-document changed)))
    (dolist (replacement (list "0.12345678901234568" :null 8))
      (let ((changed (star.documents:clone-json-value stored)))
        (setf (jsown:val (jsown:val changed "extensions") "exact") replacement)
        (signals error (star.databases.couchdb::restore-exact-storage-document changed))))
    (let ((changed (star.documents:clone-json-value stored)))
      (jsown:remkey (jsown:val changed "extensions") "exact")
      (signals error (star.databases.couchdb::restore-exact-storage-document changed)))))

(test exact-storage-evidence-is-server-derived-and-update-safe
  (let* ((token "1.00e+0")
         (document (star.documents:parse-document-object (exact-number-wire token))))
    (setf (jsown:val (jsown:val document "extensions") "_server_exact_numbers")
          (jsown:new-js ("forged" :true)))
    (let* ((stored (star.databases.couchdb::prepare-exact-storage-document document))
           (restored (star.databases.couchdb::restore-exact-storage-document stored)))
      (assert-exact-number-document restored token)
      (setf (jsown:val (jsown:val restored "extensions") "exact") 2)
      (let ((updated (star.databases.couchdb::prepare-exact-storage-document restored)))
        (is (not (jsown:keyp (jsown:val updated "extensions") "_server_exact_numbers")))
        (is (= 2 (jsown:val (jsown:val updated "extensions") "exact"))))
      (jsown:remkey (jsown:val restored "extensions") "exact")
      (let ((updated (star.databases.couchdb::prepare-exact-storage-document restored)))
        (is (not (jsown:keyp (jsown:val updated "extensions") "_server_exact_numbers")))))))

(test exact-storage-paths-distinguish-opaque-keys-and-array-indices
  (let* ((document (star.documents:parse-document-object
                    "{\"_id\":\"paths\",\"extensions\":{\"a/b~0\":[{\"0\":-0},1.0e0]}}"))
         (stored (star.databases.couchdb::prepare-exact-storage-document document))
         (restored (star.databases.couchdb::restore-exact-storage-document stored)))
    (is (string= (jsown:to-json document) (jsown:to-json restored)))
    (let* ((evidence (jsown:val (jsown:val stored "extensions") "_server_exact_numbers"))
           (records (coerce (jsown:val evidence "tokens") 'list)))
      (setf (jsown:val evidence "tokens") (list (first records) (first records)))
      (signals error (star.databases.couchdb::restore-exact-storage-document stored)))))

(test exact-storage-unmarked-history-is-not-reconstructed
  (let* ((raw "{\"_id\":\"old\",\"extensions\":{\"exact\":0.12345679}}")
         (document (star.documents:parse-document-object raw)))
    (is (eq document (star.databases.couchdb::restore-exact-storage-document document)))
    (is (string= raw (jsown:to-json document)))))

(test exact-storage-restores-original-extension-presence
  (dolist (raw '("{\"_id\":\"absent\",\"number\":-0}"
                 "{\"_id\":\"empty\",\"number\":-0,\"extensions\":{}}"
                 "{\"_id\":\"full\",\"number\":-0,\"extensions\":{\"keep\":true}}"))
    (let* ((document (star.documents:parse-document-object raw))
           (stored (star.databases.couchdb::prepare-exact-storage-document document))
           (restored (star.databases.couchdb::restore-exact-storage-document stored)))
      (is (string= raw (jsown:to-json restored))))))

(test exact-storage-token-limit-is-symmetric-and-precedes-write
  (let* ((star.databases.couchdb::+exact-number-max-token-length+ 8)
         (accepted (star.documents:parse-document-object (exact-number-wire "0.123456")))
         (rejected (star.documents:parse-document-object (exact-number-wire "0.1234567")))
         (original (symbol-function 'cl-couch:create-document))
         (writes 0))
    (assert-exact-number-document
     (star.databases.couchdb::restore-exact-storage-document
      (star.databases.couchdb::prepare-exact-storage-document accepted)) "0.123456")
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-couch:create-document)
                 (lambda (&rest args) (declare (ignore args)) (incf writes) "{}"))
           (signals error
             (star.databases.couchdb::couchdb-save-outbox-document nil "unused" rejected))
           (is (zerop writes)))
      (setf (symbol-function 'cl-couch:create-document) original))))

(test exact-storage-rejects-present-empty-evidence
  (dolist (value (list nil :null :false (jsown:empty-object)))
    (let ((document (star.documents:parse-document-object (exact-number-wire "1.0"))))
      (setf (jsown:val (jsown:val document "extensions") "_server_exact_numbers") value)
      (signals error (star.databases.couchdb::restore-exact-storage-document document)))))

(defun exact-retry-fixture-json (token &optional explicit-key)
  (format nil "{\"id\":\"canonical:precision-retry\",\"dataset\":\"canonical-tests\",\"dtype\":\"person\",\"schemaVersion\":\"0.10.1\",\"fname\":\"Ada\",\"extensions\":{\"exact\":~a,\"nested\":[false,null,[],{}]~a},\"_id\":\"canonical:precision-retry\"}"
          token (if explicit-key
                    (format nil ",\"mutation_id\":~a" (jsown:to-json explicit-key)) "")))

(defun frozen-precision-outbox (&optional explicit-key)
  ;; Literal old serialized preimage, independent of the repaired hash helper.
  (let* ((public-json (exact-retry-fixture-json "0.12345679" explicit-key))
         (hash (star.databases.couchdb::outbox-digest-string
                (concatenate 'string "updated|" public-json)))
         (mutation-id (or explicit-key hash))
         (event-id (star.databases.couchdb::outbox-digest-string
                    (concatenate 'string "event|" mutation-id)))
         (state (star.documents:parse-document-object public-json))
         (payload (star.documents:parse-document-object public-json))
         (ledger (jsown:empty-object)))
    (jsown:remkey payload "_id")
    (let ((extensions (jsown:val payload "extensions")))
      (setf (jsown:val extensions "event_id") event-id
            (jsown:val extensions "mutation_id") mutation-id
            (jsown:val extensions "event_operation") "updated"
            (jsown:val extensions "event_sequence") 1))
    (let ((entry (jsown:new-js
                  ("event_id" event-id) ("mutation_id" mutation-id)
                  ("content_hash" hash) ("document_id" "canonical:precision-retry")
                  ("sequence" 1) ("operation" "updated")
                  ("routing_key" "documents.updated.person")
                  ("status" "pending") ("created_at" "2026-10-10T00:00:00Z")
                  ("published_at" :null) ("payload" payload)
                  ("public_extensions_present" :true))))
      (setf (jsown:val ledger mutation-id) hash
            (jsown:val (jsown:val state "extensions") "_server_mutations") ledger
            (jsown:val (jsown:val state "extensions") "_server_outbox") (vector entry)
            (jsown:val state "_rev") "1-frozen")
      (values state entry mutation-id))))

(defun immutable-precision-entry-json (entry)
  (jsown:to-json
   (star.databases.couchdb::copy-json-object-excluding entry '("status" "published_at"))))

(test historical-rounded-retries-fail-closed-without-rewriting
  (dolist (key '(nil "legacy-precision-key"))
    (multiple-value-bind (state old-entry mutation-id) (frozen-precision-outbox key)
      (let ((before (jsown:to-json state)) (saves 0) (publications 0))
        (dolist (token '("0.12345678901234567890123456789"
                         "0.12345678901234567890123456788"))
          (signals star.databases.couchdb:mutation-conflict
            (star.databases.couchdb:process-outbox-mutation
             (lambda (id) (declare (ignore id)) state)
             (lambda (updated) (incf saves) updated)
             (lambda (&rest args) (declare (ignore args)) (incf publications))
             (star.documents:parse-document-object (exact-retry-fixture-json token key))
             :updated)))
        (is (= 0 saves))
        (is (= 0 publications))
        (is (string= before (jsown:to-json state)))
        (multiple-value-bind (same replay disposition)
            (star.databases.couchdb:prepare-outbox-mutation
             state (star.documents:parse-document-object
                    (exact-retry-fixture-json "0.12345679" key)) :updated)
          (is (eq :duplicate disposition))
          (is (eq state same))
          (is (eq old-entry replay))
          (is (string= mutation-id (jsown:val replay "mutation_id")))
          (is (string= before (jsown:to-json state))))))))

(test frozen-pending-precision-replay-retains-payload-and-identities
  (multiple-value-bind (state old-entry mutation-id) (frozen-precision-outbox)
    (let ((immutable (immutable-precision-entry-json old-entry))
          (ledger (jsown:to-json (star.databases.couchdb::document-mutation-ledger state)))
          (payload (jsown:to-json (jsown:val old-entry "payload")))
          (event-id (jsown:val old-entry "event_id"))
          (fail-marker t) (publications 0))
      (labels ((load-state (id) (declare (ignore id)) state)
               (save-state (updated)
                 (when fail-marker
                   (setf fail-marker nil)
                   (error "crash after publish, before marker"))
                 (setf state updated))
               (publish (routing sent id)
                 (incf publications)
                 (is (string= "documents.updated.person" routing))
                 (is (string= event-id id))
                 (is (string= payload (jsown:to-json sent)))
                 (assert-exact-number-document sent "0.12345679")))
        (signals error
          (star.databases.couchdb:recover-outbox-documents
           #'load-state #'save-state #'publish (list state)))
        (is (= 1 publications))
        (star.databases.couchdb:recover-outbox-documents
         #'load-state #'save-state #'publish (list state))
        (is (= 2 publications))
        (let ((replayed (star.databases.couchdb::find-outbox-entry state mutation-id)))
          (is (star.databases.couchdb::outbox-entry-published-p replayed))
          (is (string= immutable (immutable-precision-entry-json replayed)))
          (is-false (jsown:keyp replayed "content_encoding")))
        (is (string= ledger
                     (jsown:to-json (star.databases.couchdb::document-mutation-ledger state))))))))

(test exact-era-former-rounding-collisions-remain-distinct-updates
  (let* ((rounded (star.documents:parse-document-object
                   (exact-retry-fixture-json "0.12345679")))
         (precise (star.documents:parse-document-object
                   (exact-retry-fixture-json "0.12345678901234567890123456789")))
         (seed (star.documents:clone-document-object rounded)))
    (setf (jsown:val seed "_rev") "1-seed")
    (multiple-value-bind (first first-entry first-status)
        (star.databases.couchdb:prepare-outbox-mutation seed rounded :updated)
      (is (eq :created first-status))
      (is (string= "exact-json-v1" (jsown:val first-entry "content_encoding")))
      (setf (jsown:val first "_rev") "2-first")
      (multiple-value-bind (second second-entry second-status)
          (star.databases.couchdb:prepare-outbox-mutation first precise :updated)
        (is (eq :created second-status))
        (is (= 2 (length (star.databases.couchdb::document-outbox-entries second))))
        (dolist (field '("mutation_id" "content_hash" "event_id"))
          (is (not (string= (jsown:val first-entry field)
                            (jsown:val second-entry field)))))
        (is (= 2 (jsown:val second-entry "sequence")))
        (assert-exact-number-document (jsown:val second-entry "payload")
                                      "0.12345678901234567890123456789")
        (multiple-value-bind (same replay disposition)
            (star.databases.couchdb:prepare-outbox-mutation second precise :updated)
          (is (eq :duplicate disposition))
          (is (eq second same))
          (is (eq second-entry replay)))))))

(test fresh-explicit-key-permits-deliberate-precision-correction
  (multiple-value-bind (state old-entry old-id) (frozen-precision-outbox)
    (let ((before (immutable-precision-entry-json old-entry))
          (old-hash (jsown:val old-entry "content_hash")))
      (multiple-value-bind (updated entry disposition)
          (star.databases.couchdb:prepare-outbox-mutation
           state (star.documents:parse-document-object
                  (exact-retry-fixture-json "0.12345678901234567890123456789" "fresh-exact-key"))
           :updated)
        (is (eq :created disposition))
        (is (string= "fresh-exact-key" (jsown:val entry "mutation_id")))
        (is (string= before
                     (immutable-precision-entry-json
                      (star.databases.couchdb::find-outbox-entry updated old-id))))
        (is (string= old-hash
                     (jsown:val (star.databases.couchdb::document-mutation-ledger updated) old-id)))))))

(test legacy-numeric-collision-respects-recorded-extension-presence
  (dolist (old-present '(nil t))
    (let* ((prefix "{\"id\":\"canonical:target-presence\",\"_id\":\"canonical:target-presence\",\"dtype\":\"target\",\"schemaVersion\":\"0.10.1\",\"dataset\":\"canonical-tests\",\"actor\":\"subfinder\",\"target\":\"example.org\",\"delay\":60,\"recurring\":false,\"options\":{\"exact\":")
           (old-json (concatenate 'string prefix "0.12345679},\"extensions\":{}}"))
           (hash (star.databases.couchdb::outbox-digest-string
                  (concatenate 'string "updated|" old-json)))
           (id (if old-present hash
                   (star.databases.couchdb::outbox-digest-string
                    (concatenate 'string "canonical-extensions-absent|" hash))))
           (state (star.documents:parse-document-object old-json))
           (entry (jsown:new-js
                    ("mutation_id" id) ("content_hash" hash) ("sequence" 1)
                    ("public_extensions_present" (if old-present :true :false))))
           (ledger (jsown:new-js)))
      (setf (jsown:val ledger id) hash
            (jsown:val (jsown:val state "extensions") "_server_mutations") ledger
            (jsown:val (jsown:val state "extensions") "_server_outbox") (vector entry)
            (jsown:val state "_rev") "1-old")
      (let ((incoming (star.documents:parse-document-object
                       (concatenate 'string prefix "0.12345678901234567890123456789}"
                                    (if old-present "}" ",\"extensions\":{}}")))))
        (star.documents:validate-stored-document incoming)
        (multiple-value-bind (updated new-entry status)
            (star.databases.couchdb:prepare-outbox-mutation state incoming :updated)
          (declare (ignore updated))
          (is (eq :created status))
          (is (= 2 (jsown:val new-entry "sequence"))))))))

(test exact-binary64-projection-diagnostic
  (let* ((full "0.12345678901234567890123456789")
         (stored "0.12345678901234568"))
    (format t "~&EXACT_PROJECTION_DIAGNOSTIC jzon-full=~s jzon-stored=~s~%"
            (com.inuoe.jzon:parse full) (com.inuoe.jzon:parse stored))
    (is (star.databases.couchdb::exact-storage-number-matches-p
         (star.documents:parse-json-value stored) full))))

(test exact-storage-private-evidence-is-hidden-on-legacy-readback
  (let* ((document (star.documents:parse-document-object
                    "{\"_id\":\"legacy:exact\",\"dtype\":\"person\",\"data\":{\"value\":1.00e0}}"))
         (stored (star.databases.couchdb::prepare-exact-storage-document document))
         (wire (star.documents:parse-document-object
                (star.frontends.http-api:strip-server-tenant-fields (jsown:to-json stored)))))
    (is (string= "1.00e0" (jsown:to-json (jsown:val (jsown:val wire "data") "value"))))
    (is (not (jsown:keyp wire "extensions")))))

(test exact-binary64-projection-is-bounded-and-never-epsilon-based
  (let ((full "0.12345678901234567890123456789"))
    (is (star.databases.couchdb::exact-storage-number-matches-p
         (star.documents:parse-json-value "0.12345678901234568") full))
    (is (not (star.databases.couchdb::exact-storage-number-matches-p
              (star.documents:parse-json-value "0.12345678901234569") full))))
  (dolist (token '("-0" "1e-200" "5e-324" "1.7976931348623157e308"))
    (is (floatp (star.databases.couchdb::exact-storage-binary64
                 (star.documents:parse-json-value token)))))
  (dolist (token '("1e-400" "1e-324" "1e400" "1.7976931348623159e308"))
    (signals error (star.databases.couchdb::prepare-exact-storage-document
                    (star.documents:parse-document-object (exact-number-wire token)))))
  (let ((token (concatenate 'string "0.1" (make-string 4096 :initial-element #\0) "1")))
    (assert-exact-number-document
     (star.databases.couchdb::restore-exact-storage-document
      (star.databases.couchdb::prepare-exact-storage-document
       (star.documents:parse-document-object (exact-number-wire token)))) token)))
