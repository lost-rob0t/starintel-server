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
