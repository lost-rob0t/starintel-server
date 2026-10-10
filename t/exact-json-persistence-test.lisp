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
