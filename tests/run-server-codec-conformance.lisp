(in-package :cl-user)

(defun codec-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun codec-fixture-path (relative)
  (merge-pathnames relative (uiop:getcwd)))

(defun codec-read-json (pathname)
  (jsown:with-injective-reader
    (jsown:parse (uiop:read-file-string pathname))))

(defun codec-v09-fixtures ()
  (loop for index from 1 to 5
        append
        (jsown:val
         (codec-read-json
          (codec-fixture-path
           (format nil
                   "test/fixtures/starintel/v0.9/fixtures-~2,'0d.json"
                   index)))
         "fixtures")))

(defun codec-fixture (dtype)
  (or (find dtype
            (codec-v09-fixtures)
            :key (lambda (document) (jsown:val document "dtype"))
            :test #'string=)
      (error "Missing codec fixture for dtype ~a" dtype)))

(defun codec-v08-case (name)
  (find name
        (jsown:val
         (codec-read-json
          (codec-fixture-path
           "test/fixtures/starintel/v0.8/index-fixtures.json"))
         "cases")
        :key (lambda (case) (jsown:val case "name"))
        :test #'string=))

(defun clone-json-object (object)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json object))))

(defun test-server-codec-wrappers ()
  (let* ((fixture (codec-fixture "person"))
         (decoded (star.databases.couchdb:from-json fixture 'spec:person))
         (encoded (star.databases.couchdb:as-json decoded))
         (wire (clone-json-object encoded))
         (data (jsown:val wire "data")))
    (codec-check (typep decoded 'spec:person)
                 "FROM-JSON did not select the registered person class")
    (codec-check (not (stringp encoded))
                 "AS-JSON serialized the JSOWN object prematurely")
    (codec-check (and (listp (jsown:val data "misc"))
                      (null (jsown:val data "misc")))
                 "AS-JSON did not preserve an empty JSON array")
    (codec-check (eq :false (jsown:val data "verified"))
                 "FROM-JSON/AS-JSON collapsed false")
    (codec-check (eq :null (jsown:val data "nullable"))
                 "FROM-JSON/AS-JSON collapsed null")
    (codec-check (string= "" (jsown:val data "empty_string"))
                 "FROM-JSON/AS-JSON changed an empty string")
    (codec-check (jsown:val data "nested")
                 "FROM-JSON/AS-JSON dropped a nested object")
    (setf (spec:doc-rev decoded) "2-codec-test")
    (codec-check
     (string= "2-codec-test"
              (jsown:val
               (star.databases.couchdb:as-json decoded)
               "_rev"))
     "Canonical wrapper dropped a valid CouchDB revision")))

(defun test-versioned-class-registry ()
  (let* ((profile (star.documents:find-schema-profile "0.9.0"))
         (message-class
           (star.documents:registered-document-class profile "message")))
    (codec-check (eq 'spec:message (class-name message-class))
                 "message dtype resolved to ~s"
                 (class-name message-class))
    (codec-check
     (member "person"
             (star.documents:registered-document-dtypes profile)
             :test #'string=)
     "person is absent from the v0.9 class registry")
    (handler-case
        (progn
          (star.databases.couchdb:from-json
           (codec-fixture "person")
           'spec:message)
          (error "Expected class mismatch was accepted"))
      (star.documents:document-class-mismatch () t))))

(defun test-unknown-dtype-does-not-intern ()
  (let* ((bogus "issue-12-untrusted-dtype-9f7c2")
         (package (find-package :starintel))
         (document (clone-json-object (codec-fixture "person")))
         (before (multiple-value-list (find-symbol bogus package))))
    (setf (jsown:val document "dtype") bogus)
    (handler-case
        (progn
          (star.documents:decode-document document)
          (error "Unknown dtype was accepted"))
      (star.documents:unknown-document-dtype () t))
    (codec-check
     (equal before (multiple-value-list (find-symbol bogus package)))
     "Rejecting an unknown dtype grew the STARINTEL symbol table")))

(defun test-v08-decode-remains-read-only ()
  (let* ((case (codec-v08-case "message-preserves-false-and-empty-array"))
         (legacy (jsown:val case "input"))
         (decoded (star.documents:decode-document legacy)))
    (codec-check (typep decoded 'spec:message)
                 "v0.8 message did not resolve through the versioned registry")
    (codec-check (string= "0.8.0" (spec:doc-schema-version decoded))
                 "v0.8 decode lost source schema identity")
    (handler-case
        (progn
          (star.databases.couchdb:as-json decoded)
          (error "v0.8 decoded object was accepted by canonical writer"))
      (star.documents:read-only-document-schema () t))))

(defun test-with-json-uses-canonical-api ()
  (let* ((person (codec-fixture "person"))
         (message (codec-fixture "message"))
         (person-result
           (star.actors:with-json person
             (list (dataset) (dtype) (parse-doc))))
         (message-false
           (star.actors:with-json message
             (val "is_reply" :absent)))
         (expansion
           (prin1-to-string
            (macroexpand-1
             '(star.actors:with-json document (parse-doc))))))
    (codec-check (string= (first person-result)
                          (jsown:val person "dataset"))
                 "WITH-JSON reads the wrong dataset key")
    (codec-check (string= "person" (second person-result))
                 "WITH-JSON changed dtype")
    (codec-check (typep (third person-result) 'spec:person)
                 "WITH-JSON PARSE-DOC did not use registered class selection")
    (codec-check (eq :false message-false)
                 "WITH-JSON VAL conflated false with absence")
    (codec-check (not (search "INTERN" expansion :test #'char-equal))
                 "WITH-JSON expansion still interns request-controlled dtype text")))

(defun run-server-codec-conformance-tests ()
  (format t "~&Running server codec compatibility tests~%")
  (test-server-codec-wrappers)
  (test-versioned-class-registry)
  (test-unknown-dtype-does-not-intern)
  (test-v08-decode-remains-read-only)
  (test-with-json-uses-canonical-api)
  (format t "~&Server codec compatibility tests passed~%")
  t)

(run-server-codec-conformance-tests)
