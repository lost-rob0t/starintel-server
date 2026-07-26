(in-package :cl-user)

(defun conformance-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun read-json-injective (pathname)
  (jsown:with-injective-reader
    (jsown:parse (uiop:read-file-string pathname))))

(defun fixture-path (relative)
  (merge-pathnames relative (uiop:getcwd)))

(defun v09-fixture-documents ()
  (loop for index from 1 to 5
        for path = (fixture-path
                    (format nil
                            "test/fixtures/starintel/v0.9/fixtures-~2,'0d.json"
                            index))
        append (jsown:val (read-json-injective path) "fixtures")))

(defun find-fixture (dtype)
  (find dtype
        (v09-fixture-documents)
        :key (lambda (document)
               (jsown:val document "dtype"))
        :test #'string=))

(defun find-v08-case (name)
  (find name
        (jsown:val
         (read-json-injective
          (fixture-path
           "test/fixtures/starintel/v0.8/index-fixtures.json"))
         "cases")
        :key (lambda (case)
               (jsown:val case "name"))
        :test #'string=))

(defun test-v09-json-round-trips ()
  (let ((documents (v09-fixture-documents)))
    (conformance-check (= 49 (length documents))
                       "Expected 49 v0.9 dtype fixtures, got ~d"
                       (length documents))
    (dolist (document documents)
      (let* ((dtype (jsown:val document "dtype"))
             (encoded (jsown:to-json document))
             (decoded (jsown:with-injective-reader
                        (jsown:parse encoded))))
        (conformance-check
         (star.documents:v09-document-p document)
         "Fixture ~a is not a strict v0.9 document"
         dtype)
        (conformance-check
         (equal document decoded)
         "JSON semantic round trip changed fixture ~a"
         dtype)))))

(defun test-clos-document-round-trip ()
  (let* ((document (find-fixture "person"))
         (decoded (spec:decode document 'spec:person))
         (encoded (spec:encode decoded))
         (wire (jsown:with-injective-reader
                 (jsown:parse (jsown:to-json encoded))))
         (data (jsown:val wire "data")))
    (conformance-check
     (string= "0.9.0" (jsown:val encoded "schema_version"))
     "CLOS encoder changed schema_version")
    (conformance-check
     (string= "person" (jsown:val encoded "dtype"))
     "CLOS encoder changed dtype")
    (conformance-check
     (eq :false (jsown:val data "verified"))
     "CLOS round trip collapsed JSON false")
    (conformance-check
     (eq :null (jsown:val data "nullable"))
     "CLOS round trip collapsed JSON null")
    (conformance-check
     (and (listp (jsown:val data "misc"))
          (not (eq :false (jsown:val data "misc"))))
     "CLOS round trip collapsed an empty array")
    (conformance-check
     (string= "" (jsown:val data "empty_string"))
     "CLOS round trip changed an empty string")
    (conformance-check
     (jsown:val data "nested")
     "CLOS round trip dropped a nested object")))

(defun test-v08-index-adapter ()
  (let* ((case (find-v08-case "message-preserves-false-and-empty-array"))
         (legacy (jsown:val case "input"))
         (profile (star.documents:schema-profile-for-document legacy))
         (projection (star.documents:normalize-document-for-index legacy))
         (wire (jsown:with-injective-reader
                 (jsown:parse (jsown:to-json projection))))
         (data (jsown:val wire "data"))
         (mapping
           (star.documents:profile-data-slot-map
            profile
            (star.documents:find-schema-profile "0.9.0")
            "message")))
    (conformance-check
     (typep profile 'star.documents:v08-schema-profile)
     "Legacy fixture did not resolve to v0.8 profile")
    (conformance-check
     (assoc "isReply" mapping :test #'string=)
     "MOP slot map did not derive isReply -> is_reply")
    (conformance-check
     (string= "is_reply"
              (cdr (assoc "isReply" mapping :test #'string=)))
     "MOP slot map derived the wrong canonical key")
    (conformance-check
     (eq :false (jsown:val data "is_reply"))
     "v0.8 adapter collapsed JSON false")
    (conformance-check
     (and (listp (jsown:val data "media"))
          (not (eq :false (jsown:val data "media"))))
     "v0.8 adapter collapsed an empty array")
    (conformance-check
     (string= "0.8.0" (jsown:val projection "schema_version"))
     "v0.8 index projection lied about its source schema")
    (conformance-check
     (string= "0.9.0"
              (jsown:val
               (jsown:val projection "extensions")
               "index_schema_version"))
     "v0.8 projection did not declare its v0.9 index shape")
    (handler-case
        (progn
          (star.documents:writable-schema-profile-for-document legacy)
          (error "v0.8 document was accepted at the write boundary"))
      (star.documents:read-only-document-schema () t))))

(defun run-document-conformance-tests ()
  (format t "~&Running StarIntel document conformance tests~%")
  (test-v09-json-round-trips)
  (test-clos-document-round-trip)
  (test-v08-index-adapter)
  (format t "~&StarIntel document conformance tests passed~%")
  t)

(run-document-conformance-tests)
