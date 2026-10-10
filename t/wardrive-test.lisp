(in-package :star-server-tests)

(def-suite wardrive-tests :description "WarStar adapter against the pinned canonical boundary")
(in-suite wardrive-tests)

(defun wardrive-fixture ()
  (jsown:parse
   (uiop:read-file-string
    (asdf:system-relative-pathname :starintel-wardrive "t/fixtures/warstar-observations.json"))))

(test wardrive-canonical-readiness-is-independent-of-legacy-version
  (is (string= "0.9.0" starintel:+starintel-doc-version+))
  (is (string= "0.10.1" (starintel.canonical:release-version)))
  (is-true (star.addons.wardrive::schema-ready-p)))

(test wardrive-generated-documents-pass-http-and-rabbit-validation
  (let* ((fixture (wardrive-fixture))
         (documents
           (loop for sample in (jsown:val fixture "observations")
                 append (star.addons.wardrive::sample-documents
                         (jsown:val fixture "device_id") sample "warstar-writer"))))
    (is (equal '("geo-point" "wireless-network" "geo-point" "network-device")
               (mapcar #'star.documents:document-dtype documents)))
    (dolist (document documents)
      (is (eq document (star.frontends.http-api:validate-document-input document)))
      (let ((stored (star.rabbit:decode-rabbit-document (cons (jsown:to-json document) 1))))
        (is (string= (jsown:val document "id") (jsown:val stored "_id")))
        (is (eq stored (star.documents:validate-stored-document stored)))))))

(test wardrive-route-requires-exact-bulk-capability
  (is (string= "documents:bulk"
               (star.frontends.http-api::route-action :post "/warstar/observations")))
  (is-false (star.frontends.http-api::route-action :get "/warstar/observations")))

(test wardrive-identities-are-owned-and-stable
  (let* ((fixture (wardrive-fixture))
         (sample (first (jsown:val fixture "observations")))
         (device (jsown:val fixture "device_id"))
         (first-owner (star.addons.wardrive::sample-documents device sample "first-owner"))
         (retry (star.addons.wardrive::sample-documents (string-upcase device) sample "first-owner"))
         (other-owner (star.addons.wardrive::sample-documents device sample "other-owner")))
    (is (equal (mapcar #'star.documents:document-id first-owner)
               (mapcar #'star.documents:document-id retry)))
    (is (not (equal (mapcar #'star.documents:document-id first-owner)
                    (mapcar #'star.documents:document-id other-owner))))
    (is (every (lambda (document) (string= "first-owner" (jsown:val document "owner"))) first-owner))))
