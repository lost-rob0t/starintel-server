(in-package :star-server-tests)

(def-suite ipfs-export-user-api-tests
  :description "Issue #238 IPFS export, user self-service, billing preview, and GPL metadata")

(in-suite ipfs-export-user-api-tests)

(defun issue-238-function (package-name symbol-name)
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol symbol-name package))))
    (and symbol
         (fboundp symbol)
         (symbol-function symbol))))

(test issue-238-contract-operations-exist
  (dolist (operation-id '("exports.create"
                          "users.me.get"
                          "users.me.billing.preview"))
    (is-true
     (star.http.contract:find-http-operation operation-id :errorp nil))))

(test issue-238-contract-documents-advertise-gpl
  (let* ((openapi (jsown:parse (star.http.contract:openapi-json)))
         (info (jsown:val openapi "info"))
         (license (jsown:val info "license"))
         (manifest (jsown:parse (star.http.contract:client-manifest-json))))
    (is (string= "GPL-3.0-or-later"
                 (jsown:val license "identifier")))
    (is (string= "GPL-3.0-or-later"
                 (jsown:val manifest "license")))
    (is-true
     (search "starintel-server"
             (jsown:val manifest "source_repository")
             :test #'char-equal))))

(test issue-238-dataset-export-receipt-has-integrity-and-license
  (let* ((dataset "issue-238-export")
         (documents (loop for index below 3
                          collect (make-export-document dataset index)))
         (path (dataset-export-test-path)))
    (unwind-protect
         (multiple-value-bind (query-fn calls-fn)
             (make-export-query documents)
           (declare (ignore calls-fn))
           (let ((result
                   (star.databases.couchdb:export-by-dataset*
                    nil nil dataset path
                    :page-size 2
                    :query-fn query-fn)))
             (is-true (getf result :ok))
             (is (plusp (getf result :bytes)))
             (is (= 64 (length (getf result :sha256))))
             (is (string= "GPL-3.0-or-later"
                          (getf result :license)))
             (is-true
              (search "starintel-server"
                      (getf result :source-repository)
                      :test #'char-equal))))
      (delete-test-file path))))

(test issue-238-billing-preview-is-deterministic-and-non-mutating
  (let ((preview-fn
          (issue-238-function "STAR.BILLING" "MAKE-BILLING-PREVIEW")))
    (is-true preview-fn)
    (when preview-fn
      (let* ((items
               (list
                (jsown:new-js
                  ("description" "dataset export")
                  ("quantity" 2)
                  ("unit_price_micros" 125000))
                (jsown:new-js
                  ("description" "ipfs publish")
                  ("quantity" 1)
                  ("unit_price_micros" 50000))))
             (preview
               (funcall preview-fn
                        "alice"
                        "USD"
                        items
                        :created-at 123456)))
        (is (string= "alice" (jsown:val preview "principal_id")))
        (is (string= "USD" (jsown:val preview "currency")))
        (is (= 300000 (jsown:val preview "total_micros")))
        (is-true (jsown:val preview "preview"))
        (is-false (jsown:val-safe preview "charged"))
        (is (= 123456 (jsown:val preview "created_at")))
        (is (string= "GPL-3.0-or-later"
                     (jsown:val preview "license")))))))

(test issue-238-ipfs-publisher-uses-injected-add-function
  (let ((publish-fn
          (issue-238-function
           "STAR.ACTORS.IPFS"
           "PUBLISH-EXPORT-MANIFEST")))
    (is-true publish-fn)
    (when publish-fn
      (let* ((directory
               (merge-pathnames
                (format nil "starintel-ipfs-test-~d/" (random most-positive-fixnum))
                (uiop:temporary-directory)))
             (path (merge-pathnames "artifact.jsonl" directory))
             (manifest
               (jsown:new-js
                 ("export_id" "test-export")
                 ("dataset" "demo")
                 ("path" (namestring path))
                 ("bytes" 8)
                 ("sha256" (make-string 64 :initial-element #\a))
                 ("license" "GPL-3.0-or-later")))
             (calls 0))
        (unwind-protect
             (progn
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "{}" stream))
               (let ((result
                       (funcall
                        publish-fn
                        manifest
                        :export-root directory
                        :add-fn
                        (lambda (file)
                          (incf calls)
                          (is (equal (truename path) (truename file)))
                          (jsown:new-js
                            ("Hash" "bafyissue238")
                            ("Size" "2"))))))
                 (is (= 1 calls))
                 (is (string= "bafyissue238"
                              (jsown:val result "cid")))
                 (is (string= "published"
                              (jsown:val result "status")))))
          (when (probe-file path)
            (delete-file path))
          (when (probe-file directory)
            (ignore-errors (uiop:delete-directory-tree directory
                                                       :validate t))))))))
