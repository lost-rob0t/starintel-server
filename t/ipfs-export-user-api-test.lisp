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
        (let ((wire-preview (jsown:parse (jsown:to-json preview))))
          (is-false (jsown:val-safe wire-preview "charged")))
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
                 ("bytes" 0)
                 ("sha256" "")
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
               (setf (jsown:val manifest "bytes")
                     (with-open-file
                         (stream path
                                 :direction :input
                                 :element-type '(unsigned-byte 8))
                       (file-length stream))
                     (jsown:val manifest "sha256")
                     (string-downcase
                      (ironclad:byte-array-to-hex-string
                       (ironclad:digest-file :sha256 path))))
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


(test issue-238-users-me-never-serializes-auth-secrets
  (let* ((store (star.auth:make-memory-credential-store))
         (star.auth:*credential-store* store)
         (star:*auth-pepper* "issue-238-test-pepper")
         (user
           (star.auth:create-user
            "alice"
            "issue-238-password"
            "user"
            '("identity:read")
            :must-change-password nil
            :store store))
         (principal
           (star.auth::%make-request-principal
            :id "alice"
            :type "user"
            :scopes '("identity:read")
            :credential-id "credential-test"))
         (context
           (star.auth::%make-request-security-context
            :principal principal
            :correlation-id "issue-238-correlation"
            :deadline nil
            :authenticated-at 1))
         (star.auth:*request-security-context* context)
         (document (star.frontends.http-api::self-user-document))
         (wire (jsown:to-json document)))
    (declare (ignore user))
    (is (string=
         "alice"
         (jsown:val (jsown:val document "user") "username")))
    (is-false (search "password_hash" wire :test #'char-equal))
    (is-false (search "verifier" wire :test #'char-equal))
    (is-false (search "pepper" wire :test #'char-equal))))

(test issue-238-export-service-keeps-artifact-under-owned-root
  (let* ((root
           (merge-pathnames
            (format nil "starintel-export-root-~d/"
                    (random most-positive-fixnum))
            (uiop:temporary-directory)))
         (dataset "owned-root")
         (documents (list (make-export-document dataset 1))))
    (unwind-protect
         (multiple-value-bind (query-fn calls-fn)
             (make-export-query documents)
           (declare (ignore calls-fn))
           (let* ((manifest
                    (star.exports:create-dataset-export
                     nil nil dataset
                     :export-root root
                     :query-fn query-fn))
                  (path (pathname (jsown:val manifest "path"))))
             (is-true
              (star.exports:export-artifact-owned-p
               path :export-root root))
             (is-false
              (search ".."
                      (file-namestring path)
                      :test #'char=))))
      (when (probe-file root)
        (ignore-errors
          (uiop:delete-directory-tree root :validate t))))))
