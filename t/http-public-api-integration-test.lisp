(in-package :star-server-tests)

(in-suite http-api-tests)

(test test-public-mode-defaults-enabled
  "Public-read mode is the default server posture unless init disables it."
  (is star::*public-mode*))

(test test-public-stats-does-not-require-authentication
  "The watch-safe aggregate stats endpoint is intentionally public by default."
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:get
          (make-test-url "/api/v1/stats")
          :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
    (is (= 200 status))
    (let* ((document (jsown:parse body))
           (data (jsown:val document "data"))
           (documents (jsown:val data "documents"))
           (targets (jsown:val data "targets")))
      (is (string= "ok" (jsown:val document "status")))
      (is (integerp (jsown:val data "generated_at")))
      (is (integerp (jsown:val documents "total")))
      (is (not (null (jsown:val documents "by_dtype"))))
      (is (integerp (jsown:val targets "total")))
      (is (null (search "password" body :test #'char-equal)))
      (is (null (search "credential" body :test #'char-equal))))))

(test test-public-search-does-not-require-authentication
  "Public v1 search bypasses credential authentication but still uses the
server-owned authorization scope before the backend query executes."
  (insert-test-document
   (make-test-user
    :id "public-search-test-user"
    :name "public-search-needle"
    :platform "github"))
  (sleep 2)
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:get
          (make-test-url "/api/v1/search?q=public-search-needle&limit=5")
          :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
    (declare (ignore body))
    (is (= 200 status))))

(test test-private-mode-requires-authentication-for-v1-search
  "Init can disable public reads without removing the versioned search route."
  (let ((original star::*public-mode*))
    (unwind-protect
         (progn
           (setf star::*public-mode* nil)
           (multiple-value-bind (status)
               (perform-request
                (lambda ()
                  (dex:get
                   (make-test-url "/api/v1/search?q=test")
                   :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
             (is (= 401 status))))
      (setf star::*public-mode* original))))

(test test-private-mode-requires-authentication-for-v1-stats
  "Init can disable anonymous aggregate stats for private deployments."
  (let ((original star::*public-mode*))
    (unwind-protect
         (progn
           (setf star::*public-mode* nil)
           (multiple-value-bind (status)
               (perform-request
                (lambda ()
                  (dex:get
                   (make-test-url "/api/v1/stats")
                   :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
             (is (= 401 status))))
      (setf star::*public-mode* original))))

(test test-public-search-rejects-caller-scope-overrides
  "Anonymous callers cannot supply tenant or dataset scope to widen search."
  (dolist (query '("q=test&dataset=private"
                   "q=test&tenant=other"))
    (multiple-value-bind (status body)
        (perform-request
         (lambda ()
           (dex:get
            (make-test-url (format nil "/api/v1/search?~a" query))
            :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
      (is (= 400 status))
      (is (search "public_scope_is_server_owned" body)))))

(test test-public-api-does-not-open-document-ingest
  "Making the read plane public must not make document ingestion anonymous."
  (multiple-value-bind (status)
      (perform-request
       (lambda ()
         (dex:post
          (make-test-url "/new/document/host")
          :content "{}"
          :headers '(("Content-Type" . "application/json")
                     ("X-Test-Auth-Mode" . "unauthenticated")))))
    (is (= 401 status))))

(test test-public-api-does-not-open-target-dispatch
  "Target dispatch remains authenticated even when public search is enabled."
  (multiple-value-bind (status)
      (perform-request
       (lambda ()
         (dex:post
          (make-test-url "/new/target/nmap")
          :content "{}"
          :headers '(("Content-Type" . "application/json")
                     ("X-Test-Auth-Mode" . "unauthenticated")))))
    (is (= 401 status))))
