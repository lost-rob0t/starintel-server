(in-package :star-server-tests)

(in-suite http-api-tests)

(defun report-public-api-response (case status body)
  (format t "~&[public-api-integration] ~a status=~a body=~s~%"
          case status body)
  (finish-output))

(test test-public-mode-defaults-enabled
  "Public-read mode is the default server posture unless init disables it."
  (is (not (null star::*public-mode*))))

(test test-public-stats-does-not-require-authentication
  "The watch-safe aggregate stats endpoint is intentionally public by default."
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:get
          (make-test-url "/api/v1/stats")
          :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
    (report-public-api-response "public-stats" status body)
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
    (report-public-api-response "public-search" status body)
    (is (= 200 status))))

(test test-private-mode-requires-authentication-for-v1-search
  "Init can disable public reads without removing the versioned search route."
  (let ((original star::*public-mode*))
    (unwind-protect
         (progn
           (setf star::*public-mode* nil)
           (multiple-value-bind (status body)
               (perform-request
                (lambda ()
                  (dex:get
                   (make-test-url "/api/v1/search?q=test")
                   :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
             (report-public-api-response "private-mode-search" status body)
             (is (= 401 status))))
      (setf star::*public-mode* original))))

(test test-private-mode-requires-authentication-for-v1-stats
  "Init can disable anonymous aggregate stats for private deployments."
  (let ((original star::*public-mode*))
    (unwind-protect
         (progn
           (setf star::*public-mode* nil)
           (multiple-value-bind (status body)
               (perform-request
                (lambda ()
                  (dex:get
                   (make-test-url "/api/v1/stats")
                   :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
             (report-public-api-response "private-mode-stats" status body)
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
      (report-public-api-response
       (format nil "scope-override ~a" query)
       status body)
      (is (= 400 status))
      (is (search "public_scope_is_server_owned" body)))))

(test test-public-api-does-not-open-document-ingest
  "Making the read plane public must not make document ingestion anonymous."
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:post
          (make-test-url "/new/document/host")
          :content "{}"
          :headers '(("Content-Type" . "application/json")
                     ("X-Test-Auth-Mode" . "unauthenticated")))))
    (report-public-api-response "anonymous-document-ingest" status body)
    (is (= 401 status))))

(test test-public-api-does-not-open-target-dispatch
  "Target dispatch remains authenticated even when public search is enabled."
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:post
          (make-test-url "/new/target/nmap")
          :content "{}"
          :headers '(("Content-Type" . "application/json")
                     ("X-Test-Auth-Mode" . "unauthenticated")))))
    (report-public-api-response "anonymous-target-dispatch" status body)
    (is (= 401 status))))

(defun make-activity-integration-document
    (id added-at &key observed-at)
  "Build a minimal CouchDB fixture for activity/timeline index integration."
  (let ((document
          (jsown:new-js
            ("_id" id)
            ("dataset" "activity-integration")
            ("dtype" "document")
            ("date_added" added-at)
            ("date_updated" added-at)
            ("data" (jsown:new-js ("title" id))))))
    (when observed-at
      (setf (jsown:val document "temporal")
            (jsown:new-js ("observed_at" observed-at))))
    document))

(test test-activity-v2-is-authenticated-and-couchdb-backed
  "The compact activity API reads real indexed history and is not public."
  (multiple-value-bind (status body)
      (perform-request
       (lambda ()
         (dex:get
          (make-test-url "/api/v1/activity?range=1h")
          :headers '(("X-Test-Auth-Mode" . "unauthenticated")))))
    (declare (ignore body))
    (is (= 401 status)))
  (let ((now (star.frontends.http-api::activity-v2-unix-now)))
    (insert-test-document
     (make-activity-integration-document
      "activity-integration-a" (- now 300)))
    (insert-test-document
     (make-activity-integration-document
      "activity-integration-b" (- now 60)))
    (multiple-value-bind (status body)
        (perform-request
         (lambda ()
           (dex:get
            (make-test-url
             "/api/v1/activity?metric=documents_added&range=1h&max_points=120"))))
      (report-public-api-response "activity-v2" status body)
      (is (= 200 status))
      (let* ((document (jsown:parse body))
             (data (jsown:val document "data"))
             (samples (jsown:val data "samples")))
        (is (string= "ok" (jsown:val document "status")))
        (is (string= "documents_added" (jsown:val data "metric")))
        (is (string= "added" (jsown:val data "time_basis")))
        (is (= 60 (jsown:val data "bucket_seconds")))
        (is (<= (length samples) 120))
        (is (>= (jsown:val data "delta") 2))))))

(test test-timeline-v2-preserves-explicit-time-basis-end-to-end
  "Observed-time queries return observed rows and never substitute added time."
  (let ((now (star.frontends.http-api::activity-v2-unix-now)))
    (insert-test-document
     (make-activity-integration-document
      "activity-timeline-observed" (- now 120)
      :observed-at (- now 30)))
    (insert-test-document
     (make-activity-integration-document
      "activity-timeline-added-only" (- now 20)))
    (multiple-value-bind (status body)
        (perform-request
         (lambda ()
           (dex:get
            (make-test-url
             "/api/v1/timeline?basis=observed&range=1h&limit=200"))))
      (report-public-api-response "timeline-v2" status body)
      (is (= 200 status))
      (let* ((document (jsown:parse body))
             (data (jsown:val document "data"))
             (events (jsown:val data "events"))
             (observed
               (find "activity-timeline-observed"
                     events
                     :key (lambda (event)
                            (jsown:val event "id"))
                     :test #'string=))
             (added-only
               (find "activity-timeline-added-only"
                     events
                     :key (lambda (event)
                            (jsown:val event "id"))
                     :test #'string=)))
        (is (string= "observed" (jsown:val data "time_basis")))
        (is (not (null observed)))
        (is (string= "observed" (jsown:val observed "time_basis")))
        (is (null added-only))))))
