(in-package :star-server-tests)

(in-suite authorization-policy-tests)

(test view-key-decoder-accepts-couchdb-printer-form
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              "[dataset-1 tenant-1]"))))

(test view-key-decoder-accepts-json-array
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              "[\"dataset-1\",\"tenant-1\"]"))))

(test view-key-decoder-accepts-materialized-sequences
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              '("dataset-1" "tenant-1"))))
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              #("dataset-1" "tenant-1")))))

(defun capture-query-audit (thunk)
  (let ((captured "")
        (star.observability::*observability-enabled* "true")
        (star.observability::*observability-signals* "logs,metrics,traces")
        (star.observability::*export-batch-fn*
          (lambda (signal records)
            (when (eq signal :logs)
              (setf captured
                    (jsown:to-json
                     (star.observability::otlp-payload signal records))))
            :ok)))
    ;; These variables are intentionally rebound to run the audit path without
    ;; starting the background exporter thread. Declare them SPECIAL here so
    ;; this fixture remains correct even when ASDF compiles the test file before
    ;; loading the observability implementation's DEFVAR/DEFPARAMETER forms.
    (declare (special star.observability::*observability-enabled*
                      star.observability::*observability-signals*
                      star.observability::*export-batch-fn*))
    (star.observability:reset-exporter-state)
    (unwind-protect
         (let ((star.observability::*exporter-running* t))
           (declare (special star.observability::*exporter-running*))
           (funcall thunk)
           (let ((star.observability::*exporter-stop* t))
             (declare (special star.observability::*exporter-stop*))
             (star.observability:flush-once))
           captured)
      (star.observability:reset-exporter-state))))

(test search-query-audit-records-query-and-context
  (let* ((principal
           (make-policy-principal
            "query-auditor"
            '("search:read" "tenant:default" "dataset:dataset-a")))
         (payload
           (capture-query-audit
            (lambda ()
              (star.authorization:authorized-search-query
               "content:needle"
               :principal principal
               :requested-dataset "dataset-a"
               :metadata
               (star.authorization::request-metadata
                :route "/search"
                :method :get
                :correlation-id "operation-1"))))))
    (is (search "search.query" payload))
    (is (search "content:needle" payload))
    (is (search "query-auditor" payload))
    (is (search "dataset-a" payload))
    (is (search "operation-1" payload))))

(test denied-search-query-is-still-audited
  (let* ((principal
           (make-policy-principal
            "denied-query-user"
            '("tenant:default" "dataset:dataset-a")))
         (payload
           (capture-query-audit
            (lambda ()
              (signals star.authorization:authorization-error
                (star.authorization:authorized-search-query
                 "content:denied-needle"
                 :principal principal
                 :requested-dataset "dataset-a"
                 :metadata
                 (star.authorization::request-metadata
                  :route "/search"
                  :method :get
                  :correlation-id "operation-denied")))))))
    (is (search "search.query" payload))
    (is (search "content:denied-needle" payload))
    (is (search "denied-query-user" payload))))
