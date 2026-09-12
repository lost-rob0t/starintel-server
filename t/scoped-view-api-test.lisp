(in-package :star-server-tests)

(def-suite scoped-view-api-tests
  :description "Bounded scope-aware HTTP view contract")

(in-suite scoped-view-api-tests)

(defun scoped-test-contract (view-name)
  (star.frontends.http-api::scoped-view-contract-for "scoped" view-name))

(test scoped-view-registry-is-closed
  (is (scoped-test-contract "documents"))
  (is (scoped-test-contract "docs_added_by_day"))
  (is (scoped-test-contract "count_by_dtype"))
  (is (null (star.frontends.http-api::scoped-view-contract-for
             "data" "total"))))

(test aggregate-range-always-injects-tenant-and-dataset-prefix
  (let* ((params '(("limit" . "12")
                   ("startkey" . "\"2026-09-01\"")
                   ("endkey" . "\"2026-09-12\"")
                   ("group_level" . "3")))
         (arguments
           (star.frontends.http-api::build-scoped-view-query
            params
            (scoped-test-contract "docs_added_by_day")
            "tenant-a"
            "dataset-a")))
    (is (= 12 (getf arguments :limit)))
    (is (equal '("tenant-a" "dataset-a" "2026-09-01")
               (getf arguments :start-key)))
    (is (equal '("tenant-a" "dataset-a" "2026-09-12")
               (getf arguments :end-key)))
    (is-true (getf arguments :reduce))
    (is-false (getf arguments :include-docs))
    (is (= 3 (getf arguments :group-level)))
    (is-true (getf arguments :inclusive-end))))

(test default-prefix-range-cannot-cross-scope
  (let* ((arguments
           (star.frontends.http-api::build-scoped-view-query
            nil
            (scoped-test-contract "documents")
            "tenant-a"
            "dataset-a")))
    (is (equal '("tenant-a" "dataset-a")
               (getf arguments :start-key)))
    (is (equal '("tenant-a" "dataset-a" (:obj))
               (getf arguments :end-key)))
    (is-true (getf arguments :include-docs))
    (is-false (getf arguments :reduce))))

(test descending-default-range-reverses-only-server-owned-bounds
  (let* ((arguments
           (star.frontends.http-api::build-scoped-view-query
            '(("descending" . "true"))
            (scoped-test-contract "documents")
            "tenant-a"
            "dataset-a")))
    (is (equal '("tenant-a" "dataset-a" (:obj))
               (getf arguments :start-key)))
    (is (equal '("tenant-a" "dataset-a")
               (getf arguments :end-key)))
    (is-true (getf arguments :descending))))

(test aggregate-contract-rejects-map-mode-and-source-documents
  (signals star.frontends.http-api::http-input-error
    (star.frontends.http-api::build-scoped-view-query
     '(("reduce" . "false"))
     (scoped-test-contract "count_by_dtype")
     "tenant-a" "dataset-a"))
  (signals star.frontends.http-api::http-input-error
    (star.frontends.http-api::build-scoped-view-query
     '(("include_docs" . "true"))
     (scoped-test-contract "count_by_dtype")
     "tenant-a" "dataset-a")))

(test caller-cannot-inject-json-object-key-sentinels
  (signals star.frontends.http-api::http-input-error
    (star.frontends.http-api::build-scoped-view-query
     '(("startkey" . "{}"))
     (scoped-test-contract "docs_added_by_day")
     "tenant-a" "dataset-a")))

(test half-open-ranges-pass-inclusive-end-false
  (let ((arguments
          (star.frontends.http-api::build-scoped-view-query
           '(("inclusive_end" . "false")
             ("startkey" . "\"2026-09-01\"")
             ("endkey" . "\"2026-09-02\""))
           (scoped-test-contract "docs_added_by_day")
           "tenant-a" "dataset-a")))
    (is-false (getf arguments :inclusive-end))))
