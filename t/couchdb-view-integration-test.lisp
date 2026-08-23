(in-package :star-server-tests)

(def-suite couchdb-view-integration-tests
  :description "Real CouchDB view transport and grouped-reduction tests")

(in-suite couchdb-view-integration-tests)

(defparameter *view-integration-client* nil)
(defparameter *view-integration-database* "starintel-view-query-test")

(defun view-fixture-document (id kind rank bucket)
  (jsown:new-js
    ("_id" id)
    ("kind" kind)
    ("rank" rank)
    ("bucket" bucket)
    ("dataset" "view-query-tests")))

(defun view-fixture-design-document ()
  (jsown:new-js
    ("_id" "_design/issue21")
    ("views"
     (jsown:new-js
       ("by_key"
        (jsown:new-js
          ("map" "function(doc) { if (doc.kind && doc.rank) emit([doc.kind, doc.rank], doc.bucket); }")))
       ("counts"
        (jsown:new-js
          ("map" "function(doc) { if (doc.kind && doc.bucket) emit([doc.kind, doc.bucket], 1); }")
          ("reduce" "_sum")))))))

(defun setup-couchdb-view-integration-tests ()
  (setf *view-integration-client*
        (cl-couch:new-couchdb star:*couchdb-host*
                              star:*couchdb-port*
                              :scheme star:*couchdb-scheme*))
  (cl-couch:password-auth *view-integration-client*
                          star:*couchdb-user*
                          star:*couchdb-password*)
  (when (cl-couch:database-exists-p *view-integration-client*
                                    *view-integration-database*)
    (cl-couch:delete-database *view-integration-client*
                              *view-integration-database*))
  (cl-couch:create-database *view-integration-client*
                            *view-integration-database*)
  (cl-couch:create-document
   *view-integration-client* *view-integration-database*
   (jsown:to-json (view-fixture-design-document)))
  (dolist (document
           (list (view-fixture-document "alpha-1" "alpha" 1 "x")
                 (view-fixture-document "alpha-2" "alpha" 2 "x")
                 (view-fixture-document "alpha-3" "alpha" 3 "y")
                 (view-fixture-document "beta-1" "beta" 1 "x")
                 (view-fixture-document "beta-2" "beta" 2 "y")
                 (view-fixture-document "beta-3" "beta" 3 "y")))
    (cl-couch:create-document
     *view-integration-client* *view-integration-database*
     (jsown:to-json document)))
  (star.databases.couchdb::register-view-spec
   'issue-21-counts "issue21" "counts" :reducer-p t
   :default-reduce t :default-include-docs nil))

(defun teardown-couchdb-view-integration-tests ()
  (remhash 'issue-21-counts star.databases.couchdb::*view-registry*)
  (when (and *view-integration-client*
             (cl-couch:database-exists-p *view-integration-client*
                                         *view-integration-database*))
    (cl-couch:delete-database *view-integration-client*
                              *view-integration-database*))
  (setf *view-integration-client* nil))

(defun query-view-fixture (view &rest arguments)
  (apply #'star.databases.couchdb:query-view
         *view-integration-client*
         *view-integration-database*
         "issue21"
         view
         arguments))

(defun fixture-rows (response)
  (jsown:val response "rows"))

(defun row-ids (response)
  (mapcar (lambda (row) (jsown:val row "id"))
          (fixture-rows response)))

(test exact-compound-key-and-key-range-return-observed-rows
  (let ((exact (query-view-fixture "by_key" :key '("alpha" 2)))
        (range
          (query-view-fixture
           "by_key"
           :start-key '("alpha" 1)
           :end-key '("alpha" 3))))
    (is (equal '("alpha-2") (row-ids exact)))
    (is (equal '("alpha-1" "alpha-2" "alpha-3")
               (row-ids range)))))

(test descending-pagination-observes-order-limit-and-skip
  (let ((response
          (query-view-fixture
           "by_key"
           :start-key '("alpha" 3)
           :end-key '("alpha" 1)
           :descending t
           :skip 1
           :limit 2)))
    (is (equal '("alpha-2" "alpha-1") (row-ids response)))))

(test multi-key-post-returns-only-requested-rows
  (let ((response
          (query-view-fixture
           "by_key"
           :keys '(("alpha" 1) ("beta" 3)))))
    (is (equal '("alpha-1" "beta-3") (row-ids response)))))

(test include-docs-returns-the-observed-documents
  (let* ((response
           (query-view-fixture
            "by_key" :key '("beta" 2) :include-docs t))
         (row (first (fixture-rows response)))
         (document (jsown:val row "doc")))
    (is (string= "beta-2" (jsown:val document "_id")))
    (is (string= "beta" (jsown:val document "kind")))
    (is (= 2 (jsown:val document "rank")))))

(test update-false-and-lazy-return-observed-indexed-rows
  (query-view-fixture "by_key" :key '("alpha" 1) :update t)
  (let ((stale-ok
          (query-view-fixture
           "by_key" :key '("alpha" 1) :update nil))
        (lazy
          (query-view-fixture
           "by_key" :key '("beta" 1) :update "lazy")))
    (is (equal '("alpha-1") (row-ids stale-ok)))
    (is (equal '("beta-1") (row-ids lazy)))))

(test grouped-and-group-level-reductions-return-observed-counts
  (let ((grouped
          (fixture-rows
           (query-view-fixture "counts" :reduce t :group t)))
        (level
          (fixture-rows
           (query-view-fixture "counts" :reduce t :group-level 1))))
    (is (equal '(("alpha" "x" 2)
                 ("alpha" "y" 1)
                 ("beta" "x" 1)
                 ("beta" "y" 2))
               (mapcar (lambda (row)
                         (append (jsown:val row "key")
                                 (list (jsown:val row "value"))))
                       grouped)))
    (is (equal '(("alpha" 3) ("beta" 3))
               (mapcar (lambda (row)
                         (list (first (jsown:val row "key"))
                               (jsown:val row "value")))
                       level)))))

(test reduced-results-are-typed-and-cannot-be-documents
  (let ((result
          (star.databases.couchdb:execute-registered-view
           'issue-21-counts
           *view-integration-client*
           *view-integration-database*
           :reduce t
           :group-level 1
           :include-docs nil)))
    (is (typep result 'star.databases.couchdb:view-reduced-result))
    (is-false (typep result 'star.databases.couchdb:view-document-result))
    (dolist (row (star.databases.couchdb:view-reduced-result-rows result))
      (is-false (jsown:keyp row "doc")))))

(test real-couchdb-pool-replaces-client-after-session-loss
  (let ((connect-count 0)
        (pool
          (star.databases.couchdb::make-star-couchdb-pool
           :name "couchdb-session-integration-test"
           :max-open-count 1
           :max-idle-count 1
           :connector
           (lambda ()
             (incf connect-count)
             (cl-couch:new-couchdb star:*couchdb-host*
                                   star:*couchdb-port*
                                   :scheme star:*couchdb-scheme*)))))
    (let ((first (anypool:fetch pool)))
      (is (star.databases.couchdb::couchdb-client-session-valid-p first))
      (cl-couch:remove-auth first)
      (anypool:putback first pool)
      (let ((replacement (anypool:fetch pool)))
        (unwind-protect
             (progn
               (is (= 2 connect-count))
               (is-false (eq first replacement))
               (is
                (star.databases.couchdb::couchdb-client-session-valid-p
                 replacement)))
          (anypool:putback replacement pool))))))
