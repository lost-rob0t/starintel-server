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
  (let* ((connect-count 0)
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
                 replacement))
               (is
                (cl-couch:database-exists-p
                 replacement *view-integration-database*)))
          (anypool:putback replacement pool))))))

(test auth-couchdb-pool-replaces-client-through-shared-session-policy
  (let ((pool (star.auth::make-auth-couchdb-pool)))
    (let ((first (anypool:fetch pool)))
      (is (star.databases.couchdb::couchdb-client-session-valid-p first))
      (cl-couch:remove-auth first)
      (anypool:putback first pool)
      (let ((replacement (anypool:fetch pool)))
        (unwind-protect
             (progn
               (is-false (eq first replacement))
               (is
                (star.databases.couchdb::couchdb-client-session-valid-p
                 replacement))
               (is
                (cl-couch:database-exists-p
                 replacement *view-integration-database*)))
          (anypool:putback replacement pool))))))

(test real-couchdb-outbox-recovery-drains-pages-after-crash
  ;; The actual _design/outbox map emits one row per pending entry.  Bind a
  ;; small page size to exercise multi-page draining against real CouchDB.
  (let* ((client *view-integration-client*)
         (database *view-integration-database*)
         (ids '("outbox-page-1" "outbox-page-2" "outbox-page-3"
                "outbox-page-4" "outbox-page-5"))
         (published '())
         (fail-once t)
         (late-id "outbox-page-late")
         (injected nil))
    (cl-couch:create-document
     client database
     (uiop:read-file-string
      (asdf:system-relative-pathname
       :starintel-gserver "views/outbox.json")))
    (dolist (id ids)
      (cl-couch:create-document
       client database
       (jsown:to-json (make-outbox-recovery-test-document id))))
    (let ((star.databases.couchdb::*outbox-recovery-view-limit* 2))
      (is (= 2 (length (star.databases.couchdb::couchdb-pending-outbox-documents
                        client database))))
      (signals error
        (star.databases.couchdb:recover-couchdb-outbox
         client database
         (lambda (routing-key payload event-id)
           (declare (ignore routing-key payload))
           (push event-id published)
           (when fail-once
             (setf fail-once nil)
             (error "simulated process exit after publisher accepted event")))))
      (is (= 1 (length published)))
      (is-true
       (star.databases.couchdb:recover-couchdb-outbox
        client database
        (lambda (routing-key payload event-id)
          (declare (ignore routing-key payload))
          (push event-id published)
          ;; A concurrent writer adds work AFTER recovery began and AFTER the
          ;; bounded view returned its first batch.  Later polls must see it.
          (unless injected
            (setf injected t)
            (cl-couch:create-document
             client database
             (jsown:to-json (make-outbox-recovery-test-document late-id)))))))
      (is-true injected)
      ;; First pending event is replayed after crash; five other events publish
      ;; once, including the late arrival inserted mid-recovery.
      (let ((chronological (reverse published)))
        (is (= 7 (length chronological)))
        (is (string= (first chronological) (second chronological))))
      (is (= 6 (length (remove-duplicates published :test #'string=))))
      (is (null (star.databases.couchdb::couchdb-pending-outbox-documents
                 client database)))
      (dolist (id (append ids (list late-id)))
        (let* ((document
                 (star.databases.couchdb::couchdb-load-outbox-document
                  client database id))
               (entry
                 (first (star.databases.couchdb:document-outbox-entries document))))
          (is-true (star.databases.couchdb:outbox-entry-published-p entry)))))))
(test real-couchdb-publication-marker-cas-retries-current-revision
  ;; The *CouchDB server* produces the 409, not a test stub.  The first marker
  ;; attempt uses a stale _rev after a concurrent write, so recovery must
  ;; reload that revision and mark without publishing the event again.
  (let* ((client *view-integration-client*)
         (database *view-integration-database*)
         (id "outbox-cas-retry-1")
         (published '())
         (save-attempts 0)
         (injected nil))
    (cl-couch:create-document
     client database
     (jsown:to-json (make-outbox-recovery-test-document id)))
    (let ((initial
            (star.databases.couchdb::couchdb-load-outbox-document
             client database id)))
      (is-true
       (star.databases.couchdb:recover-outbox-documents
        (lambda (document-id)
          (star.databases.couchdb::couchdb-load-outbox-document
           client database document-id))
        (lambda (updated)
          (incf save-attempts)
          (unless injected
            (setf injected t)
            (let ((concurrent
                    (star.databases.couchdb::couchdb-load-outbox-document
                     client database id)))
              (setf (jsown:val concurrent "test_concurrent_revision")
                    "must-survive")
              (star.databases.couchdb::couchdb-save-outbox-document
               client database concurrent)))
          (star.databases.couchdb::couchdb-save-outbox-document
           client database updated))
        (lambda (routing-key payload event-id)
          (declare (ignore routing-key payload))
          (push event-id published))
        (list initial))))
    (is-true injected)
    (is (= 2 save-attempts))
    (is (= 1 (length published)))
    (let* ((stored
             (star.databases.couchdb::couchdb-load-outbox-document
              client database id))
           (entry
             (first (star.databases.couchdb:document-outbox-entries stored))))
      (is (string= "must-survive"
                   (jsown:val stored "test_concurrent_revision")))
      (is-true (star.databases.couchdb:outbox-entry-published-p entry))
      (is (string= (first published) (jsown:val entry "event_id"))))))
