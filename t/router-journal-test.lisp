(defpackage :star.router.journal.tests (:use :cl :fiveam))
(in-package :star.router.journal.tests)
(def-suite :router-journal)
(in-suite :router-journal)

(defun fixture-store (&key conflict)
  (let ((rows (make-hash-table :test #'equal)) (revision 0))
    (star.router.journal:make-journal-store
     :load-fn (lambda (key) (gethash key rows))
     :save-fn (lambda (row)
                (when conflict (setf conflict nil)
                  (error 'star.router.journal:journal-store-conflict))
                (setf (jsown:val row "_rev") (write-to-string (incf revision))
                      (gethash (jsown:val row "_id") rows) row)
                row))))

(test namespace-is-principal-tenant-and-direction-scoped
  (let ((key (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k"))))
    (dolist (args '(("q" "t" "in") ("p" "u" "in") ("p" "t" "out")))
      (is (not (equal key (apply #'star.router.journal:journal-key
                                (append args (list '("a" "m" "k"))))))))))

(test cas-retry-readback-and-snapshot-isolation
  (let* ((store (fixture-store :conflict t))
         (key (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k")))
         (calls 0))
    (multiple-value-bind (value saved)
        (star.router.journal:journal-transition
         store key (lambda (old) (is (null old)) (incf calls)
                     (values (jsown:new-js ("state" "pending")) :admitted)))
      (is (eq :admitted value)) (is (= 2 calls))
      (setf (jsown:val saved "state") "corrupted"))
    (is (equal "pending" (jsown:val (star.router.journal:journal-read store key) "state")))
    (multiple-value-bind (value saved)
        (star.router.journal:journal-transition store key
          (lambda (old) (declare (ignore old)) (values nil :duplicate)))
      (is (eq :duplicate value)) (is (equal "1" (jsown:val saved "_rev"))))))

(test corrupt-store-and-exhausted-cas-fail-closed
  (let* ((key (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k")))
         (store (star.router.journal:make-journal-store
                 :load-fn (lambda (id) (declare (ignore id)) (jsown:new-js ("type" "wrong")))
                 :save-fn #'identity)))
    (signals error (star.router.journal:journal-read store key)))
  (let ((store (star.router.journal:make-journal-store
                :max-attempts 2 :load-fn (constantly nil)
                :save-fn (lambda (row) (declare (ignore row))
                           (error 'star.router.journal:journal-store-conflict)))))
    (signals star.router.journal:journal-store-conflict
      (star.router.journal:journal-transition store
        (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k"))
        (lambda (old) (declare (ignore old)) (values (jsown:empty-object) :admitted))))))

(test journal-records-denied-before-public-effects
  (let* ((key (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k")))
         (row (jsown:new-js ("_id" key) ("type" "_server_router_execution")))
         (admin (star.auth::%make-service-call-context
                 :principal-id "admin-fixture" :principal-type "service"
                 :credential-id "fixture" :scopes '("admin")))
         (effects 0))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-fetch-document key
        (lambda (id) (declare (ignore id)) (incf effects) row) :principal admin))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-publish-document row
        (lambda (doc) (declare (ignore doc)) (incf effects)) :principal admin))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-delete-document key
        (lambda (id) (declare (ignore id)) (incf effects) row)
        (lambda (&rest args) (declare (ignore args)) (incf effects)) :principal admin))
    (signals star.authorization:authorization-error
      (star.authorization:authorized-update-document "ordinary" row
        (lambda (id) (declare (ignore id)) (incf effects)) #'identity :principal admin))
    (is (= 0 effects))
    (let* ((response (jsown:new-js ("rows" (list (jsown:new-js ("id" key) ("doc" row))))))
           (filtered (star.frontends.http-api::strip-server-tenant-from-rows response)))
      (is (null (jsown:val filtered "rows"))))))

(test search-vector-removes-private-projections-retains-ordinary-rows
  (let* ((ordinary (jsown:new-js ("id" "public-fixture")
                                ("fields" (jsown:new-js ("content" "synthetic")))))
         (response (jsown:new-js
                    ("rows" (vector
                             (jsown:new-js ("fields" (jsown:new-js ("type" "_server_router_execution"))))
                             (jsown:new-js ("doc" (jsown:new-js ("_id" "router-execution:any"))))
                             ordinary)))))
    (star.frontends.http-api::strip-server-tenant-from-rows response)
    (let ((rows (jsown:val response "rows")))
      (is (vectorp rows)) (is (= 1 (length rows)))
      (is (equal "public-fixture" (jsown:val (aref rows 0) "id"))))))

(test ambiguous-save-never-retried-or-reported-admitted
  (let* ((calls 0)
         (store (star.router.journal:make-journal-store
                 :load-fn (constantly nil)
                 :save-fn (lambda (row) (declare (ignore row))
                            (incf calls) (error "connection lost after possible commit")))))
    (signals error
      (star.router.journal:journal-transition store
        (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k"))
        (lambda (old) (declare (ignore old)) (values (jsown:empty-object) :admitted))))
    (is (= 1 calls))))

(test oversized-and-deletion-states-rejected-before-save
  (let* ((calls 0)
         (store (star.router.journal:make-journal-store
                 :max-bytes 1024 :load-fn (constantly nil)
                 :save-fn (lambda (row) (incf calls) row)))
         (key (star.router.journal:journal-key "p" "t" "in" '("a" "m" "k"))))
    (dolist (row (list (jsown:new-js ("_deleted" :true))
                       (jsown:new-js ("command" (make-string 1025 :initial-element #\x)))))
      (signals error (star.router.journal:journal-transition store key
                       (lambda (old) (declare (ignore old)) (values row :admitted)))))
    (is (= 0 calls))))
