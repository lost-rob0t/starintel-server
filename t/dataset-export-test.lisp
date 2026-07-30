(in-package :star-server-tests)

(def-suite dataset-export-tests
  :description "Hermetic dataset export pagination and atomic replacement tests")

(in-suite dataset-export-tests)

(defun dataset-export-test-path ()
  (pathname
   (format nil "/tmp/starintel-export-~d-~d.jsonl"
           (get-universal-time)
           (random most-positive-fixnum))))

(defun delete-test-file (path)
  (when (probe-file path)
    (delete-file path)))

(defun make-export-document (dataset index)
  (jsown:new-js
    ("_id" (format nil "doc-~6,'0d" index))
    ("dataset" dataset)
    ("dtype" "person")))

(defun make-export-row (document)
  (jsown:new-js
    ("key" (list (jsown:val document "dataset")
                 (jsown:val document "_id")))
    ("doc" document)))

(defun make-export-query (documents &key fail-on-call)
  (let ((rows (mapcar #'make-export-row documents))
        (calls nil)
        (call-count 0))
    (values
     (lambda (client database design-document view-name
              &key limit start-key end-key skip include-docs reduce update
                &allow-other-keys)
       (declare (ignore client database end-key update))
       (incf call-count)
       (push (list :design-document design-document
                   :view-name view-name
                   :start-key start-key
                   :skip skip
                   :limit limit
                   :include-docs include-docs
                   :reduce reduce)
             calls)
       (when (and fail-on-call (= call-count fail-on-call))
         (error "Injected export failure on call ~d" call-count))
       (let* ((cursor-id (second start-key))
              (cursor-position
                (and cursor-id
                     (position cursor-id
                               rows
                               :test #'string=
                               :key (lambda (row)
                                      (second (jsown:val row "key"))))))
              (start (+ (or cursor-position 0) skip))
              (end (min (length rows) (+ start limit))))
         (jsown:new-js
           ("rows" (if (< start (length rows))
                       (subseq rows start end)
                       nil)))))
     (lambda ()
       (nreverse calls)))))

(defun read-export-lines (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect (jsown:parse line))))

(test dataset-export-boundary-sizes
  (dolist (size '(0 1 99 100 101 200 201))
    (let* ((dataset (format nil "dataset-~d" size))
           (documents (loop for index below size
                            collect (make-export-document dataset index)))
           (path (dataset-export-test-path)))
      (unwind-protect
           (multiple-value-bind (query-fn calls-fn)
               (make-export-query documents)
             (let* ((result
                      (star.databases.couchdb:export-by-dataset*
                       nil nil dataset path
                       :page-size 100
                       :query-fn query-fn))
                    (exported (read-export-lines path))
                    (calls (funcall calls-fn)))
               (is-true (getf result :ok))
               (is (= size (getf result :exported)))
               (is (= size (length exported)))
               (is (= (ceiling size 100) (getf result :pages)))
               (is (string= "documents_by_dataset"
                            (getf (first calls) :view-name)))
               (is (zerop (getf (first calls) :skip)))
               (is-true (getf (first calls) :include-docs))
               (is-false (getf (first calls) :reduce))
               (loop for document in exported
                     do (is (string= dataset
                                    (jsown:val document "dataset"))))))
        (delete-test-file path)))))

(test dataset-export-uses-key-cursor-after-first-page
  (let* ((dataset "cursor-test")
         (documents (loop for index below 101
                          collect (make-export-document dataset index)))
         (path (dataset-export-test-path)))
    (unwind-protect
         (multiple-value-bind (query-fn calls-fn)
             (make-export-query documents)
           (let ((result
                   (star.databases.couchdb:export-by-dataset*
                    nil nil dataset path
                    :page-size 100
                    :query-fn query-fn)))
             (is-true (getf result :ok))
             (let ((calls (funcall calls-fn)))
               (is (= 2 (length calls)))
               (is (= 1 (getf (second calls) :skip)))
               (is (equal (list dataset "doc-000099")
                          (getf (second calls) :start-key))))))
      (delete-test-file path))))

(test interrupted-export-preserves-valid-target
  (let* ((dataset "interrupted-test")
         (documents (loop for index below 150
                          collect (make-export-document dataset index)))
         (path (dataset-export-test-path))
         (original (format nil "previous-valid-export~%")))
    (unwind-protect
         (progn
           (with-open-file (stream path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string original stream))
           (multiple-value-bind (query-fn calls-fn)
               (make-export-query documents :fail-on-call 2)
             (declare (ignore calls-fn))
             (let ((result
                     (star.databases.couchdb:export-by-dataset*
                      nil nil dataset path
                      :page-size 100
                      :query-fn query-fn)))
               (is-false (getf result :ok))
               (is (= 100 (getf result :exported)))
               (is (string= original
                            (alexandria:read-file-into-string path))))))
      (delete-test-file path))))
