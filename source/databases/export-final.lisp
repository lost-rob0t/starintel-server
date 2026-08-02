(in-package :star.databases.couchdb)

(defun export-by-dataset* (client database dataset path
                           &key
                             (page-size +dataset-export-page-size+)
                             (query-fn #'query-view))
  "Export DATASET as JSON Lines using bounded, key-based CouchDB pagination.

This final runtime definition intentionally supersedes the legacy skip-based
compatibility exporter still present in couchdb.lisp. The target is replaced
only after a complete, flushed export."
  (unless (and (integerp page-size) (plusp page-size))
    (error "PAGE-SIZE must be a positive integer, got ~s" page-size))
  (let* ((target-path (pathname path))
         (temporary-path (dataset-export-temp-path target-path))
         (total-exported 0)
         (pages-exported 0)
         (last-key nil)
         (completed nil))
    (ensure-directories-exist target-path)
    (unwind-protect
         (handler-case
             (progn
               (with-open-file (out temporary-path
                                    :direction :output
                                    :if-exists :error
                                    :if-does-not-exist :create
                                    :external-format :utf-8)
                 (loop
                   for result = (funcall query-fn
                                         client
                                         database
                                         "data"
                                         "documents_by_dataset"
                                         :start-key (or last-key (list dataset))
                                         :end-key (dataset-export-end-key dataset)
                                         :skip (if last-key 1 0)
                                         :limit page-size
                                         :include-docs t
                                         :reduce nil
                                         :update (zerop pages-exported))
                   for rows = (or (jsown:val-safe result "rows") nil)
                   do (when (null rows)
                        (return))
                      (incf pages-exported)
                      (loop for row in rows
                            for document = (jsown:val-safe row "doc")
                            for row-key = (jsown:val-safe row "key")
                            do (unless document
                                 (error "Dataset export row is missing doc: ~s" row))
                               (unless row-key
                                 (error "Dataset export row is missing key: ~s" row))
                               (unless (equal dataset
                                              (jsown:val-safe document "dataset"))
                                 (error "Dataset export row belongs to ~s, expected ~s"
                                        (jsown:val-safe document "dataset")
                                        dataset))
                               (write-string (jsown:to-json document) out)
                               (terpri out)
                               (incf total-exported)
                               (setf last-key row-key))
                      (when (< (length rows) page-size)
                        (return)))
                 (finish-output out))
               (uiop:rename-file-overwriting-target temporary-path target-path)
               (setf completed t)
               (list :ok t
                     :dataset dataset
                     :path (namestring target-path)
                     :exported total-exported
                     :pages pages-exported
                     :page-size page-size
                     :consistency +dataset-export-consistency+))
           (error (condition)
             (list :ok nil
                   :dataset dataset
                   :path (namestring target-path)
                   :exported total-exported
                   :pages pages-exported
                   :page-size page-size
                   :consistency +dataset-export-consistency+
                   :error-type (dataset-export-error-type condition)
                   :error (princ-to-string condition))))
      (unless completed
        (when (probe-file temporary-path)
          (ignore-errors (delete-file temporary-path)))))))
