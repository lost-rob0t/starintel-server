(in-package :star.actors)

(defun target-repository-sequence-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (error "Target repository rows must be a JSON array, got ~s" value))))

(defun query-persisted-target-documents
    (client database &key actors (query-fn #'star.databases.couchdb:query-view))
  "Query DATABASE through targets/by_actor and accept either JSOWN array form."
  (let* ((response
           (if actors
               (funcall query-fn client database "targets" "by_actor"
                        :keys actors :include-docs t :reduce nil)
               (funcall query-fn client database "targets" "by_actor"
                        :include-docs t :reduce nil)))
         (rows
           (target-repository-sequence-list
            (jsown:val-safe response "rows"))))
    (loop for row in rows
          for document = (jsown:val-safe row "doc")
          when document collect document)))
