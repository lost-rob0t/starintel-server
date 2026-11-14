(in-package :star.actors)

(defparameter *target-recovery-view-page-limit* 200
  "Rows fetched per by_actor page while draining persisted targets (#360).")

(defun target-repository-sequence-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (error "Target repository rows must be a JSON array, got ~s" value))))

(defun view-rows-with-documents (response)
  (loop for row in (target-repository-sequence-list
                    (jsown:val-safe response "rows"))
        for document = (jsown:val-safe row "doc")
        when document
          collect (list (jsown:val-safe row "key")
                        (jsown:val-safe row "id")
                        document)))

(defun query-persisted-target-documents
    (client database &key actors (query-fn #'star.databases.couchdb:query-view)
              (page-limit *target-recovery-view-page-limit*))
  "Drain every targets/by_actor page (#360).

Unfiltered recovery walks the whole key space with a (startkey,
startkey_docid) cursor — by_actor keys repeat once per actor, so the row id
must advance the cursor — starting after the null-actor rows, which sort
before every string key and would otherwise quarantine-storm boot. The
actor-filtered path pages with :keys + :skip. Both stop on the first short
page."
  (labels ((fetch (start-key start-key-doc-id)
             (funcall query-fn client database "targets" "by_actor"
                      :include-docs t :reduce nil
                      :limit page-limit
                      :start-key start-key
                      :start-key-doc-id start-key-doc-id))
           (fetch-actors (skip)
             (funcall query-fn client database "targets" "by_actor"
                      :include-docs t :reduce nil
                      :limit page-limit :skip skip :keys actors))
           (drain-range (collected count start-key start-key-doc-id)
             (let* ((rows (view-rows-with-documents (fetch start-key start-key-doc-id)))
                    (last-row (car (last rows))))
               (dolist (row rows)
                 (push (third row) collected))
               (let ((count (+ count (length rows))))
                 (if (and last-row (= (length rows) page-limit))
                     (drain-range collected count
                                  (first last-row) (second last-row))
                     (nreverse collected))))))
    (if actors
        (loop with collected
              for skip = 0 then (+ skip page-limit)
              for rows = (view-rows-with-documents (fetch-actors skip))
              do (dolist (row rows)
                   (push (third row) collected))
              while (= (length rows) page-limit)
              finally (return (nreverse collected)))
        (drain-range nil 0 "" nil))))
