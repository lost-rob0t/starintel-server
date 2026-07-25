(in-package :star.databases.couchdb)

(defun as-json (object &key (format-fn #'format-key))
  (declare (ignore format-fn))
  (if (typep object 'spec:document)
      (spec:encode object)
      (error "AS-JSON only accepts canonical StarIntel document objects in v0.9 mode")))

(defun get-targets* (client database &rest actors)
  (let* ((response (query-view client database "targets" "by_actor"
                               :keys actors
                               :include-docs t
                               :reduce nil))
         (rows (jsown:val response "rows")))
    (loop for row in rows
          for document = (jsown:val row "doc")
          for actor = (star.documents.v09:document-value document "actor")
          collect (cons actor document))))

(defun date-sort-key (document)
  (star.documents.v09:document-date-added document))

(defun date-after-p (left right)
  (cond
    ((and (stringp left) (stringp right)) (string> left right))
    ((and (numberp left) (numberp right)) (> left right))
    ((stringp left) t)
    (t nil)))

(defun sort-docs-by-date (documents)
  "Sort canonical v0.9 documents by ISO-8601 date_added descending."
  (sort documents #'date-after-p :key #'date-sort-key))

(defun seconds-ago-iso (seconds)
  (local-time:format-timestring
   nil
   (local-time:unix-to-timestamp
    (- (local-time:timestamp-to-unix (local-time:now)) seconds))
   :format local-time:+iso-8601-format+))

(defun total-documents-since (client database seconds &key (include-docs nil) (reduce nil))
  "Count canonical v0.9 documents added during the previous SECONDS."
  (declare (ignore reduce))
  (length
   (jsown:val
    (query-view client database "time" "timeline"
                :start-key (seconds-ago-iso seconds)
                :end-key (local-time:format-timestring nil (local-time:now)
                                                       :format local-time:+iso-8601-format+)
                :update t
                :include-docs include-docs
                :reduce nil)
    "rows")))
