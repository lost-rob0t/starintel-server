(in-package :star.databases.couchdb)

(fmakunbound 'as-json)
(fmakunbound 'from-json)

(defun as-json (object &key (format-fn #'format-key))
  "Compatibility wrapper over the canonical StarIntel codec.

Returns a JSOWN object. Serialization remains the caller's responsibility."
  (let ((encoded (spec:encode object :format-fn format-fn)))
    (when (typep object 'spec:document)
      (star.documents:writable-schema-profile-for-document encoded))
    encoded))

(defun from-json (json-obj class-name &key (format-fn #'format-key))
  "Compatibility wrapper over the canonical typed decoder.

Document classes are checked against the registered dtype. Explicit helper
classes, such as actor-event, use the canonical standard-object decoder."
  (if (and (symbolp class-name)
           (ignore-errors
             (nth-value 0 (subtypep class-name 'spec:document))))
      (star.documents:decode-document json-obj :expected-class class-name)
      (spec:decode json-obj class-name :format-fn format-fn)))

(defun get-targets* (client database &rest actors)
  (let* ((response (query-view client database "targets" "by_actor"
                               :keys actors
                               :include-docs t
                               :reduce nil))
         (rows (jsown:val response "rows")))
    (loop for row in rows
          for document = (jsown:val row "doc")
          for actor = (star.documents:document-value document "actor")
          collect (cons actor document))))

(defun date-sort-key (document)
  (star.documents:document-date-added document))

(defun date-after-p (left right)
  (cond
    ((and (stringp left) (stringp right)) (string> left right))
    ((and (numberp left) (numberp right)) (> left right))
    ((stringp left) t)
    (t nil)))

(defun sort-docs-by-date (documents)
  "Sort StarIntel documents by date-added descending across supported schemas."
  (sort documents #'date-after-p :key #'date-sort-key))

(defun seconds-ago-iso (seconds)
  (local-time:format-timestring
   nil
   (local-time:unix-to-timestamp
    (- (local-time:timestamp-to-unix (local-time:now)) seconds))
   :format local-time:+iso-8601-format+))

(defun total-documents-since (client database seconds &key (include-docs nil) (reduce nil))
  "Count documents added during the previous SECONDS."
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
