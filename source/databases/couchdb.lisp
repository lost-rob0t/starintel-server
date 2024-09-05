(in-package :star.databases.couchdb)

(defparameter *couchdb-pool* (anypool:make-pool :name "couchdb-connections"
                                                :connector (lambda ()
                                                             (let ((client (cl-couch:new-couchdb (uiop:getenv "COUCHDB_HOST") 5984 :scheme (string-downcase (uiop:getenv "COUCHDB_SCHEME")))))
                                                               (cl-couch:password-auth client (uiop:getenv "COUCHDB_USER") (uiop:getenv "COUCHDB_PASSWORD"))
                                                               client))

                                                :disconnector (lambda (obj)
                                                                (setf (cl-couch:couchdb-headers obj) nil))
                                                :max-open-count 2))

(defun format-key (key)
  (if (str:starts-with? "_" key)
      (string-downcase key)
      (str:camel-case key)))


(defun as-json (object &key (format-fn #'format-key))
  (let ((json-obj (jsown:empty-object)))
    (loop for slot in (mapcar #'closer-mop:slot-definition-name
                              (closer-mop:class-slots (class-of object)))
          for value = (slot-value object slot)
          do (setf (jsown:val json-obj (funcall format-fn (string slot)))
                   (typecase value
                     (string value)
                     (integer value)
                     (list (jsown:to-json value))
                     (t (to-json value)))))
    json-obj))




(defun from-json (json-obj class-name)
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name slot)
          for slot-type = (sb-mop:slot-definition-type slot)
          for key = (string-downcase (string slot-name))
          for value = (jsown:val-safe json-obj key)
          when value
            do (setf (slot-value object slot-name)
                     (cond
                       ((eq slot-type 'list) value)
                       ((eq slot-type 'string) value)
                       ((eq slot-type 'integer) value)
                       (t (from-json value (eval slot-type))))))
    object))



(defun init-views (client database)
  (let ((files (uiop:directory-files (uiop:merge-pathnames* "views/" (asdf:system-source-directory :starintel-gserver)))))
    (loop for file in files
          for jdata = (with-open-file (str file)
                        (format nil "~a~%" (read-line str)))
          do (cl-couch:create-document client database jdata))))

(defun init-db ()
  "Create the database, and all map-reduce views with it."
  (let ((database *couchdb-default-database*))
    (anypool:with-connection (client *couchdb-pool*)
      (handler-case (cl-couch:get-database client database)
        (dexador:http-request-not-found (e) (progn
                                              (log:info "Creating database: ~a" database)
                                              (cl-couch:create-database client database)
                                              (init-views client database)))))))

;; TODO use query view
(defun get-targets* (client database &rest actors)
  (let ((jdata (jsown:val-safe (jsown:parse (cl-couch:get-view client star:*couchdb-default-database* "targets" "by_actor" (jsown:to-json (jsown:new-js
                                                                                                                                            ("keys" actors) ("include_docs" "true"))))) "rows")))
    (when (> 0 (length jdata))
      (loop for row in jdata
            for doc = (jsown:val row "doc")
            for actor = (jsown:val doc "actor")
            collect (cons actor doc)))))


(defun get-view-docs (jobj)
  "Gets the doc key from view results, either pass in a json containingthe rows or the view response."
  (loop for row in (or (jsown:val-safe jobj "rows") jobj) collect (jsown:val row "doc")))


;;; TODO make this a macro
;;; (define-view ddoc view-name)
;;; Would return a function like below but also has the sort-fn from the other functions calling this
;;;

(defun query-view (client database ddoc view-name &key (limit 50)
                                                    (start-key nil)
                                                    (end-key nil)
                                                    (keys nil)
                                                    (key nil)
                                                    (descending nil)
                                                    (group nil)
                                                    (group-level 0)
                                                    (include-docs nil)
                                                    (update t)
                                                    (skip 0)
                                                    (reduce nil))
  (let ((query-obj (jsown:new-js
                     ("limit" limit)
                     ("descending" (if descending :true :false))
                     ("include_docs" (if include-docs :true :false))
                     ("update" (if update :true :false))
                     ("skip" skip)
                     ("reduce" (if reduce :true :false)))))
    (cond
      ((and (or start-key end-key))
       (jsown:parse
        (couch:get-view client database ddoc view-name
                        (jsown:to-json (jsown:extend-js query-obj
                                         ("start_key" start-key)
                                         ("end_key" end-key)))
                        :group group :group-level group-level)))
      ((and (not start-key) (not end-key) (not keys) key)
       (jsown:parse
        (couch:get-view client database ddoc view-name
                        (jsown:to-json (jsown:extend-js query-obj
                                         ("key" key)))
                        :group group :group-level group-level)))
      ((and (not start-key) (not end-key) (not key) keys)
       (jsown:parse
        (couch:get-view client database ddoc view-name
                        (jsown:to-json (jsown:extend-js query-obj
                                         ("keys" keys)))
                        :group group :group-level group-level)))
      ((and (not start-key) (not end-key) (not key) (not keys))
       (jsown:parse
        (couch:get-view client database ddoc view-name
                        (jsown:to-json query-obj)
                        :group group :group-level group-level)))
      (t
       (error "Conflicting arguments were passed")))))


(defun map-view-results (fn client database ddoc view-name &key (limit 50)
                                                             (start-key nil)
                                                             (end-key nil)
                                                             (keys nil)
                                                             (key nil)
                                                             (descending :false)
                                                             (group nil)
                                                             (group-level 0)
                                                             (include-docs :false)
                                                             (update t)
                                                             (skip 0)
                                                             (reduce :false))
  (let* ((view-results (query-view client database ddoc view-name
                                   :limit limit
                                   :start-key start-key
                                   :end-key end-key
                                   :keys keys
                                   :key key
                                   :descending descending
                                   :group group
                                   :group-level group-level
                                   :include-docs include-docs
                                   :update update
                                   :skip skip
                                   :reduce reduce))
         (rows (jsown:val view-results "rows")))
    (mapcar (lambda (row)
              (let ((key (jsown:val row "key"))
                    (value (jsown:val row "value")))
                (if include-docs
                    (let ((doc (jsown:val row "doc")))
                      (funcall fn key value doc))
                    (funcall fn key value))))
            rows)))


(defun get-neighbors (client database ddoc view-name n &rest keys)
  (let ((graph nil)
        (current-keys keys))
    (loop repeat n
          do (let ((view-results (query-view client database ddoc view-name
                                             :reduce t
                                             :group-level 4
                                             :group :true
                                             :keys current-keys)))
               (loop for row in (jsown:val view-results "rows")
                     for key = (jsown:val row "key")
                     for value = (jsown:val row "value")
                     do (let ((entry (assoc key graph :test #'equal)))
                          (if entry
                              (setf (cdr entry) (append (cdr entry) (list value)))
                              (push (cons key (list value)) graph)))
                        (setf current-keys (append current-keys (list key))))))
    graph))

;; TODO use from-json to parse documents
(defun sort-docs-by-date (docs)
  "Sort a list of documents by the dateAdded field in descending order."
  (sort docs #'> :key (lambda (doc)
                        (jsown:val doc "dateAdded"))))

;;; Mesages View

(defun messages-by-user (client database &key (limit 50)
                                           (start-key nil)
                                           (end-key nil)
                                           (keys nil)
                                           (key nil)
                                           (descending nil)
                                           (include-docs t)
                                           (update t)
                                           (skip 0)
                                           (sort-fn #'sort-docs-by-date))
  "Query the messages_by_user view, optionally sorting results by a provided function if include-docs is true."
  (let* ((view-results (query-view client database "messages" "messages_by_user"
                                   :limit limit
                                   :start-key start-key
                                   :end-key end-key
                                   :keys keys
                                   :key key
                                   :descending descending
                                   :include-docs include-docs
                                   :update update
                                   :skip skip
                                   :reduce nil))
         (rows (jsown:val view-results "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun messages-by-platform (client database &key (limit 50)
                                               (start-key nil)
                                               (end-key nil)
                                               (keys nil)
                                               (key nil)
                                               (descending nil)
                                               (include-docs t)
                                               (update t)
                                               (skip 0)
                                               (sort-fn #'sort-docs-by-date))
  "Query the messages_by_platform view in the messages design document."
  (let* ((view-result (query-view client database "messages" "messages_by_platform"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun messages-by-group (client database &key (limit 50)
                                            (start-key nil)
                                            (end-key nil)
                                            (keys nil)
                                            (key nil)
                                            (descending nil)
                                            (include-docs t)
                                            (update t)
                                            (skip 0)
                                            (sort-fn #'sort-docs-by-date))
  "Query the messages_by_group view in the messages design document."
  (let* ((view-result (query-view client database "messages" "messages_by_group"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun social-posts-by-user (client database &key (limit 50)
                                               (start-key nil)
                                               (end-key nil)
                                               (keys nil)
                                               (key nil)
                                               (descending nil)
                                               (include-docs t)
                                               (update t)
                                               (skip 0)
                                               (sort-fn #'sort-docs-by-date))
  "Query the social_posts_by_user view in the messages design document."
  (let* ((view-result (query-view client database "messages" "social_posts_by_user"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun social-posts-by-group (client database &key (limit 50)
                                                (start-key nil)
                                                (end-key nil)
                                                (keys nil)
                                                (key nil)
                                                (descending nil)
                                                (include-docs t)
                                                (update t)
                                                (skip 0)
                                                (sort-fn #'sort-docs-by-date))
  "Query the social_posts_by_group view in the messages design document."
  (let* ((view-result (query-view client database "messages" "social_posts_by_group"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun by-channel (client database &key (limit 50)
                                     (start-key nil)
                                     (end-key nil)
                                     (keys nil)
                                     (key nil)
                                     (descending nil)
                                     (include-docs t)
                                     (update t)
                                     (skip 0)
                                     (reduce nil)
                                     (sort-fn #'sort-docs-by-date))
  "Query the by_channel view in the messages design document."
  (let* ((view-result (query-view client database "messages" "by_channel"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))



(defun export-by-dataset* (client database dataset path)
  "Export all documents from CouchDB by dataset and write them to a file."
  (let* (
         (total-rows (reduce #'+  (loop for row in (jsown:val (query-view client database "data" "dataset_size"
                                                                          :key dataset :reduce t :update nil) "rows")
                                        collect (jsown:val row "value"))))

         (num-pages (floor total-rows 100))
         (total-exported 0))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for page from 1 to num-pages
            do (let* ((skip (* page 100))
                      (result (query-view client database "data" "dataset_size"
                                          :key dataset :skip skip :limit 100 :include-docs t :update nil))
                      (rows (jsown:val result "rows")))
                 (loop for row in rows
                       do (write-string (jsown:to-json (jsown:val row "doc")) out)
                          (terpri out))
                 (incf total-exported (length rows)))))
    total-exported))

;;; dataset view
(defun count-by-dtype (client database &key (limit 50)
                                         (start-key nil)
                                         (end-key nil)
                                         (keys nil)
                                         (key nil)
                                         (descending nil)
                                         (include-docs t)
                                         (update t)
                                         (skip 0)
                                         (reduce nil)
                                         (sort-fn #'sort-docs-by-date))
  "Query the count_by_dtype view in the data design document."
  (let* ((view-result (query-view client database "data" "count_by_dtype"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun dataset-size (client database &key (limit 50)
                                       (start-key nil)
                                       (end-key nil)
                                       (keys nil)
                                       (key nil)
                                       (descending nil)
                                       (include-docs t)
                                       (update t)
                                       (skip 0)
                                       (reduce nil)
                                       (sort-fn #'sort-docs-by-date))
  "Query the dataset_size view in the data design document."
  (let* ((view-result (query-view client database "data" "dataset_size"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

;; timlines view

(defun total-documents-since (client database seconds &key (include-docs nil) (reduce nil))
  "Query the time/dateAdded view for documents added in the last n seconds."
  (let* ((current-time (local-time:timestamp-to-unix (local-time:now)))
         (start-time (- current-time seconds)))
    (length (jsown:val (query-view client database "time" "timeline"
                                   :start-key start-time
                                   :update t
                                   :group-level 10
                                   :include-docs include-docs)
                       "rows"))))

;; Orgs views

(defun orgs-by-country (client database &key (limit 50)
                                          (start-key nil)
                                          (end-key nil)
                                          (keys nil)
                                          (key nil)
                                          (descending nil)
                                          (include-docs t)
                                          (update t)
                                          (skip 0)
                                          (reduce nil)
                                          (sort-fn #'sort-docs-by-date))
  "Query the by_country view in the orgs design document."
  (let* ((view-result (query-view client database "orgs" "by_country"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun orgs-by-name (client database &key (limit 50)
                                       (start-key nil)
                                       (end-key nil)
                                       (keys nil)
                                       (key nil)
                                       (descending nil)
                                       (include-docs t)
                                       (update t)
                                       (skip 0)
                                       (reduce nil)
                                       (sort-fn #'sort-docs-by-date))
  "Query the by_name view in the orgs design document."
  (let* ((view-result (query-view client database "orgs" "by_name"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

;;; Persons view

(defun persons-by-name (client database &key (limit 50)
                                          (start-key nil)
                                          (end-key nil)
                                          (keys nil)
                                          (key nil)
                                          (descending nil)
                                          (include-docs t)
                                          (update t)
                                          (skip 0)
                                          (reduce nil)
                                          (sort-fn #'sort-docs-by-date))
  "Query the by_name view in the persons design document."
  (let* ((view-result (query-view client database "persons" "by_name"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun persons-by-region (client database &key (limit 50)
                                            (start-key nil)
                                            (end-key nil)
                                            (keys nil)
                                            (key nil)
                                            (descending nil)
                                            (include-docs t)
                                            (update t)
                                            (skip 0)
                                            (reduce nil)
                                            (sort-fn #'sort-docs-by-date))
  "Query the by_region view in the persons design document."
  (let* ((view-result (query-view client database "persons" "by_region"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

;; relations
(defun relations-edges (client database &key (limit 50)
                                          (start-key nil)
                                          (end-key nil)
                                          (keys nil)
                                          (key nil)
                                          (descending nil)
                                          (include-docs t)
                                          (update t)
                                          (skip 0)
                                          (reduce nil)
                                          (sort-fn #'sort-docs-by-date))
  "Query the edges view in the relations design document."
  (let* ((view-result (query-view client database "relations" "edges"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun relations-incoming-count (client database &key (limit 50)
                                                   (start-key nil)
                                                   (end-key nil)
                                                   (keys nil)
                                                   (key nil)
                                                   (descending nil)
                                                   (include-docs t)
                                                   (update t)
                                                   (skip 0)
                                                   (reduce nil)
                                                   (sort-fn #'sort-docs-by-date))
  "Query the incoming-count view in the relations design document."
  (let* ((view-result (query-view client database "relations" "incoming_count"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun relations-outgoing-count (client database &key (limit 50)
                                                   (start-key nil)
                                                   (end-key nil)
                                                   (keys nil)
                                                   (key nil)
                                                   (descending nil)
                                                   (include-docs t)
                                                   (update t)
                                                   (skip 0)
                                                   (reduce nil)
                                                   (sort-fn #'sort-docs-by-date))
  "Query the outgoing-count view in the relations design document."
  (let* ((view-result (query-view client database "relations" "outgoing_count"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

;;; targets

(defun targets-actor-counts (client database &key (limit 50)
                                               (start-key nil)
                                               (end-key nil)
                                               (keys nil)
                                               (key nil)
                                               (descending nil)
                                               (include-docs t)
                                               (update t)
                                               (skip 0)
                                               (reduce nil)
                                               (sort-fn #'sort-docs-by-date))
  "Query the actor-counts view in the targets design document."
  (let* ((view-result (query-view client database "targets" "actor_count"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun targets-by-actor (client database &key (limit 50)
                                           (start-key nil)
                                           (end-key nil)
                                           (keys nil)
                                           (key nil)
                                           (descending nil)
                                           (include-docs t)
                                           (update t)
                                           (skip 0)
                                           (reduce nil)
                                           (sort-fn #'sort-docs-by-date))
  "Query the actor-target view in the targets design document."
  (let* ((view-result (query-view client database "targets" "by_actor"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

(defun targets-target-count (client database &key (limit 50)
                                               (start-key nil)
                                               (end-key nil)
                                               (keys nil)
                                               (key nil)
                                               (descending nil)
                                               (include-docs t)
                                               (update t)
                                               (skip 0)
                                               (reduce nil)
                                               (sort-fn #'sort-docs-by-date))
  "Query the target-count view in the targets design document."
  (let* ((view-result (query-view client database "targets" "target_count"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))

;;; Users

(defun users-by-platform (client database &key (limit 50)
                                            (start-key nil)
                                            (end-key nil)
                                            (keys nil)
                                            (key nil)
                                            (descending nil)
                                            (include-docs t)
                                            (update t)
                                            (skip 0)
                                            (reduce nil)
                                            (sort-fn #'sort-docs-by-date))
  "Query the by_platform view in the users design document."
  (let* ((view-result (query-view client database "users" "by_platform"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :include-docs include-docs
                                  :update update
                                  :skip skip
                                  :reduce reduce
                                  :group (if reduce t nil)))
         (rows (jsown:val view-result "rows")))
    (if include-docs
        (funcall sort-fn (get-view-docs rows))
        rows)))
