(in-package :star.databases.couchdb)


(defparameter +lazy+ "lazy")



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

(defun camel-case-to-lisp-case (string)
  (with-output-to-string (s)
    (loop for char across string
          for i from 0
          do (cond
               ((and (not (zerop i))
                     (upper-case-p char))
                (write-char #\- s)
                (write-char (char-downcase char) s))
               (t (write-char (char-downcase char) s))))))

(defun from-json (json-obj class-name &key (format-fn #'format-key))
  (let* ((object (make-instance class-name))
         (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
          for slot-name = (sb-mop:slot-definition-name slot)
          for slot-type = (sb-mop:slot-definition-type slot)
          for key = (funcall format-fn (string slot-name))
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
  "Create or update all views from source/views/*.json files."
  (log:info "Initializing CouchDB views for database: ~a" database)
  (dolist (jdata star:*couchdb-views*)
    (let* ((doc (jsown:parse jdata))
           (doc-id (jsown:val doc "_id")))
      (log:info "Processing view: ~a" doc-id)
      (handler-case
          (let* ((existing-doc-json (cl-couch:get-document client database doc-id))
                 (existing-doc (jsown:parse existing-doc-json))
                 (existing-rev (jsown:val existing-doc "_rev")))
            (log:info "View ~a exists, updating with rev: ~a" doc-id existing-rev)
            (setf doc (jsown:extend-js doc ("_rev" existing-rev)))
            (let* ((resp (cl-couch:create-document client database (jsown:to-json doc)))
                   (rj (ignore-errors (jsown:parse resp))))
              (if rj
                  (log:info "View ~a updated ok=~a id=~a rev=~a"
                            doc-id (jsown:val rj "ok") (jsown:val rj "id") (jsown:val rj "rev"))
                  (log:info "View ~a updated (raw response)=~s" doc-id resp))))
        (dexador:http-request-not-found ()
          (log:info "View ~a does not exist, creating new" doc-id)
          (let* ((resp (cl-couch:create-document client database jdata))
                 (rj (ignore-errors (jsown:parse resp))))
            (if rj
                (log:info "View ~a created ok=~a id=~a rev=~a"
                          doc-id (jsown:val rj "ok") (jsown:val rj "id") (jsown:val rj "rev"))
                (log:info "View ~a created (raw response)=~s" doc-id resp))))
        (dexador:http-request-conflict (e)
          (log:error "Conflict updating view ~a: ~a" doc-id e)
          (signal e))))))


(defun init-db ()
  "Create the database if needed, and ensure all map-reduce views are up to date."
  (log:info "Starting database initialization sequence")
  (log:info "Initializing main database: ~a" *couchdb-default-database*)
  (log:debug "Database connection parameters: host=~a, port=~a, scheme=~a, user=~a" 
              star:*couchdb-host* star:*couchdb-port* star:*couchdb-scheme* star:*couchdb-user*)
  (log:info "Connecting to Couchdb via ~a://~a:~a" star:*couchdb-scheme* star:*couchdb-host* star:*couchdb-port*)
  
  (let ((database *couchdb-default-database*)
        (client (new-couchdb star:*couchdb-host* star:*couchdb-port* :scheme star:*couchdb-scheme*)))
    (log:debug "Created CouchDB client for main database")
    (log:debug "Authenticating with CouchDB")
    (password-auth client star:*couchdb-user* star:*couchdb-password*)
    (log:debug "Authentication successful")
    
    (handler-case 
        (progn
          (log:debug "Checking if main database ~a exists" database)
          (get-database client database)
          (log:info "Main database ~a already exists" database))
      (dexador:http-request-not-found (e)
        (log:warn "Main database ~a does not exist (404), creating new database" database)
        (log:debug "Creating database with parameters: ~a" (list :client client :database database))
        (cl-couch:create-database client database)
        (log:info "Main database ~a created successfully" database))
      (error (e)
        (log:error "Unexpected error checking/creating main database ~a: ~a" database e)
        (signal e)))
    
    (log:info "Validating registered CouchDB views")
    (validate-view-registry)
    (log:info "Initializing views for main database ~a" database)
    (log:debug "Processing ~a view definitions for main database" (length star:*couchdb-views*))
    (handler-case 
        (progn
          (init-views client database)
          (log:info "Main database ~a views initialized successfully" database))
      (error (e)
        (log:error "Failed to initialize views for main database ~a: ~a" database e)
        (signal e)))
    
    (log:info "Main database ~a initialization completed" database))
  
  ;; Initialize event source database
  (log:info "Proceeding to event source database initialization")
  (init-event-db)
  (log:info "All database initialization completed successfully"))

(defun init-event-db ()
  "Create the event source database if needed."
  (log:info "Initializing event source database: ~a" star:*couchdb-event-log-database*)
  (let ((event-database star:*couchdb-event-log-database*)
        (client (new-couchdb star:*couchdb-host* star:*couchdb-port* :scheme star:*couchdb-scheme*)))
    (password-auth client star:*couchdb-user* star:*couchdb-password*)
    (handler-case 
        (progn
          (get-database client event-database)
          (log:info "Event source database ~a already exists" event-database))
      (dexador:http-request-not-found (e)
        (log:info "Event source database ~a does not exist, creating" event-database)
        (cl-couch:create-database client event-database)
        (log:info "Event source database ~a created successfully" event-database)))
    (log:info "Event source database ~a initialization completed" event-database)))

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
                     ("update" (case update
                                 ("lazy" "lazy")
                                 (nil :false)
                                 (t :true)))
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
  "Sort documents newest-first across legacy numeric and current ISO dates."
  (labels ((date-value (document)
             (or (star.documents:document-date-added document) 0))
           (newer-p (left right)
             (cond
               ((and (numberp left) (numberp right)) (> left right))
               ((and (stringp left) (stringp right)) (string> left right))
               ((stringp left) t)
               (t nil))))
    (sort docs #'newer-p :key #'date-value)))

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

(defun users-by-name (client database &key (limit 50)
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
  "Query the by_name view in the users design document."
  (let* ((view-result (query-view client database "users" "by_name"
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

(defun users-by-url (client database &key (limit 50)
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
  "Query the by_url view in the users design document."
  (let* ((view-result (query-view client database "users" "by_url"
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

(defun users-by-platform-and-name (client database &key (limit 50)
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
  "Query the by_platform_and_name view in the users design document."
  (let* ((view-result (query-view client database "users" "by_platform_and_name"
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

(defun groups (client database &key (limit 50)
                                 (start-key nil)
                                 (end-key nil)
                                 (keys nil)
                                 (key nil)
                                 (update "lazy")
                                 (descending nil)
                                 (skip 0))
  "Query the by_platform view in the users design document."
  (let* ((view-result (query-view client database "messages" "groups"
                                  :limit limit
                                  :start-key start-key
                                  :end-key end-key
                                  :keys keys
                                  :key key
                                  :descending descending
                                  :update update
                                  :group t
                                  :reduce t
                                  :skip skip)))
    (jsown:val view-result "rows")))

;;; Hosts

(defun hosts-by-ip (client database &key (limit 50)
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
  "Query the by_ip view in the hosts design document."
  (let* ((view-result (query-view client database "hosts" "by_ip"
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

(defun hosts-by-hostname (client database &key (limit 50)
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
  "Query the by_hostname view in the hosts design document."
  (let* ((view-result (query-view client database "hosts" "by_hostname"
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

(defun hosts-by-port (client database &key (limit 50)
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
  "Query the by_port view in the hosts design document."
  (let* ((view-result (query-view client database "hosts" "by_port"
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

(defun hosts-by-service (client database &key (limit 50)
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
  "Query the by_service view in the hosts design document."
  (let* ((view-result (query-view client database "hosts" "by_service"
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

(defun hosts-by-os (client database &key (limit 50)
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
  "Query the by_os view in the hosts design document."
  (let* ((view-result (query-view client database "hosts" "by_os"
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

;;; Emails

(defun emails-by-email (client database &key (limit 50)
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
  "Query the by_email view in the emails design document."
  (let* ((view-result (query-view client database "emails" "by_email"
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

(defun emails-by-user (client database &key (limit 50)
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
  "Query the by_user view in the emails design document."
  (let* ((view-result (query-view client database "emails" "by_user"
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

(defun emails-by-domain (client database &key (limit 50)
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
  "Query the by_domain view in the emails design document."
  (let* ((view-result (query-view client database "emails" "by_domain"
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

(defun emails-with-password (client database &key (limit 50)
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
  "Query the with_password view in the emails design document."
  (let* ((view-result (query-view client database "emails" "with_password"
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

;;; Domains

(defun domains-by-record (client database &key (limit 50)
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
  "Query the by_record view in the domains design document."
  (let* ((view-result (query-view client database "domains" "by_record"
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

(defun domains-by-record-type (client database &key (limit 50)
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
  "Query the by_record_type view in the domains design document."
  (let* ((view-result (query-view client database "domains" "by_record_type"
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

(defun domains-by-record-and-type (client database &key (limit 50)
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
  "Query the by_record_and_type view in the domains design document."
  (let* ((view-result (query-view client database "domains" "by_record_and_type"
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

(defun domains-by-resolved-address (client database &key (limit 50)
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
  "Query the by_resolved_address view in the domains design document."
  (let* ((view-result (query-view client database "domains" "by_resolved_address"
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

;;; Networks

(defun networks-by-asn (client database &key (limit 50)
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
  "Query the by_asn view in the networks design document."
  (let* ((view-result (query-view client database "networks" "by_asn"
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

(defun networks-by-org (client database &key (limit 50)
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
  "Query the by_org view in the networks design document."
  (let* ((view-result (query-view client database "networks" "by_org"
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

(defun networks-by-subnet (client database &key (limit 50)
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
  "Query the by_subnet view in the networks design document."
  (let* ((view-result (query-view client database "networks" "by_subnet"
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

;;; URLs

(defun urls-by-url (client database &key (limit 50)
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
  "Query the by_url view in the urls design document."
  (let* ((view-result (query-view client database "urls" "by_url"
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

(defun urls-by-path (client database &key (limit 50)
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
  "Query the by_path view in the urls design document."
  (let* ((view-result (query-view client database "urls" "by_path"
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

(defun urls-by-domain (client database &key (limit 50)
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
  "Query the by_domain view in the urls design document."
  (let* ((view-result (query-view client database "urls" "by_domain"
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

;;; Breaches

(defun breaches-by-url (client database &key (limit 50)
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
  "Query the by_url view in the breaches design document."
  (let* ((view-result (query-view client database "breaches" "by_url"
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

(defun breaches-by-size (client database &key (limit 50)
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
  "Query the by_size view in the breaches design document."
  (let* ((view-result (query-view client database "breaches" "by_size"
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

;;; Email Messages

(defun email-messages-by-from (client database &key (limit 50)
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
  "Query the by_from view in the email-messages design document."
  (let* ((view-result (query-view client database "email-messages" "by_from"
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

(defun email-messages-by-to (client database &key (limit 50)
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
  "Query the by_to view in the email-messages design document."
  (let* ((view-result (query-view client database "email-messages" "by_to"
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

(defun email-messages-by-participant (client database &key (limit 50)
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
  "Query the by_participant view in the email-messages design document."
  (let* ((view-result (query-view client database "email-messages" "by_participant"
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
