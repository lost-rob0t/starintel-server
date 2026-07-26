(in-package :cl-user)

(defun view-registry-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun view-registry-response (&rest rows)
  (let ((response (jsown:empty-object)))
    (setf (jsown:val response "rows") (coerce rows 'vector))
    response))

(defun view-registry-document-row (id)
  (let ((row (jsown:empty-object))
        (document (jsown:empty-object)))
    (setf (jsown:val document "_id") id
          (jsown:val document "dtype") "message"
          (jsown:val document "date_added") "2026-07-26T00:00:00Z"
          (jsown:val row "id") id
          (jsown:val row "key") "key"
          (jsown:val row "value") id
          (jsown:val row "doc") document)
    row))

(defun view-registry-map-row (key value)
  (let ((row (jsown:empty-object)))
    (setf (jsown:val row "key") key
          (jsown:val row "value") value)
    row))

(defmacro with-mocked-query-view ((function) &body body)
  `(let ((original
           (symbol-function 'star.databases.couchdb:query-view)))
     (unwind-protect
          (progn
            (setf (symbol-function 'star.databases.couchdb:query-view)
                  ,function)
            ,@body)
       (setf (symbol-function 'star.databases.couchdb:query-view)
             original))))

(defun test-checked-in-design-documents-satisfy_registry ()
  (view-registry-check
   (star.databases.couchdb:validate-view-registry)
   "Checked-in view registry validation failed")
  (dolist (entry (star.databases.couchdb:view-registry-matrix))
    (view-registry-check (getf entry :wrapper)
                         "Registry entry lacks wrapper: ~s" entry)
    (view-registry-check (getf entry :design-document)
                         "Registry entry lacks design document: ~s" entry)
    (view-registry-check (getf entry :view)
                         "Registry entry lacks view: ~s" entry)))

(defun test-missing-expected-view_fails_validation ()
  (let* ((documents
           (star.databases.couchdb::checked-in-design-document-map))
         (messages
           (star.databases.couchdb::clone-outbox-json
            (gethash "messages" documents))))
    (setf (jsown:val messages "views") (jsown:empty-object)
          (gethash "messages" documents) messages)
    (handler-case
        (progn
          (star.databases.couchdb:validate-view-registry documents)
          (error "Missing view passed registry validation"))
      (star.databases.couchdb:view-registry-error () t))))

(defun test_typed_document_map_and_reduced_results ()
  (let ((calls nil))
    (with-mocked-query-view
        ((lambda (client database ddoc view &rest arguments)
           (push (list client database ddoc view arguments) calls)
           (cond
             ((getf arguments :reduce)
              (view-registry-response
               (view-registry-map-row "key" 3)))
             ((getf arguments :include-docs)
              (view-registry-response
               (view-registry-document-row "message:1")))
             (t
              (view-registry-response
               (view-registry-map-row "key" "message:1"))))))
      (let ((documents
              (star.databases.couchdb:execute-registered-view
               'star.databases.couchdb:messages-by-user
               :client "db"
               :key "user:1"
               :reduce nil
               :include-docs t
               :sort-fn nil))
            (map-result
              (star.databases.couchdb:execute-registered-view
               'star.databases.couchdb:documents-by-dataset
               :client "db"
               :key "default"
               :reduce nil
               :include-docs nil))
            (reduced
              (star.databases.couchdb:execute-registered-view
               'star.databases.couchdb:count-by-dtype
               :client "db"
               :reduce t
               :include-docs nil
               :group t)))
        (view-registry-check
         (typep documents 'star.databases.couchdb:view-document-result)
         "Document request returned ~s" (type-of documents))
        (view-registry-check
         (typep map-result 'star.databases.couchdb:view-map-result)
         "Map request returned ~s" (type-of map-result))
        (view-registry-check
         (typep reduced 'star.databases.couchdb:view-reduced-result)
         "Reduced request returned ~s" (type-of reduced))
        (view-registry-check
         (= 1
            (length
             (star.databases.couchdb:view-document-result-documents
              documents)))
         "Document result did not retain docs")
        (view-registry-check (= 3 (length calls))
                             "Expected three queries, got ~d"
                             (length calls))))))

(defun test_by_channel_accepts_map_and_reduced_modes ()
  (with-mocked-query-view
      ((lambda (client database ddoc view &rest arguments)
         (declare (ignore client database))
         (view-registry-check (string= "messages" ddoc)
                              "by-channel used design ~a" ddoc)
         (view-registry-check (string= "by_channel" view)
                              "by-channel used view ~a" view)
         (if (getf arguments :reduce)
             (view-registry-response
              (view-registry-map-row '("group" "channel") 2))
             (view-registry-response
              (view-registry-document-row "message:channel")))))
    (let ((map-result
            (star.databases.couchdb:execute-registered-view
             'star.databases.couchdb:by-channel
             :client "db"
             :key '("group" "channel")
             :reduce nil
             :include-docs t
             :group nil
             :sort-fn nil))
          (reduced-result
            (star.databases.couchdb:execute-registered-view
             'star.databases.couchdb:by-channel
             :client "db"
             :key '("group" "channel")
             :reduce t
             :include-docs nil
             :group t)))
      (view-registry-check
       (typep map-result 'star.databases.couchdb:view-document-result)
       "by-channel map mode returned ~s" (type-of map-result))
      (view-registry-check
       (typep reduced-result 'star.databases.couchdb:view-reduced-result)
       "by-channel reduced mode returned ~s"
       (type-of reduced-result)))))

(defun test_impossible_result_shapes_fail_before_query ()
  (let ((queries 0))
    (with-mocked-query-view
        ((lambda (&rest arguments)
           (declare (ignore arguments))
           (incf queries)
           (view-registry-response)))
      (dolist (arguments
               (list
                (list
                 'star.databases.couchdb:documents-by-dataset
                 :reduce t :include-docs nil)
                (list
                 'star.databases.couchdb:messages-by-user
                 :reduce t :include-docs t)
                (list
                 'star.databases.couchdb:messages-by-user
                 :reduce nil :include-docs t :group t)))
        (handler-case
            (progn
              (apply #'star.databases.couchdb:execute-registered-view
                     (first arguments) :client "db" (rest arguments))
              (error "Invalid view shape was accepted: ~s" arguments))
          (star.databases.couchdb:view-registry-error () t)))
      (view-registry-check (zerop queries)
                           "Invalid requests reached CouchDB ~d time(s)"
                           queries))))

(defun test_compatibility_wrappers_preserve_list_results ()
  (with-mocked-query-view
      ((lambda (&rest arguments)
         (declare (ignore arguments))
         (view-registry-response
          (view-registry-document-row "message:compat"))))
    (let ((documents
            (star.databases.couchdb:messages-by-user
             :client "db"
             :key "user:1"
             :include-docs t
             :reduce nil
             :sort-fn nil)))
      (view-registry-check (listp documents)
                           "Compatibility wrapper returned ~s"
                           (type-of documents))
      (view-registry-check (= 1 (length documents))
                           "Compatibility wrapper lost documents"))))

(defun test_social_post_views_use_canonical_dtype ()
  (let* ((documents
           (star.databases.couchdb::checked-in-design-document-map))
         (messages (gethash "messages" documents))
         (views (jsown:val messages "views")))
    (dolist (view-name
             '("social_posts_by_user"
               "social_posts_by_group"
               "social_posts_by_platform"))
      (let ((map-source
              (jsown:val (jsown:val views view-name) "map")))
        (view-registry-check
         (search "social-media-post" map-source)
         "~a does not use canonical social-media-post dtype"
         view-name)
        (view-registry-check
         (not (search "social-media-posts" map-source))
         "~a retains plural dtype drift" view-name)))))

(defun run-view-registry-conformance-tests ()
  (format t "~&Running CouchDB view registry tests~%")
  (test-checked-in-design-documents-satisfy_registry)
  (test-missing-expected-view_fails_validation)
  (test_typed_document_map_and_reduced_results)
  (test_by_channel_accepts_map_and_reduced_modes)
  (test_impossible_result_shapes_fail_before_query)
  (test_compatibility_wrappers_preserve_list_results)
  (test_social_post_views_use_canonical_dtype)
  (format t "~&CouchDB view registry tests passed~%")
  t)

(run-view-registry-conformance-tests)
