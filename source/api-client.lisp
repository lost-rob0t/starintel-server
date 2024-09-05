(uiop:define-package   :starintel-gserver-client
  (:nicknames :star.api.client)
  (:use       :cl)
  (:documentation "doc")
  (:export
   #:star-client
   #:make-url
   #:api-request
   #:get-targets
   #:new-target
   #:read-targets-csv
   #:import-targets-from-csv
   #:submit-document
   #:get-document))

(in-package :star.api.client)
(defclass star-client ()
  ((base-url :initform "http://127.0.0.1:5000" :initarg :base-url :accessor base-url)
   (headers :initform '(("Accept" . "application/json")
                        ("Content-Type" . "application/json")) :initarg :headers :accessor star-client-headers))
  (:documentation "The Starintel gserver api client"))

(defun make-url (client api-url &key (query nil))
  (if query
      (quri:merge-uris (quri:make-uri :path api-url :query (quri:url-encode-params query)) (base-url client))
      (quri:merge-uris (quri:make-uri :path api-url) (base-url client))))


(defmacro api-request (client path &key
                                     (stream nil)
                                     (query nil)
                                     (content nil)
                                     (method :get)
                                     (force-binary nil)
                                     (keep-alive t))


  `(let ((resp
           (dexador:request (make-url ,client ,path :query ,query)
                            :method ,method :headers (star-client-headers ,client) :content ,content :want-stream ,stream :keep-alive ,keep-alive :force-binary ,force-binary)))



     resp))

(defmethod get-targets ((client star-client) actor-name)
  "Get all targets for ACTOR-NAME."
  (api-request client (format nil "/targets/~a" actor-name)))

(defmethod new-target ((client star-client) target-doc actor &optional (transient nil))
  "Insert new target."
  (if transient
      (api-request client (format nil "/new/target/~a" actor) :method :post
                                                              :content (jsown:to-json (jsown:extend-js (jsown:parse target-doc)
                                                                                        ("transient" t))))
      (api-request client (format nil "/new/target/~a" actor) :method :post
                                                              :content target-doc)))

(defun read-targets-csv (targets-file)
  (let ((targets (data-table:select-columns  (cl-csv:get-data-table-from-csv targets-file) (list "dataset" "target" "actor" "recurring" "delay" "options"))))
    (loop for row in (data-table:rows targets)
          collect (cons (nth 2 row) (with-output-to-string (str)
                                      (cl-json:encode-json
                                       (spec:new-target (nth 0 row)
                                                        (nth 1 row)
                                                        (nth 2 row)
                                                        :options (nth 5 row)
                                                        :recurring (cond ((string= (string-downcase (nth 3 row)) "true") t)
                                                                         ((string= (string-downcase (nth 3 row)) "t") t)
                                                                         ((string= (nth 3 row) "1") t)
                                                                         ((string= (nth 3 row) "0") nil)
                                                                         ((string= (string-downcase (nth 3 row)) "nil") nil)
                                                                         ((string= (string-downcase (nth 3 row)) "false") nil))
                                                        :delay (nth 4 row)) str))))))

(defmethod import-targets-from-csv ((client star-client) file)
  (loop for target in (read-targets-csv file)
        do (new-target client (cdr target) (car target))))

(defmethod submit-document ((client star-client) document document-type)
  "Create a new document"
  (api-request client (format nil "/new/document/~a" document-type) :method :post :content document))

(defmethod get-document ((client star-client) document-id)
  "Get the document by id."
  (api-request client (format nil "/documents/~a" document-id)))

(defmethod fts ((client star-client) &key q (limit 25) (bookmark nil) (sort nil))
  "Search documents using the full-text search (FTS) endpoint."
  (let ((query (list (cons "q" q)
                     (cons "limit" (prin1-to-string limit))
                     (cons "include_docs" "true"))))
    (when bookmark
      (push (cons "bookmark" bookmark) query))
    (when sort
      (push (cons "sort" sort) query))
    (api-request client "/search" :query query)))



(defmethod messages-by-user ((client star-client) &key user (limit 50) start-key end-key (descending nil) (skip 0))
  "Retrieve messages by user using the 'messages_by_user' view."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when user
      (push (cons "user" user) query))
    (when start-key
      (push (cons "start_key" (cl-json:encode-json-to-string start-key)) query))
    (when end-key
      (push (cons "end_key" (cl-json:encode-json-to-string end-key)) query))
    (api-request client "/documents/messages/by-user" :query query)))

(defmethod messages-by-channel ((client star-client) group channel &key (limit 50) start-key end-key (descending nil) (skip 0) (reduce nil))
  "Retrieve messages by group and channel using the 'by_channel' view."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip))
                     (cons "reduce" (if reduce "true" "false")))))
    (when start-key
      (push (cons "start_key" (cl-json:encode-json-to-string start-key)) query))
    (when end-key
      (push (cons "end_key" (cl-json:encode-json-to-string end-key)) query))
    (api-request client (format nil "/documents/messages/~a/~a" group channel) :query query)))

(defmethod messages-by-platform ((client star-client) &key platform (limit 50) start-key end-key (descending nil) (skip 0))
  "Retrieve messages by platform using the 'messages_by_platform' view."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when platform
      (push (cons "platform" platform) query))
    (when start-key
      (push (cons "start_key" (cl-json:encode-json-to-string start-key)) query))
    (when end-key
      (push (cons "end_key" (cl-json:encode-json-to-string end-key)) query))
    (api-request client "/documents/messages/by-platform" :query query)))

(defmethod messages-by-group ((client star-client) &key  (limit 50) start-key end-key (include-docs nil) (reduce nil) (descending nil) (skip 0))
  "Retrieve messages by group using the 'messages_by_group' view."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "include-docs" (if include-docs "true" "false"))
                     (cons "reduce" (if reduce "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when reduce
      (push (cons "reduce" reduce) query))

    (when (and include-docs (not reduce))
      (push (cons "include_docs" include-docs) query))
    (when start-key
      (push (cons "start_key" (cl-json:encode-json-to-string start-key)) query))
    (when end-key
      (push (cons "end_key" (cl-json:encode-json-to-string end-key)) query))
    (api-request client "/documents/messages/by-group" :query query)))

(defmethod social-posts-by-user ((client star-client) &key user (limit 50) start-key end-key (descending nil) (skip 0))
  "Retrieve social media posts by user using the 'social_posts_by_user' view."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when user
      (push (cons "user" user) query))
    (when start-key
      (push (cons "start_key" (cl-json:encode-json-to-string start-key)) query))
    (when end-key
      (push (cons "end_key" (cl-json:encode-json-to-string end-key)) query))
    (api-request client "/documents/socialmpost/by-user" :query query)))

(defmethod dataset-size ((client star-client) dataset)
  "Retrieve the size of a dataset using the 'dataset_size' view."
  (let ((query (list (cons "dataset" dataset))))
    (api-request client "/dataset-size" :query query)))

(defun do-view (client view-fn &key (batch-size 500) (reduce nil) (include-docs nil))
  "Iterate over all keys in a view and apply the provided function to each batch of results."
  (let ((start-key nil)
        (end-key nil)
        (results '())
        (batch (funcall view-fn client
                        :limit batch-size
                        :include-docs (when (and (not reduce) include-docs) t)
                        :reduce reduce)))
    (let* ()


      batch)))
