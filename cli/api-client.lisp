(in-package :star.api.client)

;;; Legacy document/query convenience API.
;;;
;;; These functions preserve the existing public surface while routing raw HTTP
;;; through the new StarIntel-owned transport/error boundary. New stable
;;; control-plane operations live in CLIENT-CONVENIENCE.LISP and are backed by
;;; the shared machine-readable contract.

(defmethod get-targets ((client star-client) actor-name)
  "Get all targets for ACTOR-NAME."
  (api-request client (format nil "/targets/~a" actor-name)))

(defmethod new-target ((client star-client) target-doc actor &optional (transient nil))
  "Insert new target."
  (if transient
      (api-request client
                   (format nil "/new/target/~a" actor)
                   :method :post
                   :content (jsown:to-json
                             (jsown:extend-js
                              (jsown:parse target-doc)
                              ("transient" t))))
      (api-request client
                   (format nil "/new/target/~a" actor)
                   :method :post
                   :content target-doc)))

(defmethod submit-document ((client star-client) document document-type)
  "Create a new document."
  (api-request client
               (format nil "/new/document/~a" document-type)
               :method :post
               :content document))

(defmethod bulk-submit ((client star-client) documents)
  "Submit multiple documents in bulk. DOCUMENTS may be a JSON string or list."
  (let ((content (if (stringp documents)
                     documents
                     (jsown:to-json documents))))
    (api-request client "/documents/bulk" :method :post :content content)))

(defmethod get-document ((client star-client) document-id)
  "Get a document by ID."
  (api-request client (format nil "/document/~a" document-id)))

(defmethod fts ((client star-client) &key q (limit 25) bookmark sort)
  "Search documents using the full-text search endpoint."
  (let ((query (list (cons "q" q)
                     (cons "limit" (prin1-to-string limit))
                     (cons "include_docs" "true"))))
    (when bookmark
      (push (cons "bookmark" bookmark) query))
    (when sort
      (push (cons "sort" sort) query))
    (api-request client "/search" :query query)))

(defmethod messages-by-user ((client star-client)
                             &key user (limit 50) start-key end-key
                               (descending nil) (skip 0))
  "Retrieve messages by user."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when user
      (push (cons "user" user) query))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/messages/by-user" :query query)))

(defmethod messages-by-channel ((client star-client) group channel
                                &key (limit 50) start-key end-key
                                  (descending nil) (skip 0) (reduce nil))
  "Retrieve messages by group and channel."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip))
                     (cons "reduce" (if reduce "true" "false"))
                     (cons "channel" channel)
                     (cons "group" group))))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/messages/by-channel" :query query)))

(defmethod messages-by-platform ((client star-client)
                                 &key platform (limit 50) start-key end-key
                                   (descending nil) (skip 0))
  "Retrieve messages by platform."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when platform
      (push (cons "platform" platform) query))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/messages/by-platform" :query query)))

(defmethod messages-by-group ((client star-client)
                              &key (limit 50) start-key end-key
                                (include-docs nil) (reduce nil)
                                (descending nil) (skip 0))
  "Retrieve messages by group."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "include-docs" (if include-docs "true" "false"))
                     (cons "reduce" (if reduce "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/messages/by-group" :query query)))

(defmethod social-posts-by-user ((client star-client)
                                 &key user (limit 50) start-key end-key
                                   (descending nil) (skip 0))
  "Retrieve social media posts by user."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (when user
      (push (cons "user" user) query))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/socialmpost/by-user" :query query)))

(defmethod dataset-size ((client star-client) dataset)
  "Retrieve the size of a dataset."
  (api-request client "/dataset-size" :query (list (cons "dataset" dataset))))

(defmethod groups ((client star-client)
                   &key (limit 50) start-key end-key
                     (update :lazy) (descending nil) (skip 0))
  "Retrieve message groups and channels."
  (let ((query (list (cons "limit" (prin1-to-string limit))
                     (cons "descending" (if descending "true" "false"))
                     (cons "skip" (prin1-to-string skip)))))
    (case update
      (:lazy (push (cons "update" "lazy") query))
      (:false (push (cons "update" "false") query))
      (t (push (cons "update" "true") query)))
    (when start-key
      (push (cons "start_key" (jsown:to-json start-key)) query))
    (when end-key
      (push (cons "end_key" (jsown:to-json end-key)) query))
    (api-request client "/documents/messages/groups" :query query)))
