(in-package :star.databases.couchdb)

(defstruct (view-spec
             (:constructor make-view-spec
                 (name design-document view-name
                  &key reducer-p
                    (default-reduce nil)
                    (default-include-docs t)
                    accepted-keywords)))
  name
  design-document
  view-name
  (reducer-p nil)
  (default-reduce nil)
  (default-include-docs t)
  accepted-keywords)

(defstruct (view-map-result
             (:constructor make-view-map-result (rows)))
  rows)

(defstruct (view-document-result
             (:constructor make-view-document-result (documents rows)))
  documents
  rows)

(defstruct (view-reduced-result
             (:constructor make-view-reduced-result (rows)))
  rows)

(define-condition view-registry-error (error)
  ((reason
    :initarg :reason
    :reader view-registry-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "CouchDB view registry error: ~a"
             (view-registry-error-reason condition)))))

(defparameter +view-wrapper-keywords+
  '(:limit :start-key :end-key :keys :key :descending
    :include-docs :update :skip :reduce :group :group-level :sort-fn))

(defparameter *view-registry* (make-hash-table :test #'eq))

(defun register-view-spec
    (name design-document view-name
     &key reducer-p (default-reduce nil) (default-include-docs t)
       (accepted-keywords +view-wrapper-keywords+))
  (setf (gethash name *view-registry*)
        (make-view-spec
         name design-document view-name
         :reducer-p reducer-p
         :default-reduce default-reduce
         :default-include-docs default-include-docs
         :accepted-keywords accepted-keywords)))

(defun registered-view-spec (name)
  (or (gethash name *view-registry*)
      (error 'view-registry-error
             :reason (format nil "view wrapper ~s is not registered" name))))

(defun registered-view-names ()
  (sort
   (loop for name being the hash-keys of *view-registry* collect name)
   #'string< :key #'symbol-name))

(defun register-public-view-specs ()
  (clrhash *view-registry*)
  (register-view-spec 'messages-by-user "messages" "messages_by_user"
                      :reducer-p t)
  (register-view-spec 'messages-by-platform "messages" "messages_by_platform"
                      :reducer-p t)
  (register-view-spec 'messages-by-group "messages" "messages_by_group"
                      :reducer-p t)
  (register-view-spec 'social-posts-by-user "messages" "social_posts_by_user"
                      :reducer-p t)
  (register-view-spec 'social-posts-by-group "messages" "social_posts_by_group"
                      :reducer-p t)
  (register-view-spec 'social-posts-by-platform
                      "messages" "social_posts_by_platform"
                      :reducer-p t)
  (register-view-spec 'by-channel "messages" "by_channel"
                      :reducer-p t)
  (register-view-spec 'groups "messages" "groups"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'count-by-dtype "data" "count_by_dtype"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'dataset-size "data" "dataset_size"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'documents-by-dataset "data" "by_dataset"
                      :default-include-docs t)
  (register-view-spec 'orgs-by-country "orgs" "by_country")
  (register-view-spec 'orgs-by-name "orgs" "by_name")
  (register-view-spec 'persons-by-name "persons" "by_name")
  (register-view-spec 'persons-by-region "persons" "by_region")
  (register-view-spec 'relations-edges "relations" "edges"
                      :reducer-p t)
  (register-view-spec 'relations-incoming-count
                      "relations" "incoming_count"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'relations-outgoing-count
                      "relations" "outgoing_count"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'targets-actor-counts "targets" "actor_count"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'targets-by-actor "targets" "by_actor")
  (register-view-spec 'targets-target-count "targets" "target_count"
                      :reducer-p t
                      :default-reduce t
                      :default-include-docs nil)
  (register-view-spec 'users-by-platform "users" "by_platform")
  (register-view-spec 'timeline-view "time" "timeline"
                      :reducer-p t)
  t)

(register-public-view-specs)

(defun view-plist-present-p (arguments keyword)
  (loop for tail on arguments by #'cddr
        thereis (eq (first tail) keyword)))

(defun view-plist-value (arguments keyword default)
  (if (view-plist-present-p arguments keyword)
      (getf arguments keyword)
      default))

(defun validate-view-wrapper-arguments (spec arguments)
  (when (oddp (length arguments))
    (error 'view-registry-error
           :reason (format nil "odd wrapper keyword list: ~s" arguments)))
  (loop for tail on arguments by #'cddr
        for keyword = (first tail)
        unless (member keyword
                       (view-spec-accepted-keywords spec)
                       :test #'eq)
          do (error 'view-registry-error
                    :reason
                    (format nil "wrapper ~a does not accept ~s"
                            (view-spec-name spec) keyword)))
  t)

(defun view-sequence-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t
     (error 'view-registry-error
            :reason (format nil "view rows are not an array: ~s" value)))))

(defun validate-view-result-shape
    (spec reduce include-docs group group-level)
  (when (and reduce (not (view-spec-reducer-p spec)))
    (error 'view-registry-error
           :reason
           (format nil "view ~a/~a has no reducer"
                   (view-spec-design-document spec)
                   (view-spec-view-name spec))))
  (when (and reduce include-docs)
    (error 'view-registry-error
           :reason "reduced view requests cannot include documents"))
  (when (and (or group
                 (and group-level (plusp group-level)))
             (not reduce))
    (error 'view-registry-error
           :reason "group/group-level requires reduce=true"))
  t)

(defun view-query-arguments
    (arguments reduce include-docs group group-level)
  (list
   :limit (view-plist-value arguments :limit 50)
   :start-key (view-plist-value arguments :start-key nil)
   :end-key (view-plist-value arguments :end-key nil)
   :keys (view-plist-value arguments :keys nil)
   :key (view-plist-value arguments :key nil)
   :descending (view-plist-value arguments :descending nil)
   :include-docs include-docs
   :update (view-plist-value arguments :update t)
   :skip (view-plist-value arguments :skip 0)
   :reduce reduce
   :group group
   :group-level (or group-level 0)))

(defun view-row-documents (rows)
  (loop for row in rows
        for document = (jsown:val-safe row "doc")
        when document collect document))

(defun execute-registered-view (name client database &rest arguments)
  "Validate and execute one registered view, returning a typed result."
  (let ((spec (registered-view-spec name)))
    (validate-view-wrapper-arguments spec arguments)
    (let* ((reduce
             (view-plist-value
              arguments :reduce (view-spec-default-reduce spec)))
           (include-docs
             (view-plist-value
              arguments :include-docs
              (view-spec-default-include-docs spec)))
           (group
             (view-plist-value arguments :group (and reduce t)))
           (group-level
             (view-plist-value arguments :group-level 0)))
      (validate-view-result-shape
       spec reduce include-docs group group-level)
      (let* ((response
               (apply #'query-view
                      client
                      database
                      (view-spec-design-document spec)
                      (view-spec-view-name spec)
                      (view-query-arguments
                       arguments reduce include-docs group group-level)))
             (rows
               (view-sequence-list
                (jsown:val-safe response "rows"))))
        (cond
          (reduce
           (make-view-reduced-result rows))
          (include-docs
           (let* ((documents (view-row-documents rows))
                  (sort-fn
                    (view-plist-value
                     arguments :sort-fn #'sort-docs-by-date)))
             (make-view-document-result
              (if sort-fn (funcall sort-fn documents) documents)
              rows)))
          (t
           (make-view-map-result rows)))))))

(defun view-result-value (result)
  (etypecase result
    (view-document-result
     (view-document-result-documents result))
    (view-map-result
     (view-map-result-rows result))
    (view-reduced-result
     (view-reduced-result-rows result))))

(defmacro define-registered-view-wrapper (name)
  `(defun ,name (client database &rest arguments)
     (view-result-value
      (apply #'execute-registered-view
             ',name client database arguments))))

(define-registered-view-wrapper messages-by-user)
(define-registered-view-wrapper messages-by-platform)
(define-registered-view-wrapper messages-by-group)
(define-registered-view-wrapper social-posts-by-user)
(define-registered-view-wrapper social-posts-by-group)
(define-registered-view-wrapper social-posts-by-platform)
(define-registered-view-wrapper by-channel)
(define-registered-view-wrapper groups)
(define-registered-view-wrapper count-by-dtype)
(define-registered-view-wrapper dataset-size)
(define-registered-view-wrapper documents-by-dataset)
(define-registered-view-wrapper orgs-by-country)
(define-registered-view-wrapper orgs-by-name)
(define-registered-view-wrapper persons-by-name)
(define-registered-view-wrapper persons-by-region)
(define-registered-view-wrapper relations-edges)
(define-registered-view-wrapper relations-incoming-count)
(define-registered-view-wrapper relations-outgoing-count)
(define-registered-view-wrapper targets-actor-counts)
(define-registered-view-wrapper targets-by-actor)
(define-registered-view-wrapper targets-target-count)
(define-registered-view-wrapper users-by-platform)

(defun get-targets* (client database &rest actors)
  (let ((documents
          (targets-by-actor
           client database
           :keys actors
           :include-docs t
           :reduce nil)))
    (loop for document in documents
          for actor = (star.documents:document-value document "actor")
          collect (cons actor document))))

(defun export-by-dataset* (client database dataset path)
  "Export map rows from data/by_dataset; never mix reduced and document rows."
  (let ((total-exported 0)
        (skip 0)
        (page-size 100))
    (with-open-file
        (stream path :direction :output :if-exists :supersede)
      (loop
        for documents =
          (documents-by-dataset
           client database
           :key dataset
           :limit page-size
           :skip skip
           :include-docs t
           :reduce nil)
        while documents
        do
           (dolist (document documents)
             (write-string (jsown:to-json document) stream)
             (terpri stream)
             (incf total-exported))
           (incf skip page-size)))
    total-exported))

(defun read-view-registry-document (path)
  (jsown:with-injective-reader
    (jsown:parse
     (uiop:read-file-string path))))

(defun checked-in-design-document-map ()
  (let ((documents (make-hash-table :test #'equal))
        (directory
          (uiop:merge-pathnames*
           "views/"
           (asdf:system-source-directory :starintel-gserver))))
    (dolist (path (uiop:directory-files directory) documents)
      (when (string-equal "json" (pathname-type path))
        (let* ((document (read-view-registry-document path))
               (id (jsown:val document "_id"))
               (name (subseq id (length "_design/"))))
          (setf (gethash name documents) document))))))

(defun design-document-has-view-p (document view-name)
  (let ((views (jsown:val-safe document "views")))
    (and views (outbox-object-has-key-p views view-name))))

(defun validate-view-registry
    (&optional (documents (checked-in-design-document-map)))
  "Fail before serving traffic when a registered design/view is absent."
  (dolist (name (registered-view-names) t)
    (let* ((spec (registered-view-spec name))
           (design-name (view-spec-design-document spec))
           (document (gethash design-name documents)))
      (unless document
        (error 'view-registry-error
               :reason
               (format nil "wrapper ~a references missing design document ~a"
                       name design-name)))
      (unless (design-document-has-view-p
               document (view-spec-view-name spec))
        (error 'view-registry-error
               :reason
               (format nil "wrapper ~a references missing view ~a/~a"
                       name design-name (view-spec-view-name spec)))))))

(defun view-registry-matrix ()
  (loop for name in (registered-view-names)
        for spec = (registered-view-spec name)
        collect
        (list
         :wrapper name
         :design-document (view-spec-design-document spec)
         :view (view-spec-view-name spec)
         :accepted-keywords (copy-list (view-spec-accepted-keywords spec))
         :reducer-p (view-spec-reducer-p spec)
         :default-reduce (view-spec-default-reduce spec)
         :default-include-docs
         (view-spec-default-include-docs spec))))
