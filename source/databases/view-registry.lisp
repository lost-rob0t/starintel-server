(in-package :star.databases.couchdb)

(defstruct (view-spec
             (:constructor make-view-spec
                 (name design-document view-name
                  &key reducer-p
                    (default-reduce nil)
                    (default-include-docs t)
                    accepted-keywords)))
  "Validated specification of one CouchDB view."
  name
  design-document
  view-name
  (reducer-p nil)
  (default-reduce nil)
  (default-include-docs t)
  accepted-keywords)

;; Accessor documentation for view-spec
(setf (documentation 'VIEW-SPEC-ACCEPTED-KEYWORDS 'function)
"Keywords accepted for a given view kind.")
(setf (documentation 'VIEW-SPEC-DEFAULT-INCLUDE-DOCS 'function)
"The =default-include-docs= slot of =view-spec=.")
(setf (documentation 'VIEW-SPEC-DEFAULT-REDUCE 'function)
"The =default-reduce= slot of =view-spec=.")
(setf (documentation 'VIEW-SPEC-DESIGN-DOCUMENT 'function)
"The =design-document= slot of =view-spec=.")
(setf (documentation 'VIEW-SPEC-NAME 'function)
"The =name= slot of =view-spec=.")
(setf (documentation 'VIEW-SPEC-REDUCER-P 'function)
"The =reducer-p= slot of =view-spec=.")
(setf (documentation 'VIEW-SPEC-VIEW-NAME 'function)
"The =view-name= slot of =view-spec=.")



(defstruct (view-map-result
             (:constructor make-view-map-result (rows)))
  "View result holding map rows (key/value pairs)."
  rows)

;; Accessor documentation for view-map-result
(setf (documentation 'VIEW-MAP-RESULT-ROWS 'function)
"Raw rows of a map view result.")



(defstruct (view-document-result
             (:constructor make-view-document-result (documents rows)))
  "View result holding included documents."
  documents
  rows)

;; Accessor documentation for view-document-result
(setf (documentation 'VIEW-DOCUMENT-RESULT-DOCUMENTS 'function)
"The =documents= slot of =view-document-result=.")
(setf (documentation 'VIEW-DOCUMENT-RESULT-ROWS 'function)
"Raw rows of a document view result.")



(defstruct (view-reduced-result
             (:constructor make-view-reduced-result (rows)))
  "View result holding reduced values."
  rows)

;; Accessor documentation for view-reduced-result
(setf (documentation 'VIEW-REDUCED-RESULT-ROWS 'function)
"Raw rows of a reduced view result.")



(define-condition view-registry-error (error)
  ((reason
    :initarg :reason
    :reader view-registry-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "CouchDB view registry error: ~a"
             (view-registry-error-reason condition))))
  (:documentation "Base condition for view registry failures."))

;; Accessor documentation for view-registry-error
(setf (documentation 'VIEW-REGISTRY-ERROR-REASON 'function)
"The =reason= slot of =view-registry-error=.")


(setf (documentation 'VIEW-REGISTRY-ERROR-REASON 'function)
"The =view-registry-error-reason= slot of =view-registry-error=.")

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
  "The registered spec for a view name, or nil."
  (or (gethash name *view-registry*)
      (error 'view-registry-error
             :reason (format nil "view wrapper ~s is not registered" name))))

(defun registered-view-names ()
  "Names of all views known to the registry."
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
  (when (and (or group group-level)
             (not reduce))
    (error 'view-registry-error
           :reason "group/group-level requires reduce=true"))
  t)

(defun view-query-arguments
    (arguments reduce include-docs group group-level)
  (append
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
    :group group)
   (when group-level (list :group-level group-level))))

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
             (view-plist-value arguments :group-level nil)))
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
  "Extract the value cell of a view result row."
  (etypecase result
    (view-document-result
     (view-document-result-documents result))
    (view-map-result
     (view-map-result-rows result))
    (view-reduced-result
     (view-reduced-result-rows result))))

(defmacro define-registered-view-wrapper (name &optional docstring)
  `(defun ,name (client database &rest arguments)
     ,@(when docstring (list docstring))
     (view-result-value
      (apply #'execute-registered-view
             ',name client database arguments))))

(define-registered-view-wrapper messages-by-user
  "Query the messages_by_user view: messages belonging to one user.")
(define-registered-view-wrapper messages-by-platform
  "Query the messages_by_platform view: messages on one platform.")
(define-registered-view-wrapper messages-by-group
  "Query the messages_by_group view: messages in one group.")
(define-registered-view-wrapper social-posts-by-user
  "Query the social_posts_by_user view: social posts by one user.")
(define-registered-view-wrapper social-posts-by-group
  "Query the social_posts_by_group view: social posts in one group.")
(define-registered-view-wrapper social-posts-by-platform
  "Query the social_posts_by_platform view: social posts on one platform.")
(define-registered-view-wrapper by-channel
  "Query the by-channel view: documents grouped by channel.")
(define-registered-view-wrapper groups
  "Run the grouped reduce view and return its group rows.")
(define-registered-view-wrapper count-by-dtype
  "Count documents grouped by their =dtype=.")
(define-registered-view-wrapper dataset-size
  "Count the documents in one dataset.")
(define-registered-view-wrapper documents-by-dataset
  "List the documents belonging to one dataset.")
(define-registered-view-wrapper orgs-by-country
  "Query organizations grouped by country.")
(define-registered-view-wrapper orgs-by-name
  "Query organizations by name.")
(define-registered-view-wrapper persons-by-name
  "Query persons by name.")
(define-registered-view-wrapper persons-by-region
  "Query persons grouped by region.")
(define-registered-view-wrapper relations-edges
  "Return relation edges as view rows.")
(define-registered-view-wrapper relations-incoming-count
  "Count incoming relations for a node.")
(define-registered-view-wrapper relations-outgoing-count
  "Count outgoing relations for a node.")
(define-registered-view-wrapper targets-actor-counts
  "Count targets grouped by owning actor.")
(define-registered-view-wrapper targets-by-actor
  "List targets owned by an actor.")
(define-registered-view-wrapper targets-target-count
  "Count targets for a given target id.")
(define-registered-view-wrapper users-by-platform
  "Query users on one platform.")

(defun get-targets* (client database &rest actors)
  "Fetch target documents, optionally filtered by actor."
  (let ((documents
          (targets-by-actor
           client database
           :keys actors
           :include-docs t
           :reduce nil)))
    (loop for document in documents
          for actor = (star.documents:document-value document "actor")
          collect (cons actor document))))

(defun checked-in-design-document-map ()
  "Return the design documents embedded in the runtime image."
  (let ((documents (make-hash-table :test #'equal)))
    (dolist (json star:*couchdb-views* documents)
      (let* ((document
               (jsown:with-injective-reader
                 (jsown:parse json)))
             (id (jsown:val-safe document "_id")))
        (unless (and (stringp id)
                     (uiop:string-prefix-p "_design/" id))
          (error 'view-registry-error
                 :reason
                 (format nil "embedded view document has invalid _id: ~s" id)))
        (setf (gethash (subseq id (length "_design/")) documents)
              document)))))

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
  "Allowed keyword/verb combinations per view kind."
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
