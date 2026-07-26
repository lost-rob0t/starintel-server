(in-package :star.frontends.http-api)

(defun view-http-param (params name)
  (cdr (assoc name params :test #'string=)))

(defun view-http-param-present-p (params name)
  (not (null (assoc name params :test #'string=))))

(defun view-http-integer (params name default)
  (let ((value (view-http-param params name)))
    (if value (parse-integer value) default)))

(defun view-http-boolean (params name default)
  (if (view-http-param-present-p params name)
      (string-equal "true" (or (view-http-param params name) "false"))
      default))

(defun view-http-json-value (params name)
  (let ((value (view-http-param params name)))
    (when value
      (jsown:with-injective-reader
        (jsown:parse value)))))

(defun view-http-common-arguments (params)
  (let ((arguments
          (list
           :limit (view-http-integer params "limit" 50)
           :descending (view-http-boolean params "descending" nil)
           :skip (view-http-integer params "skip" 0))))
    (when (view-http-param-present-p params "start_key")
      (setf arguments
            (append arguments
                    (list :start-key
                          (view-http-json-value params "start_key")))))
    (when (view-http-param-present-p params "end_key")
      (setf arguments
            (append arguments
                    (list :end-key
                          (view-http-json-value params "end_key")))))
    (when (view-http-param-present-p params "reduce")
      (setf arguments
            (append arguments
                    (list :reduce
                          (view-http-boolean params "reduce" nil)))))
    (when (view-http-param-present-p params "include_docs")
      (setf arguments
            (append arguments
                    (list :include-docs
                          (view-http-boolean params "include_docs" nil)))))
    arguments))

(defun execute-http-view (name arguments)
  (couchdb-handler (client *couchdb-pool*)
    (jsown:to-json
     (star.databases.couchdb:view-result-value
      (apply #'star.databases.couchdb:execute-registered-view
             name
             client
             star:*couchdb-default-database*
             arguments)))))

(defun execute-http-keyed-view (name params parameter)
  (execute-http-view
   name
   (append
    (view-http-common-arguments params)
    (list :key (view-http-param params parameter)))))

(defun execute-http-channel-view (params)
  (let* ((reduce (view-http-boolean params "reduce" nil))
         (group-name (view-http-param params "group"))
         (channel (view-http-param params "channel"))
         (arguments
           (append
            (view-http-common-arguments params)
            (list
             :key (list group-name channel)
             :reduce reduce
             :include-docs (not reduce)
             :group reduce))))
    (execute-http-view 'star.databases.couchdb:by-channel arguments)))

(defun registered-view-route (path name parameter)
  (setf (ningle:route *app* path :method :get)
        (lambda (params)
          (set-default-headers)
          (execute-http-keyed-view name params parameter))))

(registered-view-route
 "/documents/messages/by-user"
 'star.databases.couchdb:messages-by-user
 "user")

(registered-view-route
 "/documents/messages/by-platform"
 'star.databases.couchdb:messages-by-platform
 "platform")

(setf (ningle:route *app* "/documents/messages/by-channel" :method :get)
      (lambda (params)
        (set-default-headers)
        (execute-http-channel-view params)))

;; Compatibility alias retained for the old pluralized route.
(setf (ningle:route *app* "/documents/messages/by-groups" :method :get)
      (lambda (params)
        (set-default-headers)
        (execute-http-channel-view params)))

(setf (ningle:route *app* "/documents/messages/groups" :method :get)
      (lambda (params)
        (set-default-headers)
        (execute-http-view
         'star.databases.couchdb:groups
         (view-http-common-arguments params))))

(registered-view-route
 "/documents/social-media-posts/by-user"
 'star.databases.couchdb:social-posts-by-user
 "user")

(registered-view-route
 "/documents/social-media-posts/by-group"
 'star.databases.couchdb:social-posts-by-group
 "group")

(registered-view-route
 "/documents/social-media-posts/by-platform"
 'star.databases.couchdb:social-posts-by-platform
 "platform")

;; Historical typo remains an explicit compatibility alias.
(registered-view-route
 "/documents/socialmpost/by-user"
 'star.databases.couchdb:social-posts-by-user
 "user")

(setf (ningle:route *app* "/dataset-size" :method :get)
      (lambda (params)
        (set-default-headers)
        (let* ((reduce
                 (view-http-boolean params "reduce" t))
               (arguments
                 (append
                  (view-http-common-arguments params)
                  (list
                   :reduce reduce
                   :include-docs nil))))
          (when (view-http-param-present-p params "dataset")
            (setf arguments
                  (append arguments
                          (list :key
                                (view-http-param params "dataset")))))
          (execute-http-view
           'star.databases.couchdb:dataset-size
           arguments))))

(setf (ningle:route *app* "/targets/:actor" :method :get)
      (lambda (params)
        (set-default-headers)
        (execute-http-view
         'star.databases.couchdb:targets-by-actor
         (list
          :key (cdr (assoc :actor params :test #'string=))
          :include-docs t
          :reduce nil))))

(defparameter *http-view-registry-matrix*
  '(("/documents/messages/by-user" messages-by-user document)
    ("/documents/messages/by-platform" messages-by-platform document)
    ("/documents/messages/by-channel" by-channel map-or-reduced)
    ("/documents/messages/by-groups" by-channel map-or-reduced)
    ("/documents/messages/groups" groups reduced)
    ("/documents/social-media-posts/by-user" social-posts-by-user document)
    ("/documents/social-media-posts/by-group" social-posts-by-group document)
    ("/documents/social-media-posts/by-platform"
     social-posts-by-platform document)
    ("/documents/socialmpost/by-user" social-posts-by-user document)
    ("/dataset-size" dataset-size reduced)
    ("/targets/:actor" targets-by-actor document)))
