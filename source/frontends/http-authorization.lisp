(in-package :star.frontends.http-api)

(defun path-prefix-p (prefix path)
  (and (<= (length prefix) (length path))
       (string= prefix path :end2 (length prefix))))

(defun path-suffix-p (suffix path)
  (and (<= (length suffix) (length path))
       (string= suffix path
                :start2 (- (length path) (length suffix)))))

(defun route-action (method path)
  (cond
    ((or (public-auth-path-p path)
         (eq method :options))
     :public)
    ((and (eq method :get) (string= path "/auth/context"))
     "identity:read")
    ((string= path "/auth/credentials")
     (case method
       (:get "credentials:read")
       (:post "credentials:create")
       (otherwise nil)))
    ((and (path-prefix-p "/auth/credentials/" path)
          (eq method :post))
     (cond
       ((path-suffix-p "/rotate" path) "credentials:rotate")
       ((path-suffix-p "/revoke" path) "credentials:revoke")
       ((path-suffix-p "/disable" path) "credentials:disable")
       (t nil)))
    ((path-prefix-p "/document/" path)
     (case method
       (:get "documents:read")
       (:put "documents:write")
       (:delete "documents:delete")
       (otherwise nil)))
    ((and (eq method :post)
          (path-prefix-p "/new/document/" path))
     "documents:write")
    ((path-prefix-p "/documents/bulk" path)
     "documents:bulk")
    ((and (eq method :get) (string= path "/search"))
     "search:read")
    ((and (eq method :post)
          (path-prefix-p "/new/target/" path))
     "targets:dispatch")
    ((path-prefix-p "/targets/" path)
     (cond
       ((and (eq method :post)
             (path-suffix-p "/force-release" path))
        "targets:force-release")
       ((and (eq method :post)
             (path-suffix-p "/lease" path))
        "targets:lease")
       ((eq method :get) "targets:read")
       (t nil)))
    ((and (eq method :post)
          (path-prefix-p "/new/event/" path))
     "events:write")
    ((and (eq method :post)
          (path-prefix-p "/events/" path)
          (path-suffix-p "/replay" path))
     "events:replay")
    ((and (eq method :get)
          (path-prefix-p "/views/" path))
     "views:read")
    ((and (eq method :get)
          (string= path "/dataset-size"))
     "views:read")
    ((and (eq method :get)
          (path-prefix-p "/documents/" path))
     "views:read")
    (t nil)))

(defun legacy-unscoped-view-path-p (path)
  (and (path-prefix-p "/documents/" path)
       (not (path-prefix-p "/documents/bulk" path))))

(defun request-policy-metadata (method path correlation-id)
  (list :method (string-upcase (symbol-name method))
        :route path
        :correlation-id correlation-id))

(defun authorize-http-route! (method path correlation-id)
  (let ((action (route-action method path)))
    (cond
      ((eq action :public) nil)
      ((null action)
       (star.authorization:authorize!
        "unmapped:http-route"
        :metadata (request-policy-metadata method path correlation-id)))
      ((legacy-unscoped-view-path-p path)
       (star.authorization:authorize!
        action
        :resource
        (star.authorization:make-authorization-resource
         :tenant-id "default"
         :dataset-id "__unscoped_legacy_view__")
        :metadata (request-policy-metadata method path correlation-id)))
      (t
       (star.authorization:authorize!
        action
        :metadata (request-policy-metadata method path correlation-id))))))

(defun authorization-error-response (correlation-id)
  (list
   403
   (list :content-type "application/json"
         :cache-control "no-store"
         :x-correlation-id correlation-id)
   (list
    (jsown:to-json
     (jsown:new-js
       ("status" "error")
       ("code" "access_denied")
       ("msg" "Access denied")
       ("correlation_id" correlation-id))))))

(defun authorization-middleware (app)
  (lambda (env)
    (let* ((path (or (getf env :path-info) "/"))
           (method (or (getf env :request-method) :get))
           (correlation-id
             (or *http-correlation-id*
                 (request-correlation-id-from-env env))))
      (handler-case
          (progn
            (authorize-http-route! method path correlation-id)
            (lack.component:call app env))
        (star.authorization:authorization-error ()
          (authorization-error-response correlation-id))))))

(defmacro couchdb-handler ((client pool) &body body)
  `(handler-case
       (anypool:with-connection (,client ,pool)
         (handler-case
             (progn ,@body)
           (dex:http-request-not-found (condition)
             (log:warn "CouchDB request not found: ~a" condition)
             (setf (lack.response:response-status *response*) 404)
             (status-msg "Not Found" 'error))
           (dex:http-request-conflict (condition)
             (log:warn "CouchDB request conflict: ~a" condition)
             (setf (lack.response:response-status *response*) 409)
             (status-msg "Conflict" 'error))
           (usocket:timeout-error (condition)
             (log:error "Socket timeout connecting to database: ~a" condition)
             (setf (lack.response:response-status *response*) 504)
             (status-msg "Time out Connecting to database" 'error))
           (dex:http-request-gateway-timeout (condition)
             (log:error "Gateway timeout connecting to couchdb: ~a" condition)
             (setf (lack.response:response-status *response*) 504)
             (status-msg "Timeout connecting to couchdb" 'error))
           (dex:http-request-bad-request (condition)
             (log:error "CouchDB bad request: ~a" condition)
             (setf (lack.response:response-status *response*) 400)
             (status-msg "Bad Request"
                         'error
                         :traceback (format nil "~a" condition)))))
     (star.authorization:authorization-error (condition)
       (error condition))
     (usocket:timeout-error (condition)
       (log:error "Socket timeout getting connection from pool: ~a" condition)
       (setf (lack.response:response-status *response*) 504)
       (status-msg "Timeout getting database connection" 'error))
     (error (condition)
       (log:error "Unexpected error in couchdb-handler: ~a" condition)
       (setf (lack.response:response-status *response*) 500)
       (status-msg "Internal Server Error"
                   'error
                   :traceback (format nil "~a" condition)))))

(defmacro with-http-boundary (() &body body)
  `(let ((*http-correlation-id*
           (or *http-correlation-id* (new-correlation-id))))
     (set-default-headers)
     (set-correlation-id-header)
     (handler-case
         (progn ,@body)
       (star.authorization:authorization-error (condition)
         (declare (ignore condition))
         (setf (lack.response:response-status *response*) 403)
         (status-msg "Access denied"
                     'error
                     :code "access_denied"))
       (http-input-error (condition)
         (log:warn "HTTP input rejected correlation=~a code=~a: ~a"
                   (current-correlation-id)
                   (http-input-error-code condition)
                   condition)
         (respond-http-input-error condition))
       (bt:timeout (condition)
         (log:error "HTTP operation timed out correlation=~a: ~a"
                    (current-correlation-id)
                    condition)
         (setf (lack.response:response-status *response*) 504)
         (status-msg "Request deadline exceeded"
                     'error
                     :code "request_timeout"))
       (error (condition)
         (log:error "HTTP internal error correlation=~a: ~a"
                    (current-correlation-id)
                    condition)
         (setf (lack.response:response-status *response*) 500)
         (status-msg "Internal Server Error"
                     'error
                     :code "internal_error")))))

(setf *server*
      (lack:builder
       :accesslog
       (cors-middleware
        (authentication-middleware
         (authorization-middleware *app*)))))
