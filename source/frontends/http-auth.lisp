(in-package :star.frontends.http-api)

(defun env-header-key (name)
  (intern
   (format nil "HTTP-~a"
           (string-upcase name))
   :keyword))

(defun env-header-value (env name)
  (or (getf env (env-header-key name))
      (let ((headers (getf env :headers)))
        (cond
          ((hash-table-p headers)
           (or (gethash (string-downcase name) headers)
               (gethash name headers)))
          ((and (listp headers)
                (consp (first headers)))
           (cdr (assoc name headers :test #'string-equal)))
          (t nil)))))

(defun bounded-correlation-id-p (value)
  (and (stringp value)
       (<= 1 (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_.:")))
              value)))

(defun request-correlation-id-from-env (env)
  (let ((provided (env-header-value env "X-Correlation-ID")))
    (if (bounded-correlation-id-p provided)
        provided
        (new-correlation-id))))

(defun parse-request-timeout-ms (env)
  (let ((raw (env-header-value env "X-Request-Timeout-Ms")))
    (if raw
        (handler-case
            (let ((value (parse-integer raw :junk-allowed nil)))
              (if (<= 1 value star:*auth-max-request-timeout-ms*)
                  value
                  star:*auth-default-request-timeout-ms*))
          (error () star:*auth-default-request-timeout-ms*))
        star:*auth-default-request-timeout-ms*)))

(defun request-deadline-from-env (env)
  (+ (get-universal-time)
     (ceiling (parse-request-timeout-ms env) 1000)))

(defun public-mode-read-path-p (path)
  (member path
          '("/api/v1/search" "/api/v1/stats")
          :test #'string=))

;; Keep the default public-read surface visible to existing introspection while
;; making PUBLIC-AUTH-PATH-P the authoritative runtime decision. This means an
;; init.lisp setting of *PUBLIC-MODE* NIL can override these default entries.
(dolist (path '("/api/v1/search" "/api/v1/stats"))
  (pushnew path star:*auth-public-paths* :test #'string=))

(defun public-auth-path-p (path)
  (if (public-mode-read-path-p path)
      star::*public-mode*
      (member path star:*auth-public-paths* :test #'string=)))

(defun development-security-context (correlation-id deadline)
  (star.auth::%make-request-security-context
   :principal
   (star.auth::%make-request-principal
    :id "development-bypass"
    :type "administrator"
    :scopes (list "admin")
    :credential-id "development-bypass")
   :correlation-id correlation-id
   :deadline deadline
   :authenticated-at (get-universal-time)))

(defun authenticate-request-env (env correlation-id deadline)
  (let ((mode (string-downcase star:*auth-mode*)))
    (cond
      ((string= mode "api-key")
       (star.auth:authenticate-authorization-header
        (env-header-value env "Authorization")
        correlation-id
        deadline))
      ((and (string= mode "disabled")
            star:*auth-dev-bypass*)
       (development-security-context correlation-id deadline))
      (t
       (star.auth:signal-authentication-failure)))))

(defun authentication-error-response (correlation-id)
  (list
   401
   (list :content-type "application/json"
         :cache-control "no-store"
         :x-correlation-id correlation-id
         :www-authenticate "Bearer realm=\"starintel\"")
   (list
    (jsown:to-json
     (jsown:new-js
       ("status" "error")
       ("code" "invalid_credential")
       ("msg" "Authentication failed")
       ("correlation_id" correlation-id))))))

(defun append-response-headers (response headers)
  (if (and response (listp response) (second response))
      (list (first response)
            (append (second response) headers)
            (third response))
      response))

(defun authentication-middleware (app)
  (lambda (env)
    (let* ((path (or (getf env :path-info) "/"))
           (method (getf env :request-method))
           (correlation-id (request-correlation-id-from-env env))
           (deadline (request-deadline-from-env env)))
      (handler-case
          (let* ((context
                   (unless (or (eq method :options)
                               (public-auth-path-p path))
                     (authenticate-request-env
                      env correlation-id deadline)))
                 (star.auth:*request-security-context* context)
                 (*http-correlation-id* correlation-id)
                 (response (lack.component:call app env)))
            (append-response-headers
             response
             (list :x-correlation-id correlation-id)))
        (star.auth:authentication-error ()
          (authentication-error-response correlation-id))))))

(defun configured-origin-allowed-p (origin)
  (and (stringp origin)
       (member origin
               star:*http-cors-allowed-origins*
               :test #'string=)))

(defun cors-headers-for-origin (origin)
  (when (configured-origin-allowed-p origin)
    (list :access-control-allow-origin origin
          :access-control-allow-methods star:*http-cors-allowed-methods*
          :access-control-allow-headers star:*http-cors-allowed-headers*
          :access-control-max-age "600"
          :vary "Origin")))

(defun cors-middleware (app)
  "Apply configured credential-safe CORS. Wildcard origins are never emitted."
  (lambda (env)
    (let* ((method (getf env :request-method))
           (origin (env-header-value env "Origin"))
           (headers (cors-headers-for-origin origin)))
      (if (eq method :options)
          (if headers
              (list 204
                    (append (list :content-type "text/plain") headers)
                    (list ""))
              (list 403
                    (list :content-type "application/json")
                    (list
                     (jsown:to-json
                      (jsown:new-js
                        ("status" "error")
                        ("code" "cors_origin_denied")
                        ("msg" "Origin is not allowed"))))))
          (append-response-headers
           (lack.component:call app env)
           headers)))))

(defmacro with-http-boundary (() &body body)
  `(let ((*http-correlation-id*
           (or *http-correlation-id* (new-correlation-id))))
     (set-default-headers)
     (set-correlation-id-header)
     (handler-case
         (progn ,@body)
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

(defun request-principal (&optional request)
  (declare (ignore request))
  (or (star.auth:current-principal-id)
      "anonymous"))

(defun require-administrator-context ()
  (unless (star.auth:administrator-principal-p)
    (signal-http-input-error
     403
     "access_denied"
     "Access denied"))
  star.auth:*request-security-context*)

(setf *cors-headers* nil)
(setf *server*
      (lack:builder
       :accesslog
       (cors-middleware
        (authentication-middleware *app*))))
