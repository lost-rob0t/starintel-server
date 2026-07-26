(in-package :star-ui)

(defparameter *ui-address* "0.0.0.0")
(defparameter *ui-port* 8080)
(defparameter *api-backend-url* "http://localhost:5000")

(defparameter *app* (make-instance 'ningle:app))

(defun set-default-headers ()
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                (list :content-type "application/json"))))

(defun status-msg (msg status &key info traceback)
  (let ((json (jsown:new-js
                ("msg" msg)
                ("status" (string-downcase (symbol-name status))))))
    (when info
      (jsown:extend-js json
        ("info" info)))
    (when traceback
      (jsown:extend-js json
        ("trace" traceback)))
    (jsown:to-json json)))

;;; Star-UI routes

(setf (ningle:route *app* "/" :method :get)
      #'(lambda (params)
          (declare (ignore params))
          (log:info "GET / - redirecting to /targets")
          (setf (lack.response:response-status *response*) 302)
          (setf (lack.response:response-headers *response*)
                (append (lack.response:response-headers *response*)
                        (list :location "/targets")))
          ""))

(setf (ningle:route *app* "/targets" :method :get)
      #'(lambda (params)
          (declare (ignore params))
          (log:info "GET /targets - serving target creation UI")
          (setf (lack.response:response-headers *response*)
                (append (lack.response:response-headers *response*)
                        (list :content-type "text/html; charset=utf-8")))
          (let ((html-path (merge-pathnames "../star-ui/index.html"
                                           (asdf:system-source-directory :star-ui))))
            (if (probe-file html-path)
                (alexandria:read-file-into-string html-path)
                (progn
                  (setf (lack.response:response-status *response*) 404)
                  "UI file not found")))))

(setf (ningle:route *app* "/api/target" :method :post)
      #'(lambda (params)
          (declare (ignore params))
          (set-default-headers)
          (handler-case
              (let* ((body (jsown:parse (babel:octets-to-string
                                        (lack.request:request-content (ningle:context :request))
                                        :encoding :utf-8)))
                     (doctype (jsown:val body "doctype")))
                (log:info "POST /api/target - proxying target creation to backend")
                (log:debug "Target data: ~a" body)

                ;; Validate that it's a TARGET document
                (if (not (string-equal doctype "TARGET"))
                    (progn
                      (setf (lack.response:response-status *response*) 400)
                      (status-msg "Invalid doctype. Expected TARGET" 'error))
                    ;; Forward to backend API
                    (let* ((backend-url (format nil "~a/new/document/~a" *api-backend-url* "target"))
                           (response (dex:post backend-url
                                              :headers '(("Content-Type" . "application/json"))
                                              :content (jsown:to-json body))))
                      (log:info "Target forwarded to backend successfully")

                      ;; Return success response
                      (jsown:to-json (jsown:new-js
                                       ("status" "success")
                                       ("message" "Target created successfully")
                                       ("id" (jsown:val (jsown:parse response) "_id"))
                                       ("data" body))))))
            (dex:http-request-failed (e)
              (log:error "Backend request failed: ~a" e)
              (setf (lack.response:response-status *response*) 502)
              (status-msg "Failed to connect to backend API" 'error
                         :traceback (format nil "~a" e)))
            (error (e)
              (log:error "Error creating target: ~a" e)
              (setf (lack.response:response-status *response*) 500)
              (status-msg (format nil "Error creating target: ~a" e) 'error)))))

(setf (ningle:route *app* "/health" :method :get)
      #'(lambda (params)
          (declare (ignore params))
          (set-default-headers)
          (log:debug "Health check endpoint called")
          (status-msg "OK" 'info)))

(defparameter *server* (lack:builder
                        :accesslog
                        *app*))

(defun start-ui-server (&key (address *ui-address*) (port *ui-port*) (backend-url *api-backend-url*))
  (setf *ui-address* address)
  (setf *ui-port* port)
  (setf *api-backend-url* backend-url)
  (log:info "Starting Star-UI server")
  (log:info "UI server configuration - address: ~a port: ~a" *ui-address* *ui-port*)
  (log:info "Backend API URL: ~a" *api-backend-url*)
  (let ((server (clack:clackup *server*
                               :server :hunchentoot
                               :address *ui-address*
                               :port *ui-port*
                               :max-thread-count 20
                               :max-accept-count 50)))
    (log:info "Star-UI server started successfully on ~a:~a" *ui-address* *ui-port*)
    server))
