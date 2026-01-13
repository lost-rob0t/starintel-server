;; http-api
(in-package :star.frontends.http-api)

(defparameter *app* (make-instance 'ningle:app))

(defparameter *rabbitmq-conn* nil)
(defparameter *rabbit-lock* (bt:make-lock "rabbitmq-conn"))

(defun disconnect-rabbitmq ()
  (log:info "Disconnecting from RabbitMQ")
  (when *rabbitmq-conn*
    (cl-rabbit:destroy-connection *rabbitmq-conn*)
    (setf *rabbitmq-conn* nil)
    (log:info "RabbitMQ connection destroyed")))


;; Somhere in here a plugin called http api could be made, except not a normal plugin its a service and would start this web server instead of embeding it

(defparameter *couchdb-pool* (anypool:make-pool :name "couchdb-connections"
                                                :connector (lambda ()
                                                             (let ((client (cl-couch:new-couchdb star:*couchdb-host* star:*couchdb-port* :scheme star:*couchdb-scheme*)))
                                                               (cl-couch:password-auth client star:*couchdb-user* star:*couchdb-password*)
                                                               client))

                                                :disconnector (lambda (obj)
                                                                (setf (cl-couch:couchdb-headers obj) nil))
                                                :max-open-count 20
                                                :max-idle-count 10))

(defun connect-rabbitmq ()
  (log:info "Connecting to RabbitMQ at ~a:~a" star:*rabbit-address* star:*rabbit-port*)
  (setf *rabbitmq-conn* (cl-rabbit:new-connection))
  (let ((socket (cl-rabbit:tcp-socket-new *rabbitmq-conn*)))
    (log:debug "Opening RabbitMQ socket to ~a:~a" star:*rabbit-address* star:*rabbit-port*)
    (cl-rabbit:socket-open socket star:*rabbit-address* star:*rabbit-port*)
    (log:debug "Authenticating with RabbitMQ user: ~a" star:*rabbit-user*)
    (cl-rabbit:login-sasl-plain *rabbitmq-conn* "/" star:*rabbit-user* star:*rabbit-password*)
    (log:debug "Opening RabbitMQ channel 1")
    (cl-rabbit:channel-open *rabbitmq-conn* 1)
    (log:info "RabbitMQ connection established successfully")))



(defparameter *default-headers* (list
                                 :content-type "application/json"))

(defun set-default-headers ()
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                *default-headers*)))


(defun status-msg (msg status &key info traceback)
  (let ((json (jsown:new-js
                ("msg" msg)
                ("status"  (string-downcase (symbol-name status))))))
    (when info
      (jsown:extend-js json
        ("info" info)))
    (when traceback
      (jsown:extend-js json
        ("trace" traceback)))
    (jsown:to-json json)))


(defmacro couchdb-handler ((client pool) &body body)
  `(handler-case
       (anypool:with-connection (,client ,pool)
         (handler-case ,@body
           (dex:http-request-not-found (e)
             (log:warn "CouchDB request not found: ~a" e)
             (status-msg "Not Found" 'error))
           (dex:http-request-conflict (e)
             (log:warn "CouchDB request conflict: ~a" e)
             (status-msg "Conflict" 'error))
           (usocket:timeout-error (e)
             (log:error "Socket timeout connecting to database: ~a" e)
             (status-msg  "Time out Connecting to database" 'error))
           (dex:http-request-gateway-timeout (e)
             (log:error "Gateway timeout connecting to couchdb: ~a" e)
             (status-msg "Timeout connecting to couchdb" 'error))
           (dex:http-request-bad-request (e)
             (log:error "CouchDB bad request: ~a" e)
             (status-msg "Bad Request" 'error :traceback (format nil "~a" e)))))
     (usocket:timeout-error (e)
       (log:error "Socket timeout getting connection from pool: ~a" e)
       (status-msg "Timeout getting database connection" 'error))
     (error (e)
       (log:error "Unexpected error in couchdb-handler: ~a" e)
       (status-msg "Internal Server Error" 'error :traceback (format nil "~a" e)))))


(setf (ningle:route *app* "/health" :method :get)
      #'(lambda (params)
          (log:debug "Health check endpoint called")
          (status-msg "OK" 'info)))


(setf (ningle:route *app* "/" :method :get)
      #'(lambda (params)
          (log:info "Root endpoint called - getting server info")
          (set-default-headers)
          (let ((json (jsown:new-js
                        ("doc_spec_version" "1.0")
                        ("default-dataset" star:*couchdb-default-database*)
                        ("event_log" star:*couchdb-event-log-database*)
                        ("server" "starintel-gserver")
                        ("version" star:*star-server-version*))))
            (log:debug "Returning server info response")
            (jsown:to-json json))))




(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'(lambda (params)
          (let ((actor (cdr (assoc :actor params :test #'string=))))
            (log:info "GET /targets/:actor - actor: ~a" actor)
            (set-default-headers)
            (couchdb-handler (client *couchdb-pool*)
              (progn
                (log:debug "Querying targets view for actor: ~a" actor)
                (let ((targets (loop for row in (jsown:val (query-view client *couchdb-default-database* "targets" "by_actor" :include-docs t :key actor) "rows")
                                     collect (jsown:val row "doc"))))
                  (log:info "Found ~a targets for actor: ~a" (length targets) actor)
                  (jsown:to-json targets)))))))


(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'(lambda (params)
          (set-default-headers)
          (let* ((actor (cdr (assoc :actor params :test #'string=)))
                 (body (babel:octets-to-string (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.target.~a" actor)))
            (log:info "POST /new/target/:actor - actor: ~a routing-key: ~a" actor routing-key)
            (log:debug "Target body length: ~a" (length body))
            (star.actors:publish star.actors:*producer-agent* :body body :routing-key routing-key :properties (list (cons :type "target")))
            (log:info "Target published to RabbitMQ successfully")
            body)))

(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)
          (set-default-headers)
          (let* ((dtype  (cdr (assoc :dtype params :test #'string=)))
                 (body (babel:octets-to-string  (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.~a" dtype)))
            (log:info "POST /new/document/:dtype - dtype: ~a routing-key: ~a" dtype routing-key)
            (log:debug "Document body length: ~a" (length body))
            (star.actors:publish star.actors:*producer-agent* :body body :routing-key routing-key :properties (list (cons :type dtype)))
            (log:info "Document published to RabbitMQ successfully")
            body)))

(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((document-id  (cdr (assoc :id params :test #'string=))))
            (log:info "GET /document/:id - document-id: ~a" document-id)
            (couchdb-handler (client *couchdb-pool*)
              (progn
                (log:debug "Fetching document from database: ~a" star:*couchdb-default-database*)
                (let ((result (cl-couch:get-document client star:*couchdb-default-database* document-id)))
                  (log:debug "Document retrieved successfully")
                  result))))))


;;;  search
(setf (ningle:route *app* "/search" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((q (cdr (assoc "q" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "25"))))
            (log:info "GET /search - query: ~a limit: ~a" q limit)
            (couchdb-handler (client *couchdb-pool*)
              (progn
                (let* ((db star:*couchdb-default-database*)
                       (ddoc "search")
                       (search-name "fts")
                       (bookmark (cdr (assoc "bookmark" params :test #'string=)))
                       (sort (cdr (assoc "sort" params :test #'string=)))
                       (query (jsown:new-js
                                ("q" q)
                                ("limit" limit)
                                ("include_docs" t))))
                  (when sort
                    (log:debug "Using sort: ~a" sort)
                    (setf (jsown:val query "sort") sort))
                  (when bookmark
                    (log:debug "Using bookmark: ~a" bookmark)
                    (setf (jsown:val query "bookmark") bookmark))
                  (log:debug "Executing FTS search on ~a/~a/~a" db ddoc search-name)
                  (let ((result (cl-couch:fts-search client (jsown:to-json query) db ddoc search-name)))
                    (log:info "Search completed successfully")
                    result)))))))




;; Views api
(setf (ningle:route *app* "/documents/messages/by-user" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((user (cdr (assoc "user" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (log:info "GET /documents/messages/by-user - user: ~a limit: ~a skip: ~a" user limit skip)
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (progn
                 (log:debug "Querying messages-by-user view")
                 (messages-by-user client star:*couchdb-default-database*
                                   :limit limit
                                   :start-key (when start-key (jsown:parse start-key))
                                   :end-key (when end-key (jsown:parse end-key))
                                   :key user
                                   :descending descending
                                   :skip skip)))))))

(setf (ningle:route *app* "/documents/messages/by-channel" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((channel (cdr (assoc "channel" params :test #'string=)))
                (group (cdr (assoc "group"  params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0")))
                (reduce (equal (cdr (assoc "reduce" params :test #'string=)) "true")))
            (log:info "GET /documents/messages/by-channel - group: ~a channel: ~a limit: ~a reduce: ~a" group channel limit reduce)
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (progn
                 (log:debug "Querying by-channel view")
                 (by-channel client star:*couchdb-default-database*
                             :limit limit
                             :start-key (when start-key (jsown:parse start-key))
                             :end-key (when end-key (jsown:parse end-key))
                             :key (list group channel)
                             :descending descending
                             :skip skip
                             :update nil
                             :include-docs (if reduce nil t)
                             :group (if reduce t nil)
                             :reduce reduce)))))))



(setf (ningle:route *app* "/documents/messages/by-groups" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((channel (cdr (assoc :channel params :test #'string=)))
                (group (cdr (assoc :group params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0")))
                (reduce (equal (cdr (assoc "reduce" params :test #'string=)) "true")))
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (by-channel client star:*couchdb-default-database*
                           :limit limit
                           :start-key (when start-key (jsown:parse start-key))
                           :end-key (when end-key (jsown:parse end-key))
                           :key (list group channel)
                           :descending descending
                           :skip skip
                           :include-docs (if reduce nil t)
                           :reduce reduce))))))

(setf (ningle:route *app* "/documents/messages/by-platform" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((platform (cdr (assoc "platform" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (log:info "GET /documents/messages/by-platform - platform: ~a limit: ~a" platform limit)
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (progn
                 (log:debug "Querying messages-by-platform view")
                 (messages-by-platform client star:*couchdb-default-database*
                                       :limit limit
                                       :start-key (when start-key (jsown:parse start-key))
                                       :end-key (when end-key (jsown:parse end-key))
                                       :key platform
                                       :descending descending
                                       :skip skip)))))))

(setf (ningle:route *app* "/documents/messages/groups" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let (
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "100")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (groups client star:*couchdb-default-database*
                       :limit limit
                       :update "lazy"
                       :descending descending
                       :skip skip))))))

(setf (ningle:route *app* "/documents/socialmpost/by-user" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((user (cdr (assoc "user" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (social-posts-by-user client star:*couchdb-default-database*
                                     :limit limit
                                     :start-key (when start-key (jsown:parse start-key))
                                     :end-key (when end-key (jsown:parse end-key))
                                     :key user
                                     :descending descending
                                     :skip skip))))))

(setf (ningle:route *app* "/dataset-size" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((dataset (cdr (assoc "dataset" params :test #'string=))))
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (dataset-size client star:*couchdb-default-database*
                             :key dataset
                             :include-docs nil
                             :reduce t))))))

(setf (ningle:route *app* "/dataset-size" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (reduce (cdr (assoc "reduce" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (dataset-size client star:*couchdb-default-database*
                             :limit limit
                             :start-key (when start-key (jsown:parse start-key))
                             :end-key (when end-key (jsown:parse end-key))
                             :descending descending
                             :reduce reduce
                             :include-docs (if reduce nil t)
                             :skip skip))))))


(setf (ningle:route *app* "/new/event/:id")
      #'(lambda (params)
          (set-default-headers)
          (let ((event)))))


(defparameter *server* (lack:builder
                        :accesslog
                        *app*))

(defun start-http-api ()
  (log:info "Starting HTTP API server")
  (log:info "Server configuration - address: ~a port: ~a" star:*http-api-address* star:*http-api-port*)
  (log:info "Hunchentoot threading - max threads: 50, max accept: 100")
  (connect-rabbitmq)
  (log:info "Starting Clack server with Hunchentoot backend")
  (let ((server (clack:clackup *server*
                               :server :hunchentoot
                               :address star:*http-api-address*
                               :port star:*http-api-port*
                               :max-thread-count 50
                               :max-accept-count 100
                               :request-timeout 300)))
    (log:info "HTTP API server started successfully on ~a:~a" star:*http-api-address* star:*http-api-port*)
    server))
