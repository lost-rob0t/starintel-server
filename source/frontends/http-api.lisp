;; http-api
(in-package :star.frontends.http-api)

(defparameter *app* (make-instance 'ningle:app))

(defparameter *rabbitmq-conn* nil)
(defparameter *rabbit-lock* (bt:make-lock "rabbitmq-conn"))

(defun disconnect-rabbitmq ()
  (when *rabbitmq-conn*
    (cl-rabbit:destroy-connection *rabbitmq-conn*)
    (setf *rabbitmq-conn* nil)))


;; Somhere in here a plugin called http api could be made, except not a normal plugin its a service and would start this web server instead of embeding it

(defparameter *couchdb-pool* (anypool:make-pool :name "couchdb-connections"
                                                :connector (lambda ()
                                                             (let ((client (cl-couch:new-couchdb (uiop:getenv "COUCHDB_HOST") 5984 :scheme (string-downcase (uiop:getenv "COUCHDB_SCHEME")))))
                                                               (cl-couch:password-auth client (uiop:getenv "COUCHDB_USER") (uiop:getenv "COUCHDB_PASSWORD"))
                                                               client))

                                                :disconnector (lambda (obj)
                                                                (setf (cl-couch:couchdb-headers obj) nil))
                                                :max-open-count 20))

(defun connect-rabbitmq ()
  (setf *rabbitmq-conn* (cl-rabbit:new-connection))
  (let ((socket (cl-rabbit:tcp-socket-new *rabbitmq-conn*)))
    (cl-rabbit:socket-open socket star:*rabbit-address* star:*rabbit-port*)
    (cl-rabbit:login-sasl-plain *rabbitmq-conn* "/" star:*rabbit-user* star:*rabbit-password*)
    (cl-rabbit:channel-open *rabbitmq-conn* 1)))



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
  `(anypool:with-connection (,client ,pool)
     (handler-case ,@body
       (dex:http-request-not-found (e)
         (status-msg "Not Found" 'error))
       (dex:http-request-conflict (e)
         (status-msg "Conflict" 'error))
       (usocket:timeout-error (e)
         (status-msg  "Time out Connecting to database" 'error))
       (dex:http-request-gateway-timeout (e)
         (status-msg "Timeout connecting to couchdb" 'error))
       (dex:http-request-bad-request (e)
         (status-msg "Bad Request" 'error :traceback (format nil "~a" e))))))



(setf (ningle:route *app* "/" :method :get)
      #'(lambda (params)
          (couchdb-handler (client *couchdb-pool*)
            (let ((json (jsown:new-js
                          ("doc_spec_version" spec:+starintel-doc-version+)
                          ("default-dataset" star:*couchdb-default-database*)
                          ("event_log" star:*couchdb-event-log-database*)
                          ("stats" (jsown:new-js
                                     ("dataset-size" (dataset-size client star:*couchdb-default-database* :update :lazy :reduce t :include-docs nil)))))))
              (set-default-headers)
              (jsown:to-json json)))))




(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (couchdb-handler (client *couchdb-pool*)
            (let ((targets (loop for row in (jsown:val (query-view client *couchdb-default-database* "targets" "by_actor" :include-docs t :key (cdr (assoc :actor params :test #'string=))) "rows")
                                 collect (jsown:val row "doc"))))
              (log:debug targets)
              (jsown:to-json targets)))))


(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'(lambda (params)
          (let* ((actor (cdr (assoc :actor params :test #'string=)))
                 (body (babel:octets-to-string (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.target.~a" actor)))
            (with-rabbitmq (*rabbitmq-conn*)
              (cl-rabbit:basic-publish *rabbitmq-conn* 1 :routing-key routing-key :exchange "documents" :properties (list (cons :type "target")) :body body))
            body)))


(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)
          (set-default-headers)
          (let* ((dtype  (cdr (assoc :dtype params :test #'string=)))
                 (body (babel:octets-to-string  (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.~a" dtype)))
            (with-rabbitmq (*rabbitmq-conn*)
              (cl-rabbit:basic-publish *rabbitmq-conn* 1 :routing-key routing-key :exchange "documents" :properties (list (cons :type dtype)) :body body))

            body)))

(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((document-id  (cdr (assoc :id params :test #'string=))))
            (couchdb-handler (client *couchdb-pool*)
              (cl-couch:get-document client star:*couchdb-default-database* document-id)))))


;;;  search
(setf (ningle:route *app* "/search" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (couchdb-handler (client *couchdb-pool*)
            (let* ((db star:*couchdb-default-database*)
                   (ddoc "search")
                   (search-name "fts")
                   (q (cdr (assoc "q" params :test #'string=)))
                   (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "25")))
                   (bookmark (cdr (assoc "bookmark" params :test #'string=)))
                   (sort (cdr (assoc "sort" params :test #'string=)))
                   (query (jsown:new-js
                            ("q" q)
                            ("limit" limit)
                            ("include_docs" t))))
              (when sort
                (setf (jsown:val query "sort") sort))
              (when bookmark
                (setf (jsown:val query "bookmark") bookmark))
              (cl-couch:fts-search client (jsown:to-json query) db ddoc search-name)))))




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
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (messages-by-user client star:*couchdb-default-database*
                                 :limit limit
                                 :start-key (when start-key (jsown:parse start-key))
                                 :end-key (when end-key (jsown:parse end-key))
                                 :key user
                                 :descending descending
                                 :skip skip))))))

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
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
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
                           :reduce reduce))))))



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
            (jsown:to-json
             (couchdb-handler (client *couchdb-pool*)
               (messages-by-platform client star:*couchdb-default-database*
                                     :limit limit
                                     :start-key (when start-key (jsown:parse start-key))
                                     :end-key (when end-key (jsown:parse end-key))
                                     :key platform
                                     :descending descending
                                     :skip skip))))))

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
  (connect-rabbitmq)
  (clack:clackup *server*
                 :server :hunchentoot
                 :address star:*http-api-address*
                 :port star:*http-api-port*))
