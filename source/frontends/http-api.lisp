;; http-api
(in-package :star.frontends.http-api)

(defparameter *app* (make-instance 'ningle:app))



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




;; TODO stats on error shoudnt be a string when error condiction is hit
(setf (ningle:route *app* "/" :method :get)
      #'(lambda (params)
          (with-couchdb (client :couchdb-main)
            (let ((json (jsown:new-js
                          ("doc_spec_version" spec:+starintel-doc-version+)
                          ("default-dataset" star:*couchdb-default-database*)
                          ;; ("event_log" star:*couchdb-event-log-database*)
                          ("stats" (jsown:new-js
                                     ("dataset-size" (or (dataset-size client star:*couchdb-default-database* :update :lazy :reduce t :include-docs nil) "Error fetching datasets.")))))))
              (set-default-headers)
              (jsown:to-json json)))))

(setf (ningle:route *app* "/health" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (jsown:to-json
           (jsown:new-js
             ("status" "ok")
             ("server-version" star:+star-server-version+)
             ("spec-version" spec:+starintel-doc-version+)))))


(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (with-couchdb (client :couchdb-main)
            (let ((targets (loop for row in (jsown:val (query-view client *couchdb-default-database* "targets" "by_actor" :include-docs t :key (cdr (assoc :actor params :test #'string=))) "rows")
                                 collect (jsown:val row "doc"))))
              (log:debug targets)
              (jsown:to-json targets)))))

;; REVIEW is this needed
;; target document has the data lol
(setf (ningle:route *app* "/new/target/:actor" :method :post)
      ;; Please make sure  _rev tag is set correctly.
      #'(lambda (params)
          (with-couchdb (client :couchdb-main)
            (let* ((actor (cdr (assoc :actor params :test #'string=)))
                   (body (babel:octets-to-string (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                   ;; TODO since we are inserting into DB here, we should use the new routing key
                   ;; documents.new includes _rev
                   ;; documents.injest.target would be the injest location
                   (routing-key (format nil "documents.new.target.~a" actor))
                   (rev-tag (jsown:val-safe (jsown:parse (cl-couch:create-document client *couchdb-default-database* body)) "rev"))
                   (document-with-rev (jsown:to-json (jsown:extend-js (jsown:parse body)
                                                       ("_rev" rev-tag)))))

              (act:! star.actors:*publish-service* (list :routing-key routing-key :exchange "documents" :properties (list (cons :type "target")) :body document-with-rev))
              `(201 (:content-type "text/plain") (,document-with-rev))))))


(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)
          (with-couchdb (client :couchdb-main)
            (let* ((dtype (cdr (assoc :dtype params :test #'string=)))
                   (body (babel:octets-to-string (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                   (routing-key (format nil "documents.new.~a" dtype))
                   (rev-tag (jsown:val-safe (jsown:parse (cl-couch:create-document client *couchdb-default-database* body)) "rev"))
                   (document-with-rev (jsown:to-json (jsown:extend-js (jsown:parse body)
                                                       ("_rev" rev-tag)))))
              (act:! star.actors:*publish-service* (list :routing-key routing-key :exchange "documents" :properties (list (cons :type dtype)) :body document-with-rev))
              `(201 (:content-type "text/plain") (,document-with-rev))))))

(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((document-id  (cdr (assoc :id params :test #'string=))))
            (with-couchdb (client :couchdb-main)
              (cl-couch:get-document client star:*couchdb-default-database* document-id)))))


;;;  search
(setf (ningle:route *app* "/search" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
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
             (with-couchdb (client :couchdb-main)
               (dataset-size client star:*couchdb-default-database*
                             :limit limit
                             :start-key (when start-key (jsown:parse start-key))
                             :end-key (when end-key (jsown:parse end-key))
                             :descending descending
                             :reduce reduce
                             :include-docs (if reduce nil t)
                             :skip skip))))))

;; TODO Eventing api 
(setf (ningle:route *app* "/new/event/:id")
      #'(lambda (params)
          (set-default-headers)
          (let ((event)))))


;; TODO configuration object
;; We shoudnt be passing these by hand
(defun start-http-api (&key listen-address
                         api-port
                         couchdb-user
                         couchdb-password
                         couchdb-host
                         couchdb-port
                         http-scheme
                         (server :hunchentoot))
  (let ((*server* (lack:builder
                   (:couchdb-pool :couchdb-main :connect-args `(:host ,couchdb-host
                                                                :port ,couchdb-port
                                                                :scheme ,http-scheme
                                                                :user ,couchdb-user
                                                                :password ,couchdb-password)
                    :pool-args '(:max-open-count 20))
                   :accesslog
                   *app*))
        (context star.actors:*sys*))


    (clack:clackup *server*
                   :server server
                   :address listen-address
                   :port api-port)))
