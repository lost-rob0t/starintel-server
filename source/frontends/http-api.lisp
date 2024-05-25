;; http-api
(in-package :star.frontends.http-api)

(defparameter *app* (make-instance 'ningle:app))

(defparameter *rabbitmq-conn* nil)
(defparameter *rabbit-lock* (bt:make-lock "rabbitmq-conn"))


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


(defun disconnect-rabbitmq ()
  (when *rabbitmq-conn*
    (cl-rabbit:destroy-connection *rabbitmq-conn*)
    (setf *rabbitmq-conn* nil)))

;; Incase i need it?
(defmacro with-rabbitmq ((connection) &body body)
  `(let ((conn ,connection))
     (bt:with-lock-held (*rabbit-lock*)
       ,@body)))








(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'(lambda (params)
          (let ((targets (loop for row in (anypool:with-connection (client *couchdb-pool*)
                                            (jsown:val (anypool:with-connection (client *couchdb-pool*) (query-view client *couchdb-default-database* "targets" "by_actor" :include-docs t :key (cdr (assoc :actor params :test #'string=)))) "rows"))
                               collect (jsown:val row "doc"))))
            (jsown:to-json targets))))

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

          (let* ((dtype  (cdr (assoc :dtype params :test #'string=)))
                 (body (babel:octets-to-string  (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.~a" dtype)))
            (with-rabbitmq (*rabbitmq-conn*)
              (cl-rabbit:basic-publish *rabbitmq-conn* 1 :routing-key routing-key :exchange "documents" :properties (list (cons :type dtype)) :body body))

            body)))

(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)
          (let ((document-id  (cdr (assoc :id params :test #'string=))))
            (anypool:with-connection (client *couchdb-pool*)
              (cl-couch:get-document client star:*couchdb-default-database* document-id)))))


;;;  search
(setf (ningle:route *app* "/search" :method :get)
      #'(lambda (params)
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
            (jsown:to-json
             (handler-case
                 (anypool:with-connection (client *couchdb-pool*)
                   (cl-couch:fts-search* client (jsown:to-json query) db ddoc search-name))
               (dex:http-request-bad-request (e) (jsown:new-js
                                                  ("error" t)
                                                  ("msg" "Invalid Query was sent")))
               (usocket:timeout-error (e) (jsown:new-js
                                           ("error" t)
                                           ("msg" "couchdb query timed out"))))))))



;; Views api
(setf (ningle:route *app* "/documents/messages/by-user" :method :get)
      #'(lambda (params)
          (let ((user (cdr (assoc "user" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
               (messages-by-user client star:*couchdb-default-database*
                                 :limit limit
                                 :start-key (when start-key (jsown:parse start-key))
                                 :end-key (when end-key (jsown:parse end-key))
                                 :key user
                                 :descending descending
                                 :skip skip))))))

(setf (ningle:route *app* "/documents/messages/:group/:channel" :method :get)
      #'(lambda (params)
          (let ((channel (cdr (assoc :channel params :test #'string=)))
                (group (cdr (assoc :group params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0")))
                (reduce (equal (cdr (assoc "reduce" params :test #'string=)) "true")))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
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
          (let ((platform (cdr (assoc "platform" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
               (messages-by-platform client star:*couchdb-default-database*
                                     :limit limit
                                     :start-key (when start-key (jsown:parse start-key))
                                     :end-key (when end-key (jsown:parse end-key))
                                     :key platform
                                     :descending descending
                                     :skip skip))))))

(setf (ningle:route *app* "/documents/messages/by-group" :method :get)
      #'(lambda (params)
          (let ((group (cdr (assoc "group" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
               (messages-by-group client star:*couchdb-default-database*
                                  :limit limit
                                  :start-key (when start-key (jsown:parse start-key))
                                  :end-key (when end-key (jsown:parse end-key))
                                  :key group
                                  :descending descending
                                  :skip skip))))))

(setf (ningle:route *app* "/documents/socialmpost/by-user" :method :get)
      #'(lambda (params)
          (let ((user (cdr (assoc "user" params :test #'string=)))
                (limit (parse-integer (or (cdr (assoc "limit" params :test #'string=)) "50")))
                (start-key (cdr (assoc "start_key" params :test #'string=)))
                (end-key (cdr (assoc "end_key" params :test #'string=)))
                (descending (equal (cdr (assoc "descending" params :test #'string=)) "true"))
                (skip (parse-integer (or (cdr (assoc "skip" params :test #'string=)) "0"))))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
               (social-posts-by-user client star:*couchdb-default-database*
                                     :limit limit
                                     :start-key (when start-key (jsown:parse start-key))
                                     :end-key (when end-key (jsown:parse end-key))
                                     :key user
                                     :descending descending
                                     :skip skip))))))

(setf (ningle:route *app* "/dataset-size" :method :get)
      #'(lambda (params)
          (let ((dataset (cdr (assoc "dataset" params :test #'string=))))
            (jsown:to-json
             (anypool:with-connection (client *couchdb-pool*)
               (dataset-size client star:*couchdb-default-database*
                             :key dataset
                             :reduce t))))))

(defparameter *server* (lack:builder
                        :accesslog
                        *app*))

(defun start-http-api ()
  (handler-case
      (progn
        (connect-rabbitmq)
        (clack:clackup *server*
                       :server :hunchentoot
                       :address star:*http-api-address*
                       :port star:*http-api-port*))))
;; (error (e)
;;   (progn

;;     (cl-rabbit:channel-close *rabbitmq-conn* 1)
;;     (disconnect-rabbitmq)))
