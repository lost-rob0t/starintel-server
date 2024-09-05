;; (uiop:define-package   :star.actors.extractors
;;   (:import-from :serapeum :dict :@ :dict*)
;;   (:import-from :star.databases.couchdb :as-json :format-key)
;;   (:use       :cl :star :star.rabbit :star.actors :star.producers :sento.agent :sento.actor :sento.actor-system :sento.actor-context :lparallel)
;;   (:documentation "doc")
;;   (:export
;;    #:extractor
;;    #:extractor-handlers
;;    #:extractor-fields
;;    #:make-extractor
;;    #:start-url-consumer))

;; (in-package :star.actors.extractors)

;; (defparameter *extractor-actor* nil "Actor that extracts data from messages and socialmposts")
;; (defparameter *extractor-fields* '("content"))
;; (defparameter *url-regex* (ppcre:create-scanner "(.*)/.*"))
;; (defclass extractor (star.producers:producer star.rabbit:rabbit-consumer)
;;   ((handlers :initarg :handlers :initform (dict) :type hash-table :accessor extractor-handlers)
;;    (fields :initarg :fields :initform nil :type list :accessor extractor-fields)))

;; (defun make-extractor (&rest args &key handlers fields stream name fn exchange-name user password host port &allow-other-keys)
;;   (apply #'make-instance 'extractor
;;          :handlers (or handlers (serapeum:dict "content" #'content-handler))
;;          :fields (or fields '("content"))
;;          :stream stream
;;          :name name
;;          :fn (or fn #'handle-message)
;;          :exchange-name (or exchange-name "documents")
;;          :user user
;;          :password password
;;          :host host
;;          :port port
;;          args))



;; (defmethod consumer-read ((extractor extractor))
;;   "Reads a message from the RabbitMQ stream."
;;   (star.consumers:with-consumer-lock (extractor)
;;     (let ((msg (star.rabbit::stream-read (star.consumers:consumer-stream extractor))))
;;       (cons (babel:octets-to-string (cl-rabbit:message/body (cl-rabbit:envelope/message msg)) :encoding :utf-8)
;;             (cl-rabbit:envelope/delivery-tag msg)))))

;; (defmethod consume ((extractor extractor) message)
;;   "Processes a message using the handle-message function."
;;   (funcall (star.consumers:consumer-fn extractor) extractor message))

;; (defmethod start-consumer ((extractor extractor))
;;   (bt:make-thread (lambda ()
;;                     (loop
;;                       for data = (consumer-read extractor)
;;                       do (consume extractor data)
;;                       do (receive-result (star.consumers:consumer-channel extractor))))
;;                   :name (star.consumers:consumer-name extractor)))


;; (defmethod producer-publish ((extractor extractor) &key body routing-key (properties nil))
;;   "Publishes a message to the RabbitMQ exchange."
;;   (with-producer-lock (extractor)
;;     (cl-rabbit:basic-publish (producer-conn extractor) 1 :exchange (producer-exchange extractor) :body body :properties properties :routing-key routing-key)))

;; (defun handle-message (extractor message)
;;   "Handles any new incoming documents and sends it to the appropriate actors."
;;   (let ((connection (rabbit-stream-connection (star.consumers:consumer-stream extractor)))
;;         (body (jsown:parse (car message)))
;;         (msg-key (cdr message)))
;;     (loop for field in (extractor-fields extractor)
;;           for value = (jsown:val-safe body field)
;;           for handler = (serapeum:@ (extractor-handlers extractor) field)
;;           for result = (when value (funcall handler body value))
;;           do (progn
;;                (print "1")
;;                (when result
;;                  (producer-publish extractor :body (getf result :relation) :routing-key "documents.new.relation")
;;                  (producer-publish extractor :body (getf result :result) :routing-key (format nil "documents.new.~a" (getf result :dtype)))
;;                  (print result)
;;                  (force-output))))
;;     (cl-rabbit:basic-ack connection 1 msg-key)))

;; (defun content-handler (source-document  value)
;;   (handler-case (let ((urls (ppcre:all-matches-as-strings *url-regex* value)))

;;                   (loop for url in urls
;;                         collect (let* ((dataset (jsown:val source-document "dataset"))
;;                                        (url-doc (make-instance 'spec:url
;;                                                                :url url
;;                                                                :dtype "url"
;;                                                                :dataset (jsown:val source-document "dataset")
;;                                                                :date-added (jsown:val source-document "dateAdded")
;;                                                                :date-updated (spec:unix-now)
;;                                                                :sources '("extractor")
;;                                                                :content ""))

;;                                        (relation-doc (make-instance 'spec:relation :sources '("extractor") :dtype "relation" :dataset dataset :date-added (spec:unix-now) :date-updated (spec:unix-now) :source  (jsown:val source-document "_id") :target (spec:doc-id url-doc) :id (cms-ulid:ulid) :note "extracted")))
;;                                   (list :result (jsown:to-json (as-json url-doc))
;;                                         :dtype "url"
;;                                         :relation (jsown:to-json (as-json relation-doc))))))
;;     (error (condition)
;;       (error "Failed to handle document: ~A" condition))))


;; (defun start-url-consumer (&key (host "localhost") (port 5672) (username "guest") (password "guest") (n 1))
;;   (loop for i from 1 to n
;;         do (format t "starting: extractor ~a~%" i)
;;         do (let* ((stream (make-instance 'rabbit-queue-stream
;;                                          :host host
;;                                          :port port
;;                                          :user username
;;                                          :password password
;;                                          :queue-name "injest-urls"
;;                                          :exchange-name "documents"
;;                                          :routing-key "documents.new.message"))
;;                   (extractor (make-extractor :stream stream
;;                                              :name (format nil "~a-~a" "urlextractor" i)
;;                                              :host host
;;                                              :fields '("content")
;;                                              :handlers (dict "content" #'content-handler)
;;                                              :fn #'handle-message
;;                                              :exchange-name "documents"
;;                                              :exchange-type "topic"
;;                                              :exchange-durable t
;;                                              :port port
;;                                              :user username
;;                                              :password password)))
;;              (open-stream stream)
;;              (producer-connect extractor)
;;              (start-consumer extractor))))
