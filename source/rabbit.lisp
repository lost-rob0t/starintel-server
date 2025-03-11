(in-package :star.rabbit)

(defvar +injest-queue+ "injest-documents")
(defvar +updates-queue+ "documents-updates")
(defvar +new-queue+ "documents-new")
(defvar +injest-key+ "documents.injest.#")
(defvar +new-key+ "documents.new.#")
(defvar +update-key+ "documents.update.#")
(defvar +targets-key+ "documents.new.target.*")
;; TODO Finish migrating consumers
;; 
(defvar *couchdb-pool* nil)



(defun message->string (msg &key (encoding :utf-8))
  "take a rabbitmq message and return the boddy as a string"
  (babel:octets-to-string (cl-rabbit:message/body msg) :encoding encoding))

                                        ;TODO

(defun handle-new-document (msg)
  "Handles any new incoming documents and sends it to the appropriate actors."
  (let* ((props (cl-rabbit:message/properties msg))
         (dtype (assoc :type props :test #'equal))
         (body (jsown:parse (message->string msg))))
    (cons (cdr dtype) body)))


(defun insert (client database document)
  (format nil "~a~%" (couch:create-document client database (jsown:to-json* document))))
;; (dex:http-request-conflict (e) (log:warn e))
;; (dex:http-request-unauthorized (e) (log:error e))

(defun handle-document (self message)
  (log:debug "GOT DOCUMENT: ~A" (car message))
  (let* (
         (connection (rabbit-stream-connection (consumer-stream self)))
         (document (car message))
         (msg-key (cdr message))
         ;; TODO research how to use futures in sento
         (fut (future:with-fut (act:? star.actors:*couchdb-inserts* (list :document document
                                                                          :database star:*couchdb-default-database*)))))
    (future:fcompleted (future:frecover fut
                                        (error (e) (log:error e))
                                        (dex:http-request-conflict (e) (declare (ignore e)))) (result)
      (cl-rabbit:basic-ack connection 1 msg-key))))



(defun transient-p (message)
  (jsown:val-safe (jsown:parse (car message)) "transient"))

(defun insertp (message)
  (null (transient-p message)))


;; Handle New Document consumers:2 ends here

;; [[file:../source.org::*Handle New Target consumers][Handle New Target consumers:1]]
(defun handle-target (self message)
  
  (log:debug "GOT TARGET: ~A" (car message))
  "Handles any new incoming documents and sends it to the appropriate actors."
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (body (jsown:parse (car message)))
        (msg-key (cdr message)))
    (tell star.actors:*targets* (cons 1 body))
    (cl-rabbit:basic-ack connection 1 msg-key)))


(defun start-consumers (&key injest-workers rabbit-user rabbit-password rabbit-address rabbit-port couchdb-host couchdb-port couchdb-scheme couchdb-user couchdb-password)
  (log:info "Starting Consumers.")
  (let ((*couchdb-pool* (anypool:make-pool :name "consumers"
                                           :connector (lambda ()
                                                        (let ((client (cl-couch:new-couchdb couchdb-host couchdb-port :scheme (string-downcase couchdb-scheme))))
                                                          (cl-couch:password-auth client couchdb-user couchdb-password)
                                                          client))
                                           :disconnector (lambda (obj)
                                                           (setf (cl-couch:couchdb-headers obj) nil))
                                           :max-open-count 20)))
    (start-consumer
     (create-rabbit-consumer :name "documents"
                             :n injest-workers
                             :queue-name "injest"
                             :exchange-name "documents" 
                             :routing-key +injest-key+
                             :username rabbit-user
                             :password rabbit-password
                             :host rabbit-address
                             :port rabbit-port
                             :handler-fn #'handle-document
                             :test-fn #'insertp))
    (start-consumer
     (create-rabbit-consumer :name "documents"
                             :n injest-workers
                             :queue-name "injest-targets"
                             :exchange-name "documents"
                             :routing-key +targets-key+
                             :username rabbit-user
                             :password rabbit-password
                             :host rabbit-address
                             :port rabbit-port
                             :handler-fn #'handle-target
                             :test-fn #'insertp))))
