(in-package :star.actors)

(defparameter +couchdb-storage-dispatcher+ :pinned
  "Dispatcher used for blocking CouchDB actor work.")

(defstruct (couchdb-get-request
            (:constructor make-couchdb-get-request
                (&key
                   (database star:*couchdb-default-database*)
                   document-id
                   revision)))
  "Deferred CouchDB GET command executed by the couchdb service actor."
  database
  document-id
  revision)

;; Accessor documentation for couchdb-get-request
(setf (documentation 'COUCHDB-GET-REQUEST-DATABASE 'function)
"The =database= slot of =couchdb-get-request=.")
(setf (documentation 'COUCHDB-GET-REQUEST-DOCUMENT-ID 'function)
"The =document-id= slot of =couchdb-get-request=.")
(setf (documentation 'COUCHDB-GET-REQUEST-REVISION 'function)
"Expected revision carried by the deferred get, or nil.")



(defstruct (couchdb-insert-request
            (:constructor make-couchdb-insert-request
                (&key
                   (database star:*couchdb-default-database*)
                   document-id
                   document)))
  "Deferred CouchDB insert command executed by the couchdb service actor."
  database
  document-id
  document)

;; Accessor documentation for couchdb-insert-request
(setf (documentation 'COUCHDB-INSERT-REQUEST-DATABASE 'function)
"The =database= slot of =couchdb-insert-request=.")
(setf (documentation 'COUCHDB-INSERT-REQUEST-DOCUMENT 'function)
"The JSON document payload for the deferred insert.")
(setf (documentation 'COUCHDB-INSERT-REQUEST-DOCUMENT-ID 'function)
"The =document-id= slot of =couchdb-insert-request=.")



(defstruct (couchdb-delete-request
            (:constructor make-couchdb-delete-request
                (&key
                   (database star:*couchdb-default-database*)
                   document-id
                   revision)))
  "Deferred CouchDB DELETE command executed by the couchdb service actor."
  database
  document-id
  revision)

;; Accessor documentation for couchdb-delete-request
(setf (documentation 'COUCHDB-DELETE-REQUEST-DATABASE 'function)
"The =database= slot of =couchdb-delete-request=.")
(setf (documentation 'COUCHDB-DELETE-REQUEST-DOCUMENT-ID 'function)
"The =document-id= slot of =couchdb-delete-request=.")
(setf (documentation 'COUCHDB-DELETE-REQUEST-REVISION 'function)
"Optimistic concurrency revision for the deferred delete.")



(defstruct (couchdb-result
            (:constructor make-couchdb-result
                (&key status operation database document-id revision value
                      error-type error-message)))
  "Result envelope returned by couchdb service actor handlers."
  status
  operation
  database
  document-id
  revision
  value
  error-type
  error-message)

;; Accessor documentation for couchdb-result
(setf (documentation 'COUCHDB-RESULT-DATABASE 'function)
"The =database= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-DOCUMENT-ID 'function)
"The =document-id= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-ERROR-MESSAGE 'function)
"Human readable failure reason, when the result failed.")
(setf (documentation 'COUCHDB-RESULT-ERROR-TYPE 'function)
"The =error-type= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-OPERATION 'function)
"The =operation= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-REVISION 'function)
"The =revision= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-STATUS 'function)
"The =status= slot of =couchdb-result=.")
(setf (documentation 'COUCHDB-RESULT-VALUE 'function)
"The =value= slot of =couchdb-result=.")



(defun make-couchdb-agent (context pool
                           &key error-fun
                             (dispatcher-id +couchdb-storage-dispatcher+))
  "Wrap the injected CouchDB POOL in an agent on the storage dispatcher."
  (declare (ignore error-fun))
  (unless pool
    (error "MAKE-COUCHDB-AGENT requires an injected CouchDB pool."))
  (make-agent (lambda () pool) context dispatcher-id))

(defun start-couchdb-agent (system
                            &optional
                              (pool star.databases.couchdb:*couchdb-pool*))
  "Start the CouchDB pool agent without creating an unused standalone client."
  (setf *couchdb-agent* (make-couchdb-agent system pool)))

(defun couchdb-agent-get (agent database document-id &optional revision)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (if revision
        (cl-couch:get-document client database document-id revision)
        (cl-couch:get-document client database document-id))))

(defun parse-couchdb-document (document)
  (etypecase document
    (string (jsown:parse document))
    (list document)))

(defun delete-couchdb-document (client database document-id
                                 &optional revision
                                 &key
                                   (get-fn #'cl-couch:get-document)
                                   (delete-fn #'cl-couch:delete-document))
  "Delete DOCUMENT-ID using REVISION, fetching the current revision when absent."
  (let* ((resolved-revision
           (or revision
               (let* ((document (funcall get-fn client database document-id))
                      (parsed (parse-couchdb-document document)))
                 (jsown:val-safe parsed "_rev")))))
    (unless resolved-revision
      (error "CouchDB document ~a/~a has no revision."
             database document-id))
    (values (funcall delete-fn
                     client database document-id resolved-revision)
            resolved-revision)))

(defun couchdb-agent-delete (agent database document-id &optional revision)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (delete-couchdb-document client database document-id revision)))

(defun normalize-couchdb-get-request (message)
  (typecase message
    (couchdb-get-request message)
    (string
     (make-couchdb-get-request :document-id message))
    (list
     (make-couchdb-get-request
      :database (or (getf message :database)
                    star:*couchdb-default-database*)
      :document-id (or (getf message :document-id)
                       (getf message :id))
      :revision (or (getf message :revision)
                    (getf message :rev))))
    (t
     (error "Unsupported CouchDB GET request: ~s" message))))

(defun normalize-couchdb-insert-request (message)
  (typecase message
    (couchdb-insert-request message)
    (list
     (make-couchdb-insert-request
      :database (or (getf message :database)
                    star:*couchdb-default-database*)
      :document-id (or (getf message :document-id)
                       (getf message :id))
      :document (getf message :document)))
    (t
     (error "Unsupported CouchDB INSERT request: ~s" message))))

(defun normalize-couchdb-delete-request (message)
  (typecase message
    (couchdb-delete-request message)
    (list
     (make-couchdb-delete-request
      :database (or (getf message :database)
                    star:*couchdb-default-database*)
      :document-id (or (getf message :document-id)
                       (getf message :id))
      :revision (or (getf message :revision)
                    (getf message :rev))))
    (t
     (error "Unsupported CouchDB DELETE request: ~s" message))))

(defun ensure-couchdb-request-id (operation document-id)
  (unless (and (stringp document-id) (plusp (length document-id)))
    (error "CouchDB ~a request requires a non-empty document id."
           operation)))

(defun couchdb-error-result (operation database document-id condition)
  (make-couchdb-result
   :status :error
   :operation operation
   :database database
   :document-id document-id
   :error-type (string-downcase (princ-to-string (type-of condition)))
   :error-message (princ-to-string condition)))

(defun complete-couchdb-request (result)
  "Return RESULT for ASK-S and explicitly reply only to a real async sender."
  (when *sender*
    (reply result *sender*))
  result)

(defun make-couchdb-get-handler (agent &key (get-fn #'couchdb-agent-get))
  "Actor handler for deferred CouchDB reads."
  (lambda (message)
    (let ((request nil))
      (complete-couchdb-request
       (handler-case
           (progn
             (setf request (normalize-couchdb-get-request message))
             (ensure-couchdb-request-id
              :get
              (couchdb-get-request-document-id request))
             (make-couchdb-result
              :status :success
              :operation :get
              :database (couchdb-get-request-database request)
              :document-id (couchdb-get-request-document-id request)
              :revision (couchdb-get-request-revision request)
              :value (funcall get-fn
                              agent
                              (couchdb-get-request-database request)
                              (couchdb-get-request-document-id request)
                              (couchdb-get-request-revision request))))
         (dexador:http-request-not-found ()
           (make-couchdb-result
            :status :not-found
            :operation :get
            :database (and request (couchdb-get-request-database request))
            :document-id (and request
                              (couchdb-get-request-document-id request))))
         (error (condition)
           (couchdb-error-result
            :get
            (and request (couchdb-get-request-database request))
            (and request (couchdb-get-request-document-id request))
            condition)))))))

(defun make-couchdb-insert-handler
    (agent
     &key
       (exists-fn #'couchdb-document-exists-p)
       (insert-fn #'couchdb-agent-insert))
  "Actor handler for deferred CouchDB inserts."
  (lambda (message)
    (let ((request nil))
      (complete-couchdb-request
       (handler-case
           (progn
             (setf request (normalize-couchdb-insert-request message))
             (ensure-couchdb-request-id
              :insert
              (couchdb-insert-request-document-id request))
             (unless (couchdb-insert-request-document request)
               (error "CouchDB INSERT request requires a document."))
             (if (funcall exists-fn
                          agent
                          (couchdb-insert-request-database request)
                          (couchdb-insert-request-document-id request))
                 (make-couchdb-result
                  :status :exists
                  :operation :insert
                  :database (couchdb-insert-request-database request)
                  :document-id (couchdb-insert-request-document-id request))
                 (make-couchdb-result
                  :status :success
                  :operation :insert
                  :database (couchdb-insert-request-database request)
                  :document-id (couchdb-insert-request-document-id request)
                  :value (funcall insert-fn
                                  agent
                                  (couchdb-insert-request-database request)
                                  (couchdb-insert-request-document request)))))
         (dexador:http-request-conflict ()
           (make-couchdb-result
            :status :conflict
            :operation :insert
            :database (and request (couchdb-insert-request-database request))
            :document-id (and request
                              (couchdb-insert-request-document-id request))))
         (error (condition)
           (couchdb-error-result
            :insert
            (and request (couchdb-insert-request-database request))
            (and request (couchdb-insert-request-document-id request))
            condition)))))))

(defun make-couchdb-delete-handler (agent &key (delete-fn #'couchdb-agent-delete))
  "Actor handler for deferred CouchDB deletes."
  (lambda (message)
    (let ((request nil))
      (complete-couchdb-request
       (handler-case
           (progn
             (setf request (normalize-couchdb-delete-request message))
             (ensure-couchdb-request-id
              :delete
              (couchdb-delete-request-document-id request))
             (multiple-value-bind (value revision)
                 (funcall delete-fn
                          agent
                          (couchdb-delete-request-database request)
                          (couchdb-delete-request-document-id request)
                          (couchdb-delete-request-revision request))
               (make-couchdb-result
                :status :success
                :operation :delete
                :database (couchdb-delete-request-database request)
                :document-id (couchdb-delete-request-document-id request)
                :revision revision
                :value value)))
         (dexador:http-request-not-found ()
           (make-couchdb-result
            :status :not-found
            :operation :delete
            :database (and request (couchdb-delete-request-database request))
            :document-id (and request
                              (couchdb-delete-request-document-id request))))
         (dexador:http-request-conflict ()
           (make-couchdb-result
            :status :conflict
            :operation :delete
            :database (and request (couchdb-delete-request-database request))
            :document-id (and request
                              (couchdb-delete-request-document-id request))
            :revision (and request
                           (couchdb-delete-request-revision request))))
         (error (condition)
           (couchdb-error-result
            :delete
            (and request (couchdb-delete-request-database request))
            (and request (couchdb-delete-request-document-id request))
            condition)))))))

(defun start-couchdb-gets (system)
  (setf *couchdb-gets*
        (actor-of system
                  :name "*couchdb-gets*"
                  :dispatcher +couchdb-storage-dispatcher+
                  :receive (make-couchdb-get-handler *couchdb-agent*))))

(defun start-couchdb-inserts (system)
  (setf *couchdb-inserts*
        (actor-of system
                  :name "*couchdb-inserts*"
                  :dispatcher +couchdb-storage-dispatcher+
                  :receive (make-couchdb-insert-handler *couchdb-agent*))))

(defvar *couchdb-deletes* nil
  "Actor responsible for deterministic CouchDB delete requests.")

(defun start-couchdb-deletes (system)
  "Start the actor handling deferred CouchDB deletes."
  (setf *couchdb-deletes*
        (actor-of system
                  :name "*couchdb-deletes*"
                  :dispatcher +couchdb-storage-dispatcher+
                  :receive (make-couchdb-delete-handler *couchdb-agent*))))

(defun start-couchdb-delete-actor-hook ()
  (start-couchdb-deletes *sys*))

(nhooks:add-hook star:*actors-start-hook* #'start-couchdb-delete-actor-hook)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(couchdb-get-request
            make-couchdb-get-request
            couchdb-get-request-database
            couchdb-get-request-document-id
            couchdb-get-request-revision
            couchdb-insert-request
            make-couchdb-insert-request
            couchdb-insert-request-database
            couchdb-insert-request-document-id
            couchdb-insert-request-document
            couchdb-delete-request
            make-couchdb-delete-request
            couchdb-delete-request-database
            couchdb-delete-request-document-id
            couchdb-delete-request-revision
            couchdb-result
            couchdb-result-status
            couchdb-result-operation
            couchdb-result-database
            couchdb-result-document-id
            couchdb-result-revision
            couchdb-result-value
            couchdb-result-error-type
            couchdb-result-error-message
            make-couchdb-get-handler
            make-couchdb-insert-handler
            make-couchdb-delete-handler
            delete-couchdb-document
            *couchdb-deletes*
            start-couchdb-deletes)
          :star.actors))

(setf (documentation 'MAKE-COUCHDB-GET-REQUEST 'function)
  "Build a deferred CouchDB get request for DATABASE and DOCUMENT-ID.")

(setf (documentation 'MAKE-COUCHDB-INSERT-REQUEST 'function)
  "Build a deferred CouchDB insert request carrying DOCUMENT.")

(setf (documentation 'MAKE-COUCHDB-DELETE-REQUEST 'function)
  "Build a deferred CouchDB delete request for DOCUMENT-ID at REVISION.")
