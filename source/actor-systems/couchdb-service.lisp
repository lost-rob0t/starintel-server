(uiop:define-package   :star.actors.couchdb
  (:use       :star :cl :star.databases.couchdb)
  (:documentation "doc")
  (:export
   #:make-couchdb-agent
   #:couchdb-agent-pool
   #:pool-stats
   #:with-couchdb-agent
   #:define-couchdb-service
   #:make-update-event
   #:make-ingest-event
   #:make-delete-event
   #:ingest-event
   #:event-data
   #:update-event
   #:event-rev
   #:make-delete-service
   #:make-ingest-service
   #:target-event
   #:make-target-event))

(in-package :star.actors.couchdb)


(defun make-couchdb-agent (context
                           &key (user "admin")
                             (password "password")
                             (host "localhost")
                             (port 5984)
                             (scheme "http")
                             (max-idle 2)
                             (max-cons 10))
  (sento.agent:make-agent (lambda ()
                            (anypool:make-pool :name "couchdb-connections"
                                               :connector (lambda ()
                                                            (let ((client (cl-couch:new-couchdb host port :scheme scheme)))
                                                              (cl-couch:password-auth client user password)
                                                              client))

                                               :max-idle-count max-idle
                                               :disconnector (lambda (obj)
                                                               (setf (cl-couch:couchdb-headers obj) nil))
                                               :max-open-count max-cons))))



(defun couchdb-agent-pool (agent)
  "Get the couchdb client pool for use with cl-couch"
  (agt:agent-get agent #'identity))

(defun pool-stats (agent)
  (let ((pool (couchdb-agent-pool agent)))
    (log:info "Active conns in pool: ~A" (anypool:pool-active-count pool))
    (log:info "IDLE: ~A" (anypool:pool-idle-count pool))))

(defmacro with-couchdb-agent ((client agent) &body body)
  `(let ((pool (couchdb-agent-pool ,agent)))
     (anypool:with-connection (,client pool)
       ,@body)))


(defclass ingest-event ()
  ((data :initarg :data :accessor event-data))
  (:documentation "Event that will insert a document into the database"))

(defclass update-event ()
  ((data :initarg :data :accessor event-data)
   (_rev :initarg :rev :accessor event-rev))

  (:documentation "Event that will insert a document into the database"))

(defclass delete-event ()
  ((_id :initarg :id :accessor event-id)
   (_rev :initarg :rev :accessor event-rev))
  (:documentation "Document Delete Event"))

(defun make-update-event (&key data rev)
  "Create an ingest event for publishing to the event stream."
  (make-instance 'update-event :data data :rev rev))

(defun make-ingest-event (data)
  "Create an ingest event for publishing to the event stream."
  (make-instance 'ingest-event :data data))

(defun make-delete-event (&key id rev)
  "Create a delete event for publishing to the event stream."
  (make-instance 'delete-event :id id :rev rev))

(defclass target-event ()
  ((data :initarg :data :accessor event-data))
  (:documentation "Event to listen for target documents"))

(defun make-target-event (&key data target)
  "Create a target event for publishing to the event stream."
  (make-instance 'target-event :data data :target target))

(defun make-ingest-service (&key system eventstream agent (name "ingest") (dispatcher-id :couchdb))
  "Create a CouchDB document ingestion service actor.
   Uses futures for async operations and handles common CouchDB errors."
  (ac:actor-of system
               :name name
               :init (lambda (self)
                       (ev:subscribe eventstream self 'ingest-event))
               :receive (lambda (self msg)
                          (declare (ignore self))
                          (let ((doc (event-data msg)))
                            (with-couchdb-agent (client agent)
                              (tasks:with-context (system dispatcher-id)
                                (tasks:task-async (lambda ()
                                                    (handler-case
                                                        (let* ((rev (jsown:val (cl-couch:create-document client star:*couchdb-default-database* doc) "rev")))
                                                          (jsown:extend-js doc ("_rev" rev)))
                                                      (dex:http-request-conflict (e) (declare (ignore e)))))

                                                  :on-complete-fun (lambda (result)
                                                                     (ev:publish eventstream (list :type (jsown:val result "dtype")
                                                                                                   :doc result))))))))))

(defun make-delete-service (&key system eventstream agent (name "delete") (dispatcher-id :couchdb))
  "Create a CouchDB document deleteion service actor.
   Uses futures for async operations and handles common CouchDB errors."
  (ac:actor-of system
               :name name
               :init (lambda (self)
                       (ev:subscribe eventstream self 'delete-event))
               :receive (lambda (self msg)
                          (declare (ignore self))
                          (let ((rev (event-rev msg))
                                (id (event-id msg)))
                            (with-couchdb-agent (client agent)
                              (tasks:with-context (system dispatcher-id)
                                (let ((insert-task (tasks:task-async (lambda ()
                                                                       (cl-couch:delete-document client star:*couchdb-default-database* id rev))


                                                                     :on-complete-fun (lambda (result)
                                                                                        (when (consp result)
                                                                                          (log:error result)))))))))))))

