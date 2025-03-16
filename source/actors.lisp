;;;; ** Actor System

(in-package :star.actors)
(defvar *sys* nil "the main actor system")
(defvar *events* nil
  "Actor event stream")

(defvar *producer-agent* nil "Rabbitmq Producer")
(defvar *publish-service* nil "Rabbitmq Publish actor")
(defvar *ingest-service* nil "Couchdb Document ingest actor. Apon insertion publishes a new document event.")
(defvar *delete-service* nil "Couchdb Document delete actor. Subscribes to delete events and will delete the document speficed by id and rev.")
(defvar +ingest-queue+ "document-ingest")
(defvar +updates-queue+ "document-updates")
(defvar +new-queue+ "new-document")
(defvar +update-ingest-key+ "documents.update.#")
(defvar +ingest-key+ "documents.injest.#")
(defvar +new-key+ "documents.new.#")
(defvar +targets-key+ "documents.new.target.*")


;;;; *** Target Routing
(defparameter *actor-index-agent* nil "Actor index agent is responsible for registering actors for targets.")

(defun start-actor-index (system)
  "Start the actor index for target routing."
  (setf *actor-index-agent* (make-agent #'serapeum:dict system)))

;;;; Register an actor for recieving target inputs
;;;; Actors must be registered with actor-index before they will get any target messages.
(defun register-actor (actor-name actor-symbol)
  (setf (agent-get *actor-index-agent* #'identity) (serapeum:dict* (agent-get *actor-index-agent*) actor-name actor-symbol)))

;;;; Return the destination actor symbol by actor name string
(defun get-dest-actor (actor)
  (serapeum:@  (agent-get *actor-index-agent* #'identity) actor))

;;;; Send the the target to the destination actor
(defun route-target (target actor)
  (let ((dest (get-dest-actor actor)))
    (format t "got ~a" actor)
    (when dest
      (tell dest target))))

;;;; *** Target Actor
;;;; The target actor is responsible for routing TARGET documents to actors. Actors can reside over rabbitmq or in same proccess with lisp
;; TODO Target services over ZMQ
(defparameter *targets* nil "The Target actor.
It is responsble for routing TARGET documents to actors. Actors can reside over rabbitmq or in same-process with lisp.")

;;;; *** Target Operations
;;;; Fetch targets from database
(defun get-targets (client database)
  (let ((jdata (jsown:val-safe (jsown:parse (cl-couch:get-view client star:*couchdb-default-database* "targets" "actor-targets" (jsown:to-json (jsown:new-js)))))))
    (when (> 0 (length jdata))
      (loop for row in jdata
            for doc = (jsown:val row "doc")
            for actor = (jsown:val doc "actor")
            collect (cons actor doc)))))

;;;; Sumbit the target for execution.
;;;; target actor will route the message to a registered lisp actor or submit to rabbitmq
;;;; REVIEW Why should it care about first time?
(defun sumbit-target (target &optional (first-time t))
  "Create a message for the *targets* actor."
  (tell *targets*  (if first-time
                       (cons t target)
                       (cons nil target))))

;;;; return t if this is the first time we handled this target.
(defun first-time-p (msg)
  (car msg))

;;;; Return t if this target document is transient, which means not to save in database.
(defun target-transient-p (target)
  (when (jsown:val-safe target "transient")
    t))

;;;;  Start the targets loader
(defun start-target-loader ()
  (let (targets (get-targets (anypool:with-connection (client *couchdb-pool*)
                               (get-targets client star:*couchdb-default-database*))))
    (loop for target in targets
          do (submit-target target t))))


;;;; Start the target routing actor.

(defun start-target-actor (system eventstream)
  (setf *targets* (actor-of system
                            :init (lambda (self)
                                    (ev:subscribe eventstream self star.actors.couchdb:target-event))
                            :name "*targets*"
                            :receive (lambda (msg)
                                       (let* ((target (star.actors.couchdb:event-data msg))
                                              (actor (jsown:val target "actor"))
                                              (delay (jsown:val-safe target "delay")))
                                         (cond
                                           ((not (get-dest-actor actor))
                                            (rabbit! star.actors.rabbitmq:*publish-service*
                                                     :body (jsown:to-json target)
                                                     :routing-key (format nil "actors.~a.new-target" actor)))
                                           
                                           ((and (get-dest-actor actor) 
                                                 (jsown:val target "recurring")
                                                 (first-time-p msg))
                                            (wt:schedule-recurring *target-timer* 
                                                                   0.0 
                                                                   delay 
                                                                   (lambda ()
                                                                     (submit-target target nil))
                                                                   (jsown:val target "target")))
                                           
                                           ((and (get-dest-actor actor)
                                                 (not (first-time-p msg)))
                                            (route-target target actor))))))))

;;;; Start the target timer
;;;; The target timer handles recurring targets.
(defparameter *target-timer* nil "simple wheel timer for targets")
(defun start-target-timer ()
  (setf *target-timer* (wt:make-wheel-timer :resolution 10 :max-size 1000)))


;; REVIEW What uses this?
(defmacro with-json (jobject &body body)
  `(macrolet ((val (key) `(jsown:val-safe ,jobject ,key))
              (dataset () `(jsown:val ,jobject "datast"))
              (date-added () `(jsown:val ,jobject "dateAdded"))
              (date-updated () `(jsown:val ,jobject "dateUpdated"))
              (dtype () `(jsown:val ,jobject "dtype"))
              (parse-doc () `(star.databases.couchdb:from-json ,jobject (intern (jsown:val ,jobject "dtype") :spec))))
     ,@body))



(defun insert (client database document)
  (format nil "~a~%" (couch:create-document client database (jsown:to-json* document))))
;; (dex:http-request-conflict (e) (log:warn e))
;; (dex:http-request-unauthorized (e) (log:error e))



(defun transient-p (message)
  (jsown:val-safe (jsown:parse (car message)) "transient"))

(defun insertp (message)
  (null (transient-p message)))

;; TODO Make Target Events
(defun handle-target (self message)
  
  (log:debug "GOT TARGET: ~A" (car message))
  "Handles any new incoming documents and sends it to the appropriate actors."
  (let ((connection (rabbit-stream-connection (consumer-stream self)))
        (body (jsown:parse (car message)))
        (msg-key (cdr message)))
    (tell star.actors:*targets* (cons 1 body))
    (cl-rabbit:basic-ack connection 1 msg-key)))


(defun start-consumers (&key injest-workers rabbit-user rabbit-password rabbit-address rabbit-port eventstream)
  (log:info "Starting Consumers.")
  (flet ((handle-document (self message)
           (let (
                 (connection (rabbit-stream-connection (consumer-stream self)))
                 (document (jsown:parse  (car message)))
                 (msg-key (cdr message)))
             ;; maybe a more in depth actor that handles acking?
             (ev:publish eventstream (star.actors.couchdb:make-ingest-event document))
             (cl-rabbit:basic-ack connection 1 msg-key))))
    (start-consumer
     (create-rabbit-consumer :name "documents"
                             :n injest-workers
                             :queue-name +ingest-queue+
                             :exchange-name "documents" 
                             :routing-key +ingest-key+
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


(defun start-actors (&key rabbit-user rabbit-host rabbit-password rabbit-vhost rabbit-port
                       couchdb-user couchdb-password couchdb-host couchdb-port couchdb-scheme)
  (log:info (setf *sys* (make-actor-system  (or star:*actor-system-config* `(:dispatchers (:shared (:workers 4 :strategy :random)
                                                                                           :rabbitmq (:workers 8 :strategy :round-robin)
                                                                                           :star-actors (:workers 4 :strategy :round-robin)
                                                                                           :ingest (:workers ,star:*injest-workers* :strategy :round-robin))
                                                                             :scheduler (:enabled :true :resolution 100 :max-size 500)
                                                                             :eventstream (:dispatcher-id :shared)
                                                                             :timeout-timer (:resolution 500 :max-size 1000))))))
  ;; HACK Lets Not do this
  (let ((*gc-timer* (wt:make-wheel-timer)))
    (wt:schedule-recurring *gc-timer* 1 3600 (lambda ()
                                               (sb-ext:gc :full t))))

  (log:info (setf *events* (ev:make-eventstream *sys*)))
  ;; Create RabbitMQ producer agent before service
  (setf *producer-agent* (star.actors.rabbitmq:make-producer-agent *sys*
                                                                   :name "default-producer"
                                                                   :host rabbit-host
                                                                   :port rabbit-port
                                                                   :user rabbit-user
                                                                   :password rabbit-password
                                                                   :vhost rabbit-vhost))

  ;; Connect the producer agent
  ;; HACK this is kinda costly
  (star.actors.rabbitmq:producer-agent-connect *producer-agent*)

  ;; Create publish service using connected agent
  (setf *publish-service* (star.actors.rabbitmq:make-publish-service *producer-agent* *publish-service* *sys* :workers 2))

  (setf *couchdb-agent* (star.actors.couchdb:make-couchdb-agent *sys*
                                                                :password couchdb-password
                                                                :user couchdb-user
                                                                :port couchdb-port
                                                                :host couchdb-host
                                                                :scheme couchdb-scheme))
  (setf *ingest-service* (star.actors.couchdb:make-ingest-service :system *sys* :eventstream *events* :agent *couchdb-agent*))
  (setf *delete-service* (star.actors.couchdb:make-delete-service :system *sys* :eventstream *events* :agent *couchdb-agent*))
  (start-actor-index *sys*)
  (start-target-timer)
  (start-target-actor *sys*)

  ;; Rewrite this so that it uses the correct arguments
  (flet ((handle-document (self message)
           (let ((connection (rabbit-stream-connection (consumer-stream self)))
                 (document (jsown:parse (car message)))
                 (msg-key (cdr message)))
             (ev:publish eventstream (star.actors.couchdb:make-ingest-event document))
             (cl-rabbit:basic-ack connection 1 msg-key))))
    
    (start-consumer
     (create-rabbit-consumer :name "documents"
                             :n star:*injest-workers* 
                             :queue-name +ingest-queue+
                             :exchange-name "documents"
                             :routing-key +ingest-key+
                             :username rabbit-user
                             :password rabbit-password
                             :host rabbit-host
                             :port rabbit-port
                             :handler-fn #'handle-document
                             :test-fn #'insertp))
    
    (start-consumer
     (create-rabbit-consumer :name "documents"
                             :n star:*injest-workers*
                             :queue-name "injest-targets" 
                             :exchange-name "documents"
                             :routing-key +targets-key+
                             :username rabbit-user
                             :password rabbit-password 
                             :host rabbit-host
                             :port rabbit-port
                             :handler-fn #'handle-target
                             :test-fn #'insertp)))
  
  (nhooks:run-hook star:*actors-start-hook*))
