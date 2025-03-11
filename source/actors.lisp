;;;; ** Actor System

(in-package :star.actors)
(defvar *sys* nil "the main actor system")
(defvar *producer-agent* nil "Rabbitmq Producer")
(defvar *publish-service* nil "Rabbitmq Publish actor")
(defvar *couchdb-agent* nil "Rabbitmq Publish actor")
(defvar *couchdb-gets* nil "Actor responsible for performing individual document GET operations in CouchDB")
(defvar *couchdb-bulk-gets* nil "Actor responsible for performing bulk document GET operations in CouchDB")
(defvar *couchdb-bulk-inserts* nil "Actor responsible for performing bulk document INSERT operations in CouchDB")


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
  (let ((jdata (jsown:val-safe (jsown:parse (cl-couch:get-view client star:*couchdb-default-database* "targets" "actor-targets" (jsown:to-json (jsown:new-js
                                                                                                                                                 ("include_docs" "true"))))) "rows")))
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
(defun start-target-actor (system)
  (setf *targets* (actor-of system
                            :name "*targets*"
                            :receive (lambda (msg)
                                       (let* ((target (cdr msg))
                                              (actor (jsown:val target "actor"))
                                              (delay (jsown:val-safe target "delay")))
                                         (if (not (get-dest-actor actor))
                                             ;; DEPRECATED Use the producer actor
                                             (progn
                                               (star.rabbit:emit-document  "documents" (format nil "actors.~a.new-target" actor)
                                                                           (jsown:to-json target)
                                                                           :host star:*rabbit-address*
                                                                           :port star:*rabbit-port*
                                                                           :username star:*rabbit-user* :password star:*rabbit-password*)))

                                         (if (and (get-dest-actor actor) (jsown:val target "recurring") (first-time-p msg))
                                             (wt:schedule-recurring *target-timer* 0.0 delay (lambda ()
                                                                                               (submit-target target nil))
                                                                    (jsown:val target "target")))
                                         (if (and (get-dest-actor actor) (not (first-time-p msg)))
                                             (route-target target actor)))))))

;;;; Start the target timer
;;;; The target timer handles recurring targets.
(defparameter *target-timer* nil "simple wheel timer for targets")
(defun start-target-timer ()
  (setf *target-timer* (wt:make-wheel-timer :resolution 10 :max-size 1000)))



(defmacro with-json (jobject &body body)
  `(macrolet ((val (key) `(jsown:val-safe ,jobject ,key))
              (dataset () `(jsown:val ,jobject "datast"))
              (date-added () `(jsown:val ,jobject "dateAdded"))
              (date-updated () `(jsown:val ,jobject "dateUpdated"))
              (dtype () `(jsown:val ,jobject "dtype"))
              (parse-doc () `(star.databases.couchdb:from-json ,jobject (intern (jsown:val ,jobject "dtype") :spec))))
     ,@body))



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

  ;; (setf *couchdb-agent* (star.actors.couchdb:make-couchdb-agent *sys*
  ;;                                                               :password couchdb-password
  ;;                                                               :user couchdb-user
  ;;                                                               :port couchdb-port
  ;;                                                               :host couchdb-host
  ;;                                                               :scheme couchdb-scheme))
  ;; (setf *couchdb-gets* (star.actors.couchdb:make-get-service *couchdb-agent* *sys* "couchdb-get"))
  ;; (setf *couchdb-inserts* (star.actors.couchdb:make-insert-service *couchdb-agent* *sys* "couchdb-insert"))
  ;; (setf *couchdb-bulk-gets* (star.actors.couchdb:make-bulk-get-service *couchdb-agent* *sys* "couchdb-bulk-gets"))
  ;; (setf *couchdb-bulk-inserts* (star.actors.couchdb:make-bulk-insert-service *couchdb-agent* *sys* "couchdb-bulk-inserts"))
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*)
  (nhooks:run-hook star:*actors-start-hook*))
;; actor entry point:1 ends here
