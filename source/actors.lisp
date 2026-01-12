;;;; ** Actor System

(in-package :star.actors)
(defparameter *sys* nil "the main actor system")

(defun start-actor-system ()
  "Start the actors."
  (setf *sys* (make-actor-system `(:dispatchers
                                   (:pinned (:workers ,star:*injest-workers* :strategy :random))
                                   :timeout-timer
                                   (:resolution 500 :max-size 1000)
                                   :eventstream
                                   (:dispatcher-id :shared)
                                   :scheduler
                                   (:enabled :true :resolution 100 :max-size 500)))))


;;;; *** Target Routing
(defparameter *actor-index-agent* nil "Actor index agent is responsible for registering actors for targets.")

(defun start-actor-index (system)
  "Start the actor index for target routing."
  (log:info "Starting actor index for target routing")
  (setf *actor-index-agent* (make-agent #'serapeum:dict system))
  (log:info "Actor index started successfully"))

;;;; Register an actor for recieving target inputs
;;;; Actors must be registered with actor-index before they will get any target messages.
(defun register-actor (actor-name actor-symbol)
  (log:info "Registering actor: ~a -> ~a" actor-name actor-symbol)
  (setf (agent-get *actor-index-agent* #'identity) (serapeum:dict* (agent-get *actor-index-agent*) actor-name actor-symbol))
  (log:debug "Actor registered successfully: ~a" actor-name))

;;;; Return the destination actor symbol by actor name string
(defun get-dest-actor (actor)
  (let ((dest (serapeum:@  (agent-get *actor-index-agent* #'identity) actor)))
    (log:debug "Looking up destination actor for: ~a -> ~a" actor dest)
    dest))

;;;; Send the the target to the destination actor
(defun route-target (target actor)
  (log:info "Routing target to actor: ~a" actor)
  (let ((dest (get-dest-actor actor)))
    (log:debug "Destination actor lookup result: ~a" dest)
    (if dest
        (progn
          (log:info "Sending target to destination actor: ~a" dest)
          (tell dest target)
          (log:debug "Target sent successfully to: ~a" dest))
        (log:warn "No destination actor found for: ~a" actor))))

;;;; *** Couchdb Actors
;;;; These are sorta kinda maybe deprecated.
;;;; In the future these will not be removed, but instead re-worked, outside of consumers these can provide feedback to incase db op failed
;;;; Consumers just consume and no way to really provide said feedback.
(defparameter *couchdb-agent* nil)
(defun make-couchdb-agent (context client
                           &key (error-fun nil) (dispatcher-id :shared))
  (make-agent (lambda ()
                star.databases.couchdb:*couchdb-pool*)))


;;;; Get the couchdb client for use with cl-couch
(defun couchdb-agent-client (agent)
  "Get the couchdb client for use with cl-couch"
  (agt:agent-get agent #'identity))

;;;; Preform a insert operation into couchdb.
(defun couchdb-agent-insert (agent database document)
  "Preform a insert operation into couchdb."
  (anypool:with-connection (client (couchdb-agent-client agent))
    (format t "~a~%" (jsown:to-json document))
    (force-output t)
    (cl-couch:create-document client database document)))

;;;; Preform a update operation on couchdb. You must provide the revision tag.
;;;; Couchdb uses the _rev tag. you can learn more about docment revisions here
;;;; https://dba.stackexchange.com/a/299078
(defun couchdb-agent-update (agent database document revision)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (cl-couch:create-document client database (jsown:to-json
                                               (jsown:extend-js (jsown:parse document)
                                                 ("_rev" revision))))))
;;;; Preform a delete operation on couchdb.
(defun couchdb-agent-delete (agent database document-id)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (cl-couch:delete-document client database document-id)))

;;;;Couchdb views are key-value btrees that are generated from map-reduce results over a couchdb database
;;;;this allows for fast lookup and creating analytic querys
;;;;Read more about views here: https://docs.couchdb.org/en/stable/ddocs/views/intro.html
;;;;Query a couchdb view.
(defun couchdb-agent-get-view (agent database ddoc view query-json)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (cl-couch:get-view client database ddoc view query-json)))

(defun couchdb-document-exists-p (agent database id)
  (anypool:with-connection (client (couchdb-agent-client agent))
    (cl-couch:document-exists-p client database id)))

;;;; Start the couchdb agent.
(defun start-couchdb-agent (system)
  (let ((client (couch:new-couchdb star:*couchdb-host* star:*couchdb-port*)))
    (couch:password-auth client star:*couchdb-user* star:*couchdb-password*)
    (setf *couchdb-agent* (make-couchdb-agent system client))))


(defparameter *couchdb-inserts* nil "Actor responsible for handling couchdb inserts")
;;;; Start the couchdb inserts actor
(defun start-couchdb-inserts (system)
  (setf *couchdb-inserts* (actor-of system
                                    :name "*couchdb-inserts*"
                                    :receive (lambda (msg)
                                               (let ((database (getf msg :database star:*couchdb-default-database*))
                                                     (doc (getf msg :document))
                                                     (id (getf msg :id)))

                                                 (when (not (couchdb-document-exists-p *couchdb-agent* database id))
                                                   (reply (couchdb-agent-insert *couchdb-agent* database doc))))))))


(defparameter *couchdb-gets* nil "The Couchdb actor responsible for handling document gets.")
;;;; Start the couchdb GET actor.
;; FIXME
(defun start-couchdb-gets (system)
  (setf *couchdb-gets* (ac:actor-of system :name "*couchdb-gets*"
                                           :receive (lambda (doc-id &optional (rev nil))
                                                      (let ((pool *couchdb-pool*)
                                                            (db (uiop:getenv "COUCHDB_DATABASE")))
                                                        (with-context (*sys*)
                                                          (anypool:with-connection (client pool)
                                                            (task-async (lambda ()
                                                                          (handler-case
                                                                              (cl-couch:get-document client db doc-id rev)
                                                                            (dex:http-request-not-found (e) nil)
                                                                            (dex:http-request-unauthorized (e) nil)))
                                                                        :on-complete-fun (lambda (doc)
                                                                                           (reply doc))))))))))
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
(defun sumbit-target (target &optional (first-time t))
  "Create a message for the *targets* actor."
  (log:info "Submitting target - first-time: ~a actor: ~a"
            first-time (jsown:val-safe target "actor"))
  (log:debug "Target details: recurring=~a delay=~a"
             (jsown:val-safe target "recurring")
             (jsown:val-safe target "delay"))
  (tell *targets*  (if first-time
                       (cons t target)
                       (cons nil target)))
  (log:debug "Target submitted to *targets* actor"))

;;;; return t if this is the first time we handled this target.
(defun first-time-p (msg)
  (car msg))

;;;; Return t if this target document is transient, which means not to save in database.
(defun target-transient-p (target)
  (when (jsown:val-safe target "transient")
    t))

;;;;  Start the targets loader
(defun start-target-loader ()
  (log:info "Starting target loader")
  (let (targets (get-targets (anypool:with-connection (client *couchdb-pool*)
                               (get-targets client star:*couchdb-default-database*))))
    (log:info "Loaded ~a targets from database" (length targets))
    (loop for target in targets
          do (submit-target target t))
    (log:info "Target loader completed - all targets submitted")))


;;;; Start the target routing actor.
(defun start-target-actor (system)
  (log:info "Starting target actor")
  (setf *targets* (actor-of system
                            :name "*targets*"
                            :receive (lambda (msg)
                                       (log:debug "*targets* actor received message")
                                       (let* ((target (cdr msg))
                                              (actor (jsown:val target "actor"))
                                              (delay (jsown:val-safe target "delay"))
                                              (recurring (jsown:val-safe target "recurring"))
                                              (target-id (jsown:val-safe target "target")))
                                         (log:info "Processing target message - actor: ~a recurring: ~a first-time: ~a"
                                                   actor recurring (first-time-p msg))
                                         (if (not (get-dest-actor actor))
                                             ;; DEPRECATED Use the producer actor
                                             (progn
                                               (log:info "No local destination actor found for ~a, emitting to RabbitMQ" actor)
                                               (let ((routing-key (format nil "actors.~a.new-target" actor)))
                                                 (log:debug "Publishing to RabbitMQ - exchange: documents routing-key: ~a" routing-key)
                                                 (star.rabbit:emit-document  "documents" routing-key
                                                                             (jsown:to-json target)
                                                                             :host star:*rabbit-address*
                                                                             :port star:*rabbit-port*
                                                                             :username star:*rabbit-user* :password star:*rabbit-password*)
                                                 (log:info "Target published to RabbitMQ successfully"))))

                                         (when (and (get-dest-actor actor) recurring (first-time-p msg))
                                           (log:info "Scheduling recurring target - actor: ~a delay: ~a target-id: ~a"
                                                     actor delay target-id)
                                           (wt:schedule-recurring *target-timer* 0.0 delay (lambda ()
                                                                                             (submit-target target nil))
                                                                  target-id)
                                           (log:debug "Recurring target scheduled successfully"))
                                         (when (and (get-dest-actor actor) (not (first-time-p msg)))
                                           (log:debug "Routing non-first-time target to actor: ~a" actor)
                                           (route-target target actor))))))
  (log:info "Target actor started successfully"))

;;;; Start the target timer
;;;; The target timer handles recurring targets.
(defparameter *target-timer* nil "simple wheel timer for targets")
(defun start-target-timer ()
  (log:info "Starting target timer - resolution: 10 max-size: 1000")
  (setf *target-timer* (wt:make-wheel-timer :resolution 10 :max-size 1000))
  (log:info "Target timer started successfully"))



(defmacro with-json (jobject &body body)
  `(macrolet ((val (key) `(jsown:val-safe ,jobject ,key))
              (dataset () `(jsown:val ,jobject "datast"))
              (date-added () `(jsown:val ,jobject "dateAdded"))
              (date-updated () `(jsown:val ,jobject "dateUpdated"))
              (dtype () `(jsown:val ,jobject "dtype"))
              (parse-doc () `(star.databases.couchdb:from-json ,jobject (intern (jsown:val ,jobject "dtype") :spec))))
     ,@body))




;;;; Define a actor and its start function
(defmacro define-actor ((name system) &body body)
  (let ((start-fn-name (intern (format nil "START-~A" (str:replace-all "*" "" (symbol-name name))))))
    `(progn
       (defvar ,name nil)
       (defun ,start-fn-name ()
         (setf ,name
               (actor-of ,system
                         :name ,(symbol-name name)
                         :receive ,@body)))
       (serapeum:add-hook starintel-gserver:*actors-start-hook* #',start-fn-name :append t))))

;;;; * Producer Agent
;;;; The producer agent
(defun make-producer-agent (producer context)
  (make-agent (lambda ()
                (star.producers:producer-connect producer)
                producer) context))

(defparameter *producer-lock* "")
(defparameter *producer-agent* nil)


(defun publish (agent &key body (properties nil) routing-key)
  (log:info (star.producers:publish (agent-get agent #'identity) :body (jsown:to-json body) :properties properties :routing-key routing-key)))






(defun start-actors (&key rabbit-user rabbit-host rabbit-password rabbit-vhost rabbit-port)
  (start-actor-system)
  (setf *producer-agent* (make-producer-agent (star.producers:make-producer :name "actor-producer"
                                                                            :exchange-name "documents"
                                                                            :host rabbit-host
                                                                            :port rabbit-port
                                                                            :user rabbit-user
                                                                            :password rabbit-password
                                                                            :vhost rabbit-vhost) *sys*))

  ;; FIXME
  (let ((*gc-timer* (wt:make-wheel-timer)))
    (wt:schedule-recurring *gc-timer* 1 3600 (lambda ()
                                               (sb-ext:gc :full t))))
  (start-couchdb-agent *sys*)
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*)
  (nhooks:run-hook star:*actors-start-hook*))
;; actor entry point:1 ends here
