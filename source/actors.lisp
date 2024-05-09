;; [[file:../source.org::*Actor system setup][Actor system setup:1]]
(in-package :sento-user)
(defparameter *sys* nil "the main actor system")
(defun start-actor-system ()
  (setf *sys* (make-actor-system '(:dispatchers
                                   (:pinned (:workers 4 :strategy :random))
                                   :timeout-timer
                                   (:resolution 500 :max-size 1000)
                                   :eventstream
                                   (:dispatcher-id :shared)
                                   :scheduler
                                   (:enabled :true :resolution 100 :max-size 500)))))
;; Actor system setup:1 ends here

;; [[file:../source.org::*Eventing][Eventing:1]]
(defclass message-event ()
  ((topic :initarg :topic :initform (error "Topic for event stream is required.") :reader message-topic)
   (data :initarg :data :type string :initform "" :reader message-data))
  (:documentation "A basic class that holds message event topic and data"))
;; Eventing:1 ends here

;; [[file:../source.org::*Eventing][Eventing:2]]
(defgeneric topic-match-p (msg topic)
  (:documentation "generic interface that matches if a msg matches the subbed topic."))

(defmethod topic-match-p ((msg message-event) topic)
  "Return T if topic matches msg's topic"
  (string= topic (string-downcase (message-topic msg))))


(defun new-event (&key topic data (eventstream *sys*))
  "Create an publish a new message-event.
    This is a simple wrapper around the publish from sento."
  (publish eventstream (make-instance 'message-event :topic topic :data data)))


(defmacro with-topics ((&key msg topics) &body body)
  "A macro that will check if MSG topic "
  `(loop for topic in ,topics

         if (topic-match-p ,msg topic)
           do (progn ,@body)))
;; Eventing:2 ends here

;; [[file:../source.org::*API][API:1]]
(defun make-api-msg (sender data)
  (cons data sender))
;; API:1 ends here

;; [[file:../source.org::*Targets][Targets:1]]
(defparameter *actor-index-agent* nil)

(defun start-actor-index (system)
  (setf *actor-index-agent* (make-agent #'serapeum:dict system)))

(defun register-actor (actor-name actor-symbol)
  (setf (agent-get *actor-index-agent* #'identity) (serapeum:dict* (agent-get *actor-index-agent*) actor-name actor-symbol)))

(defun get-dest-actor (actor)
  (serapeum:@  (agent-get *actor-index-agent* #'identity) actor))

(defun route-target (target actor)
  (let ((dest (get-dest-actor actor)))
    (format t "got ~a" actor)
    (when dest
      (tell dest target))))
;; Targets:1 ends here

(in-package :sento-user)

(defparameter *couchdb-agent* nil)
(defun make-couchdb-agent (context client
                           &key (error-fun nil) (dispatcher-id :shared))
  (make-agent (lambda ()
                client
                context dispatcher-id)))


(defun couchdb-agent-client (agent)
  (agt:agent-get agent #'identity))


(defun couchdb-agent-insert (agent database document)
  (cl-couch:create-document (couchdb-agent-client agent) database document))

(defun couchdb-agent-update (agent database document revision)
  (cl-couch:create-document (couchdb-agent-client agent) (jsown:to-json)
                            (jsown:extend-js (jsown:parse document)
                              ("_rev" revision))))

(defun couchdb-agent-delete (agent database document-id)
  (cl-couch:delete-document (couchdb-agent-client agent) database document-id))

(defun couchdb-agent-get-view (agent database ddoc view query-json)
  (cl-couch:get-view (couchdb-agent-client agent) database ddoc view query-json))


(defun start-couchdb-agent (system)
  (let ((client (couch:new-couchdb star:*couchdb-host* star:*couchdb-port*)))
    (couch:password-auth client star:*couchdb-user* star:*couchdb-password*)
    (setf *couchdb-agent* (make-couchdb-agent system client))))

;; [[file:../source.org::*couchdb-insert actor][couchdb-insert actor:1]]
(defparameter *couchdb-inserts* nil)
(defun start-couchdb-inserts (system)
  (setf *couchdb-inserts* (actor-of system
                                    :name "*couchdb-inserts*"
                                    :receive (lambda (msg)
                                               (let ((destination-db star:*couchdb-default-database*))
                                                 (format t "inserting: ~a" msg)
                                                 (when (not (cl-couch:document-exists-p (couchdb-agent-client *couchdb-agent*) destination-db (jsown:val (cdr msg) "_id")))
                                                   (print (couchdb-agent-insert *couchdb-agent* destination-db (jsown:to-json* (cdr msg)))))
                                                 (force-output))))))
;; couchdb-insert actor:1 ends here

;; [[file:../source.org::*couchdb-get actor][couchdb-get actor:1]]
(defparameter *couchdb-gets* nil "The Couchdb actor responsible for handling document gets.")

(defun start-couchdb-gets (system)
  (setf *couchdb-gets* (ac:actor-of system :name "*couchdb-gets*"
                                           :receive (lambda (msg)
                                                      (let ((pool *couchdb-pool*)
                                                            (db (uiop:getenv "COUCHDB_DATABASE")))
                                                        (with-context (*sys*)
                                                          (anypool:with-connection (client pool)
                                                            (task-async (lambda ()
                                                                          (handler-case
                                                                              (cl-couch:get-document client db (car msg))
                                                                            (dex:http-request-not-found (e) nil)
                                                                            (dex:http-request-unauthorized (e) nil)))
                                                                        :on-complete-fun (lambda (doc)
                                                                                           (reply doc (cdr msg)))))))))))
;; couchdb-get actor:1 ends here

;; [[file:../source.org::*finish bulk insert actor][finish bulk insert actor:1]]
;; (defparameter *couchdb-bulk-insert* (ac:actor-of *sys*
;;                                                  :name "*couchdb-bulk-insert*"
;;                                                  :receive (lambda (msg)
;;                                                             (let ((destination-db (uiop:getenv "COUCHDB_DATABASE"))
;;                                                                   (pool *couchdb-pool*))
;;                                                               (anypool:with-connection (client pool)
;;                                                                 (cl-couch:bulk-create-documents client destination-db msg :batch "normal"))))))
;; finish bulk insert actor:1 ends here

(defparameter *targets* nil)
(defparameter *target-filter* ())
(defun get-targets (client database)
  (let ((jdata (jsown:val-safe (jsown:parse (cl-couch:get-view client star:*couchdb-default-database* "targets" "actor-targets" (jsown:to-json (jsown:new-js
                                                                                                                                                 ("include_docs" "true"))))) "rows")))
    (when (> 0 (length jdata))
      (loop for row in jdata
            for doc = (jsown:val row "doc")
            for actor = (jsown:val doc "actor")
            collect (cons actor doc)))))

(defun sumbit-target (target &optional (first-time t))
  "Create a message for the *targets* actor."
  (tell *targets*  (if first-time
                       (cons t target)
                       (cons nil target))))

(defun first-time-p (msg)
  (car msg))

(defun target-transient-p (target)
  (when (jsown:val-safe target "transient")
    t))

(defun start-target-loader ()
  (let (targets (get-targets (anypool:with-connection (client *couchdb-pool*)
                               (get-targets client star:*couchdb-default-database*))))
    (loop for target in targets
          do (submit-target target t))))


(defun start-target-actor (system)
  (setf *targets* (actor-of system
                            :name "*targets*"
                            :receive (lambda (msg)
                                       (let* ((target (cdr msg))
                                              (actor (jsown:val target "actor"))
                                              (delay (jsown:val-safe target "delay")))
                                         (print target)
                                         (if (not (get-dest-actor actor))
                                             (progn (print "not in system")
                                                    (star.consumers:emit-document  "documents" (format nil "actors.~a.new-target" actor)
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

;; [[file:../source.org::*Target Actor][Target Actor:2]]
(defparameter *target-timer* nil)
(defun start-target-timer ()
  (setf *target-timer* (wt:make-wheel-timer :resolution 10 :max-size 1000)))
;; Target Actor:2 ends here

;; [[file:../source.org::*actor entry point][actor entry point:1]]
(defun start-actors ()
  (start-actor-system)
  (start-couchdb-agent *sys*)
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*))
;; actor entry point:1 ends here
