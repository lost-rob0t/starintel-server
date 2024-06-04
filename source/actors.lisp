;; [[file:../source.org::*Actor system setup][Actor system setup:1]]
(in-package :star.actors)
(defparameter *sys* nil "the main actor system")


(defclass pattern ()
  ((name :initarg :name :initform (error "Pattern requires a name") :accessor pattern-name)
   (transient :initarg :transient :initform nil :accessor transientp :documentation "Messages created by this pattern will be considered transient, and will not be saved to a database, use this if your pattern creates lots of targets that are not needed again later.")
   (match-fn :initarg :match-fn :accessor match-fn :initform #'identity)
   (subs :initarg :subscriptions :accessor subcriptions :type list :documentation "List of actor names to tell them about the message, it just runs (tell name <document>)"))

  (:documentation "Document pattern matcher, based on a match-fn you supply. When the handler returns non nill, the subs actors are then sent a message with the message being the document"))



(defgeneric notify-subs (self message)
  (:documentation "Notify subscribers about matched documents."))


(defmethod notify-subs ((pattern pattern) message)
  (when (funcall (match-fn pattern) message)
    (loop for actor in (subcriptions pattern)
          do (log:error actor)
          do (act:tell actor message))))

(defmacro define-pattern ((name subs) &body body)
  `(make-instance 'pattern :name ,name  :subscriptions ,subs
                           :match-fn ,@body))

(defun add-pattern (pattern)
  (push pattern star:*document-patterns*))



(defun start-actor-system ()
  (setf *sys* (make-actor-system `(:dispatchers
                                   (:pinned (:workers ,star:*injest-workers* :strategy :random))
                                   :timeout-timer
                                   (:resolution 500 :max-size 1000)
                                   :eventstream
                                   (:dispatcher-id :shared)
                                   :scheduler
                                   (:enabled :true :resolution 100 :max-size 500)))))



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


(defparameter *couchdb-agent* nil)
(defun make-couchdb-agent (context client
                           &key (error-fun nil) (dispatcher-id :shared))
  (make-agent (lambda ()
                star.databases.couchdb:*couchdb-pool*)))



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
                                    :receive (lambda (doc)
                                               (let ((destination-db star:*couchdb-default-database*))
                                                 (when (not (cl-couch:document-exists-p (couchdb-agent-client *couchdb-agent*) destination-db (jsown:val doc "_id")))
                                                   (reply (couchdb-agent-insert *couchdb-agent* destination-db (jsown:to-json* doc)))))))))

;; couchdb-insert actor:1 ends here

;; [[file:../source.org::*couchdb-get actor][couchdb-get actor:1]]
(defparameter *couchdb-gets* nil "The Couchdb actor responsible for handling document gets.")

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

;; [[file:../source.org::*Target Actor][Target Actor:2]]
(defparameter *target-timer* nil)
(defun start-target-timer ()
  (setf *target-timer* (wt:make-wheel-timer :resolution 10 :max-size 1000)))
;; Target Actor:2 ends here

;; [[file:../source.org::*actor entry point][actor entry point:1]]
;;
;;

(defmacro with-json (jobject &body body)
  `(macrolet ((val (key) `(jsown:val-safe ,jobject ,key))
              (dataset () `(jsown:val ,jobject "datast"))
              (date-added () `(jsown:val ,jobject "dateAdded"))
              (date-updated () `(jsown:val ,jobject "dateUpdated"))
              (dtype () `(jsown:val ,jobject "dtype"))
              (parse-doc () `(star.databases.couchdb:from-json ,jobject (intern (jsown:val ,jobject "dtype") :spec))))
     ,@body))




;; [[file:../source.org::*actor entry point][actor entry point:1]]
(defmacro define-actor (var context &body body)
  (let ((start-fn (gensym "START-FN-")))

    `(let ((,start-fn (lambda ()
                        (setf ,var (actor-of ,context
                                             :name (symbol-name ,var)
                                             :receive ,@body)))))
       (nhooks:add-hook star:*actors-start-hook* ,start-fn :append t))))


(defun make-producer-agent (producer context)
  (make-agent (lambda ()
                producer) context))

(defparameter *producer-lock* "")
(defparameter *producer-agent* nil)


(defun publish (agent &key body (properties nil))
  (star.producers:publish (agent-get agent #'identity) :body body :properties properties))




(defparameter *pattern-agent* nil "Agent Responsible for holding patterns")
(defparameter *pattern-actor* nil "Actor responsible for routing matched messages to actors.")

(defun make-pattern-agent (context)
  (make-agent (lambda ()
                star:*document-patterns*)
              context))

(defun get-patterns (pattern-agent &key (fn #'identity))
  (agent-get pattern-agent fn))



(defun add-pattern* (pattern-agent pattern)
  (let ((old (agent-get pattern-agent #'identity)))
    (agent-update pattern-agent (lambda ()
                                  (push pattern old)))))



(defun start-pattern-agent (context)
  (setf *pattern-agent* (make-pattern-agent context)))


(defun start-pattern-actor (context)
  (setf *pattern-actor* (actor-of context :name "patterns"
                                          :receive (lambda (msg)
                                                     (let ((patterns (get-patterns *pattern-agent*)))
                                                       (dolist (pattern patterns)
                                                         (notify-subs pattern msg)))))))


(defparameter *url-regex* (ppcre:create-scanner "https?:\\/\\/(www\\.)?\[-a-zA-Z0-9@:%.\_\\+~#=\]{1,256}\\.\[a-zA-Z0-9()\]{1,6}\\b(\[-a-zA-Z0-9()@:%\_\\+.~#?&//=\]\*)"))

(defparameter *url-extractor-fields* '("content" "bio"))

(defparameter *url-extractor* nil "")

(defun get-urls (str)
  (loop for url in (ppcre:all-matches-as-strings *url-regex* str)
        do (progn (print url) (force-output))
        collect url))




(defun start-url-extractor (context)
  (setf *url-extractor* (actor-of context
                                  :name "url-extractor"
                                  :receive
                                  (lambda (msg)
                                    (print msg)
                                    (force-output)
                                    (let* ((dataset (jsown:val msg "dataset"))
                                           (docs
                                             (alexandria:flatten (loop for field in *url-extractor-fields*
                                                                       for content = (jsown:val-safe msg field)
                                                                       for results = (get-urls content)
                                                                       collect (loop for url in results
                                                                                     for doc = (spec:new-url dataset :url url :content "")
                                                                                     for rel = (spec:new-relation dataset (jsown:val msg "_id") (spec:doc-id doc) "extracted")
                                                                                     collect (list (as-json rel) (as-json doc)))))))
                                      (loop for doc in docs
                                            do (publish *producer-agent* :body (jsown:to-json doc) :properties (list (cons :type (jsown:val doc "dtype"))))
                                            do (print doc)
                                            do (force-output))
                                      (log:e docs)
                                      (force-output))))))








(defun start-actors (&key rabbit-user rabbit-host rabbit-password rabbit-vhost rabbit-port)
  (start-actor-system)
  (setf *producer-agent* (make-producer-agent (star.producers:make-producer :name "actor-producer"
                                                                            :exchange-name "documents"
                                                                            :user rabbit-user
                                                                            :password rabbit-password
                                                                            :vhost rabbit-vhost) *sys*))

  (start-url-extractor *sys*)
  (add-pattern (define-pattern ("url-extractor" (list *url-extractor*))
                 (lambda (msg)
                   (string= (jsown:val msg "dtype") "message"))))
  (start-couchdb-agent *sys*)
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*)
  (start-pattern-agent *sys*)
  (start-pattern-actor *sys*)
  (nhooks:run-hook star:*actors-start-hook*))
;; actor entry point:1 ends here
