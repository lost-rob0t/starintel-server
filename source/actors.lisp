(in-package :sento-user)
(defparameter *sys* nil "the main actor system")
(defun start-actor-system ()
  (setf *sys* (make-actor-system))
)

(defclass message-event ()
  ((topic :initarg :topic :initform (error "Topic for event stream is required.") :reader message-topic)
   (data :initarg :data :type string :initform "" :reader message-data))
  (:documentation "A basic class that holds message event topic and data"))

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

(defparameter *couchdb-pool*
  (anypool:make-pool :name "couchdb-connections"
                     :connector (lambda ()
                                  (let ((client (cl-couch:new-couchdb (uiop:getenv "COUCHDB_HOST") 5984 :scheme (string-downcase (uiop:getenv "COUCHDB_SCHEME")))))
                                    (cl-couch:password-auth client (uiop:getenv "COUCHDB_USER") (uiop:getenv "COUCHDB_PASSWORD"))
                                    client))

                     :disconnector (lambda (obj)
                                     (setf (cl-couch:couchdb-headers obj) nil))
                     :max-open-count 20))

(defvar *my-thread* nil)

(defun start--pool-monitoring ()
  (setf *my-thread*
        (bt:make-thread
         (lambda ()
           (loop
             do (progn (format t "Active count: ~a, Idle count: ~a~%"
                               (anypool:pool-active-count *couchdb-pool*)
                               (anypool:pool-idle-count *couchdb-pool*))
                       (force-output)
                       (sleep 1))
             finally (bt:thread-yield))))))

(defun stop--pool-monitoring ()
  (when *my-thread*
    (bt:destroy-thread *my-thread*)
    (setf *my-thread* nil)))

(defun start-couchdb-inserts ()
  (defparameter *couchdb-insert* (ac:actor-of *sys*
                                              :name "*couchdb-insert*"
                                              :receive (lambda (msg)
                                                         (let ((destination-db (uiop:getenv "COUCHDB_DATABASE"))
                                                               (pool *couchdb-pool*))

                                                           (with-context (*sys* :pinned)
                                                             (task-start
                                                              (lambda ()
                                                                (anypool:with-connection (client pool)
                                                                  (cl-couch:create-document client destination-db (cdr msg) :batch "normal"))))))))))

;; (defparameter *couchdb-bulk-insert* (ac:actor-of *sys*
;;                                                  :name "*couchdb-bulk-insert*"
;;                                                  :receive (lambda (msg)
;;                                                             (let ((destination-db (uiop:getenv "COUCHDB_DATABASE"))
;;                                                                   (pool *couchdb-pool*))
;;                                                               (anypool:with-connection (client pool)
;;                                                                 (cl-couch:bulk-create-documents client destination-db msg :batch "normal"))))))

(defun start-actors ()
  (start-actor-system)
  (start-couchdb-inserts))
