(uiop:define-package   :star.actors.subfinder
  (:use       :cl :star.databases.couchdb :sento.agent :sento.actor :sento.actor-system :sento.actor-context)
  (:documentation "doc"))

(in-package :star.actors.subfinder)


(defvar *subfinder* nil
  "doc")

(define-actor ((subfinder *sys*))
  (lambda (target)
    (log-actor-event "subfinder" :event-type "scan-start" :details (doc-id target))
    (with-context (star.actors:*sys*)
      (task-async (lambda ()
                    (multiple-value-bind (output error exit)
                        (uiop:run-program (list "subfinder" "-silent" "-d" (target-target target)))
                      (loop for domain in output
                            for document = (spec:new-domain (doc-dataset target) :record domain)
                            for relation = (spec:new-relation (doc-dataset target) :source (doc-id target) :target (doc-id domain) :note "subdomain")
                            do (publish *producer-agent* :body (star.databases.couchdb:as-json document))
                            do (publish *producer-agent* :body (star.databases.couchdb:as-json relation))
                            do (log-actor-event "subfinder" :event-type "new-domain" :details (star.databases.couchdb:as-json document)))
                      (log-actor-event "subfinder" :event-type "scan-finished" :details (doc-id target))))))))

;; (defun start-subfinder ()
;;   "doc"
;;   (setf *subfinder* (actor-of-system
;;                      :name "*subfinder*"
;;                      :receive (lambda (target)
;;                                 (log-actor-event "subfinder" :event-type "scan-start" :details (doc-id target))
;;                                 (with-context (star.actors:*sys*)
;;                                   (task-async (lambda ()
;;                                                 (multiple-value-bind (output error exit)
;;                                                     (uiop:run-program (list "subfinder" "-silent" "-d" (target-target target)))
;;                                                   (loop for domain in output
;;                                                         for document = (spec:new-domain (doc-dataset target) :record domain)
;;                                                         for relation = (spec:new-relation (doc-dataset target) :source (doc-id target) :target (doc-id domain) :note "subdomain")
;;                                                         do (publish *producer-agent* :body (star.databases.couchdb:as-json document))
;;                                                         do (publish *producer-agent* :body (star.databases.couchdb:as-json relation))
;;                                                         do (log-actor-event "subfinder" :event-type "new-domain" :details (star.databases.couchdb:as-json document)))
;;                                                   ()))))))))


