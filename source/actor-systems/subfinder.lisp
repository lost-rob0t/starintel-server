(uiop:define-package   :star.actors.subfinder
  (:use       :cl :star.databases.couchdb :sento.agent :sento.actor :sento.actor-system :sento.actor-context :star.actors)
  (:documentation "doc"))

(in-package :star.actors.subfinder)


(defvar *subfinder* nil
  "doc")

(define-actor (*subfinder* star.actors:*sys*)
  (lambda (target)
    (let* ((dataset   (doc-dataset target))
           (target-id (doc-id target))
           (target-str (target-target target))
           (cmd (list "subfinder" "-silent" "-d" target-str)))

      (log:info "[subfinder] scan-start id=~a target=~a dataset=~a cmd=~s"
                target-id target-str dataset cmd)
      (log-actor-event "subfinder" :event-type "scan-start" :details target-id)

      (with-context (star.actors:*sys*)
        (task-async
         (lambda ()
           (let ((t0 (get-internal-real-time)))
             (labels ((elapsed-seconds ()
                        (/ (- (get-internal-real-time) t0)
                           internal-time-units-per-second)))

               (handler-case
                   (progn
                     (log:debug "[subfinder] running cmd=~s id=~a" cmd target-id)

                     (multiple-value-bind (out err code)
                         (uiop:run-program cmd
                                           :output :string
                                           :error-output :string
                                           :ignore-error-status t)
                       (let* ((raw-out (or out ""))
                              (raw-err (or err ""))
                              (lines
                                (loop for line in (uiop:split-string raw-out :separator '(#\Newline #\Return))
                                      for trimmed = (string-trim '(#\Space #\Tab) line)
                                      unless (or (null trimmed) (string= trimmed ""))
                                        collect trimmed)))

                         (log:info "[subfinder] run-finished id=~a exit=~d domains=~d elapsed=~,,2fs out-bytes=~d err-bytes=~d"
                                   target-id code (length lines) (elapsed-seconds)
                                   (length raw-out) (length raw-err))

                         (when (and raw-err (> (length raw-err) 0))
                           (log:warn "[subfinder] stderr id=~a exit=~d:~%~a"
                                     target-id code raw-err)
                           (log-actor-event "subfinder" :event-type "stderr" :details raw-err))

                         (log-actor-event "subfinder" :event-type "scan-exit" :details (format nil "~d" code))
                         (log-actor-event "subfinder" :event-type "scan-count" :details (format nil "~d" (length lines)))

                         (dolist (subdomain lines)
                           (handler-case
                               (let* ((domain-doc (spec:new-domain dataset :record subdomain))
                                      (rel-doc    (spec:new-relation dataset
                                                                     target-id
                                                                     (doc-id domain-doc)
                                                                     :note "subdomain"))
                                      (domain-json (star.databases.couchdb:as-json domain-doc))
                                      (rel-json    (star.databases.couchdb:as-json rel-doc)))

                                 (log:debug "[subfinder] emit domain id=~a record=~s" (doc-id domain-doc) subdomain)

                                 (publish *producer-agent* :body domain-json)
                                 (publish *producer-agent* :body rel-json)

                                 (log:debug "[subfinder] published domain id=~a relation id=~a"
                                            (doc-id domain-doc) (doc-id rel-doc))

                                 (log-actor-event "subfinder" :event-type "new-domain" :details domain-json)
                                 (log-actor-event "subfinder" :event-type "new-relation" :details rel-json))

                             (error (e)
                               (log:error "[subfinder] domain-failed target-id=~a subdomain=~s err=~a"
                                          target-id subdomain e)
                               (log-actor-event "subfinder" :event-type "domain-error"
                                                            :details (format nil "subdomain=~s err=~a" subdomain e)))))

                         (log:info "[subfinder] scan-finished id=~a processed=~d elapsed=~,,2fs"
                                   target-id (length lines) (elapsed-seconds))
                         (log-actor-event "subfinder" :event-type "scan-finished" :details target-id))))

                 (error (e)
                   (log:error "[subfinder] scan-crashed id=~a elapsed=~,,2fs err=~a"
                              target-id (elapsed-seconds) e)
                   (log-actor-event "subfinder" :event-type "scan-error" :details (format nil "~a" e))))))))))))

(nhooks:add-hook star:*actors-start-hook*
                 (lambda () (star.actors:register-actor "subfinder" *subfinder*)))

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


