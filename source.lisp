;; [[file:source.org::*Eventing][Eventing:3]]
(ac:actor-of *sys* :name "personator"
                   :init (lambda (self)
                           (ev:subscribe self self 'message-event))
                   :receive (lambda (msg)
                              (with-topics (:msg msg :topics '("New-Person"))
                                (format nil "~a" (starintel:doc-id msg)))))
;; Eventing:3 ends here

;; [[file:source.org::*Document Handler][Document Handler:1]]
(defun start-document-handler-actor ()
  (defparameter *document-handler* (ac:actor-of *sys*)))
;; Document Handler:1 ends here
