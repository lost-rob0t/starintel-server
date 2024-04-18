;; [[file:source.org::*Eventing][Eventing:3]]
(ac:actor-of *sys* :name "personator"
                   :init (lambda (self)
                           (ev:subscribe self self 'message-event))
                   :receive (lambda (msg)
                              (with-topics (:msg msg :topics '("New-Person"))
                                (format nil "~a" (starintel:doc-id msg)))))
;; Eventing:3 ends here
