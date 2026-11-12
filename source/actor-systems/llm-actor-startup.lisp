;;;; Sento registration for llm.starintel.actor

(in-package :star.actors)

(defun start-llm-actor ()
  "Create and register the local llm.starintel.actor after its dispatcher is loaded."
  (setf *llm-actor*
        (sento.actor-context:actor-of
         *sys*
         :name "llm.starintel.actor"
         :receive #'llm-dispatch-message))
  (register-actor "llm.starintel.actor" *llm-actor*)
  *llm-actor*)

(nhooks:add-hook star:*actors-start-hook* #'start-llm-actor)
