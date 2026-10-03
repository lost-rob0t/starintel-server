(uiop:define-package :star.entity-resolution
  (:use :cl)
  (:export
   #:evidence-item
   #:make-evidence-item
   #:evidence-item-ref
   #:evidence-item-source
   #:evidence-item-kind
   #:evidence-item-confidence
   #:evidence-item-verification
   #:evidence-item-actor
   #:evidence-item-rule-version
   #:evidence-item-observed-at
   #:link-decision
   #:link-decision-status
   #:link-decision-confidence
   #:link-decision-evidence
   #:link-decision-evidence-refs
   #:link-decision-strong-evidence-refs
   #:link-decision-weak-evidence-refs
   #:link-decision-conflicting-evidence-refs
   #:link-decision-independent-source-count
   #:link-decision-reasons
   #:+person-link-accept-threshold+
   #:+strong-person-link-evidence-kinds+
   #:+weak-person-link-evidence-kinds+
   #:+conflicting-person-link-evidence-kinds+
   #:evaluate-person-link-candidate)
  (:documentation
   "Pure evidence gate for Person relation candidates. Weak correlation can
produce candidates, but cannot become an accepted identity/person link."))

(in-package :star.entity-resolution)
