(uiop:define-package :star.actor-registry
  (:use :cl)
  (:export
   #:+actor-registry-query-limit+
   #:actor-registry
   #:actor-registry-error
   #:actor-registry-error-code
   #:actor-registry-error-path
   #:build-actor-registry
   #:actor-registry-count
   #:actor-registry-find
   #:actor-registry-list
   #:query-actor-registry)
  (:documentation
   "Pure validation and safe discovery over StarLang portable manifests."))
