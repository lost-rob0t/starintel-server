(uiop:define-package :star.ids
  (:use :cl)
  (:export #:ulid))

(in-package :star.ids)

;; cms-ulid keeps its previous value in a shared mutable closure. Serialize
;; every server call so concurrent workers cannot issue the same ID.
(defvar *ulid-lock* (bt:make-lock "starintel-ulid"))

(defun ulid (&rest arguments)
  (bt:with-lock-held (*ulid-lock*)
    (apply #'cms-ulid:ulid arguments)))
