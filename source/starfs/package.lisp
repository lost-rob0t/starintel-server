(uiop:define-package :star.starfs
  (:use :cl)
  (:export
   ;; Conditions.
   #:starfs-error
   #:block-not-found
   #:block-not-found-content-id
   #:block-integrity-error
   #:content-authentication-error
   ;; Content addressing.
   #:+content-id-prefix+
   #:content-id-p
   #:content-id-from-bytes
   ;; Block-store port (backend-neutral).
   #:block-store
   #:put-block
   #:get-block
   #:block-exists-p
   #:delete-block
   ;; Local content-addressed filesystem backend.
   #:local-block-store
   #:local-block-store-root
   #:make-local-block-store
   ;; Encrypted content store over any block-store backend.
   #:content-store
   #:make-content-store
   #:put-content
   #:get-content))

(in-package :star.starfs)
