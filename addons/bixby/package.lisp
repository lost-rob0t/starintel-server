(uiop:define-package :star.addons.bixby
  (:use :cl)
  (:export
   #:*public-base-url*
   #:*redirect-uri*
   #:*read-scopes*
   #:*operations-scopes*
   #:configure-bixby
   #:bixby-oauth-settings
   #:create-bixby-oauth-client))
