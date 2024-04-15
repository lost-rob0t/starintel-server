(uiop:define-package   :starintel-settings
  (:nicknames :star.settings)
  (:use :cl)
  (:documentation "settings for the starinte-gserver package")
  (:export
   #:*rabbit-address*
   #:*rabbit-port*
   #:*rabbit-user*
   #:*rabbit-password*))

(in-package :star.settings)
(defparameter *rabbit-address* "127.0.0.1" "The address rabbitmq is running on.")
(defparameter *rabbit-port* 5672 "The port that rabbitmq is listening on.")
(defparameter *rabbit-user* "guest" "the username for rabbimq")
(defparameter *rabbit-password* "guest" "the password for the rabbitmq user.")
