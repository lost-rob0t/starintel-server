;; [[file:../source.org::*Settings][Settings:1]]

;; Settings:1 ends here

;; [[file:../source.org::*Listen Address][Listen Address:1]]
(in-package :star)
(defparameter *http-api-address* "0.0.0.0" "the listen address")
(defparameter *http-api-port* 5000 "the port the api server listen on")
(defparameter *http-api-base-path* "/api" "the base url to use for the api endpoint")
(defparameter *http-cert-file* nil "path to the http api cert providing https")
(defparameter *http-key-file* nil "path to the http cert providing https")
(defparameter *http-scheme* 'http "use https or not.")
;; Listen Address:1 ends here

;; [[file:../source.org::*Authentication][Authentication:1]]
(defparameter *rabbit-address* "127.0.0.1" "The address rabbitmq is running on.")
(defparameter *rabbit-port* 5672 "The port that rabbitmq is listening on.")
(defparameter *rabbit-user* "guest" "the username for rabbimq")
(defparameter *rabbit-password* "guest" "the password for the rabbitmq user.")
;; Authentication:1 ends here
