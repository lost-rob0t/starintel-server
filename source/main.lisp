(in-package :starintel-gserver)
(defun server/options ()
  (list
   (clingon:make-option
    :string
    :description "Path to init file"
    :short-name #\i
    :long-name "init"
    :initial-value "./init.lisp"
    :env-vars '("STAR_SERVER_INIT_FILE")
    :key :init-value)
   (clingon:make-option
    :boolean
    :description "Enable Remote debugging"
    :short-name #\d
    :long-name "debugger"
    :key :debugger)))

;; [[file:../source.org::*Server command handler][Server command handler:1]]
;; (defun server/handler (cmd))
;; (let ((debugger (clingon:getopt cmd :debugger))
;;       (init-file (clingon:getopt cmd :init-value)))
;;   (when debugger
;;     (slynk:create-server :port 50006 :dont-close t))
;;   (load init-file :if-does-not-exist :create)
;;   (sento-user::start-actors)
;;   (star.consumers::start-rabbit-document-thread :host *rabbit-address* :port *rabbit-port*)
;;   (star.consumers::start-rabbit-targets-thread :host *rabbit-address* :port *rabbit-port*)
;;   (init-db)
;;   (star.frontends.http-api::start-http-api)
;;   (sento-user::start-target-loader))


;; (defun server/command ()
;;   "A command to greet someone"
;;   (clingon:make-command
;;    :name "start"
;;    :description "start the server"
;;    :version "0.1.0"
;;    :authors '("nsaspy <nsaspy@airmail.cc>")
;;    :license "GPL v3"
;;    :options (server/options)
;;    :handler #'server/handler))


(defun main ()
  (let ((init-file "init.lisp"))
    (sento-user::start-actor-system)
    (setf lparallel:*kernel* (lparallel:make-kernel 12))
    (load init-file :if-does-not-exist :create)
    (init-db)
    (sento-user::start-actors)
    (star.consumers::start-document-consumer 12 :host *rabbit-address* :port *rabbit-port*)
    (star.consumers::start-rabbit-targets-thread :host *rabbit-address* :port *rabbit-port*))
  ;; (star.frontends.http-api::start-http-api)

  (loop for thread in (bt:all-threads)

        if (not (equal thread (bt:current-thread)))
          do (bt:join-thread thread)))

;; Server command handler:1 ends here
