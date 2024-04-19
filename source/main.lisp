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
(defun server/handler (cmd)
  (let ((debugger (clingon:getopt cmd :debugger))
        (init-file (clingon:getopt cmd :init-value)))
    (when debugger
      (slynk:create-server :port 50006 :dont-close t))
    (load init-file :if-does-not-exist :create)
    (sento-user::start-actors)


    (sento-user::start-actors)
    (star.frontends.http-api::start-http-api)
    (star.rabbit:start-rabbit-document-thread :host *rabbit-address* :port *rabbit-port*)))

(defun server/command ()
  "A command to greet someone"
  (clingon:make-command
   :name "start"
   :description "start the server"
   :version "0.1.0"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (server/options)
   :handler #'server/handler))


(defun main ()
  (clingon:run (server/command))
  (loop do (sleep 3)))
;; Server command handler:1 ends here
