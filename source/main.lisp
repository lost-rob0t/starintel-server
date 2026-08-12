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
    :key :init-value)))

(defun initialize-runtime (init-file)
  (safe-load-init init-file)
  (log:info "Creating ~a worker threads" star:*ingest-workers*)
  (setf lparallel:*kernel*
        (lparallel:make-kernel star:*ingest-workers*))
  (star.databases.couchdb:init-db)
  (star.auth:initialize-auth-store)
  (star.auth:ensure-initial-user)
  (star.actors:start-actors
   :rabbit-host *rabbit-address*
   :rabbit-vhost "/"
   :rabbit-port *rabbit-port*
   :rabbit-user *rabbit-user*
   :rabbit-password *rabbit-password*)
  (star.frontends.http-api::start-http-api)
  (star.rabbit:start-consumers)
  (star.actors:start-event-consumer 2))

(defun server/handler (command)
  (initialize-runtime (clingon:getopt command :init-value))
  (loop for thread in (bt:all-threads)
        unless (equal thread (bt:current-thread))
          do (bt:join-thread thread)))

(defun server/command ()
  (clingon:make-command
   :name "start"
   :description "Start the server"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (server/options)
   :handler #'server/handler))

(defun main/commands ()
  (list (server/command)))

(defun main/handler (command)
  (clingon:print-usage-and-exit command t))

(defun main/command ()
  (clingon:make-command
   :name "star-server"
   :version *star-server-version*
   :description "StarIntel unified API and document-consuming service."
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'main/handler
   :sub-commands (main/commands)))

(defun start-debugger ()
  (format t "Creating slynk server on port: ~a" star:*slynk-port*)
  (slynk:create-server :port star:*slynk-port*))

(defun main ()
  (clingon:run (main/command)))

(defun repl/main (init-file)
  "Load and start the server from the REPL."
  (initialize-runtime init-file))
