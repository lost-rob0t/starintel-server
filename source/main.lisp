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
   ))

(defun server/handler (cmd)
  (let ((debugger (clingon:getopt cmd :debugger))
        (init-file (clingon:getopt cmd :init-value)))

    (load init-file :if-does-not-exist :create)
    (log:info (format nil "Creating ~a worker threads" star:*injest-workers*))
    (setf lparallel:*kernel* (lparallel:make-kernel star:*injest-workers*))
    (star.databases.couchdb:init-db)
    (star.actors:start-actors :rabbit-host *rabbit-address*
                              :rabbit-vhost "/"
                              :rabbit-port *rabbit-port*
                              :rabbit-user *rabbit-user*
                              :rabbit-password *rabbit-password*)
    (star.frontends.http-api::start-http-api)
    (star.rabbit:start-consumers)
    (star.actors:start-event-consumer 2))

  (loop for thread in (bt:all-threads)
        if (not (equal thread (bt:current-thread)))
          do (bt:join-thread thread)))



(defun server/command ()
  "Start server"
  (clingon:make-command
   :name "start"
   :description "start the server"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (server/options)
   :handler #'server/handler)
  )

(defun main/commands ()
  (list
   (server/command)))

(defun main/handler (cmd)
  "Print usage/exit"
  (clingon:print-usage-and-exit cmd t))


(defun main/command ()
  (clingon:make-command :name "star-server"
                        :version *star-server-version*
                        :description "Starintel unified API and document consuming service."
                        :authors '("nsaspy <nsaspy@airmail.cc>")
                        :license "GPL v3"
                        :handler #'main/handler
                        :sub-commands (main/commands)))

(defun start-debugger ()
  (ql:quickload '("slynk" "bordeaux-threads"))
  (format t "Creating slynk server on port: ~a" star:*slynk-port*)
  (slynk:create-server :port star:*slynk-port*))



(defun main ()
  (let ((app (main/command)))
    (clingon:run app)))


