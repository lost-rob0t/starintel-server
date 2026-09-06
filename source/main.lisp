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
    :persistent t
    :key :init-value)))

(defun initialize-runtime (init-file)
  (star.runtime:start-runtime init-file))

(defun server/handler (command)
  (let ((runtime
          (initialize-runtime (clingon:getopt command :init-value))))
    (unwind-protect
         (star.runtime:run-runtime-loop runtime)
      (star.runtime:stop-runtime runtime :reason :handler-exit))))

(defun server/command ()
  (clingon:make-command
   :name "start"
   :description "Start the server"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (server/options)
   :handler #'server/handler))

(defun main/commands ()
  (list (server/command)
        (admin/command)))

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
  "Start a SLY/Slynk server on =*slynk-port*= for live debugging.

Connect from Emacs with =M-x sly-connect= and the port configured by
=STAR_SLYNK_PORT= (default 4009)."
  (format t "Creating slynk server on port: ~a" star:*slynk-port*)
  (slynk:create-server :port star:*slynk-port*))

(defun main ()
  "Binary entry point; parses argv via clingon and dispatches subcommands.

Subcommands:
- =start= :: run the server runtime loop until interrupted
- =admin= :: administrative utilities"
  (clingon:run (main/command)))

(defun repl/main (init-file)
  "Load and start the server from the REPL.

Unlike =main= this returns after the runtime stops instead of exiting
the image, so callers can inspect state afterwards.  INIT-FILE is the
init script path, usually =./init.lisp=."
  (initialize-runtime init-file))
