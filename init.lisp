(format t "Starting starintel....")
(in-package :star)
(defparameter *server* "database.star.intel")
(setq *rabbit-address* *server*)
(setq *couchdb-host* *server*)
(setq *couchdb-default-database* "starintel-gserver-devel")
(start-debugger)

(log:config :daily "logs/gserver.log"
            :file2
            :sane)
(ql:quickload :hackmode-phish)
