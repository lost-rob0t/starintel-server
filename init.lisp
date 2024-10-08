(format t "Starting starintel....")
(in-package :star)
(setq *rabbit-address* "rabbitmq")
(setq *couchdb-host* "couchdb")
(setq *couchdb-default-database* "starintel")
(start-debugger)

(log:config :daily "logs/gserver.log"
            :file2
            :sane)
