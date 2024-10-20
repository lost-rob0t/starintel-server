;; Example config
(in-package :star)
(format t "Starting starintel....")
(setq *rabbit-address* "rabbitmq")
(setq *couchdb-host* "couchdb")
(setq *couchdb-default-database* "starintel")
;; You can invoke sylnk like so
;; (start-debugger)
;; Set log config path to logs
(log:config :daily "logs/gserver.log"
            :file2
            :sane)

;;
