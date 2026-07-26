;; Example config
(in-package :star)
(format t "Starting starintel....")
(setq *rabbit-address* "rabbitmq")
(setq *couchdb-host* "bots.star.intel")
(setq *couchdb-default-database* "starintel")
;; You can invoke sylnk like so
;; (start-debugger)
;; Set log config path to logs
;; here it defaults to the /config/logs/star-server.log
;; Just uncomment this if you want to not use docker the docker volumne
;; TODO Make a docker vol for logs, instead of writing to the config dir SECURITY risk
;; (log:config :daily "./logs/gserver.log"
;;             :file2
;;             :sane)
(log:config :daily "/config/logs/gserver.log"
            :file2
            :sane)


