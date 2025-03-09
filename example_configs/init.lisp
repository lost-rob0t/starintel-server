;; Example config
(in-package :star)
(format t "Starting starintel....~%")
(log:info "RabbitMQ Service: ~a~%" (setf *rabbit-address* "rabbitmq"))
(log:info "Couchdb Service: ~a~%" (setf *couchdb-host* "couchdb"))
(log:info "Listening On ~a : ~a~%" (setf *http-scheme* "http") (setf *http-api-address* "0.0.0.0"))

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


