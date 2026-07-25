(in-package :star-server-tests)

(defparameter *project-asdf-systems*
  '(:starintel-gserver
    :starintel-gserver-client
    :star-cli
    :star-ui
    :star-migrations
    :starintel-gserver-tests
    :starintel-gserver-integration-tests))

(defparameter *project-packages*
  '(:starintel-gserver
    :star.consumers
    :star.producers
    :star.databases.couchdb
    :star.rabbit
    :star.actors
    :star.actors.url-extractor
    :star.actors.matcher
    :starintel-gserver-http-api
    :starintel-gserver-client
    :star-cli
    :star-ui
    :star.migrations
    :star-server-tests))

(def-suite system-load-tests
  :description "Compile/load coverage for every project system and package")

(in-suite system-load-tests)

(test every-asdf-system-loads
  (dolist (system *project-asdf-systems*)
    (is (asdf:find-system system nil)
        "ASDF system ~s is discoverable" system)
    (finishes
      (asdf:load-system system))))

(test every-package-exists
  (dolist (package *project-packages*)
    (is (find-package package)
        "Package ~s exists after loading its ASDF system" package)))
