(in-package :star-server-tests)

(defparameter *project-asdf-systems*
  '(:starintel-gserver
    :starintel-gserver-client
    :star-cli
    :star-ui
    :star-migrations
    :starintel-gserver-tests))

(defparameter *project-packages*
  '(:starintel-gserver
    :star.consumers
    :star.producers
    :star.databases.couchdb
    :star.rabbit
    :star.actors
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

(test every-unit-asdf-system-is-loaded
  (dolist (system *project-asdf-systems*)
    (let ((component (asdf:find-system system nil)))
      (is (not (null component))
          "ASDF system ~s is discoverable" system)
      (is (and component
               (asdf:component-loaded-p component))
          "ASDF system ~s was loaded through declared dependencies" system))))

(test every-package-exists
  (dolist (package *project-packages*)
    (is (find-package package)
        "Package ~s exists after loading its ASDF system" package)))

(test canonical-root-and-explicit-legacy-packages-stay-distinct
  (is-false (find-package :spec))
  (is (find-package :starintel.legacy))
  (is (string= "0.10.1" (starintel:schema-version)))
  (is (string= "0.9.0" starintel.legacy:+starintel-doc-version+))
  (is-false (find-class 'starintel:host nil))
  (is (find-class 'starintel.legacy:host nil)))
