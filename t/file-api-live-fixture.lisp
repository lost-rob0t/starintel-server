;;;; Explicitly opt-in disposable HTTP/Rabbit/CouchDB fixture, not production init.
(require :asdf)
(asdf:load-system :starintel-gserver)

;; The Nix wrapper pre-registers packaged systems. Reload the complete serial
;; checkout after dependencies load, so this live fixture proves current source.
(let* ((definition-path (merge-pathnames "../source/starintel-gserver.asd" *load-truename*))
       (definition (with-open-file (stream definition-path) (read stream)))
       (source-directory (uiop:pathname-directory-pathname definition-path)))
  (dolist (component (getf (cddr definition) :components))
    (let ((relative (or (getf (cddr component) :pathname) (second component))))
      (load (merge-pathnames (make-pathname :type "lisp" :defaults relative)
                            source-directory)))))

(unless (equal "1" (uiop:getenv "STAR_FILE_FIXTURE_ALLOWED"))
  (error "This fixture requires explicit STAR_FILE_FIXTURE_ALLOWED=1."))

;; Pinned Dexador snapshots HTTP_PROXY at load and does not implement NO_PROXY.
;; This opt-in fixture talks only to local Docker backends; keep environment
;; proxy/CA settings intact for dependency fetching outside this Lisp process.
(setf dexador:*default-proxy* nil)

(setf star:*couchdb-host* "face-intel-test-couch"
      star:*couchdb-port* 5984
      star:*couchdb-user* "fixture-user"
      star:*couchdb-password* "fixture-password-not-for-production"
      star:*couchdb-default-database* "server-file-runtime-fixture"
      star:*rabbit-address* "face-intel-test-rabbit"
      star:*rabbit-port* 5672
      star:*rabbit-user* "fixture-user"
      star:*rabbit-password* "fixture-password-not-for-production"
      star:*auth-mode* "api-key"
      star:*auth-dev-bypass* nil
      star:*auth-pepper* "file-fixture-pepper-not-for-production"
      star.auth:*credential-store* (star.auth:make-memory-credential-store))

(multiple-value-bind (record token)
    (star.auth:create-api-key
     "file-fixture-client" "api_client"
     '("documents:read" "documents:write" "dataset:*" "tenant:default"))
  (declare (ignore record))
  (let ((path (or (uiop:getenv "STAR_FILE_FIXTURE_TOKEN_PATH")
                  (error "Set explicit STAR_FILE_FIXTURE_TOKEN_PATH."))))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string token stream))))

(star.actors::start-actor-system)
(setf star.actors:*producer-agent*
      (star.actors::make-producer-agent
       (star.producers:make-producer
        :name "file-fixture-producer" :exchange-name "documents"
        :host star:*rabbit-address* :port star:*rabbit-port*
        :user star:*rabbit-user* :password star:*rabbit-password* :vhost "/")
       star.actors::*sys*))

(defparameter *file-fixture-consumers*
  (list
   (star.rabbit::make-document-consumer
    :name "file-fixture-files" :queue-name "files.ingest"
    :routing-key "files.ingest.#" :handler-fn #'star.rabbit::handle-file-ingest)
   (star.rabbit::make-document-consumer
    :name "file-fixture-documents" :queue-name "documents.ingest"
    :routing-key "documents.ingest.#" :handler-fn #'star.rabbit:handle-document)
   (star.rabbit::make-document-consumer
    :name "file-fixture-updates" :queue-name "documents.update"
    :routing-key "documents.update.#" :handler-fn #'star.rabbit:handle-update-document)))

(dolist (consumer *file-fixture-consumers*)
  (star.consumers:start-consumer consumer))

(clack:clackup star.frontends.http-api::*server*
               :server :hunchentoot :address "0.0.0.0" :port 5985)
(format t "~&Disposable authenticated file fixture ready on internal port 5985.~%")
(finish-output)
(loop (sleep 1))
