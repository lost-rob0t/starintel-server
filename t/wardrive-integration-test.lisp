(in-package :star-server-tests)

(def-suite wardrive-integration-tests :description "Real HTTP -> Rabbit -> CouchDB WarStar ingestion")
(in-suite wardrive-integration-tests)

(defvar *wardrive-server* nil)
(defvar *wardrive-consumers* nil)
(defvar *wardrive-store* nil)
(defvar *wardrive-keys* nil)
(defparameter *wardrive-database* "starintel-warstar-acceptance")
(defparameter *wardrive-url* "http://127.0.0.1:5556")

(defun wardrive-http-app (env)
  ;; Real credentials and production middleware; no development bypass.
  (let ((star:*auth-mode* "api-key")
        (star:*auth-dev-bypass* nil)
        (star:*auth-pepper* "wardrive-acceptance-test-only")
        (star.auth:*credential-store* *wardrive-store*))
    (lack.component:call star.frontends.http-api::*server* env)))

(defun wardrive-credential (owner scopes)
  (let ((star:*auth-pepper* "wardrive-acceptance-test-only"))
    (nth-value 0 (star.auth:create-api-key *wardrive-store* owner scopes))))

(defun wardrive-poll (thunk &optional (seconds 15))
  (loop with deadline = (+ (get-internal-real-time)
                          (* seconds internal-time-units-per-second))
        for value = (funcall thunk)
        when value return value
        when (>= (get-internal-real-time) deadline)
          do (error "WarStar acceptance polling deadline exceeded")
        do (sleep 0.1)))

(defun setup-wardrive-integration ()
  (setf star:*couchdb-default-database* *wardrive-database*)
  (star.databases.couchdb:init-db)
  (setf *wardrive-store* (star.auth:make-memory-credential-store)
        *wardrive-keys*
        (loop for (label owner scopes) in
              '((:writer "warstar-writer" ("documents:bulk" "documents:write" "documents:read"
                                          "tenant:default" "dataset:warstar"))
                (:other "warstar-other" ("documents:bulk" "documents:write" "documents:read"
                                        "tenant:default" "dataset:warstar"))
                (:no-bulk "warstar-no-bulk" ("documents:write" "tenant:default" "dataset:warstar"))
                (:no-write "warstar-no-write" ("documents:bulk" "tenant:default" "dataset:warstar"))
                (:wrong-dataset "warstar-wrong-dataset" ("documents:bulk" "documents:write"
                                                        "tenant:default" "dataset:other")))
              collect (cons label (wardrive-credential owner scopes))))
  (star.actors:start-actors :rabbit-user star:*rabbit-user*
                            :rabbit-password star:*rabbit-password*
                            :rabbit-host star:*rabbit-address*
                            :rabbit-port star:*rabbit-port* :rabbit-vhost "/")
  (setf *wardrive-consumers* (star.rabbit:start-consumers))
  (wardrive-poll (lambda () (every #'star.consumers::consumer-ready-p *wardrive-consumers*)))
  (setf *wardrive-server* (clack:clackup #'wardrive-http-app :port 5556 :silent t)))

(defun teardown-wardrive-integration ()
  (when *wardrive-server* (clack:stop *wardrive-server*) (setf *wardrive-server* nil))
  (dolist (consumer *wardrive-consumers*)
    (star.consumers::stop-consumer-and-wait consumer))
  (setf *wardrive-consumers* nil)
  (star.actors::stop-actors)
  (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
    (cl-couch:delete-database client *wardrive-database*)))

(defun wardrive-post (fixture &optional (credential :writer))
  (perform-request
   (lambda ()
     (dex:post (concatenate 'string *wardrive-url* "/warstar/observations")
               :content (jsown:to-json fixture)
               :headers (append '(("Content-Type" . "application/json"))
                                (when credential
                                  (list (cons "Authorization"
                                              (concatenate 'string "Bearer "
                                                           (cdr (assoc credential *wardrive-keys*)))))))))))

(defun wardrive-ack-count ()
  (loop for worker in (or (star.consumers::consumer-worker-instances (first *wardrive-consumers*))
                         (list (first *wardrive-consumers*)))
        sum (star.consumers:consumer-settlement-count worker :ack)))

(defun wardrive-get (id)
  (perform-request
   (lambda ()
     (dex:get (format nil "~a/document/~a" *wardrive-url* id)
              :headers (list (cons "Authorization"
                                   (concatenate 'string "Bearer "
                                                (cdr (assoc :writer *wardrive-keys*)))))))))

(defun wardrive-stored (id)
  ;; Read only. All fixture documents enter exclusively via the HTTP route.
  (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
    (handler-case (jsown:parse (cl-couch:get-document client *wardrive-database* id))
      (dex:http-request-not-found () nil))))

(defun wardrive-expected (fixture owner)
  (loop for sample in (jsown:val fixture "observations")
        append (star.addons.wardrive::sample-documents
                (jsown:val fixture "device_id") sample owner)))

(defun wardrive-persisted (fixture owner)
  (loop for expected in (wardrive-expected fixture owner)
        for id = (jsown:val expected "id")
        collect (wardrive-poll (lambda () (wardrive-stored id)))))

(defun wardrive-rejected-fixture (device-id)
  (let ((fixture (wardrive-fixture)))
    (setf (jsown:val fixture "device_id") device-id)
    fixture))

(test wardrive-real-ingestion-and-identical-retry
  (let* ((fixture (wardrive-fixture))
         (expected (wardrive-expected fixture "warstar-writer")))
    (multiple-value-bind (status body) (wardrive-post fixture)
      (format t "~&[warstar-acceptance] POST status=~d body=~a~%" status body)
      (is (= 202 status))
      (is (= 2 (jsown:val (jsown:parse body) "observations")))
      (is (= 4 (jsown:val (jsown:parse body) "documents"))))
    (let ((stored (wardrive-persisted fixture "warstar-writer")))
      (loop for document in stored for wire in expected
            do (is (string= "0.10.1" (jsown:val document "schemaVersion")))
               (is (string= "warstar-writer" (jsown:val document "owner")))
               (is (string= "default" (jsown:val document "tenant_id")))
               (is (string= (jsown:val wire "id") (jsown:val document "_id")))
               (is (eq document (star.documents:validate-stored-document document)))
               (multiple-value-bind (status body) (wardrive-get (jsown:val document "id"))
                 (is (= 200 status))
                 (let ((response (jsown:parse body)))
                   (is-false (jsown:keyp response "_id"))
                   (is-false (jsown:keyp response "tenant_id"))
                   (is (eq response (star.documents:validate-document response)))))
               (format t "~&[warstar-acceptance] persisted ~a ~a rev=~a~%"
                       (jsown:val document "_id") (jsown:val document "dtype")
                       (jsown:val document "_rev")))
      (is (string= "39.96120000" (jsown:val (first stored) "latitude")))
      (is (string= "-82.99880000" (jsown:val (first stored) "longitude")))
      (is (string= "wpa2-psk" (jsown:val (second stored) "security")))
      (is (string= (jsown:val (first stored) "id")
                   (jsown:val (jsown:val (second stored) "location") "id")))
      (is (equal '("import") (jsown:val (fourth stored) "sourceKinds")))
      (is (string= "E" (jsown:val (jsown:val (fourth stored) "extensions") "radio")))
      ;; Wait for the normal outbox to mark publication before replay.
      (dolist (document stored)
        (wardrive-poll
         (lambda ()
           (every #'star.databases.couchdb::outbox-entry-published-p
                  (star.databases.couchdb::document-outbox-entries
                   (wardrive-stored (jsown:val document "id")))))))
      (wardrive-poll (lambda () (>= (wardrive-ack-count) 4)))
      (let ((revisions (mapcar (lambda (document)
                                (jsown:val (wardrive-stored (jsown:val document "id")) "_rev")) stored))
            (before (wardrive-ack-count)))
        (is (= 202 (nth-value 0 (wardrive-post fixture))))
        (wardrive-poll (lambda () (>= (wardrive-ack-count) (+ before 4))))
        (is (equal revisions
                   (mapcar (lambda (document)
                             (jsown:val (wardrive-stored (jsown:val document "id")) "_rev")) stored)))))
    ;; A second authenticated owner using identical installation/row IDs gets
    ;; distinct documents, rather than colliding with the first owner's data.
    (is (= 202 (nth-value 0 (wardrive-post fixture :other))))
    (let ((other (wardrive-persisted fixture "warstar-other")))
      (is (every (lambda (document) (string= "warstar-other" (jsown:val document "owner"))) other))
      (is (not (equal (mapcar #'star.documents:document-id expected)
                      (mapcar #'star.documents:document-id other)))))))

(test wardrive-rejects-without-persisting-any-batch-document
  (dolist (credential '(nil :no-bulk :no-write :wrong-dataset))
    (let ((fixture (wardrive-rejected-fixture "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")))
      (is (= (if credential 403 401) (nth-value 0 (wardrive-post fixture credential))))
      (dolist (owner '("warstar-writer" "warstar-no-bulk" "warstar-no-write" "warstar-wrong-dataset"))
        (dolist (document (wardrive-expected fixture owner))
          (is-false (wardrive-stored (jsown:val document "id")))))))
  (let ((fixture (wardrive-rejected-fixture "aaaaaaaa-bbbb-4ccc-8ddd-ffffffffffff")))
    ;; A generated schema violation in the second observation cannot leak the
    ;; first observation into Rabbit. signalDbm must be an integer.
    (setf (jsown:val (second (jsown:val fixture "observations")) "level") -65.5)
    (is (= 422 (nth-value 0 (wardrive-post fixture))))
    (setf (jsown:val (second (jsown:val fixture "observations")) "level") -65)
    (sleep 0.5)
    (dolist (document (wardrive-expected fixture "warstar-writer"))
      (is-false (wardrive-stored (jsown:val document "id")))))
  (let ((fixture (wardrive-fixture)))
    (setf (jsown:val fixture "observations")
          (make-list 101 :initial-element (first (jsown:val fixture "observations"))))
    (is (= 422 (nth-value 0 (wardrive-post fixture)))))
  (let ((fixture (wardrive-fixture)))
    (setf (jsown:val fixture "observations") #())
    (is (= 422 (nth-value 0 (wardrive-post fixture))))))

(defun run-wardrive-integration-tests ()
  (run-required-suite 'wardrive-integration-tests
                      :setup #'setup-wardrive-integration
                      :teardown #'teardown-wardrive-integration))
