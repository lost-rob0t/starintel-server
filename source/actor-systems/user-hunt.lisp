(in-package :star.actors)
(defparameter wmn-relations-p t "Will the actor create relations?")
(defparameter wmn-agent nil "Current WhatsMyName schema, containing strings to know if a username exists.")
(defparameter user-hunt nil "Username hunting actor that uses the wmn-data.json file provided by whatsmyname.app. Will create relations when wmn-relations-p is non nil.")
(defparameter wmn-data-url "https://raw.githubusercontent.com/WebBreacher/WhatsMyName/main/wmn-data.json")

(defun start-wmn-system (context)
  (setf wmn-agent (make-wmn-agent))
  (wt:schedule-recurring sys 0 wmn-update-delay #'update-wmn-data))


(defun parse-url (uri)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (quri:parse-uri uri)
    `(('scheme . ,scheme) ('userinfo . ,userinfo) ('host . ,host) ('port . ,port) ('path . ,path) ('query . ,query) ('fragment . ,fragment))))

(defun get-wmn-data (url)
  (jsown:val (jsown:parse (dex:get url)) "sites"))

(defvar wmn-data (get-wmn-data wmn-data-url))

(defun account-exists-p (user-obj test-json)
  (handler-case
      (multiple-value-bind (body code) (dex:get (spec:user-url user-obj))
        (cond ((or (= code (jsown:val test-json "m_code")) (str:contains? (jsown:val test-json "m_string") body)) nil)
              ((or (= code (jsown:val test-json "e_code")) (str:contains? (jsown:val test-json "e_string") body)) t)))
    (error (e) (log:error e))))


(defun make-new-users (source)
  (loop for website in wmn-data
        for uri = (str:replace-first "{account}" (spec:user-name source) (jsown:val website "uri_check"))
        for platform = (cdaddr (parse-url uri))
        collect (list (spec:new-user (spec:doc-dataset source) :name (spec:user-name source) :sources (alexandria:flatten (list "userhunter" (spec:doc-sources source))) :platform platform :url uri) website)))


(defun test-user (source user &optional (relation t))
  (let ((source-id (spec:doc-id source)))
    (when (account-exists-p (nth 0 user) (nth 1 user))
      (list (when relation (spec:new-relation (spec:doc-dataset source) source-id (spec:doc-id (nth 0 user)) "speculation")) (nth 0 user)))))


(defun test-users (source users &optional (relations t))
  (let ((source-id (spec:doc-id source)))
    (lparallel:pmap 'list (lambda (x)
                            (when (account-exists-p (nth 0 x) (nth 1 x))
                              (list (when relations (spec:new-relation (spec:doc-dataset source) source-id (spec:doc-id (nth 0 x)) "speculation")) (nth 0 x)))) users)))


(defclass user-hunting-consumer (star.rabbit:rabbit-consumer)
  ()
  (:documentation "Custom consumer class for the user hunting actor"))


(defmethod consume ((consumer user-hunting-consumer) message)
  (let* ((source-user (star.databases.couchdb:from-json (car message) 'spec:user))
         (potential-users (make-new-users source-user))
         (results (test-users source-user potential-users wmn-relations-p)))
    (loop for result in results
          for relation = (first result)
          for user = (second result)
          do (progn
               (star.actors:publish star.actors:producer-agent :body (to-json relation) :properties '((:type . "relation")))
               (star.actors:publish star.actors:producer-agent :body (to-json user) :properties '((:type . "user")))))))



(defun start-user-hunting-consumer (n &key (port star:rabbit-port) (host star:rabbit-address) (username star:rabbit-user) (password star:rabbit-password))
  (log:info (format nil "Creating ~a user hunting consumer threads." n))
  (loop for i from 1 to n
        do ()
        for stream = (make-instance 'star.rabbit:rabbit-queue-stream :host host :port port :user username :password password :queue-name "user-hunting" :exchange-name "documents" :routing-key "actors.userhunt.new.target")
        for consumer = (make-instance 'user-hunting-consumer :name (format nil "~a-~a" "user-hunting-consumer" i) :stream stream :fn #'consume)
        do (star.rabbit:open-stream stream)
        do (star.rabbit:start-consumer consumer)))
