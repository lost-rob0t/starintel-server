;; TODO Move this to :star.actors.user-hunt
(in-package :star.actors)

(defparameter *wmn-relations-p* t "Will the actor create relations?")
(defparameter *wmn-agent* nil "Current WhatsMyName schema, containing strings to know if a username exists.")
(defparameter *user-hunt* nil "Username hunting actor that uses the wmn-data.json file provided by whatsmyname.app. Will create relations when *wmn-relations-p* is non nil.")
(defparameter *wmn-data-url* "https://raw.githubusercontent.com/WebBreacher/WhatsMyName/main/wmn-data.json")


(defun start-wmn-system (context)
  (setf *wmn-agent* (make-wmn-agent))
  (wt:schedule-recurring *sys* 0 *wmn-update-delay* #'update-wmn-data))


(defun parse-url (uri)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (quri:parse-uri uri)
    `(('scheme . ,scheme) ('userinfo . ,userinfo) ('host . ,host) ('port . ,port) ('path . ,path) ('query . ,query) ('fragment . ,fragment))))

(defun get-wmn-data (url)
  (jsown:val (jsown:parse (dex:get url)) "sites"))


(defvar *wmn-data* (get-wmn-data *wmn-data-url*))


(defun account-exists-p (user-obj test-json)
  (handler-case
      (multiple-value-bind (body code) (dex:get (spec:user-url user-obj))
        (cond ((or (= code (jsown:val test-json "m_code")) (str:contains? (jsown:val test-json "m_string") body)) nil)
              ((or (= code (jsown:val test-json "e_code")) (str:contains? (jsown:val test-json "e_string") body)) t)))
    ;; NOTE there are alot of random stuff that just goes wrong, i am logging it so if something terrible it could be spotted
    (error (e) (log:error e))))

(defun make-new-users (source)
  (loop for website in *wmn-data*
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

;; TODO re-write using lparallel futures
(defun hunt-user (username)
  (let* ((source (spec:new-user "test" :name username :platform "discord.com"))
         (users (make-new-users source)))

    (remove-if #'null  (alexandria:flatten (test-users source users nil)))))


(define-actor *user-hunt* *sys*
  (lambda (msg)
    (let* ((source-user (from-json msg 'spec:user))
           (potential-users (make-new-users source-user))
           (channel (lparallel:make-channel))
           (finished-p nil))


      (lparallel:pmapcar (lambda (user)
                           (lparallel:submit-task channel (lambda ()
                                                            (test-user source-user user *wmn-relations-p*)))) potential-users)
      (while (not finished-p)
             (let ((result (remove-if #'null  (lparallel:try-receive-result channel :timeout 10))))
               (when result
                 (loop for doc in result
                       for json = (as-json doc)
                       for routing-key = (format nil "documents.new.~a" (spec:doc-type doc))
                       do (publish *producer-agent* :body json :properties (cons :type (spec:doc-type doc)) :routing-key routing-key))))))))
