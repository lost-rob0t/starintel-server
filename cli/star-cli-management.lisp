(in-package :star-cli)

;;; Remote authentication and administration commands. These handlers are
;;; intentionally terminal adapters over starintel-gserver-client. They contain
;;; no endpoint paths, JSON protocol parsing, Authorization header construction,
;;; or Dexador calls.

(defun management/global-options ()
  (append
   (global-options)
   (list
    (clingon:make-option
     :string
     :description "StarIntel API key for authenticated commands"
     :long-name "token"
     :env-vars '("STAR_SERVER_TOKEN" "STAR_SERVER_API_KEY")
     :key :api-key))))

(defun make-client (cmd)
  "Create a StarIntel client from inherited command options."
  (let* ((client (make-star-client
                  :base-url (clingon:getopt cmd :base-url)
                  :user-agent "star-cli/0.2"))
         (api-key (clingon:getopt cmd :api-key)))
    (if (and api-key (plusp (length api-key)))
        (client-with-api-key client api-key)
        client)))

(defun management/print-json (value)
  (cond
    ((stringp value) (format t "~a~%" value))
    ((null value) (format t "null~%"))
    (t (format t "~a~%" (jsown:to-json value)))))

(defun management/exit-for-condition (condition)
  (cond
    ((typep condition 'client-authentication-error) 4)
    ((typep condition 'client-authorization-error) 5)
    ((typep condition 'client-validation-error) 2)
    ((typep condition 'client-not-found-error) 3)
    ((typep condition 'client-conflict-error) 6)
    ((typep condition 'client-transport-error) 7)
    (t 1)))

(defun management/run (thunk)
  (handler-case
      (funcall thunk)
    (star-client-error (condition)
      (format *error-output* "star-cli: ~a~%" condition)
      (clingon:exit (management/exit-for-condition condition)))
    (error (condition)
      (format *error-output* "star-cli: ~a~%" condition)
      (clingon:exit 1))))

(defun management/read-secret-file (path)
  (with-open-file (stream path :direction :input)
    (let ((value (read-line stream nil nil)))
      (unless (and value (plusp (length value)))
        (error "Secret file is empty: ~a" path))
      value)))

(defun management/resolve-secret (cmd &key password-key file-key stdin-key)
  (let* ((password (clingon:getopt cmd password-key))
         (file (clingon:getopt cmd file-key))
         (stdin-p (clingon:getopt cmd stdin-key))
         (count (count-if #'identity (list password file stdin-p))))
    (unless (= count 1)
      (error "Choose exactly one password source: direct value, file, or stdin"))
    (cond
      (password password)
      (file (management/read-secret-file file))
      (stdin-p
       (let ((value (read-line *standard-input* nil nil)))
         (unless (and value (plusp (length value)))
           (error "Password stdin was empty"))
         value)))))

(defun management/password-options (&key (prefix "password"))
  (let ((password-key (intern (string-upcase (format nil "~a-value" prefix)) :keyword))
        (file-key (intern (string-upcase (format nil "~a-file" prefix)) :keyword))
        (stdin-key (intern (string-upcase (format nil "~a-stdin" prefix)) :keyword)))
    (list
     (clingon:make-option
      :string
      :description "Password value (prefer file or stdin)"
      :long-name prefix
      :key password-key)
     (clingon:make-option
      :string
      :description "Read password from a file"
      :long-name (format nil "~a-file" prefix)
      :key file-key)
     (clingon:make-option
      :boolean
      :description "Read password from standard input"
      :long-name (format nil "~a-stdin" prefix)
      :initial-value nil
      :key stdin-key))))

(defun management/parse-scopes (value)
  (when value
    (remove-if
     (lambda (scope) (zerop (length scope)))
     (mapcar
      (lambda (scope)
        (string-trim '(#\Space #\Tab #\Newline #\Return) scope))
      (uiop:split-string value :separator ",")))))

(defun management/group-handler (command)
  (clingon:print-usage-and-exit command t))

;;; Auth commands

(defun auth/login-options ()
  (append
   (list
    (clingon:make-option
     :string
     :description "Human username"
     :long-name "username"
     :required t
     :key :auth-username))
   (management/password-options :prefix "password")))

(defun auth/login-handler (command)
  (management/run
   (lambda ()
     (let ((password
             (management/resolve-secret
              command
              :password-key :password-value
              :file-key :password-file
              :stdin-key :password-stdin)))
       (multiple-value-bind (result response)
           (login (make-star-client
                   :base-url (clingon:getopt command :base-url)
                   :user-agent "star-cli/0.2")
                  (clingon:getopt command :auth-username)
                  password)
         (declare (ignore response))
         (management/print-json
          (jsown:new-js
            ("api_key" (login-result-api-key result))
            ("credential" (login-result-credential result))
            ("user" (login-result-user result))
            ("correlation_id" (or (login-result-correlation-id result) :null)))))))))

(defun auth/context-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (value response)
         (auth-context (make-client command))
       (declare (ignore response))
       (management/print-json value)))))

(defun auth/change-password-options ()
  (append
   (management/password-options :prefix "current-password")
   (management/password-options :prefix "new-password")))

(defun auth/change-password-handler (command)
  (management/run
   (lambda ()
     (let ((current
             (management/resolve-secret
              command
              :password-key :current-password-value
              :file-key :current-password-file
              :stdin-key :current-password-stdin))
           (new
             (management/resolve-secret
              command
              :password-key :new-password-value
              :file-key :new-password-file
              :stdin-key :new-password-stdin)))
       (multiple-value-bind (value response)
           (change-password (make-client command) current new)
         (declare (ignore response))
         (management/print-json value))))))

(defun auth/command ()
  (clingon:make-command
   :name "auth"
   :description "Authenticate and inspect the remote StarIntel session"
   :handler #'management/group-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "login"
     :description "Log in with a human username and password"
     :options (auth/login-options)
     :handler #'auth/login-handler)
    (clingon:make-command
     :name "context"
     :description "Inspect the current authenticated context"
     :handler #'auth/context-handler)
    (clingon:make-command
     :name "change-password"
     :description "Change the current human user's password"
     :options (auth/change-password-options)
     :handler #'auth/change-password-handler))))

;;; Remote administrator user commands

(defun remote-admin/user-create-options ()
  (append
   (list
    (clingon:make-option
     :string
     :description "Username"
     :long-name "username"
     :required t
     :key :remote-username)
    (clingon:make-option
     :string
     :description "Comma-separated scopes"
     :long-name "scopes"
     :required t
     :key :remote-scopes)
    (clingon:make-option
     :string
     :description "Principal type"
     :long-name "principal-type"
     :initial-value "user"
     :key :remote-principal-type)
    (clingon:make-option
     :boolean
     :description "Do not require a password change"
     :long-name "no-must-change-password"
     :initial-value nil
     :key :remote-no-must-change-password))
   (management/password-options :prefix "password")))

(defun remote-admin/user-create-handler (command)
  (management/run
   (lambda ()
     (let ((password
             (management/resolve-secret
              command
              :password-key :password-value
              :file-key :password-file
              :stdin-key :password-stdin)))
       (multiple-value-bind (value response)
           (create-user
            (make-client command)
            (clingon:getopt command :remote-username)
            password
            (management/parse-scopes (clingon:getopt command :remote-scopes))
            :principal-type (clingon:getopt command :remote-principal-type)
            :must-change-password
            (not (clingon:getopt command :remote-no-must-change-password)))
         (declare (ignore response))
         (management/print-json value))))))

(defun remote-admin/user-list-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (value response)
         (list-users (make-client command))
       (declare (ignore response))
       (management/print-json value)))))

(defun remote-admin/user-set-password-options ()
  (append
   (list
    (clingon:make-option
     :string
     :description "Username"
     :long-name "username"
     :required t
     :key :remote-username)
    (clingon:make-option
     :boolean
     :description "Do not require a password change"
     :long-name "no-must-change-password"
     :initial-value nil
     :key :remote-no-must-change-password))
   (management/password-options :prefix "password")))

(defun remote-admin/user-set-password-handler (command)
  (management/run
   (lambda ()
     (let ((password
             (management/resolve-secret
              command
              :password-key :password-value
              :file-key :password-file
              :stdin-key :password-stdin)))
       (multiple-value-bind (value response)
           (reset-user-password
            (make-client command)
            (clingon:getopt command :remote-username)
            password
            :must-change-password
            (not (clingon:getopt command :remote-no-must-change-password)))
         (declare (ignore response))
         (management/print-json value))))))

(defun remote-admin/user-command ()
  (clingon:make-command
   :name "user"
   :description "Manage remote human users through the authenticated API"
   :handler #'management/group-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create a human user"
     :options (remote-admin/user-create-options)
     :handler #'remote-admin/user-create-handler)
    (clingon:make-command
     :name "list"
     :description "List human users"
     :handler #'remote-admin/user-list-handler)
    (clingon:make-command
     :name "set-password"
     :description "Administratively reset a human user's password"
     :options (remote-admin/user-set-password-options)
     :handler #'remote-admin/user-set-password-handler))))

;;; Remote administrator credential commands

(defun remote-admin/credential-create-options ()
  (list
   (clingon:make-option
    :string
    :description "Credential owner"
    :long-name "owner"
    :required t
    :key :remote-owner)
   (clingon:make-option
    :string
    :description "Principal type"
    :long-name "principal-type"
    :initial-value "service"
    :key :remote-principal-type)
   (clingon:make-option
    :string
    :description "Comma-separated scopes"
    :long-name "scopes"
    :required t
    :key :remote-scopes)
   (clingon:make-option
    :integer
    :description "Optional credential lifetime in seconds"
    :long-name "expires-in-seconds"
    :key :remote-expires-in-seconds)))

(defun remote-admin/print-credential-secret (result)
  (management/print-json
   (jsown:new-js
     ("api_key" (credential-secret-result-api-key result))
     ("credential" (credential-secret-result-credential result))
     ("correlation_id"
      (or (credential-secret-result-correlation-id result) :null)))))

(defun remote-admin/credential-create-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (result response)
         (create-credential
          (make-client command)
          (clingon:getopt command :remote-owner)
          (clingon:getopt command :remote-principal-type)
          (management/parse-scopes (clingon:getopt command :remote-scopes))
          :expires-in-seconds (clingon:getopt command :remote-expires-in-seconds))
       (declare (ignore response))
       (remote-admin/print-credential-secret result)))))

(defun remote-admin/credential-list-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (value response)
         (list-credentials (make-client command))
       (declare (ignore response))
       (management/print-json value)))))

(defun remote-admin/credential-id-option ()
  (clingon:make-option
   :string
   :description "Credential identifier"
   :long-name "credential-id"
   :required t
   :key :remote-credential-id))

(defun remote-admin/credential-rotate-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (result response)
         (rotate-credential
          (make-client command)
          (clingon:getopt command :remote-credential-id)
          :overlap-seconds (or (clingon:getopt command :remote-overlap-seconds) 0))
       (declare (ignore response))
       (remote-admin/print-credential-secret result)))))

(defun remote-admin/credential-revoke-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (value response)
         (revoke-credential
          (make-client command)
          (clingon:getopt command :remote-credential-id))
       (declare (ignore response))
       (management/print-json value)))))

(defun remote-admin/credential-disable-handler (command)
  (management/run
   (lambda ()
     (multiple-value-bind (value response)
         (disable-credential
          (make-client command)
          (clingon:getopt command :remote-credential-id))
       (declare (ignore response))
       (management/print-json value)))))

(defun remote-admin/credential-command ()
  (clingon:make-command
   :name "credential"
   :description "Manage remote API credentials"
   :handler #'management/group-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create an API credential"
     :options (remote-admin/credential-create-options)
     :handler #'remote-admin/credential-create-handler)
    (clingon:make-command
     :name "list"
     :description "List API credentials"
     :handler #'remote-admin/credential-list-handler)
    (clingon:make-command
     :name "rotate"
     :description "Rotate an API credential"
     :options
     (list
      (remote-admin/credential-id-option)
      (clingon:make-option
       :integer
       :description "Old/new overlap in seconds"
       :long-name "overlap-seconds"
       :initial-value 0
       :key :remote-overlap-seconds))
     :handler #'remote-admin/credential-rotate-handler)
    (clingon:make-command
     :name "revoke"
     :description "Revoke an API credential"
     :options (list (remote-admin/credential-id-option))
     :handler #'remote-admin/credential-revoke-handler)
    (clingon:make-command
     :name "disable"
     :description "Disable an API credential"
     :options (list (remote-admin/credential-id-option))
     :handler #'remote-admin/credential-disable-handler))))

(defun remote-admin/command ()
  (clingon:make-command
   :name "admin"
   :description "Manage the remote server through the authenticated SDK"
   :handler #'management/group-handler
   :sub-commands (list (remote-admin/user-command)
                       (remote-admin/credential-command))))

(defun main/command ()
  "Main command definition with authenticated management surfaces."
  (clingon:make-command
   :name "star-cli"
   :version "0.2.0"
   :description "Command-line client for StarIntel Gserver API"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (management/global-options)
   :handler #'main/handler
   :sub-commands (list
                  (auth/command)
                  (remote-admin/command)
                  (document/command)
                  (target/command)
                  (query/command)
                  (bulk/command)
                  (bbp/command)
                  (gen/command))))
