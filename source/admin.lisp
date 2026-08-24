(in-package :starintel-gserver)

;;; Server-local administration deliberately bypasses HTTP transport while
;;; reusing the same star.auth lifecycle services as the HTTP handlers.

(defun random-admin-password ()
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun read-admin-password-file (path)
  (let ((value (and path (read-secret-file path))))
    (unless (and value (plusp (length value)))
      (error "Password file is missing or empty: ~a" path))
    value))

(defun resolve-admin-password (&key password password-file password-stdin random-password)
  "Resolve exactly one password source and return PASSWORD, GENERATED-P."
  (let ((source-count (count-if #'identity
                                (list password
                                      password-file
                                      password-stdin
                                      random-password))))
    (unless (= source-count 1)
      (error "Choose exactly one of --password, --password-file, --password-stdin, or --random-password"))
    (cond
      (password (values password nil))
      (password-file (values (read-admin-password-file password-file) nil))
      (password-stdin
       (let ((value (read-line *standard-input* nil nil)))
         (unless (and value (plusp (length value)))
           (error "Password stdin was empty"))
         (values value nil)))
      (random-password
       (values (random-admin-password) t)))))

(defun parse-admin-scopes (value)
  (and value (split-comma-setting value)))

(defun admin-create-user* (username password scopes
                           &key
                             (principal-type "user")
                             administrator
                             (must-change-password t)
                             (store star.auth::*credential-store*))
  (let ((effective-principal-type
          (if administrator "administrator" principal-type))
        (effective-scopes
          (if administrator '("admin") scopes)))
    (unless effective-scopes
      (error "Normal users require at least one scope"))
    (star.auth:create-user
     username
     password
     effective-principal-type
     effective-scopes
     :must-change-password must-change-password
     :store store)))

(defun admin-reset-user-password* (username password
                                   &key
                                     (must-change-password t)
                                     (store star.auth::*credential-store*))
  (star.auth:admin-set-user-password
   username
   password
   :must-change-password must-change-password
   :store store))

(defun admin-create-credential* (owner principal-type scopes
                                 &key expires-in-seconds
                                   (store star.auth::*credential-store*))
  (star.auth:create-api-key
   owner principal-type scopes
   :expires-in-seconds expires-in-seconds
   :store store))

(defun admin-rotate-credential* (credential-id overlap-seconds
                                 &key (store star.auth::*credential-store*))
  (star.auth:rotate-api-key credential-id overlap-seconds :store store))

(defun admin-revoke-credential* (credential-id
                                 &key (store star.auth::*credential-store*))
  (star.auth:revoke-api-key credential-id :store store))

(defun admin-disable-credential* (credential-id
                                  &key (store star.auth::*credential-store*))
  (star.auth:disable-api-key credential-id :store store))

(defun admin/initialize-store (command)
  (safe-load-init (clingon:getopt command :init-value))
  (star.auth:initialize-auth-store :force t))

(defun admin/print-json (value)
  (format t "~a~%" (jsown:to-json value)))

(defun admin/print-user-with-password (record password generated-p)
  (let ((document
          (jsown:new-js
            ("user" (star.auth:user-metadata-json record)))))
    (when generated-p
      (setf (jsown:val document "temporary_password") password))
    (admin/print-json document)))

(defun admin/print-credential-secret (record raw-key)
  (admin/print-json
   (jsown:new-js
     ("api_key" raw-key)
     ("credential" (star.auth:api-key-metadata-json record)))))

(defun admin/call (command thunk)
  (handler-case
      (progn
        (admin/initialize-store command)
        (funcall thunk))
    (star.auth:credential-lifecycle-error (condition)
      (format *error-output* "star-server admin: ~a (~a)~%"
              (star.auth:credential-lifecycle-error-message condition)
              (star.auth:credential-lifecycle-error-code condition))
      (clingon:exit 1))
    (error (condition)
      (format *error-output* "star-server admin: ~a~%" condition)
      (clingon:exit 1))))

(defun admin/password-options ()
  (list
   (clingon:make-option
    :string
    :description "Password value (prefer --password-file or --password-stdin)"
    :long-name "password"
    :key :admin-password)
   (clingon:make-option
    :string
    :description "Read the password from a file"
    :long-name "password-file"
    :key :admin-password-file)
   (clingon:make-option
    :boolean
    :description "Read the password from standard input"
    :long-name "password-stdin"
    :initial-value nil
    :key :admin-password-stdin)
   (clingon:make-option
    :boolean
    :description "Generate a cryptographically random temporary password"
    :long-name "random-password"
    :initial-value nil
    :key :admin-random-password)))

(defun admin/password-from-command (command)
  (resolve-admin-password
   :password (clingon:getopt command :admin-password)
   :password-file (clingon:getopt command :admin-password-file)
   :password-stdin (clingon:getopt command :admin-password-stdin)
   :random-password (clingon:getopt command :admin-random-password)))

(defun admin/user-create-options ()
  (append
   (list
    (clingon:make-option
     :string
     :description "Username"
     :long-name "username"
     :required t
     :key :admin-username)
    (clingon:make-option
     :string
     :description "Comma-separated scopes for a normal user"
     :long-name "scopes"
     :key :admin-scopes)
    (clingon:make-option
     :string
     :description "Principal type for a normal user"
     :long-name "principal-type"
     :initial-value "user"
     :key :admin-principal-type)
    (clingon:make-option
     :boolean
     :description "Create an administrator with the admin scope"
     :long-name "admin"
     :initial-value nil
     :key :admin-administrator)
    (clingon:make-option
     :boolean
     :description "Do not require the new user to change the password"
     :long-name "no-must-change-password"
     :initial-value nil
     :key :admin-no-must-change-password))
   (admin/password-options)))

(defun admin/user-create-handler (command)
  (admin/call
   command
   (lambda ()
     (multiple-value-bind (password generated-p)
         (admin/password-from-command command)
       (let ((record
               (admin-create-user*
                (clingon:getopt command :admin-username)
                password
                (parse-admin-scopes (clingon:getopt command :admin-scopes))
                :principal-type (clingon:getopt command :admin-principal-type)
                :administrator (clingon:getopt command :admin-administrator)
                :must-change-password
                (not (clingon:getopt command :admin-no-must-change-password)))))
         (admin/print-user-with-password record password generated-p))))))

(defun admin/user-list-handler (command)
  (admin/call
   command
   (lambda ()
     (admin/print-json (star.auth:list-user-metadata)))))

(defun admin/user-set-password-options ()
  (append
   (list
    (clingon:make-option
     :string
     :description "Username"
     :long-name "username"
     :required t
     :key :admin-username)
    (clingon:make-option
     :boolean
     :description "Do not require a password change after this reset"
     :long-name "no-must-change-password"
     :initial-value nil
     :key :admin-no-must-change-password))
   (admin/password-options)))

(defun admin/user-set-password-handler (command)
  (admin/call
   command
   (lambda ()
     (multiple-value-bind (password generated-p)
         (admin/password-from-command command)
       (let ((record
               (admin-reset-user-password*
                (clingon:getopt command :admin-username)
                password
                :must-change-password
                (not (clingon:getopt command :admin-no-must-change-password)))))
         (admin/print-user-with-password record password generated-p))))))

(defun admin/user-command ()
  (clingon:make-command
   :name "user"
   :description "Manage local human users without the HTTP listener"
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create a human user"
     :options (admin/user-create-options)
     :handler #'admin/user-create-handler)
    (clingon:make-command
     :name "list"
     :description "List human users"
     :handler #'admin/user-list-handler)
    (clingon:make-command
     :name "set-password"
     :description "Reset a human user's password"
     :options (admin/user-set-password-options)
     :handler #'admin/user-set-password-handler))))

(defun admin/credential-create-options ()
  (list
   (clingon:make-option
    :string
    :description "Credential owner"
    :long-name "owner"
    :required t
    :key :admin-owner)
   (clingon:make-option
    :string
    :description "Principal type"
    :long-name "principal-type"
    :initial-value "service"
    :key :admin-principal-type)
   (clingon:make-option
    :string
    :description "Comma-separated scopes"
    :long-name "scopes"
    :required t
    :key :admin-scopes)
   (clingon:make-option
    :integer
    :description "Optional expiration in seconds"
    :long-name "expires-in-seconds"
    :key :admin-expires-in-seconds)))

(defun admin/credential-create-handler (command)
  (admin/call
   command
   (lambda ()
     (multiple-value-bind (record raw-key)
         (admin-create-credential*
          (clingon:getopt command :admin-owner)
          (clingon:getopt command :admin-principal-type)
          (parse-admin-scopes (clingon:getopt command :admin-scopes))
          :expires-in-seconds (clingon:getopt command :admin-expires-in-seconds))
       (admin/print-credential-secret record raw-key)))))

(defun admin/credential-list-handler (command)
  (admin/call
   command
   (lambda ()
     (admin/print-json (star.auth:list-api-key-metadata)))))

(defun admin/credential-id-option ()
  (clingon:make-option
   :string
   :description "Credential identifier"
   :long-name "credential-id"
   :required t
   :key :admin-credential-id))

(defun admin/credential-rotate-handler (command)
  (admin/call
   command
   (lambda ()
     (multiple-value-bind (record raw-key)
         (admin-rotate-credential*
          (clingon:getopt command :admin-credential-id)
          (or (clingon:getopt command :admin-overlap-seconds) 0))
       (admin/print-credential-secret record raw-key)))))

(defun admin/credential-status-handler (command operation)
  (admin/call
   command
   (lambda ()
     (let ((record
             (funcall operation
                      (clingon:getopt command :admin-credential-id))))
       (admin/print-json (star.auth:api-key-metadata-json record))))))

(defun admin/credential-command ()
  (clingon:make-command
   :name "credential"
   :description "Manage API credentials without the HTTP listener"
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create an API credential"
     :options (admin/credential-create-options)
     :handler #'admin/credential-create-handler)
    (clingon:make-command
     :name "list"
     :description "List API credentials"
     :handler #'admin/credential-list-handler)
    (clingon:make-command
     :name "rotate"
     :description "Rotate an API credential"
     :options
     (list
      (admin/credential-id-option)
      (clingon:make-option
       :integer
       :description "Overlap window for the old credential"
       :long-name "overlap-seconds"
       :initial-value 0
       :key :admin-overlap-seconds))
     :handler #'admin/credential-rotate-handler)
    (clingon:make-command
     :name "revoke"
     :description "Revoke an API credential"
     :options (list (admin/credential-id-option))
     :handler
     (lambda (command)
       (admin/credential-status-handler command #'admin-revoke-credential*)))
    (clingon:make-command
     :name "disable"
     :description "Disable an API credential"
     :options (list (admin/credential-id-option))
     :handler
     (lambda (command)
       (admin/credential-status-handler command #'admin-disable-credential*))))))

(defun admin/command ()
  (clingon:make-command
   :name "admin"
   :description "Server-local bootstrap and recovery administration"
   :options (server/options)
   :sub-commands (list (admin/user-command)
                       (admin/credential-command))))
