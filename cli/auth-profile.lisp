(in-package :star-cli)

(defun auth/environment-value (name)
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl (uiop:getenv name))

(defun auth/runtime-directory ()
  #P"/tmp/")

(defun auth/config-path (command)
  (or (auth/environment-value "STAR_AUTH_CONFIG")
      (clingon:getopt command :auth-config)
      (merge-pathnames
       "starintel/auth.json"
       (uiop:ensure-directory-pathname
        (or (auth/environment-value "XDG_CONFIG_HOME")
            (and (auth/environment-value "HOME")
                 (merge-pathnames ".config/"
                                  (uiop:ensure-directory-pathname
                                   (auth/environment-value "HOME"))))
            (merge-pathnames ".config/" (user-homedir-pathname)))))))

(defun auth/secret-tool-run (arguments input)
  ;; UIOP treats strings as filenames. Keep credential contents in a stream,
  ;; and use NIL (empty stdin) for lookup/clear, which have no input.
  (with-input-from-string (input-stream (or input ""))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (cons "secret-tool" arguments)
                          :directory (auth/runtime-directory)
                          :input (and input input-stream)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (unless (eql exit-code 0)
        (error "Secret Service request failed"))
      output)))

(defvar *secret-service-runner* #'auth/secret-tool-run)

(defun auth/secret-service-call (arguments &optional input)
  (handler-case
      (funcall *secret-service-runner* arguments input)
    (error ()
      (error "Secret Service unavailable or locked; unlock a Secret Service-compatible wallet and retry"))))

(defun auth/safe-credential-metadata (credential)
  (let ((result (jsown:new-js)))
    (dolist (key '("id" "credential_id" "owner" "principal_type" "scopes"
                   "status" "created_at" "expires_at"))
      (let ((value (jsown:val-safe credential key)))
        (when value
          (setf (jsown:val result key) value))))
    result))

(defun auth/safe-user-metadata (user)
  (let ((result (jsown:new-js)))
    (dolist (key '("username" "principal_type" "scopes" "status" "created_at"
                   "password_updated_at" "must_change_password"))
      (let ((value (jsown:val-safe user key)))
        (when value
          (setf (jsown:val result key) value))))
    result))

(defun auth/safe-server-url-p (url)
  (and (stringp url)
       (or (and (>= (length url) 7)
                (string-equal "http://" url :end2 7))
           (and (>= (length url) 8)
                (string-equal "https://" url :end2 8)))
       (not (find-if (lambda (marker) (search marker url :test #'char-equal))
                      '("@" "?" "#")))))

(defun auth/store-secret (profile secret)
  (unless (and (stringp profile) (plusp (length profile)))
    (error "A wallet profile name is required"))
  (unless (and (stringp secret) (plusp (length secret)))
    (error "Refusing to store an empty credential"))
  (auth/secret-service-call
   (list "store" "--label" (format nil "StarIntel CLI: ~a" profile)
         "application" "star-cli" "profile" profile)
   (format nil "~a~%" secret))
  t)

(defun auth/lookup-secret (profile)
  (let ((secret
          (string-right-trim
           '(#\Return #\Newline)
           (auth/secret-service-call
            (list "lookup" "application" "star-cli" "profile" profile)))))
    (unless (plusp (length secret))
      (error "No credential is stored for profile ~a" profile))
    secret))

(defun auth/clear-secret (profile)
  (auth/secret-service-call
   (list "clear" "application" "star-cli" "profile" profile))
  t)

(defun auth/empty-config ()
  (jsown:new-js ("version" 1)
                ("default_profile" :null)
                ("profiles" nil)))

(defun auth/json-object-keys (object)
  (unless (and (consp object) (eq (first object) :obj))
    (error "invalid auth config object"))
  (mapcar #'car (rest object)))

(defun auth/ensure-config-keys (object allowed)
  (unless (every (lambda (key) (member key allowed :test #'string=))
                 (auth/json-object-keys object))
    (error "unsupported auth config field")))

(defun auth/read-config (path)
  (if (probe-file path)
      (handler-case
          (let ((config (jsown:parse (uiop:read-file-string path))))
            (auth/ensure-config-keys
             config '("version" "default_profile" "profiles"))
            (unless (and (eql 1 (jsown:val-safe config "version"))
                         (listp (jsown:val-safe config "profiles")))
              (error "unsupported configuration shape"))
            (dolist (profile (jsown:val-safe config "profiles"))
              (auth/ensure-config-keys profile '("name" "server_url"))
              (let ((name (jsown:val-safe profile "name"))
                    (url (jsown:val-safe profile "server_url")))
                (unless (and (stringp name) (plusp (length name)))
                  (error "auth profile name is required"))
                (when (and url (not (eq url :null))
                           (not (auth/safe-server-url-p url)))
                  (error "invalid server URL in auth config"))))
            config)
        (error () (error "Invalid auth config at ~a" path)))
      (auth/empty-config)))

(defun auth/write-config (path config)
  (let* ((target (pathname path))
         (temporary (pathname (format nil "~a.tmp" (namestring target))))
         (success nil))
    (handler-case
        (ensure-directories-exist target)
      (error ()
        (error "Cannot create the auth config directory for ~a"
               (namestring target))))
    (unwind-protect
         (progn
           (handler-case
               (with-open-file (stream temporary :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                 (write-string (jsown:to-json config) stream)
                 (terpri stream)
                 (finish-output stream))
             (error ()
               (error "Cannot write auth config temporary file ~a"
                      (namestring temporary))))
           (handler-case
               (uiop:rename-file-overwriting-target temporary target)
              (error ()
                (error "Cannot publish auth config at ~a" (namestring target))))
           (setf success t))
      (unless success
        (ignore-errors (delete-file temporary))))
    target))

(defun auth/profile-name (command config)
  (or (clingon:getopt command :auth-profile)
      (auth/environment-value "STAR_SERVER_PROFILE")
      (let ((default (jsown:val-safe config "default_profile")))
        (and (stringp default) default))))

(defun auth/profile-entry (config name)
  (when name
    (find name (jsown:val-safe config "profiles")
          :key (lambda (profile) (jsown:val-safe profile "name"))
          :test #'string=)))

(defun auth/require-configured-profile (command name)
  (let ((config (auth/read-config (auth/config-path command))))
    (or (auth/profile-entry config name)
        (error "Unknown auth profile ~a" name))))

(defun auth/selected-profile (command)
  (let* ((config (auth/read-config (auth/config-path command)))
         (name (auth/profile-name command config))
         (profile (auth/profile-entry config name)))
    (when (and name (null profile))
      (error "Unknown auth profile ~a" name))
    (values config name profile)))

(defun auth/selected-base-url (command)
  (multiple-value-bind (config name profile) (auth/selected-profile command)
    (declare (ignore config name))
    (let* ((profile-url (and profile (jsown:val-safe profile "server_url")))
           (url (or (clingon:getopt command :base-url)
                    (and (stringp profile-url) profile-url)
                    "http://127.0.0.1:5000")))
      (unless (auth/safe-server-url-p url)
        (error "Server URL must be an HTTP(S) base URL without userinfo, query, or fragment"))
      url)))

(defun auth/selected-api-key (command)
  (or (auth/environment-value "STAR_SERVER_TOKEN")
      (auth/environment-value "STAR_SERVER_API_KEY")
      (multiple-value-bind (config name profile) (auth/selected-profile command)
        (declare (ignore config profile))
        (and name (auth/lookup-secret name)))))

(defun auth/require-profile-name (command)
  (multiple-value-bind (config name profile) (auth/selected-profile command)
    (declare (ignore config profile))
    (or name (error "Select an auth profile with --profile before using wallet credentials"))))

(defun auth/profile-list-handler (command)
  (management/run
   (lambda ()
     (let* ((config (auth/read-config (auth/config-path command)))
            (profiles (jsown:val-safe config "profiles")))
       (management/print-json
        (mapcar (lambda (profile)
                  (jsown:new-js
                    ("name" (jsown:val-safe profile "name"))
                    ("server_url" (or (jsown:val-safe profile "server_url") :null))
                    ("wallet" "freedesktop-secret-service")))
                profiles))))))

(defun auth/profile-path-handler (command)
  (management/run
   (lambda ()
     (format t "~a~%" (namestring (pathname (auth/config-path command)))))))

(defun auth/profile-name-option ()
  (clingon:make-option :string :description "Profile name"
                       :long-name "name" :required t :key :profile-name))

(defun auth/profile-add-handler (command)
  (management/run
   (lambda ()
     (let* ((path (auth/config-path command))
            (config (auth/read-config path))
            (name (clingon:getopt command :profile-name))
            (server-url (clingon:getopt command :profile-server-url))
            (profiles (jsown:val-safe config "profiles")))
       (unless (and (plusp (length name))
                    (every (lambda (character)
                             (or (alphanumericp character)
                                 (find character "._-")))
                           name))
         (error "Profile names may contain only letters, digits, dot, underscore, and hyphen"))
       (when (auth/profile-entry config name)
         (error "Auth profile already exists: ~a" name))
       (when (and server-url (not (auth/safe-server-url-p server-url)))
         (error "Server URL must be an HTTP(S) base URL without userinfo, query, or fragment"))
       (setf (jsown:val config "profiles")
             (append profiles
                     (list (jsown:new-js
                             ("name" name)
                             ("server_url" (or server-url :null))))))
       (when (member (jsown:val-safe config "default_profile") '(nil :null))
         (setf (jsown:val config "default_profile") name))
       (auth/write-config path config)
       (management/print-json
        (jsown:new-js ("name" name)
                      ("server_url" (or server-url :null))
                      ("wallet" "freedesktop-secret-service")))))))

(defun auth/profile-use-handler (command)
  (management/run
   (lambda ()
     (let* ((path (auth/config-path command))
            (config (auth/read-config path))
            (name (clingon:getopt command :profile-name)))
       (unless (auth/profile-entry config name)
         (error "Unknown auth profile ~a" name))
       (setf (jsown:val config "default_profile") name)
       (auth/write-config path config)
       (management/print-json (jsown:new-js ("default_profile" name)))))))

(defun auth/token-set-handler (command)
  (management/run
   (lambda ()
     (let* ((name (or (clingon:getopt command :profile-name)
                      (auth/require-profile-name command)))
            (secret (read-line *standard-input* nil nil)))
       (auth/require-configured-profile command name)
       (unless (and secret (plusp (length secret)))
         (error "Credential stdin was empty"))
       (auth/store-secret name secret)
       (management/print-json
        (jsown:new-js ("profile" name) ("stored" t)))))))

(defun auth/token-clear-handler (command)
  (management/run
   (lambda ()
     (let ((name (or (clingon:getopt command :profile-name)
                     (auth/require-profile-name command))))
       (auth/require-configured-profile command name)
       (auth/clear-secret name)
       (management/print-json
        (jsown:new-js ("profile" name) ("cleared" t)))))))
