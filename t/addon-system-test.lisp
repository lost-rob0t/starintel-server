(in-package :star-server-tests)

(def-suite addon-system-tests
  :description "ASDF add-on lifecycle and Bixby package boundary")

(in-suite addon-system-tests)

(defun call-bixby (name &rest arguments)
  (apply #'uiop:symbol-call :star.addons.bixby name arguments))

(defun load-bixby-addon-or-report-cause ()
  (handler-case
      (star:load-addon :starintel-bixby)
    (star:addon-error (condition)
      (error "Bixby add-on load failed: ~a"
             (or (star:addon-error-cause condition)
                 condition)))))

(defun reload-bixby-addon-or-report-cause ()
  (handler-case
      (star:reload-addon :starintel-bixby)
    (star:addon-error (condition)
      (error "Bixby add-on reload failed: ~a"
             (or (star:addon-error-cause condition)
                 condition)))))

(test bixby-is-an-optional-asdf-addon-over-core-oauth
  (let ((before (star:addon-status :starintel-bixby)))
    (when (and before
               (eq :active (star:addon-state-status before)))
      (star:unload-addon :starintel-bixby)))
  (let* ((loaded (load-bixby-addon-or-report-cause))
         (generation (star:addon-state-generation loaded)))
    (is (eq :active (star:addon-state-status loaded)))
    (is (string= "starintel-bixby" (star:addon-state-system loaded)))
    (call-bixby
     :configure-bixby
     :public-base-url "https://api.starintel.example/"
     :redirect-uri "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
    (let ((settings (call-bixby :bixby-oauth-settings)))
      (is (string= "https://api.starintel.example/oauth/authorize"
                   (getf settings :authorize-endpoint)))
      (is (string= "https://api.starintel.example/oauth/token"
                   (getf settings :token-endpoint)))
      (is (equal '("documents:read" "search:read")
                 (getf settings :read-scopes)))
      (is (equal '("targets:dispatch")
                 (getf settings :operations-scopes))))
    (let ((reloaded (reload-bixby-addon-or-report-cause)))
      (is (eq :active (star:addon-state-status reloaded)))
      (is (> (star:addon-state-generation reloaded) generation)))
    (let ((settings-after-reload (call-bixby :bixby-oauth-settings)))
      (is (string= "https://api.starintel.example/oauth/authorize"
                   (getf settings-after-reload :authorize-endpoint)))
      (is (string= "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb"
                   (getf settings-after-reload :redirect-uri)))
      (is (equal '("targets:dispatch")
                 (getf settings-after-reload :operations-scopes))))
    (let ((stopped (star:unload-addon :starintel-bixby)))
      (is (eq :stopped (star:addon-state-status stopped))))))

(test bixby-client-registration-uses-standard-oauth-client-store
  (let* ((star:*auth-pepper* "bixby-addon-test-pepper")
         (store (make-oauth-test-world)))
    (load-bixby-addon-or-report-cause)
    (call-bixby
     :configure-bixby
     :public-base-url "https://api.starintel.example"
     :redirect-uri "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb"
     :read-scopes '("documents:read" "search:read")
     :operations-scopes '("targets:dispatch"))
    (multiple-value-bind (client secret)
        (call-bixby
         :create-bixby-oauth-client
         :include-operations t
         :store store)
      (is (plusp (length secret)))
      (is (equal '("documents:read" "search:read" "targets:dispatch")
                 (star.auth:oauth-client-record-allowed-scopes client)))
      (is (equal
           '("https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
           (star.auth:oauth-client-record-redirect-uris client))))
    (star:unload-addon :starintel-bixby)))