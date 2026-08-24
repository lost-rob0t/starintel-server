(in-package :star-server-tests)

(def-suite addon-system-tests
  :description "ASDF add-on lifecycle and Bixby package boundary")

(in-suite addon-system-tests)

(test bixby-is-an-optional-asdf-addon-over-core-oauth
  (let ((before (star:addon-status :starintel-bixby)))
    (when (and before
               (eq :active (star:addon-state-status before)))
      (star:unload-addon :starintel-bixby)))
  (let* ((loaded (star:load-addon :starintel-bixby))
         (generation (star:addon-state-generation loaded)))
    (is (eq :active (star:addon-state-status loaded)))
    (is (string= "starintel-bixby" (star:addon-state-system loaded)))
    (star.addons.bixby:configure-bixby
     :public-base-url "https://api.starintel.example/"
     :redirect-uri "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
    (let ((settings (star.addons.bixby:bixby-oauth-settings)))
      (is (string= "https://api.starintel.example/oauth/authorize"
                   (getf settings :authorize-endpoint)))
      (is (string= "https://api.starintel.example/oauth/token"
                   (getf settings :token-endpoint)))
      (is (equal '("documents:read" "search:read")
                 (getf settings :read-scopes))))
    (let ((reloaded (star:reload-addon :starintel-bixby)))
      (is (eq :active (star:addon-state-status reloaded)))
      (is (> (star:addon-state-generation reloaded) generation)))
    (let ((stopped (star:unload-addon :starintel-bixby)))
      (is (eq :stopped (star:addon-state-status stopped))))))

(test bixby-client-registration-uses-standard-oauth-client-store
  (let* ((star:*auth-pepper* "bixby-addon-test-pepper")
         (store (make-oauth-test-world)))
    (star:load-addon :starintel-bixby)
    (star.addons.bixby:configure-bixby
     :public-base-url "https://api.starintel.example"
     :redirect-uri "https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb"
     :read-scopes '("documents:read" "search:read")
     :operations-scopes '("targets:dispatch"))
    (multiple-value-bind (client secret)
        (star.addons.bixby:create-bixby-oauth-client
         :include-operations t
         :store store)
      (is (plusp (length secret)))
      (is (equal '("documents:read" "search:read" "targets:dispatch")
                 (star.auth:oauth-client-record-allowed-scopes client)))
      (is (equal
           '("https://playground-starIntelIntelligence.oauth.aibixby.com/auth/external/cb")
           (star.auth:oauth-client-record-redirect-uris client))))
    (star:unload-addon :starintel-bixby)))
