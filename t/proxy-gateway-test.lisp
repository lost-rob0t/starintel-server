(in-package :star-server-tests)

(def-suite proxy-gateway-tests
  :description "Core proxy gateway and Evomi provider contract")

(in-suite proxy-gateway-tests)

(test proxy-gateway-has-typed-request-contract
  (is (fboundp (find-symbol "MAKE-PROXY-REQUEST" :star.actors)))
  (is (fboundp (find-symbol "MAKE-PROXY-GATEWAY-HANDLER" :star.actors))))

(defun make-proxy-test-system ()
  (make-actor-system '(:dispatchers
                       (:pinned (:workers 2 :strategy :random)
                        :proxy (:workers 2 :strategy :random))
                       :timeout-timer (:resolution 50 :max-size 100))))

(defun proxy-test-request (&key (mode :rotating) seed
                                (project-id "project-a") (country "US"))
  (star.actors:make-proxy-request
   :mode mode :project-id project-id :seed seed :country country))

(test proxy-actors-return-rotating-and-seeded-sticky-configurations
  (let ((system (make-proxy-test-system))
        (calls nil))
    (unwind-protect
         (let* ((provider
                  (actor-of system :name "test-evomi-provider"
                           :dispatcher :proxy
                           :receive
                           (star.actors:make-evomi-provider-handler
                            :api-key "test-api-key"
                            :session-secret "test-session-secret"
                            :transport
                            (lambda (url api-key)
                              (push (list url api-key) calls)
                              (values "rp.evomi.com:1000:alice:provider-pass" 200)))))
                (gateway
                  (actor-of system :name "test-proxy-gateway"
                           :dispatcher :pinned
                           :receive
                           (star.actors:make-proxy-gateway-handler provider)))
                (rotating
                  (sento.actor:ask-s gateway (proxy-test-request) :time-out 3))
                (request (proxy-test-request :mode :sticky :seed "target-42"))
                (sticky-a (sento.actor:ask-s gateway request :time-out 3))
                (sticky-b (sento.actor:ask-s gateway request :time-out 3))
                (rotating-config (star.actors:proxy-result-config rotating))
                (sticky-config (star.actors:proxy-result-config sticky-a)))
           (is (eq :ok (star.actors:proxy-result-status rotating)))
           (is (eq :rotating
                   (star.actors:proxy-config-stability rotating-config)))
           (is (null (star.actors:proxy-config-session-id rotating-config)))
           (is (string= "provider-pass"
                        (star.actors:proxy-config-password rotating-config)))
           (is (eq :ok (star.actors:proxy-result-status sticky-a)))
           (is (eq :best-effort
                   (star.actors:proxy-config-stability sticky-config)))
           (is (= 10 (length (star.actors:proxy-config-session-id sticky-config))))
           (is (string= (star.actors:proxy-config-session-id sticky-config)
                        (star.actors:proxy-config-session-id
                         (star.actors:proxy-result-config sticky-b))))
           (is (string= (star.actors:proxy-config-password sticky-config)
                        (star.actors:proxy-config-password
                         (star.actors:proxy-result-config sticky-b))))
           (is (not (search "target-42"
                            (star.actors:proxy-config-password sticky-config))))
           (is (= 3 (length calls)))
           (is (every (lambda (call)
                        (and (search "product=rp" (first call))
                             (search "countries=US" (first call))
                             (not (search "apikey" (first call)))
                             (string= "test-api-key" (second call))))
                      calls)))
      (ac:shutdown system))))

(test proxy-gateway-validates-before-provider-and-reports-vpn-unsupported
  (let ((system (make-proxy-test-system))
        (provider-calls 0))
    (unwind-protect
         (let* ((gateway
                  (actor-of system :name "test-proxy-validation"
                           :dispatcher :pinned
                           :receive
                           (star.actors:make-proxy-gateway-handler
                            nil :ask-fn
                            (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (incf provider-calls)))))
                (missing-seed
                  (sento.actor:ask-s
                   gateway (proxy-test-request :mode :sticky) :time-out 3))
                (bad-project
                  (sento.actor:ask-s
                   gateway (proxy-test-request :project-id "bad/project")
                   :time-out 3))
                (bad-lifetime
                  (sento.actor:ask-s
                   gateway (star.actors:make-proxy-request
                            :mode :sticky :project-id "project-a"
                            :seed "target-42" :lifetime-minutes 1441)
                   :time-out 3))
                (vpn
                  (sento.actor:ask-s gateway
                                      (proxy-test-request :mode :vpn)
                                      :time-out 3))
                (capabilities
                  (sento.actor:ask-s gateway
                                      (proxy-test-request :mode :capabilities)
                                      :time-out 3)))
           (is (eq :invalid-request
                   (star.actors:proxy-result-status missing-seed)))
           (is (eq :invalid-request
                   (star.actors:proxy-result-status bad-project)))
           (is (eq :invalid-request
                   (star.actors:proxy-result-status bad-lifetime)))
           (is (eq :unsupported (star.actors:proxy-result-status vpn)))
           (is (null (getf (star.actors:proxy-result-capabilities capabilities)
                           :vpn)))
           (is (= 0 provider-calls)))
      (ac:shutdown system))))

(test evomi-provider-requires-configuration-before-network
  (let ((system (make-proxy-test-system))
        (calls 0))
    (unwind-protect
         (let* ((provider
                  (actor-of system :name "test-evomi-unconfigured"
                           :dispatcher :proxy
                           :receive
                           (star.actors:make-evomi-provider-handler
                            :api-key nil :session-secret nil
                            :transport (lambda (&rest arguments)
                                         (declare (ignore arguments))
                                         (incf calls)))))
                (result
                  (sento.actor:ask-s provider (proxy-test-request)
                                      :time-out 3)))
           (is (eq :unavailable (star.actors:proxy-result-status result)))
           (is (= 0 calls)))
      (ac:shutdown system))))

(test evomi-provider-fails-closed-and-redacts-credentials
  (let ((system (make-proxy-test-system)))
    (unwind-protect
         (let* ((provider
                  (actor-of system :name "test-evomi-failure"
                           :dispatcher :proxy
                           :receive
                           (star.actors:make-evomi-provider-handler
                            :api-key "secret-api-key"
                            :session-secret "secret-session-key"
                            :transport
                            (lambda (url key)
                              (declare (ignore url key))
                              (values "{\"error\":\"secret-provider-body\"}" 403)))))
                (result
                  (sento.actor:ask-s provider (proxy-test-request)
                                      :time-out 3)))
           (is (eq :provider-error (star.actors:proxy-result-status result)))
           (is (null (star.actors:proxy-result-config result)))
           (is (not (search "secret"
                            (princ-to-string result)))))
      (ac:shutdown system)))
  (let* ((request (proxy-test-request :mode :sticky :seed "target-42"))
         (config
           (star.actors:parse-evomi-proxy
            "rp.evomi.com:1000:alice:provider-pass" request "0123456789"))
         (result (star.actors:make-proxy-result :status :ok :config config)))
    (is (not (search "provider-pass" (princ-to-string config))))
    (is (not (search "provider-pass" (princ-to-string result))))
    (is (not (search "target-42" (princ-to-string request))))
    (signals error
      (star.actors:parse-evomi-proxy
       "attacker.example:1000:alice:provider-pass" request nil))))

(test proxy-gateway-async-requests-and-startup-lifecycle
  (let ((system (make-proxy-test-system))
        (old-provider star.actors:*evomi-provider*)
        (old-gateway star.actors:*proxy-gateway*))
    (unwind-protect
         (progn
           (star.actors:start-proxy-actors
            system :api-key "test-api-key" :session-secret "test-session-key"
            :transport (lambda (url key)
                         (declare (ignore url key))
                         (values "rp.evomi.com:1000:user:pass" 200)))
           (is (not (null star.actors:*evomi-provider*)))
           (is (not (null star.actors:*proxy-gateway*)))
           (let* ((first
                    (sento.actor:ask
                     star.actors:*proxy-gateway*
                     (proxy-test-request :mode :sticky :seed "target-a")
                     :time-out 3))
                  (second
                    (sento.actor:ask
                     star.actors:*proxy-gateway*
                     (proxy-test-request :mode :sticky :seed "target-b")
                     :time-out 3))
                  (result-a (wait-for-future-result first))
                  (result-b (wait-for-future-result second)))
             (is (typep result-a 'star.actors:proxy-result))
             (is (typep result-b 'star.actors:proxy-result))
             (is (eq :ok (star.actors:proxy-result-status result-a)))
             (is (eq :ok (star.actors:proxy-result-status result-b)))
             (is (not
                  (string= (star.actors:proxy-config-session-id
                            (star.actors:proxy-result-config result-a))
                           (star.actors:proxy-config-session-id
                            (star.actors:proxy-result-config result-b)))))))
      (ac:shutdown system)
      (setf star.actors:*evomi-provider* old-provider
            star.actors:*proxy-gateway* old-gateway))))
