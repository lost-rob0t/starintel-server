(asdf:defsystem "starintel-router-server"
  :description "Optional canonical router command adapter to authorized Server ingest"
  :license "GPL-3.0-or-later"
  :depends-on ("starintel-gserver" "starintel-router" "starlang-runtime")
  :serial t
  :components ((:file "source/router/application")
               (:file "source/router/sc01-application")
               (:file "source/router/sc01-host")
               (:file "source/router/target")
               (:file "source/router/lifecycle"))
  :in-order-to ((asdf:test-op (asdf:test-op "starintel-router-server/tests"))))

(asdf:defsystem "starintel-router-server/tests"
  :depends-on ("starintel-router-server" "fiveam")
  :serial t
  :components ((:file "t/router-application-test")
               (:file "t/router-sc01-application-test")
               (:file "t/router-sc01-host-test")
               (:file "t/router-lifecycle-test")
               (:file "t/router-host-test"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (and (uiop:symbol-call :fiveam :run! :router-application)
                          (uiop:symbol-call :fiveam :run! :router-sc01-application)
                          (uiop:symbol-call :fiveam :run! :router-sc01-host)
                          (uiop:symbol-call :fiveam :run! :router-lifecycle)
                          (uiop:symbol-call :fiveam :run! :router-host))
               (error "Router application tests failed"))))
