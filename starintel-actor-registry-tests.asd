(asdf:defsystem :starintel-actor-registry-tests
  :version "0.1.0"
  :description "Focused actor registry contract tests"
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:starintel-gserver #:fiveam #:jsown)
  :components
  ((:module "t"
    :serial t
    :components
    ((:file "package")
     (:file "test-runner")
     (:file "actor-registry-test"))))
  :perform
  (test-op (operation component)
    (declare (ignore operation component))
    (uiop:symbol-call
     :star-server-tests
     :run-required-suite
     (intern "ACTOR-REGISTRY-TESTS" :star-server-tests))))
