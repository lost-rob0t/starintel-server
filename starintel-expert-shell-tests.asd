(asdf:defsystem :starintel-expert-shell-tests
  :version "0.1.0"
  :description "Tests for the Lisa-backed StarIntel expert shell."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:starintel-expert-shell
               #:fiveam)
  :components
  ((:module "t"
    :serial t
    :components
    ((:file "expert-shell-test"))))
  :perform
  (test-op (operation component)
    (declare (ignore operation component))
    (uiop:symbol-call
     :star.expert.shell.tests
     :run-expert-shell-tests)))
