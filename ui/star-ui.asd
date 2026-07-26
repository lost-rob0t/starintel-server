(asdf:defsystem :star-ui
  :version "0.1.0"
  :description "Web UI for StarIntel target creation"
  :author "nsaspy@airmail.cc"
  :license "GPL v3"
  :serial t
  :build-operation program-op
  :build-pathname "star-ui"
  :entry-point "star-ui::main"
  :components ((:file "package")
               (:file "ui-server")
               (:file "main"))
  :depends-on (#:ningle
               #:clack
               #:clack-handler-hunchentoot
               #:lack
               #:lack/middleware/accesslog
               #:jsown
               #:babel
               #:alexandria
               #:dexador
               #:log4cl))
