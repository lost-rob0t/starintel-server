(asdf:defsystem :star-cli
  :version      "0.2.0"
  :description  "Command-line client for StarIntel Gserver API"
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial       t
  :build-operation program-op
  :build-pathname "star-cli"
  :entry-point "star-cli::main"
  :components   ((:file "star-cli")
                 (:file "star-cli-management"))
  :depends-on   (#:starintel-gserver-client
                 #:clingon
                 #:jsown
                 #:quri))
