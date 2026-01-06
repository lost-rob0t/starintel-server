(asdf:defsystem :star-cli
  :version      "0.1.0"
  :description  "Command-line client for StarIntel Gserver API"
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial       t
  :build-operation program-op
  :build-pathname "star-cli"
  :entry-point "star-cli::main"
  :components   ((:file "source/star-cli"))
  :depends-on   (#:starintel-gserver-client
                 #:clingon
                 #:dexador
                 #:jsown
                 #:quri))
