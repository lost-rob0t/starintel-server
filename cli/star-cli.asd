(asdf:defsystem :star-cli
  :version      "0.1.0"
  :description  "Command-line client for StarIntel Gserver API"
  :author       "nsaspy@airmail.cc"
  :license      "AGPL-3.0-only"
  :serial       t
  :build-operation program-op
  :build-pathname "star-cli"
  :entry-point "star-cli::main"
  :components   ((:file "star-cli"))
  :depends-on   (#:starintel-gserver-client
                 #:clingon
                 #:dexador
                 #:jsown
                 #:quri))
