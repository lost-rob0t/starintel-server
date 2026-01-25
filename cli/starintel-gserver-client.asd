(asdf:defsystem :starintel-gserver-client
  :version      "0.1.0"
  :description  "Api client for gserver."
  :author       "nsaspy@airmail.cc"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "api-client"))
  :depends-on   (#:starintel #:jsown #:uuid #:dexador #:quri))
