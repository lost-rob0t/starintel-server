(asdf:defsystem :starintel-gserver-client
  :version      "0.1.0"
  :description  "Api client for gserver."
  :author       " <unseen@flake>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "api-client"))
  :depends-on   (#:starintel #:cl-json #:uuid #:dexador #:cl-csv #:data-table #:cl-csv-data-table))
