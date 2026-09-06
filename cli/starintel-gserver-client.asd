(asdf:defsystem :starintel-gserver-client
  :version      "0.2.0"
  :description  "Reusable Common Lisp client for the StarIntel gserver HTTP protocol."
  :author       "nsaspy@airmail.cc"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "http-contract-package")
                 (:file "http-contract")
                 (:file "http-contract-final")
                 (:file "http-contract-targets")
                 (:file "client-package")
                 (:file "client-runtime")
                 (:file "client-compat")
                 (:file "client-runtime-final")
                 (:file "generated-operations")
                 (:file "client-convenience")
                 (:file "api-client"))
  :depends-on   (#:starintel
                 #:jsown
                 #:uuid
                 #:dexador
                 #:quri
                 #:bordeaux-threads))