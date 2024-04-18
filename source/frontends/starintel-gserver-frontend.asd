;; [[file:../../source.org::*asdf][asdf:1]]
(asdf:defsystem :starintel-gserver-frontend
  :version      "0.1.0"
  :description  "Front end API system(s) for the starintel data system"
  :author       "nsaspy@airmail.cc"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "package")
                 (:file "http-api"))
  :depends-on   (#:ningle #:clack #:lack #:starintel-gserver))
;; asdf:1 ends here
