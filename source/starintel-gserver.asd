(asdf:defsystem :starintel-gserver
  :version      "0.1.0"
  :description  "hackable/moddable starintel acess api."
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :components   ((:file "gserver-settings")
                 (:file "package" :depends-on ("gserver-settings"))
                 (:file "actors" :depends-on ("gserver-settings"))
                 (:file "rabbit" :depends-on ("actors" "gserver-settings"))
                 (:file "main" :depends-on ("actors" "rabbit" "package" "gserver-settings")))

  :depends-on   (#:alexandria #:cl-rabbit #:sento #:starintel #:babel #:cl-couch #:cl-json :cl-ulid #:anypool #:cl-cpus))
