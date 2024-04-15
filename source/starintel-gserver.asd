(asdf:defsystem :starintel-gserver
  :version      "0.1.0"
  :description  "hackable/moddable starintel acess api."
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :components   ((:file "package")
                 (:file "gserver-settings")
                 (:file "actors")
                 (:file "rabbit" :depends-on ("actors")))

  :depends-on   (#:alexandria #:cl-rabbit #:sento #:starintel #:babel #:cl-couch #:cl-json :cl-ulid #:anypool #:cl-cpus))
