;; [[file:../source.org::*ASDF][ASDF:1]]
(asdf:defsystem :starintel-gserver
  :version      "0.1.0"
  :description  "hackable/moddable starintel acess api."
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :components   (
                 (:file "package")
                 (:file "gserver-settings" :depends-on ("package"))
                 (:file "actors" :depends-on ("gserver-settings"))
                 (:file "rabbit" :depends-on ("actors" "gserver-settings"))
                 (:file "frontends/http-api" :depends-on ("gserver-settings"))
                 (:file "main" :depends-on ("actors" "rabbit" "package" "gserver-settings" "frontends/http-api")))

  :depends-on   (#:starintel #:cl-couch  #:alexandria #:cl-rabbit #:sento #:babel #:cl-json :cl-ulid #:anypool #:cl-cpus
                    #:clack #:ningle #:hunchentoot))
;; ASDF:1 ends here
