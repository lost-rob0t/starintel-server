(asdf:defsystem :starintel-expert-shell
  :version "0.1.0"
  :description "Lisa-backed expert operator shell for StarIntel."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :build-operation program-op
  :build-pathname "star-expert"
  :entry-point "star.expert.shell:main"
  :depends-on (#:starintel-gserver-client
               #:lisa
               #:jsown)
  :components
  ((:module "expert-shell"
    :serial t
    :components
    ((:file "package")
     (:file "model")
     (:file "rules")
     (:file "shell")))))
