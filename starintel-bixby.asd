(asdf:defsystem :starintel-bixby
  :version "0.1.0"
  :description "Optional Samsung Bixby integration package for StarIntel Server."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:starintel-gserver)
  :components
  ((:module "addons/bixby"
    :serial t
    :components
    ((:file "package")
     (:file "bixby")))))
