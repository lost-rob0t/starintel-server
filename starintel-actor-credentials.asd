(asdf:defsystem :starintel-actor-credentials
  :version "0.1.0"
  :description "Optional actor account credential resolver for StarIntel Server."
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:starintel-gserver)
  :components
  ((:module "addons/actor-credentials"
    :serial t
    :components
    ((:file "package")
     (:file "actor-credentials")))))
