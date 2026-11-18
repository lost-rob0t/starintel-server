(asdf:defsystem :starintel-biz-auth
  :version "0.1.0"
  :description "Identity lifecycle (create/ban) in the canonical StarIntel auth system (ZITADEL) for authorized service principals."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:starintel-gserver)
  :components
  ((:module "addons/biz-auth"
    :serial t
    :components
    ((:file "package")
     (:file "biz-auth")))))
