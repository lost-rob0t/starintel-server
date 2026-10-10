(asdf:defsystem :starintel-wardrive
  :description "Optional WarStar Wi-Fi and Bluetooth ingest add-on"
  :version "0.1.0"
  :license "GPL-3.0-or-later"
  :depends-on (#:starintel-gserver)
  :serial t
  :components ((:file "addons/wardrive/package")
               (:file "addons/wardrive/wardrive")))
