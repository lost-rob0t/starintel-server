(asdf:defsystem :star-migrations
  :version      "0.2.0"
  :description  "Pure validation helpers for Prolog-derived StarIntel schema migrations"
  :author       "nsaspy@airmail.cc"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "migrations"))
  :depends-on   (#:jsown))
