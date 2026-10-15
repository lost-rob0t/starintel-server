(asdf:defsystem #:starintel-audit
  :description "Optional inert CouchDB authorization audit adapter"
  :version "0.1.0"
  :license "GPL-3.0-or-later"
  :depends-on (#:jsown #:drakma #:ironclad #:babel #:cl-ppcre)
  :serial t
  :components ((:file "addons/audit/couchdb")))
