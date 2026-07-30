(asdf:defsystem :star-migrations
  :version      "0.1.0"
  :description  "Migrate documents to newer versions"
  :author       " <unseen@flake>"
  :serial       t
  :license      "AGPL-3.0-only"
  :components   ((:file "migrations"))
  :depends-on   (#:lparallel #:dexador #:cl-couch))
