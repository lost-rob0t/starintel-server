(asdf:defsystem :starintel-observability
  :version "0.1.0"
  :description "StarIntel telemetry abstraction: OTLP export, W3C trace context, redaction, metrics."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :components
  ((:module "source/observability"
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "trace")
     (:file "redaction")
     (:file "otlp")
     (:file "api")
     (:file "http-middleware"))))
  :depends-on
  (#:uiop #:dexador #:jsown #:bordeaux-threads #:ironclad #:cl-ppcre
   #:lack))
