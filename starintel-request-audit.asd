(asdf:defsystem #:starintel-request-audit
  :description "Optional Lisp request-risk audit and explicitly opt-in refusal"
  :version "0.1.0"
  :license "GPL-3.0-or-later"
  :depends-on (#:starintel-gserver #:starintel-audit #:com.inuoe.jzon #:bordeaux-threads)
  :serial t
  :components ((:file "addons/request-audit/core")
               (:file "addons/request-audit/model")
               (:file "addons/request-audit/runtime")))
