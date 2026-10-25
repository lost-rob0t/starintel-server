(asdf:defsystem "starintel-router"
  :description "Opt-in native StarRouter CFFI boundary; no listeners on load"
  :license "GPL-3.0-or-later"
  :depends-on ("cffi" "bordeaux-threads" "star-zmq" "star-actor-wire")
  :serial t
  :components ((:file "source/router/native")
               (:file "source/router/security")
               (:file "source/router/sc01")
               (:file "source/router/host")))
