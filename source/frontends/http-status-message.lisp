(in-package :star.frontends.http-api)

;; STATUS-MSG is defined once in http-boundary-core.lisp, alongside the
;; correlation-ID context it requires. Keeping the implementation there avoids
;; load-order-dependent redefinition and guarantees that client responses never
;; expose TRACEBACK or internal condition text.
