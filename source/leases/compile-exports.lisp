(in-package :star.leases)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; ASDF/Nix can enter compilation with STAR.LEASES already interned from a
  ;; dependency image. Make the protocol validation surface explicit before
  ;; application-layer files are read, rather than depending on image state.
  (export '(valid-lease-identifier-p
            valid-lease-reason-p
            valid-lease-filter-p
            valid-lease-component-filter-p
            valid-lease-metadata-p
            lease-metadata-object-p
            utf-8-byte-length)))
