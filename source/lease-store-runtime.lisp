(in-package :star)

;; Runtime composition root for the target lease store. Nothing here starts
;; threads or network I/O by itself: the default backend (memory) is inert
;; until lease consumers adopt it, and the valkey backend only opens its
;; connection pool lazily per operation. See doc/lease-store-usage.org.

(defvar *lease-store* nil
  "The initialized global target lease store installed by
=initialize-lease-store=, or NIL. Consumers use it only through the
=star.leases= protocol operations (acquire/renew/release/get/list/revoke);
the concrete backend class is a composition-root detail.")

(defun lease-store-unix-milliseconds ()
  "Current Unix time in milliseconds, for lease-store lifecycle deadlines."
  (multiple-value-bind (seconds microseconds)
      (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun lease-store-backend-name (backend)
  "Normalize BACKEND (=memory= or =valkey=, string or keyword) to its
lowercase name, signaling a configuration error for anything else."
  (let ((name (string-downcase
               (if (stringp backend)
                   backend
                   (princ-to-string backend)))))
    (unless (member name '("memory" "valkey") :test #'string=)
      (error "Unsupported STAR_LEASE_STORE_BACKEND ~s (use memory or valkey)"
             backend))
    name))

(defun build-lease-store
    (&key backend host port password-file audit-hook metrics-hook)
  "Construct one lease store for BACKEND without installing it.

=valkey= requires HOST, PORT and a readable PASSWORD-FILE; =memory= ignores
them. Both backends receive the audit/metrics hooks through their existing
metrics-hook seam, so =starintel_lease_*_total= counters flow regardless of
backend. No network connection is made here."
  (let ((backend-name (lease-store-backend-name backend)))
    (cond
      ((string= backend-name "memory")
       (star.leases:make-memory-lease-store
        :audit-hook audit-hook
        :metrics-hook metrics-hook))
      ((string= backend-name "valkey")
       (unless password-file
         (error "VALKEY_PASSWORD_FILE is required when STAR_LEASE_STORE_BACKEND is valkey"))
       (star.leases:make-valkey-lease-store
        :host host
        :port port
        :password-file password-file
        :audit-hook audit-hook
        :metrics-hook metrics-hook)))))

(defun initialize-lease-store
    (&key force
       (backend *lease-store-backend*)
       (host *valkey-lease-host*)
       (port *valkey-lease-port*)
       (password-file *valkey-lease-password-file*)
       (metrics-hook (observability-lease-metrics-hook))
       audit-hook)
  "Build and install the global =*lease-store*= from configuration.

Backend selection follows =*lease-store-backend*= (=STAR_LEASE_STORE_BACKEND=;
=memory= default, =valkey= with =VALKEY_HOST=/=VALKEY_PORT=/
=VALKEY_PASSWORD_FILE=). The =star:observability-lease-metrics-hook= is
attached by default, so lease outcome counters flow whenever the
observability addon is loaded. Idempotent: without =:force= an existing
store is returned unchanged. Never connects to Valkey here; with the valkey
backend selected, configuration errors (missing password file) signal
immediately while an unreachable server degrades per the adapter's bounded
retry contract (retryable =:backend-unavailable=/:timeout=/:outcome-unknown=
outcomes). Returns the store."
  (when (and *lease-store* (not force))
    (return-from initialize-lease-store *lease-store*))
  (let ((new-store
          (build-lease-store
           :backend backend :host host :port port
           :password-file password-file
           :audit-hook audit-hook :metrics-hook metrics-hook)))
    (when *lease-store*
      (ignore-errors
        (star.leases:close-lease-store
         *lease-store*
         :deadline (+ (lease-store-unix-milliseconds) 5000)
         :request-id "initialize-lease-store-force")))
    (setf *lease-store* new-store)))

(defun shutdown-lease-store
    (&key (deadline (+ (lease-store-unix-milliseconds) 5000))
       (request-id "shutdown-lease-store"))
  "Close and uninstall the global =*lease-store*=. Idempotent; returns t when
a store was actually closed."
  (let ((store *lease-store*))
    (setf *lease-store* nil)
    (when store
      (star.leases:close-lease-store
       store :deadline deadline :request-id request-id)
      t)))