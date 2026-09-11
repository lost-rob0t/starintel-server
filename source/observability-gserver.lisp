(in-package :star)

;; StarIntel observability glue. Everything here adapts the pure
;; star.observability library to the gserver boundaries. The library never
;; references gserver internals; this and source/observability-consumers.lisp
;; are the only adapters.

(defun observability-route-template (env)
  "Match the request against the HTTP contract table and stash the normalized
route template on the env, so the HTTP middleware labels stay bounded and
request identifiers never become metric labels."
  (let* ((method (getf env :request-method :get))
         (path (or (getf env :path-info) "/"))
         (template (observability-match-route (string-upcase (princ-to-string method)) path)))
    (when template
      (setf (getf env :star-route-template) template))))

(defun observability-match-route (method path)
  "Return the contract route template matching METHOD and PATH, or NIL.
Contract paths use :name placeholders matching any single non-empty segment."
  (let ((request-parts (uiop:split-string path :separator "/")))
    (loop for operation in (star.http.contract:all-http-operations)
          when (and (eq (star.http.contract:http-operation-method operation)
                        (intern (string-upcase method) :keyword))
                    (observability-path-matches-p
                     (star.http.contract:http-operation-path operation)
                     request-parts))
            return (star.http.contract:http-operation-path operation))))

(defun observability-path-matches-p (template parts)
  "TEMPLATE is a contract path like /api/v1/documents/:id; PARTS is the
request path split on slashes. Literal segments match exactly; :name matches
any single segment; the empty leading segment must match."
  (let ((template-parts (uiop:split-string template :separator "/")))
    (and (= (length template-parts) (length parts))
         (loop for want in template-parts
               for got in parts
               always (cond
                        ((string= want "") (string= got ""))
                        ((char= (char want 0) #\:) t)
                        (t (string= want got)))))))

;; ---- HTTP middleware wiring -----------------------------------------------

(defparameter *observability-active* nil
  "Non-nil while the observability addon has been started via the init-file
addon lifecycle. The HTTP boundary stays a pass-through until then.")

(defun observability-active-p ()
  "True when the observability addon was loaded via init.lisp and is active."
  (and *observability-active*
       (star.observability:observability-enabled-p)))

(defun observability-maybe-wrap (app)
  "Wrap APP with request-time observability gating. When the addon has been
loaded through init.lisp, requests flow through the telemetry middleware;
otherwise APP is called directly with no telemetry work."
  (let (memo)
    (lambda (env)
      (if (observability-active-p)
          (progn
            (unless memo
              (setf memo (star.observability:observability-http-middleware app)))
            (funcall memo env))
          (funcall app env)))))

(defun observability-wrapped-server (app)
  "Build the observability layer used in the lack builder: a request-time
gate that only instruments when the addon was loaded via init.lisp."
  (observability-maybe-wrap app))

;; ---- Lease metrics ---------------------------------------------------------

(defun observability-lease-metrics-hook ()
  "A lease-store metrics hook suitable for the existing metrics-hook seams.
Stale fencing-token rejections get a dedicated counter."
  (lambda (outcome-code &rest info)
    (declare (ignore info))
    (case outcome-code
      (:stale-token
       (star.observability:record-counter
        "starintel_lease_stale_writer_rejections_total" 1))
      (:conflict
       (star.observability:record-counter "starintel_lease_conflicts_total" 1))
      (:acquired
       (star.observability:record-counter
        "starintel_lease_acquisitions_total" 1))
      (t
       (star.observability:record-counter
        "starintel_lease_outcomes_total" 1
        :attributes
        (list (cons "outcome"
                    (string-downcase (princ-to-string outcome-code)))))))))

;; ---- Lifecycle -------------------------------------------------------------

(defun start-observability ()
  "Addon start (init.lisp: =load-addon :starintel-observability=): start the
exporter thread and wire the instrumented CouchDB view transport. Safe to
call repeatedly."
  (star.observability:start-exporter)
  (install-observability-couchdb-transport)
  (setf *observability-active* t))

(defun stop-observability ()
  "Addon stop: flush what is queued and return every boundary to a
pass-through."
  (setf *observability-active* nil)
  (star.observability:stop-exporter))

(defun install-observability-couchdb-transport ()
  "Wrap the current default CouchDB view transport with the instrumented
wrapper once. Document bodies are never recorded; only operation, database,
status and duration. Tests that swap the transport keep full control."
  (unless (get 'star-observability-transport 'installed)
    (setf *couchdb-view-transport*
          (observability-couchdb-transport *couchdb-view-transport*)
          (get 'starintel-couchdb-observability 'installed) t)))

(defun observability-couchdb-transport (transport)
  "Wrap a CouchDB view transport function (CLIENT REQUEST) with a client
span and bounded metrics. Database names are bounded identifiers; document
contents are never recorded."
  (lambda (client request)
    (let* ((uri (star.databases.couchdb:couchdb-view-request-uri request))
           (method (star.databases.couchdb:couchdb-view-request-method request))
           (database (observability-couchdb-database-from-uri uri))
           (start (star.observability:now-unix-nanos))
           (start-real (get-internal-real-time))
           (*current-trace-context*
             (star.observability:child-context
              (star.observability:current-context))))
      (handler-case
          (let ((result (funcall transport client request)))
            (observability-record-couchdb-outcome
             method database :ok (- (get-internal-real-time) start-real) start)
            result)
        (error (condition)
          (observability-record-couchdb-outcome
           method database :error (- (get-internal-real-time) start-real) start)
          (error condition))))))

(defun observability-couchdb-database-from-uri (uri)
  "Extract the database segment of a CouchDB URI, or \"unknown\". URI may be
a string or a quri URI."
  (let* ((path (typecase uri
                 (quri:uri (quri:uri-path uri))
                 (string uri)
                 (t nil)))
         (parts (and path (uiop:split-string path :separator "/"))))
    (or (second parts) "unknown")))

(defun observability-record-couchdb-outcome (method database kind elapsed-real start)
  (let ((elapsed-ms (/ elapsed-real internal-time-units-per-second 0.001))
        (method-name (string-downcase (symbol-name method))))
    (star.observability:record-counter
     "starintel_couchdb_requests_total" 1
     :attributes
     (list (cons "operation" method-name)
           (cons "status_class"
                 (if (eq kind :ok) "2xx" "5xx"))))
    (star.observability:queue-span
     (star.observability:current-context)
     (format nil "couchdb.~a" method-name)
     3 start (star.observability:now-unix-nanos)
     (list (cons "db.operation" method-name)
           (cons "db.name" database)
           (cons "status" (string-downcase (symbol-name kind)))
           (cons "duration.ms" elapsed-ms))
     (eq kind :ok))))

;; ---- Addon lifecycle -------------------------------------------------------

(defparameter *observability-addon-registered* nil
  "Guard so the addon registration is idempotent across ASDF reloads.")

(defun start-observability-addon ()
  "Addon start: exporter thread + transport wiring."
  (start-observability))

(defun stop-observability-addon ()
  "Addon stop: flush and stop the exporter."
  (stop-observability))

(defun ensure-observability-addon ()
  "Register the observability addon with the trusted addon lifecycle.
Registration is metadata only; the addon starts via the addon lifecycle."
  (unless *observability-addon-registered*
    (register-addon "observability"
                    :system :starintel-observability
                    :start #'start-observability-addon
                    :stop #'stop-observability-addon)
    (setf *observability-addon-registered* t)))

;; Registration is metadata only (the addon-design contract): it makes the
;; addon visible to the lifecycle and loadable from init.lisp, and it never
;; starts telemetry by itself. Operators opt in with:
;;
;;   (load-addon :starintel-observability)
;;
;; in the trusted init file. Without that line no exporter thread, no queue,
;; no ids, and no counters ever run.
(ensure-observability-addon)