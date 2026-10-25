(defpackage :star.router.server
  (:use :cl)
  (:export #:bind-target-route #:unbind-target-route))
(in-package :star.router.server)

(defstruct (target-route (:constructor %make-target-route))
  actor host route envelope-function descriptor payload-field)
(defvar *routes* (make-hash-table :test #'equal))
(defvar *routes-lock* (bordeaux-threads:make-lock "native target deployment bindings"))

(defun resolve-route (actor)
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (let ((binding (gethash actor *routes*)))
      (when binding
        (star.actors::make-target-destination-handle
         :zmq actor :component binding :routing-key (target-route-descriptor binding))))))

(defun bind-target-route (actor host route envelope-function &key (payload-field "target"))
  "Trusted init binds an existing actor name to native delivery, without granting access.
ENVELOPE-FUNCTION projects the full existing dispatch envelope into its emitted
manifest's canonical message. No alternate scheduler or persistence is installed.
HOST must pin a singleton CURVE allowlist: ROUTER identity alone cannot bind a
recipient key when multiple authenticated peers can claim the same route."
  (unless (star.actors::valid-target-actor-name-p actor)
    (error "Invalid native actor binding"))
  (check-type host star.router::host)
  (unless (star.router:secure-host-p host :singleton-peer-p t)
    (error "Native application target routes require a running singleton CURVE host"))
  (check-type envelope-function function)
  (unless (and (stringp payload-field) (<= 1 (length payload-field) 256))
    (error "Invalid native Target payload field"))
  (check-type route (simple-array (unsigned-byte 8) (*)))
  (unless (<= 1 (length route) 255) (error "Invalid native route size"))
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (when (gethash actor *routes*) (error "Actor already has a native deployment binding"))
    (when (and star.actors::*target-zmq-destination-resolver*
               (not (eq star.actors::*target-zmq-destination-resolver* #'resolve-route)))
      (error "Another native deployment resolver is installed"))
    (setf (gethash actor *routes*)
          (%make-target-route
           :actor (copy-seq actor) :host host :route (copy-seq route)
           :envelope-function envelope-function
           :payload-field (copy-seq payload-field)
           :descriptor (format nil "star.actor2actor/1:~{~2,'0X~}"
                               (coerce route 'list)))
          star.actors::*target-zmq-destination-resolver* #'resolve-route))
  actor)

(defun unbind-target-route (actor)
  "Remove an owned deployment binding; existing schedules fail closed on stale binding."
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (remhash actor *routes*)
    (when (and (zerop (hash-table-count *routes*))
               (eq star.actors::*target-zmq-destination-resolver* #'resolve-route))
      (setf star.actors::*target-zmq-destination-resolver* nil)))
  nil)

(defmethod star.actors::send-zmq-target ((binding target-route) dispatch)
  (unless (star.router:secure-host-p (target-route-host binding) :singleton-peer-p t)
    (error 'star.actors::target-destination-unavailable :reason "secure native host unavailable"))
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (unless (eq binding (gethash (target-route-actor binding) *routes*))
      (error 'star.actors::target-destination-unavailable :reason "stale native binding")))
  (let* ((command (funcall (target-route-envelope-function binding) dispatch))
         (execution-id (star.actors::target-dispatch-envelope-execution-id dispatch))
         (record (star.actors::target-dispatch-envelope-record dispatch)))
    ;; A projection must retain invocation identity. Lease/fencing remain in the
    ;; original dispatch document and must be included by the declared contract.
    (unless (and (eq (getf command :kind) :command)
                 (equal (getf command :message-id) execution-id)
                 (equal (getf command :idempotency-key) execution-id)
                 (equal (getf command :correlation-id)
                        (star.actors::target-dispatch-envelope-trace-id dispatch))
                 (equal (getf command :causation-id) (star.actors::target-record-id record))
                 (equal (getf command :dataset)
                        (star.documents:document-dataset (star.actors::target-record-document record)))
                 (equal (getf command :actor) (target-route-actor binding)))
      (error 'star.actors::invalid-target-dispatch :reason "native projection loses dispatch identity"))
    (let ((expected
            (star-actor-wire:decode-json-value
             (babel:string-to-octets
              (star.documents:document-json (star.actors::target-dispatch-document dispatch))
              :encoding :utf-8)))
          (actual (cdr (assoc (target-route-payload-field binding) (getf command :payload)
                              :test #'equal))))
      (unless (equalp expected actual)
        (error 'star.actors::invalid-target-dispatch
               :reason "native projection loses canonical Target or lease/fencing metadata")))
    (unless (eq :queued
                (star.router:await-send
                 (star.router:enqueue-envelope (target-route-host binding)
                                                (target-route-route binding) command)))
      (error 'star.actors::target-destination-unavailable :reason "native transport backpressure"))))
