;;; Trusted synthetic enrollment and real lease protocol backing Target outcomes.
(in-package :embedded-router-application-test)
(defvar *fixture-outbound* nil)
(defvar *fixture-enrollment* nil)
(defvar *fixture-invocation* nil)
(defvar *fixture-lease-store* nil)

(defun enroll-fixture-actor ()
  (let* ((file (state-path "enrollment-generation"))
         (generation (1+ (parse-integer (uiop:read-file-string file)))))
    (write-key "enrollment-generation" (write-to-string generation))
    (setf *fixture-enrollment*
          (staractorprotocol:make-star-actor-reference
           :domain-id "fixture" :logical-path "embedded-actor" :node-id "loopback"
           :generation generation :protocol-revision 1))))

(defun fixture-target-identity (document)
  (star.leases:make-lease-identity
   :tenant-id *dataset* :program-id "embedded-fixture" :target-namespace "fixture"
   :target-id (star.documents:document-id document) :actor-name *actor*))

(defun fixture-lease-policy (owner)
  (star.router.leases:make-policy
   (setf *fixture-lease-store* (star.leases:make-memory-lease-store)) owner #'fixture-target-identity
   "synthetic-fixture-server" :ttl-ms 120000))

(defun bind-fixture-target (host manifest public authenticate)
  (enroll-fixture-actor)
  (let ((policy (fixture-lease-policy (crash-principal))))
    ;; This is the repository's actual deterministic lease backend, with its
    ;; normal acquire/get/expiry/fencing implementation and no fake IDs. It
    ;; establishes single-process lease conformance, not distributed failover.
    (setf *fixture-outbound*
          (star.router.outbound:make-coordinator
           manifest (fixture-journal)
           (lambda (key) (when (equalp key public) (values (funcall authenticate) *dataset*)))
           (star.router.leases:make-outbound-observer
            policy (lambda (context document)
                     (declare (ignore context document)) *fixture-enrollment*))
           :wait-seconds 60))
    (star.router.server:bind-target-route
     *actor* host *route*
     (lambda (dispatch)
       (let ((command (target-command dispatch)))
         (setf *fixture-invocation* (getf command :message-id)) command))
     :outbound *fixture-outbound* :actor-reference *fixture-enrollment*
     :context-function (lambda (dispatch) (declare (ignore dispatch))
                         (values (funcall authenticate) *dataset*))
     :lease-policy policy)))

(defun send-outcome (socket manifest envelope)
  (star-zmq:send-frames socket
    (list (star-actor-wire:encode-binding-message :zmq *profiles* manifest envelope))))

(defun outcome-barrier (socket manifest suffix)
  (send-outcome socket manifest (pressure-event (concatenate 'string "outbound-" suffix)))
  (wait-marker (concatenate 'string "outbound-" suffix "-passed")))

(defun actor-target-outcomes (socket manifest target)
  (let ((completed (staractorprotocol:make-ack-envelope
                    target :message-id (star.ids:ulid) :actor *actor* :sender *actor* :status :completed)))
    (send-outcome socket manifest
      (staractorprotocol:make-ack-envelope target :message-id (star.ids:ulid)
                                          :actor *actor* :sender *actor* :status :accepted))
    (send-outcome socket manifest
      (staractorprotocol:make-reply-envelope
       target :message-id (star.ids:ulid) :message-type "star.router.fixture/result@1"
       :actor *actor* :sender *actor*
       :payload (list (cons "status" "committed") (cons "durable" t))))
    (send-outcome socket manifest completed)
    (outcome-barrier socket manifest "terminal"))
  ;; Ask Python for a second real HTTP Target only after the first settles.
  (marker "request-stale-target")
  (let ((stale (star-actor-wire:decode-binding-message
                :zmq *profiles* manifest (first (star-zmq:receive-frames socket)))))
    (write-key "stale-target.json"
               (babel:octets-to-string (star-actor-wire:encode-binding-message
                                       :zmq *profiles* manifest stale) :encoding :utf-8))
    (marker "release-target-lease")
    (wait-marker "target-lease-released")
    (send-outcome socket manifest
      (staractorprotocol:make-ack-envelope stale :message-id (star.ids:ulid)
                                          :actor *actor* :sender *actor* :status :completed))
    (outcome-barrier socket manifest "stale-lease")))

(defun outbound-barrier-event (envelope)
  (dolist (suffix '("stale-lease" "terminal"))
    (when (equal (getf envelope :message-id)
                 (getf (pressure-event (concatenate 'string "outbound-" suffix)) :message-id))
      (marker (concatenate 'string "outbound-" suffix "-observed")))))

(defun service-outbound-controls ()
  (when (and (probe-file (state-path "release-target-lease"))
             (not (probe-file (state-path "target-lease-released"))))
    (let* ((wire (jsown:parse (uiop:read-file-string (state-path "stale-target.json"))))
           (document (star.frontends.http-api::parse-json-octets
                      (octets (jsown:to-json (jsown:val (jsown:val wire "payload") "target")))
                      "application/json"))
           (identity (fixture-target-identity document))
           (now (* 1000 (- (get-universal-time) 2208988800)))
           (found (star.leases:get-lease *fixture-lease-store* identity
                                         :deadline (+ now 1000) :request-id (star.ids:ulid))))
      (fixture-assert (eq :found (star.leases:lease-outcome-code found)) "second Target has an active acquired lease")
      (let* ((lease (star.leases:lease-outcome-lease found))
             (released (star.leases:release-lease
                        *fixture-lease-store* identity
                        :lease-id (star.leases:lease-record-lease-id lease)
                        :owner-principal-id (star.leases:lease-record-owner-principal-id lease)
                        :service-instance-id (star.leases:lease-record-service-instance-id lease)
                        :fencing-token (star.leases:lease-record-fencing-token lease)
                        :deadline (+ now 1000) :request-id (star.ids:ulid))))
        (fixture-assert (eq :released (star.leases:lease-outcome-code released)) "actual owner released Target lease")))
    (marker "target-lease-released"))
  (dolist (suffix '("stale-lease" "terminal"))
    (when (and (probe-file (state-path (concatenate 'string "outbound-" suffix "-observed")))
               (not (probe-file (state-path (concatenate 'string "outbound-" suffix "-passed")))))
      (let ((row (star.router.outbound:invocation-snapshot *fixture-outbound* *fixture-invocation*)))
        (if (equal suffix "terminal")
            (progn (fixture-assert (equal "yes" (jsown:val row "terminal")) "actual Target terminal outcome")
                   (fixture-assert (equal "completed" (jsown:val row "state")) "Target completed ACK")
                   (fixture-assert (stringp (jsown:val row "reply")) "Target reply retained"))
            (fixture-assert (and (equal "no" (jsown:val row "terminal"))
                                (member (jsown:val row "state") '("pending" "transport-queued") :test #'equal))
                            "completion after actual lease release could not settle Target")))
      (marker (concatenate 'string "outbound-" suffix "-passed")))))
