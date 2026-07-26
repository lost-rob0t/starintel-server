(in-package :star.databases.couchdb)

(defun couchdb-save-quarantine-record (client database record)
  "Persist one server-internal quarantine record before Rabbit settlement."
  (let* ((response
           (jsown:parse
            (cl-couch:create-document
             client database (jsown:to-json record))))
         (saved (clone-outbox-json record)))
    (when (outbox-object-has-key-p response "rev")
      (setf (jsown:val saved "_rev") (jsown:val response "rev")))
    saved))

(defun couchdb-get-quarantine-record (client database quarantine-id)
  (jsown:with-injective-reader
    (jsown:parse
     (cl-couch:get-document client database quarantine-id))))

(defun couchdb-list-quarantine-records
    (client database &key (status "quarantined") (limit 100))
  "Return quarantine documents through the checked-in status view."
  (let* ((result
           (query-view client database "quarantine" "by_status"
                       :key status
                       :include-docs t
                       :reduce nil
                       :limit limit))
         (rows (jsown:val result "rows")))
    (loop for row in rows
          collect (jsown:val row "doc"))))

(defun update-quarantine-record
    (client database quarantine-id updater &key (max-attempts 8))
  (loop for attempt from 1 to max-attempts
        do
           (let* ((current
                    (couchdb-get-quarantine-record
                     client database quarantine-id))
                  (updated (funcall updater (clone-outbox-json current))))
             (handler-case
                 (return
                   (let* ((response
                            (jsown:parse
                             (cl-couch:create-document
                              client database (jsown:to-json updated))))
                          (saved (clone-outbox-json updated)))
                     (when (outbox-object-has-key-p response "rev")
                       (setf (jsown:val saved "_rev")
                             (jsown:val response "rev")))
                     saved))
               (dexador:http-request-conflict ()
                 (when (= attempt max-attempts)
                   (error "Quarantine update conflict budget exhausted for ~a"
                          quarantine-id)))))))

(defun mark-quarantine-replayed
    (client database quarantine-id new-trace-id)
  (update-quarantine-record
   client database quarantine-id
   (lambda (record)
     (setf (jsown:val record "status") "replayed"
           (jsown:val record "replayed_at") (spec:utc-now)
           (jsown:val record "replay_count")
           (1+ (or (jsown:val-safe record "replay_count") 0))
           (jsown:val record "last_replay_trace_id") new-trace-id)
     record)))

(defun replay-quarantine-record
    (client database quarantine-id publish-fn &key corrected-body)
  "Explicitly replay a quarantined delivery with fresh attempt history.

PUBLISH-FN receives EXCHANGE, ROUTING-KEY, BODY, and PROPERTIES. The stored
record is marked replayed only after publishing returns successfully."
  (let ((record
          (couchdb-get-quarantine-record client database quarantine-id)))
    (unless (string= "quarantined" (jsown:val record "status"))
      (error "Quarantine record ~a is not replayable from status ~a"
             quarantine-id
             (jsown:val record "status")))
    (multiple-value-bind (body properties exchange routing-key)
        (star.consumers:quarantine-replay-envelope
         record :corrected-body corrected-body)
      (funcall publish-fn exchange routing-key body properties)
      (mark-quarantine-replayed
       client database quarantine-id
       (star.consumers:delivery-trace-id properties)))))
