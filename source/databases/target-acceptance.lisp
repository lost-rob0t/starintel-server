(in-package :star.databases.couchdb)

(define-condition target-acceptance-store-conflict (error) ())

(defun couchdb-load-target-acceptance (client database acceptance-id)
  (handler-case
      (jsown:with-injective-reader
        (jsown:parse
         (cl-couch:get-document client database acceptance-id)))
    (dexador:http-request-not-found () nil)))

(defun couchdb-save-target-acceptance (client database document)
  "Create or update one durable target acceptance document."
  (handler-case
      (let* ((response
               (jsown:parse
                (cl-couch:create-document
                 client database (jsown:to-json document))))
             (saved (clone-outbox-json document)))
        (when (outbox-object-has-key-p response "rev")
          (setf (jsown:val saved "_rev") (jsown:val response "rev")))
        saved)
    (dexador:http-request-conflict ()
      (error 'target-acceptance-store-conflict))))

(defun couchdb-update-target-acceptance
    (client database acceptance-id updater &key (max-attempts 8))
  (loop for attempt from 1 to max-attempts
        do
           (let* ((current
                    (or (couchdb-load-target-acceptance
                         client database acceptance-id)
                        (error "Target acceptance ~a disappeared" acceptance-id)))
                  (updated (funcall updater (clone-outbox-json current))))
             (handler-case
                 (return
                   (couchdb-save-target-acceptance
                    client database updated))
               (target-acceptance-store-conflict ()
                 (when (= attempt max-attempts)
                   (error "Target acceptance update conflict budget exhausted for ~a"
                          acceptance-id)))))))

(defun couchdb-persist-target-acceptance
    (client database desired duplicate-predicate
     &key (max-attempts 8))
  "Persist DESIRED or return the existing equivalent record.

Returns DOCUMENT and one of :CREATED, :RESUMED, :DUPLICATE, or :CONFLICT."
  (let ((acceptance-id (jsown:val desired "_id")))
    (loop for attempt from 1 to max-attempts
          do
             (let ((existing
                     (couchdb-load-target-acceptance
                      client database acceptance-id)))
               (cond
                 ((null existing)
                  (handler-case
                      (return
                        (values
                         (couchdb-save-target-acceptance
                          client database desired)
                         :created))
                    (target-acceptance-store-conflict ()
                      (when (= attempt max-attempts)
                        (error "Target acceptance create conflict budget exhausted for ~a"
                               acceptance-id)))))
                 ((not (funcall duplicate-predicate existing desired))
                  (return (values existing :conflict)))
                 ((member (jsown:val existing "status")
                          '("accepted" "scheduled" "dispatched")
                          :test #'string=)
                  (return (values existing :duplicate)))
                 (t
                  (return (values existing :resumed))))))))
