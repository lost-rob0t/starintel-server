(defpackage :star.router.journal
  (:use :cl)
  (:export #:make-journal-store #:make-couchdb-journal-store #:journal-store-conflict
           #:journal-key #:journal-read #:journal-transition))
(in-package :star.router.journal)

(define-condition journal-store-conflict (error) ())
(defstruct (journal-store (:constructor %make-journal-store))
  load-fn save-fn max-attempts max-bytes)

(defun make-journal-store (&key load-fn save-fn (max-attempts 8) (max-bytes 1048576))
  "Internal CAS port. SAVE-FN must durably commit or signal; conflict alone is retried.
Callbacks must not run application effects. Ambiguous I/O propagates unchanged."
  (unless (and (functionp load-fn) (functionp save-fn)
               (integerp max-attempts) (<= 1 max-attempts 32)
               (integerp max-bytes) (<= 1024 max-bytes 4194304))
    (error "Invalid router journal port"))
  (%make-journal-store :load-fn load-fn :save-fn save-fn
                      :max-attempts max-attempts :max-bytes max-bytes))

(defun journal-key (principal tenant direction canonical-scope)
  "Trusted authority namespace plus canonical runtime scope; no credentials stored.
Scope elements are opaque strings, never delimiter-concatenated identifiers."
  (unless (and (listp canonical-scope) (<= 1 (length canonical-scope) 8))
    (error "Invalid journal scope"))
  (let ((parts (append (list principal tenant direction) canonical-scope)))
    (dolist (part parts)
      (unless (and (stringp part) (<= 1 (length part) 2048))
        (error "Invalid journal scope element")))
    (concatenate 'string "router-execution:"
                 (star.databases.couchdb::outbox-digest-string
                  (jsown:to-json (coerce parts 'vector))))))

(defun valid-key-p (key)
  (and (stringp key) (= (length key) 81)
       (string= "router-execution:" key :end2 17)
       (every (lambda (char) (find char "0123456789abcdef")) (subseq key 17))))

(defun snapshot (store row)
  (unless (star.databases.couchdb::json-object-p row)
    (error "Invalid router journal row"))
  (when (or (jsown:keyp row "_deleted") (jsown:keyp row "_attachments"))
    (error "Router journal rows cannot delete state or contain attachments"))
  (let ((text (jsown:to-json row)))
    (when (> (length (babel:string-to-octets text :encoding :utf-8))
             (journal-store-max-bytes store))
      (error "Router journal row exceeds configured bound"))
    (jsown:with-injective-reader (jsown:parse text))))

(defun checked-row (store key row)
  (when row
    (let ((copy (snapshot store row)))
      (unless (and (equal key (jsown:val-safe copy "_id"))
                   (equal "_server_router_execution" (jsown:val-safe copy "type"))
                   (let ((rev (jsown:val-safe copy "_rev")))
                     (and (stringp rev) (plusp (length rev)))))
        (error "Router journal namespace collision or corrupt revision"))
      copy)))

(defun journal-read (store key)
  (check-type store journal-store)
  (unless (valid-key-p key) (error "Invalid journal key"))
  (checked-row store key (funcall (journal-store-load-fn store) key)))

(defun journal-transition (store key update-function)
  "Return VALUE and durable ROW. UPDATER returns new JSON row (or NIL), VALUE.
UPDATER can rerun after a CAS conflict: it must be pure. NIL means no write.
Canonical command identity and lifecycle transitions belong to the caller's
StarLang authority; this adapter only owns storage revision/namespace bounds."
  (check-type update-function function)
  (loop for attempt from 1 to (journal-store-max-attempts store)
        for old = (journal-read store key)
        do (multiple-value-bind (new value)
               (funcall update-function (and old (snapshot store old)))
             (unless new (return (values value old)))
             (let ((candidate (snapshot store new)))
               (setf (jsown:val candidate "_id") key
                     (jsown:val candidate "type") "_server_router_execution")
               (when (jsown:keyp candidate "_rev") (jsown:remkey candidate "_rev"))
               (when old (setf (jsown:val candidate "_rev") (jsown:val old "_rev")))
               (setf candidate (snapshot store candidate))
               (handler-case
                   (let ((saved (checked-row store key
                                  (funcall (journal-store-save-fn store) candidate))))
                     (unless (and saved
                                  (not (equal (and old (jsown:val old "_rev"))
                                              (jsown:val saved "_rev"))))
                       (error "Router journal save returned no new revision"))
                     (return (values value saved)))
                 (journal-store-conflict (condition)
                   (when (= attempt (journal-store-max-attempts store))
                     (error condition))))))))

(defun make-couchdb-journal-store (client database &key (max-attempts 8) (max-bytes 1048576))
  "Reuse the Server's CouchDB revision adapter in its existing database.
A private client header list requests full commit on every write, before any
caller can expose admission/completion. Never mutate pooled client's headers.
This does not make a separate application document write and journal atomic."
  (unless (and (stringp database) (plusp (length database)))
    (error "Journal database must be configured"))
  (let ((durable-client
          (make-instance 'cl-couch:couchdb-client
           :url (cl-couch::couchdb-url client)
           :cookie (cl-couch::couchdb-cookie client)
           :headers (cons '("X-Couch-Full-Commit" . "true")
                          (remove "X-Couch-Full-Commit" (copy-tree (cl-couch::couchdb-headers client))
                                  :key #'car :test #'string-equal)))))
    (make-journal-store
     :max-attempts max-attempts :max-bytes max-bytes
     :load-fn (lambda (key)
                (star.databases.couchdb::couchdb-load-outbox-document durable-client database key))
     :save-fn (lambda (row)
                (handler-case
                    (star.databases.couchdb::couchdb-save-outbox-document durable-client database row)
                  (star.databases.couchdb::outbox-store-conflict ()
                    (error 'journal-store-conflict)))))))
