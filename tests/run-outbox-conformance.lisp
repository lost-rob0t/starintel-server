(in-package :cl-user)

(defvar *outbox-test-store* (make-hash-table :test #'equal))
(defvar *outbox-test-published* nil)
(defvar *outbox-test-fail-next-publish* nil)

(defun outbox-test-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun outbox-test-fixture-path (relative)
  (merge-pathnames relative (uiop:getcwd)))

(defun outbox-test-read-json (pathname)
  (jsown:with-injective-reader
    (jsown:parse (uiop:read-file-string pathname))))

(defun outbox-test-person-fixture ()
  (loop for index from 1 to 5
        for fixture-file =
          (outbox-test-read-json
           (outbox-test-fixture-path
            (format nil
                    "test/fixtures/starintel/v0.9/fixtures-~2,'0d.json"
                    index)))
        for fixture =
          (find "person"
                (jsown:val fixture-file "fixtures")
                :key (lambda (document) (jsown:val document "dtype"))
                :test #'string=)
        when fixture
          return fixture))

(defun outbox-test-clone (object)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json object))))

(defun outbox-test-extension-object (document)
  (handler-case
      (jsown:val document "extensions")
    (error ()
      (let ((extensions (jsown:empty-object)))
        (setf (jsown:val document "extensions") extensions)
        extensions))))

(defun outbox-test-document (id mutation-id &key (title "Original"))
  (let* ((document (outbox-test-clone (outbox-test-person-fixture)))
         (extensions (outbox-test-extension-object document)))
    (setf (jsown:val document "_id") id
          (jsown:val document "title") title
          (jsown:val extensions "mutation_id") mutation-id
          (jsown:val document "extensions") extensions)
    document))

(defun outbox-test-reset ()
  (clrhash *outbox-test-store*)
  (setf *outbox-test-published* nil
        *outbox-test-fail-next-publish* nil))

(defun outbox-test-revision-generation (revision)
  (if (and revision (stringp revision))
      (parse-integer revision :junk-allowed t)
      0))

(defun outbox-test-load (document-id)
  (let ((document (gethash document-id *outbox-test-store*)))
    (and document (outbox-test-clone document))))

(defun outbox-test-save (document)
  (let* ((document-id (jsown:val document "_id"))
         (current (gethash document-id *outbox-test-store*))
         (expected-revision
           (handler-case (jsown:val document "_rev")
             (error () nil)))
         (current-revision
           (and current (jsown:val current "_rev"))))
    (when (and current
               (not (and expected-revision
                         (string= expected-revision current-revision))))
      (error 'star.databases.couchdb:outbox-store-conflict))
    (when (and (null current) expected-revision)
      (error 'star.databases.couchdb:outbox-store-conflict))
    (let* ((saved (outbox-test-clone document))
           (generation
             (1+
              (outbox-test-revision-generation current-revision)))
           (revision (format nil "~d-test" generation)))
      (setf (jsown:val saved "_rev") revision
            (gethash document-id *outbox-test-store*) saved)
      (outbox-test-clone saved))))

(defun outbox-test-publish (routing-key payload event-id)
  (when *outbox-test-fail-next-publish*
    (setf *outbox-test-fail-next-publish* nil)
    (error "forced publish failure"))
  (push (list routing-key (outbox-test-clone payload) event-id)
        *outbox-test-published*)
  t)

(defun outbox-test-process (document operation)
  (star.databases.couchdb:process-outbox-mutation
   #'outbox-test-load
   #'outbox-test-save
   #'outbox-test-publish
   document
   operation))

(defun outbox-test-single-stored-document ()
  (loop for document being the hash-values of *outbox-test-store*
        return (outbox-test-clone document)))

(defun outbox-test-event-sequences ()
  (mapcar
   (lambda (published)
     (jsown:val
      (jsown:val (second published) "extensions")
      "event_sequence"))
   (reverse *outbox-test-published*)))

(defun test-new-publish-failure-is-retry-safe ()
  (outbox-test-reset)
  (let ((document (outbox-test-document "person:new-failure" "mutation:new-1")))
    (setf *outbox-test-fail-next-publish* t)
    (handler-case
        (progn
          (outbox-test-process document :new)
          (error "forced publish failure was swallowed"))
      (simple-error () t))
    (let* ((stored (outbox-test-load "person:new-failure"))
           (entries
             (star.databases.couchdb:document-outbox-entries stored)))
      (outbox-test-check (= 1 (length entries))
                         "new mutation created ~d outbox entries"
                         (length entries))
      (outbox-test-check
       (not (star.databases.couchdb:outbox-entry-published-p
             (first entries)))
       "failed publication was marked published"))
    (outbox-test-process document :new)
    (outbox-test-process document :new)
    (outbox-test-check (= 1 (length *outbox-test-published*))
                       "duplicate new delivery emitted ~d physical events"
                       (length *outbox-test-published*))
    (outbox-test-check
     (star.databases.couchdb:outbox-entry-published-p
      (first
       (star.databases.couchdb:document-outbox-entries
        (outbox-test-load "person:new-failure"))))
     "retried new event remained pending")))

(defun test-update-publish-failure-is-retry-safe ()
  (outbox-test-reset)
  (outbox-test-process
   (outbox-test-document "person:update-failure" "mutation:new")
   :new)
  (setf *outbox-test-published* nil)
  (let ((update
          (outbox-test-document
           "person:update-failure"
           "mutation:update-1"
           :title "Updated")))
    (setf *outbox-test-fail-next-publish* t)
    (handler-case
        (progn
          (outbox-test-process update :updated)
          (error "forced update publish failure was swallowed"))
      (simple-error () t))
    (outbox-test-process update :updated)
    (outbox-test-process update :updated)
    (outbox-test-check (= 1 (length *outbox-test-published*))
                       "duplicate update emitted ~d physical events"
                       (length *outbox-test-published*))
    (outbox-test-check
     (string= "documents.updated.person"
              (first (first *outbox-test-published*)))
     "update used the wrong routing key")))

(defun test-crash-recovery-publishes-pending-event ()
  (outbox-test-reset)
  (let ((document
          (outbox-test-document "person:recovery" "mutation:recovery")))
    (star.databases.couchdb:persist-outbox-mutation
     #'outbox-test-load #'outbox-test-save document :new)
    (outbox-test-check (null *outbox-test-published*)
                       "persistence unexpectedly published")
    (star.databases.couchdb:recover-outbox-documents
     #'outbox-test-load
     #'outbox-test-save
     #'outbox-test-publish
     (list (outbox-test-single-stored-document)))
    (outbox-test-check (= 1 (length *outbox-test-published*))
                       "recovery published ~d events"
                       (length *outbox-test-published*))))

(defun test-duplicate-mutation-does-not-append-outbox-entry ()
  (outbox-test-reset)
  (let ((document
          (outbox-test-document "person:duplicate" "mutation:duplicate")))
    (outbox-test-process document :new)
    (outbox-test-process document :new)
    (let ((entries
            (star.databases.couchdb:document-outbox-entries
             (outbox-test-load "person:duplicate"))))
      (outbox-test-check (= 1 (length entries))
                         "duplicate mutation appended ~d entries"
                         (length entries)))))

(defun test-conflicting-idempotency-key-is-rejected ()
  (outbox-test-reset)
  (outbox-test-process
   (outbox-test-document "person:conflict" "mutation:conflict" :title "A")
   :new)
  (handler-case
      (progn
        (outbox-test-process
         (outbox-test-document "person:conflict" "mutation:conflict" :title "B")
         :updated)
        (error "conflicting idempotency key was accepted"))
    (star.databases.couchdb:mutation-conflict () t)))

(defun test-recovery-preserves-document-event-order ()
  (outbox-test-reset)
  (star.databases.couchdb:persist-outbox-mutation
   #'outbox-test-load
   #'outbox-test-save
   (outbox-test-document "person:ordered" "mutation:ordered-new" :title "A")
   :new)
  (star.databases.couchdb:persist-outbox-mutation
   #'outbox-test-load
   #'outbox-test-save
   (outbox-test-document "person:ordered" "mutation:ordered-1" :title "B")
   :updated)
  (star.databases.couchdb:persist-outbox-mutation
   #'outbox-test-load
   #'outbox-test-save
   (outbox-test-document "person:ordered" "mutation:ordered-2" :title "C")
   :updated)
  (star.databases.couchdb:recover-outbox-documents
   #'outbox-test-load
   #'outbox-test-save
   #'outbox-test-publish
   (list (outbox-test-single-stored-document)))
  (outbox-test-check (equal '(1 2 3) (outbox-test-event-sequences))
                     "recovery emitted sequences ~s"
                     (outbox-test-event-sequences)))

(defun test-update-of-missing-document-is-rejected ()
  (outbox-test-reset)
  (handler-case
      (progn
        (outbox-test-process
         (outbox-test-document "person:missing" "mutation:missing")
         :updated)
        (error "missing update target was accepted"))
    (star.databases.couchdb:missing-document-for-update () t)))

(defun run-outbox-conformance-tests ()
  (format t "~&Running CouchDB outbox conformance tests~%")
  (test-new-publish-failure-is-retry-safe)
  (test-update-publish-failure-is-retry-safe)
  (test-crash-recovery-publishes-pending-event)
  (test-duplicate-mutation-does-not-append-outbox-entry)
  (test-conflicting-idempotency-key-is-rejected)
  (test-recovery-preserves-document-event-order)
  (test-update-of-missing-document-is-rejected)
  (format t "~&CouchDB outbox conformance tests passed~%")
  t)

(run-outbox-conformance-tests)
