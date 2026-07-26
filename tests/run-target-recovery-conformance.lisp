(in-package :cl-user)

(defun target-recovery-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun target-recovery-document
    (id &key (actor "scanner") (target "example.org")
             (delay 0) (recurring nil) (revision "1-test")
             lease-owner lease-expires-at)
  (let ((data (jsown:empty-object))
        (document (jsown:empty-object)))
    (setf (jsown:val data "actor") actor
          (jsown:val data "target") target
          (jsown:val data "delay") delay
          (jsown:val data "recurring") (if recurring :true :false)
          (jsown:val data "options") #())
    (when lease-owner
      (setf (jsown:val data "lease_owner") lease-owner))
    (when lease-expires-at
      (setf (jsown:val data "lease_expires_at") lease-expires-at))
    (setf (jsown:val document "_id") id
          (jsown:val document "_rev") revision
          (jsown:val document "dataset") "default"
          (jsown:val document "dtype") "target"
          (jsown:val document "schema_version") "0.9.0"
          (jsown:val document "version") 1
          (jsown:val document "date_added") "2026-07-26T00:00:00Z"
          (jsown:val document "date_updated") "2026-07-26T00:00:00Z"
          (jsown:val document "sources") #()
          (jsown:val document "evidence") #()
          (jsown:val document "data") data)
    document))

(defun target-recovery-row (document)
  (let ((row (jsown:empty-object)))
    (setf (jsown:val row "doc") document)
    row))

(defun target-recovery-response (&rest documents)
  (let ((response (jsown:empty-object)))
    (setf (jsown:val response "rows")
          (coerce (mapcar #'target-recovery-row documents) 'vector))
    response))

(defun test-target-repository-uses-configured-database-and_canonical_view ()
  (let ((seen nil)
        (document (target-recovery-document "target:configured")))
    (let ((documents
            (star.actors:query-persisted-target-documents
             :client-token
             "configured-db"
             :query-fn
             (lambda (client database ddoc view &key include-docs reduce
                                                   &allow-other-keys)
               (setf seen
                     (list client database ddoc view include-docs reduce))
               (target-recovery-response document)))))
      (target-recovery-check (= 1 (length documents))
                             "Configured database query lost the document")
      (target-recovery-check
       (equal seen
              '(:client-token "configured-db" "targets" "by_actor" t nil))
       "Repository query arguments were ~s" seen))))

(defun test-empty-target_repository_is_empty ()
  (multiple-value-bind (records invalid-count)
      (star.actors:load-persisted-target-records
       :client-token
       "empty-db"
       :query-fn
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (target-recovery-response))
       :quarantine-fn
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (error "Empty database attempted quarantine")))
    (target-recovery-check (null records)
                           "Empty database returned records")
    (target-recovery-check (zerop invalid-count)
                           "Empty database returned invalid rows")))

(defun test-invalid-persisted_target_is_quarantined ()
  (let ((invalid (target-recovery-document
                  "target:invalid" :actor "" :target ""))
        (quarantined nil))
    (multiple-value-bind (records invalid-count)
        (star.actors:load-persisted-target-records
         :client-token
         "configured-db"
         :query-fn
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (target-recovery-response invalid))
         :quarantine-fn
         (lambda (client database document condition)
           (setf quarantined
                 (list client database document condition))))
      (target-recovery-check (null records)
                             "Invalid target entered recovery")
      (target-recovery-check (= 1 invalid-count)
                             "Invalid target count was ~d" invalid-count)
      (target-recovery-check quarantined
                             "Invalid target was not quarantined")
      (target-recovery-check
       (typep (fourth quarantined) 'star.actors:invalid-persisted-target)
       "Invalid target quarantine reason was ~s" (fourth quarantined)))))

(defun test-invalid_target_quarantine_id_is_stable ()
  (let* ((document (target-recovery-document
                    "target:bad" :actor "" :revision "7-rev"))
         (condition
           (make-condition 'star.actors:invalid-persisted-target
                           :document-id "target:bad"
                           :reason "bad actor"))
         (left
           (star.actors::invalid-target-quarantine-record
            "configured-db" document condition))
         (right
           (star.actors::invalid-target-quarantine-record
            "configured-db" document condition)))
    (target-recovery-check
     (string= (jsown:val left "_id") (jsown:val right "_id"))
     "Repeated invalid target recovery produced duplicate quarantine IDs")))

(defun test_one_shot_and_recurring_targets_recover_once ()
  (let* ((one-shot
           (star.actors:parse-target-record
            (target-recovery-document "target:once")))
         (recurring
           (star.actors:parse-target-record
            (target-recovery-document
             "target:repeat" :delay 60 :recurring t)))
         (calls nil)
         (original (symbol-function 'star.actors:submit-target)))
    (clrhash star.actors:*recovered-target-fingerprints*)
    (unwind-protect
         (progn
           (setf (symbol-function 'star.actors:submit-target)
                 (lambda (record &key first-time-p recovered-p)
                   (push (list (star.actors:target-record-id record)
                               first-time-p recovered-p)
                         calls)))
           (target-recovery-check
            (eq :recovered (star.actors:recover-target-record one-shot))
            "One-shot target was not recovered")
           (target-recovery-check
            (eq :recovered (star.actors:recover-target-record recurring))
            "Recurring target was not recovered")
           (target-recovery-check
            (eq :duplicate (star.actors:recover-target-record one-shot))
            "Repeated one-shot recovery was not deduplicated")
           (target-recovery-check
            (eq :duplicate (star.actors:recover-target-record recurring))
            "Repeated recurring recovery was not deduplicated")
           (setf calls (nreverse calls))
           (target-recovery-check
            (equal calls
                   '(("target:once" nil t)
                     ("target:repeat" t t)))
            "Recovery submissions were ~s" calls))
      (setf (symbol-function 'star.actors:submit-target) original)
      (clrhash star.actors:*recovered-target-fingerprints*))))

(defun test-active_lease_suppresses_recovery ()
  (let* ((record
           (star.actors:parse-target-record
            (target-recovery-document
             "target:leased"
             :lease-owner "worker-1"
             :lease-expires-at "2999-01-01T00:00:00Z")))
         (called nil)
         (original (symbol-function 'star.actors:submit-target)))
    (clrhash star.actors:*recovered-target-fingerprints*)
    (unwind-protect
         (progn
           (setf (symbol-function 'star.actors:submit-target)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf called t)))
           (target-recovery-check
            (eq :leased (star.actors:recover-target-record record))
            "Active lease was not respected")
           (target-recovery-check (not called)
                                  "Leased target was resubmitted"))
      (setf (symbol-function 'star.actors:submit-target) original)
      (clrhash star.actors:*recovered-target-fingerprints*))))

(defun test_typed_target_command_preserves_document ()
  (let* ((record
           (star.actors:parse-target-record
            (target-recovery-document "target:typed")))
         (command
           (star.actors::make-target-command
            record :first-time-p nil :recovered-p t)))
    (target-recovery-check
     (typep command 'star.actors:target-command)
     "Target command is not typed")
    (target-recovery-check
     (eq record (star.actors:target-command-record command))
     "Target command lost its record")
    (target-recovery-check
     (not (star.actors:target-command-first-time-p command))
     "Target command first-time flag changed")
    (target-recovery-check
     (star.actors:target-command-recovered-p command)
     "Target command recovery flag changed")))

(defun run-target-recovery-conformance-tests ()
  (format t "~&Running persisted target recovery tests~%")
  (test-target-repository-uses-configured-database-and_canonical_view)
  (test-empty-target_repository-is_empty)
  (test-invalid-persisted_target_is_quarantined)
  (test-invalid_target_quarantine_id_is_stable)
  (test_one_shot_and_recurring_targets_recover_once)
  (test-active_lease_suppresses_recovery)
  (test_typed_target_command_preserves_document)
  (format t "~&Persisted target recovery tests passed~%")
  t)

(run-target-recovery-conformance-tests)
