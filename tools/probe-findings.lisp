;;;; Dynamic leak probes for starintel-server registries, driven by
;;;; cl-alloc-tools.  Run from the flake devshell:
;;;;
;;;;   nix develop -c sbcl --noinform --disable-debugger \
;;;;     --load tools/cl-alloc-tools.lisp --load tools/probe-findings.lisp --quit

(asdf:load-system :starintel-gserver)

(defpackage #:starintel-probe
  (:use #:cl #:cl-alloc-tools #:jsown))
(in-package #:starintel-probe)

(defvar *captured-jobs* nil)

(defun %capture-tell (worker job)
  "Capture the told job; real tell is async (mailbox), running the worker
outside submit's lock.  Executing synchronously here would re-enter
*bulk-ingest-lock* via release-bulk-job-slot (recursive lock error)."
  (declare (ignore worker))
  (push job *captured-jobs*))

(defun %drain-captured-jobs ()
  "Run captured jobs to terminal state, mirroring bulk-worker-handler."
  (dolist (job (nreverse (shiftf *captured-jobs* nil)))
    (unwind-protect
         (star.frontends.http-api:execute-bulk-job
          job
          :publish-fn (lambda (document) (declare (ignore document))))
      (star.frontends.http-api::release-bulk-job-slot job))))

(defun %ensure-workers-stub ()
  "Stand in for start-bulk-ingest-workers: the submit path only needs a
non-empty worker list to round-robin over."
  (unless star.frontends.http-api::*bulk-ingest-workers*
    (setf star.frontends.http-api::*bulk-ingest-workers*
          '(:probe-worker :probe-worker :probe-worker :probe-worker)))
  t)

(defun %submit-terminal-bulk-jobs (n)
  "Submit in chunks below the 4-jobs-per-principal pending cap, draining
each chunk to terminal state before the next."
  (loop with remaining = n
        while (plusp remaining)
        for chunk = (min 4 remaining)
        do (loop repeat chunk
                 do (star.frontends.http-api:submit-bulk-ingest-job
                     (list (new-js ("dtype" "person")))
                     "probe-principal"
                     :ensure-workers-fn #'%ensure-workers-stub
                     :tell-fn #'%capture-tell))
           (decf remaining chunk)
           (%drain-captured-jobs)))

(defun probe-bulk-jobs (&key (warmup 200) (measured 2000))
  (let ((before (hash-table-count
                 star.frontends.http-api::*bulk-ingest-jobs*)))
    (%submit-terminal-bulk-jobs warmup)
    (let (growth)
      (with-heap-growth (growth :stream *terminal-io*)
        (%submit-terminal-bulk-jobs measured))
      (let* ((after (hash-table-count
                     star.frontends.http-api::*bulk-ingest-jobs*))
             (retained (- after before))
             (job-row (find "BULK-INGEST-JOB" growth
                            :key (lambda (r) (symbol-name (first r)))
                            :test #'string=)))
        (format *terminal-io*
                "~&PROBE bulk-jobs: ~D terminal jobs; registry ~D -> ~D~%"
                measured before after)
        (when job-row
          (format *terminal-io*
                  "PROBE bulk-jobs: heap growth ~:D bytes / ~D structs in ~A~%"
                  (third job-row) (second job-row) (first job-row)))
        (format *terminal-io* "PROBE bulk-jobs: verdict ~A~%"
                (if (>= retained measured)
                    "LEAK-CONFIRMED: terminal jobs are never removed"
                    "INCONCLUSIVE"))))))

(defun %probe-envelope (sid)
  (star.actors::%make-target-dispatch-envelope
   (star.actors::%make-target-record
    (format nil "probe-target-~a" sid) "probe-actor" "probe-target" 1
    nil nil (new-js ("dtype" "person")) "1" nil nil)
   nil sid "probe-execution" 0 "probe-trace" "probe-lease" 1
   (+ (get-universal-time) 3600)))

(defun probe-target-schedules (&key (measured 3000))
  (let ((before (hash-table-count star.actors::*active-target-schedules*)))
    (let ((results
            (loop for i below measured
                  for sid = (format nil "probe-fired-~d" i)
                  collect
                  (star.actors::register-target-schedule
                   (%probe-envelope sid)
                   (lambda (envelope) (declare (ignore envelope)))
                   ;; simulate the wheel timer firing this one-shot now
                   (lambda (id delay callback)
                     (declare (ignore id delay))
                     (funcall callback))
                   (lambda (&rest args) (declare (ignore args)))))))
      (let* ((after (hash-table-count star.actors::*active-target-schedules*))
             (retained (- after before))
             (scheduled (count :scheduled results)))
        (format *terminal-io*
                "~&PROBE target-schedules: ~D fired one-shot schedules; ~
registry ~D -> ~D~%" scheduled before after)
        (format *terminal-io* "PROBE target-schedules: verdict ~A~%"
                (if (= retained scheduled)
                    "LEAK-CONFIRMED: fired one-shot schedules retained forever"
                    "INCONCLUSIVE"))))))

(probe-bulk-jobs)
(probe-target-schedules)
