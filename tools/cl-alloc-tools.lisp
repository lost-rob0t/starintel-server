;;;; cl-alloc-tools.lisp --- allocation profiling, leak hunting, bounded-memory
;;;; streaming for SBCL.
;;;;
;;;; Self-contained, no ASDF system required:
;;;;
;;;;   (load "tools/cl-alloc-tools.lisp")
;;;;   (cl-alloc-tools:selftest)
;;;;
;;;; Everything is SBCL-specific except where noted; non-SBCL hosts get
;;;; error-signaling stubs so portable code can call these forms safely.

(defpackage #:cl-alloc-tools
  (:use #:cl)
  (:export #:with-allocation-accounting
           #:retained-bytes-of
           #:*registry*
           #:reset-registry
           #:track-instance
           #:with-tracking
           #:census
           #:census-report
           #:make-bounded-queue
           #:bounded-queue-push
           #:bounded-queue-pop
           #:run-pipeline
           #:map-stream-chunks
           #:with-allocation-profiling
           #:selftest))
(in-package #:cl-alloc-tools)

;;; ------------------------------------------------------------------
;;; Net-retained allocation accounting
;;;;
;;; Gross allocation is not portably observable, so we measure what
;;; survives a full GC before/after the body. The delta is *net retained*
;;; bytes: the number leaks hunts start from.

#+sbcl
(progn
  (declaim (inline dynamic-bytes))
  (defun dynamic-bytes () (sb-kernel:dynamic-usage))
  (defun force-full-gc () (sb-ext:gc :full t)))

#-sbcl
(defparameter *unsupported-host* "cl-alloc-tools requires SBCL")

(defmacro with-allocation-accounting ((place &key (stream '*trace-output*)) &body body)
  "Run BODY, bind PLACE to the net bytes retained by it, and return BODY's values.
A full GC runs before and after, so the delta is *net retained* bytes --
exactly what leak hunts start from.  A zero delta means BODY retained
nothing (it may still have consed heavily and collected).
  (with-allocation-accounting (n) (make-list 100000)) => n ~= 800000
Use NIL as PLACE when only the report line is wanted."
  (let ((before (gensym "BEFORE")) (delta (gensym "DELTA")))
    `(let ((,before (progn (force-full-gc) (dynamic-bytes)))
           ,@(when place `((,place :pending))))
       (multiple-value-prog1 (progn ,@body)
         (force-full-gc)
         (let ((,delta (- (dynamic-bytes) ,before)))
           ,@(when place `((setf ,place ,delta)))
           (format ,stream "~&;; retained ~:D byte~:P~%" ,delta))))))

(defun retained-bytes-of (thunk)
  "FUNCTION variant of WITH-ALLOCATION-ACCOUNTING; returns (values result bytes)."
  (force-full-gc)
  (let ((before (dynamic-bytes)))
    (let ((result (funcall thunk)))
      (force-full-gc)
      (values result (- (dynamic-bytes) before)))))
