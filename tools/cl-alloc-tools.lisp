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
           #:census-diff
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
  "Run BODY, SETF PLACE to the net bytes retained by it, and return BODY's values.
A full GC runs before and after, so the delta is *net retained* bytes --
exactly what leak hunts start from.  A zero delta means BODY retained
nothing (it may still have consed heavily and collected).
  (let (n) (with-allocation-accounting (n) (make-list 100000))) => n ~= 800000
PLACE must be a setfable location bound by the caller; use NIL when only
the report line is wanted."
  (let ((before (gensym "BEFORE")) (delta (gensym "DELTA")))
    `(let ((,before (progn (force-full-gc) (dynamic-bytes))))
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

;;; ------------------------------------------------------------------
;;; Leak hunting: weak instance registry
;;;;
;;; Tracked objects are held only weakly, so the registry never causes
;;; the leak it is hunting.  A full GC prunes dead entries; the census
;;; counts what survives.

#+sbcl
(defparameter *registry* (make-hash-table :test #'eq :weakness :key)
  "Weak map: object -> registration metadata.  Entries vanish on their
own when the object becomes unreachable.")

(defun reset-registry ()
  (clrhash *registry*)
  (values))

(defun track-instance (object &key (label nil labelp))
  "Register OBJECT weakly; re-tracking replaces the metadata.
Typical integration:
  (defmethod initialize-instance :after ((o buffer) &key)
    (cl-alloc-tools:track-instance o))
Returns OBJECT."
  (setf (gethash object *registry*)
        (list (type-of object) (if labelp label :unlabeled)))
  object)

(defun census (&key (gc t))
  "Return an alist ((type label . count) ...) of live tracked objects,
sorted by descending count.  Runs a full GC first by default so the
numbers are trustworthy."
  (when gc (force-full-gc))
  (let ((counts (make-hash-table :test #'equal)))
    (maphash (lambda (object meta)
               (declare (ignore object))
               (incf (gethash meta counts 0)))
             *registry*)
    (let (alist)
      (maphash (lambda (meta count) (push (cons meta count) alist)) counts)
      (sort alist #'> :key #'cdr))))

(defun census-report (&key (stream *trace-output*) (gc t))
  "Print a human table from CENSUS; returns the alist it printed."
  (let ((alist (census :gc gc)))
    (format stream "~&;; --- leak census: ~:D live tracked object~:P ---~%"
            (reduce #'+ alist :key #'cdr :initial-value 0))
    (dolist (row alist)
      (destructuring-bind ((type label) . count) row
        (format stream ";;   ~8D  ~A~@[ label ~S~]~%"
                count type
                (unless (eq label :unlabeled) label))))
    alist))

(defun census-diff (before after)
  "Compare two censuses; returns rows whose count changed, as
((type label before after delta) ...), sorted by descending growth."
  (let ((rows (make-hash-table :test #'equal)))
    (dolist (row before)
      (setf (gethash (car row) rows) (list (car row) (cdr row) 0)))
    (dolist (row after)
      (let ((slot (or (gethash (car row) rows)
                      (setf (gethash (car row) rows) (list (car row) 0 0)))))
        (setf (third slot) (cdr row))))
    (let (diffs)
      (maphash (lambda (_ row)
                 (unless (= (second row) (third row))
                   (push (list (first row) (second row) (third row)
                               (- (third row) (second row)))
                         diffs)))
               rows)
      (sort diffs #'> :key #'fourth))))

(defmacro with-tracking ((place &key (label nil labelp)) &body body)
  "Run BODY with a fresh registry; PLACE is SETF to the census of
objects created (and still live) under it.  PLACE must be setfable and
bound by the caller.  Combined with CENSUS-DIFF you can locate which
phase of a workload leaks:
  (let (before after)
    (with-tracking (before) (run-phase 1))
    (with-tracking (after)  (run-phase 2)))"
  `(let ((*registry* (make-hash-table :test #'eq :weakness :key)))
     (multiple-value-prog1 (progn ,@body)
       (setf ,place (census))
       ,@(when labelp
           `((format *trace-output* "~&;; tracking ~S: ~S~%" ,label ,place))))))
