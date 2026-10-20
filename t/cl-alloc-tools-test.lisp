;;;; Tests for tools/cl-alloc-tools.lisp.
;;;; Run: sbcl --noinform --disable-debugger \
;;;;          --load tools/cl-alloc-tools.lisp --load t/cl-alloc-tools-test.lisp

(defpackage #:cl-alloc-tools-test
  (:use #:cl #:cl-alloc-tools))
(in-package #:cl-alloc-tools-test)

(defclass buffer () ((data :initarg :data :reader buffer-data)))

(defvar *kept* nil)

(defun spawn-dead (n)
  "Create N tracked buffer objects that die deterministically.
Runs in a joined thread: SBCL scans registers and the stack
conservatively, so dead objects from the censusing frame itself can
linger; a terminated thread's registers and stack are simply gone.
The *REGISTRY* dynamic binding is forwarded into the thread because
new threads start from the root binding of specials."
  (let ((registry *registry*))
    (sb-thread:join-thread
     (sb-thread:make-thread
      (lambda ()
        (let ((*registry* registry))
          (loop repeat n do (track-instance (make-instance 'buffer))))))))
  nil)

(defun test-accounting ()
  (multiple-value-bind (_ bytes) (retained-bytes-of
                                   (lambda ()
                                     (setf *kept* (make-list 100000))
                                     :ok))
    (declare (ignore _))
    (assert (> bytes 100000))
    (multiple-value-bind (__ released) (retained-bytes-of
                                         (lambda () (setf *kept* nil) :ok))
      (declare (ignore __))
      (assert (minusp released))))
  :accounting-ok)

(defun test-registry ()
  ;; objects still referenced at census time are counted; dead ones vanish
  (let ((live (make-instance 'buffer))
        (live-str "a live string"))
    (with-tracking (c1)
      (track-instance live)
      (track-instance live-str :label "named")
      (spawn-dead 2))
    (assert (= 2 (reduce #'+ c1 :key #'cdr :initial-value 0)) ()
            "live tracked objects should survive: ~S" c1)
    (assert (member "named" c1
                    :key (lambda (row) (second (car row))) :test #'equal)))
  ;; re-tracking an object replaces its metadata (one row per object)
  (let ((dup (make-instance 'buffer)))
    (with-tracking (c2)
      (track-instance dup)
      (track-instance dup :label "again"))
    (assert (equal '((buffer "again") . 1) (first c2)) ()
            "re-tracking replaces metadata: ~S" c2))
  ;; the registry itself must not keep objects alive
  (flet ((dead-only ()
           (with-tracking (c3)
             (spawn-dead 3))
           :done))
    (multiple-value-bind (result c3) (dead-only)
      (declare (ignore result))
      (assert (null c3) ()
              "weak registry must not resurrect dead objects: ~S" c3)))
  :registry-ok)

(defun test-streaming ()
  ;; bounded queue: FIFO order across threads
  (let ((q (make-bounded-queue 2)))
    (assert (eq t (bounded-queue-push q 1)))
    (assert (eql 1 (bounded-queue-pop q)))
    (close-bounded-queue q)
    (assert (eq '+queue-eof+ (bounded-queue-pop q)))
    (assert (null (bounded-queue-push q 2))))
  ;; pipeline: 100k items through a capacity-8 queue; consumer sums
  (let ((total 0)
        (seen 0))
    (run-pipeline (lambda (push)
                    (loop for i from 1 to 100000 do (funcall push i)))
                  (lambda (i) (incf total i) (incf seen))
                  :capacity 8)
    (assert (= 100000 seen))
    (assert (= (loop for i from 1 to 100000 sum i) total)))
  ;; producer errors propagate to the calling thread
  (let ((failed nil))
    (handler-case
        (run-pipeline (lambda (push)
                        (funcall push 1)
                        (funcall push 2)
                        (error "producer boom"))
                      (lambda (i) (declare (ignore i))))
      (error (e) (assert (search "producer boom" (princ-to-string e)))
             (setf failed t)))
    (assert failed))
  :streaming-ok)

(defun test-chunked ()
  ;; 1 MB string through 4 KB chunks: every char seen, nothing retained
  (let* ((meg (make-string (* 1024 1024) :initial-element #\7))
         (total 0)
         (retained (with-input-from-string (s meg)
                     (nth-value
                      1 (retained-bytes-of
                         (lambda ()
                           (map-stream-chunks
                            s 4096
                            (lambda (buffer nread)
                              (incf total nread)
                              (assert (char= #\7 (aref buffer 0))))
                            :element-type 'character)))))))
    (assert (= (* 1024 1024) total) () "lost chars: ~D" total)
    (assert (< retained 100000) () "chunked read must stay bounded: ~D"
            retained))
  :chunked-ok)

(defun test-census-diff ()
  (let (before after)
    (let ((survivors1 (list (make-instance 'buffer))))
      (with-tracking (before)
        (mapc #'track-instance survivors1)
        (spawn-dead 3))
      (assert (= 1 (length survivors1))))
    (let ((survivors2 (list (make-instance 'buffer)
                            (make-instance 'buffer))))
      (with-tracking (after)
        (mapc #'track-instance survivors2)
        (spawn-dead 1))
      (assert (= 2 (length survivors2))))
    (let ((diff (census-diff before after)))
      (assert (= 1 (length diff)) () "expected one changed row: ~S" diff)
      (assert (equal '(buffer :unlabeled) (first (first diff))))
      (assert (= 1 (second (first diff))))
      (assert (= 2 (third (first diff))))
      (assert (= 1 (fourth (first diff))))))
  :diff-ok)

(format t "~&~A ~A ~A ~A ~A~%"
        (test-accounting) (test-registry) (test-census-diff)
        (test-streaming) (test-chunked))
(format t "~&all cl-alloc-tools tests passed~%")
