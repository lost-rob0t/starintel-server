(in-package :star-server-tests)

(def-suite runtime-lifecycle-tests
  :description "Owned runtime lifecycle and dependency-aware readiness")

(in-suite runtime-lifecycle-tests)

(test boot-rabbit-recovers-before-constructing-or-starting-intake
  (dolist (recovery-fails-p '(nil t))
    (let ((events nil)
          (saved nil)
          (names '("documents-ingest" "documents-update" "documents-targets"))
          (handles (loop for name in '("documents-ingest" "documents-update" "documents-targets")
                         collect (cons name (gensym "CONSUMER"))))
          (failure (make-condition 'simple-error :format-control "simulated recovery failure")))
      (flet ((stub (symbol function)
               (push (cons symbol (symbol-function symbol)) saved)
               (sb-ext:without-package-locks
                 (setf (symbol-function symbol) function))))
        (unwind-protect
             (progn
               (stub 'star.rabbit::recover-pending-publications
                     (lambda ()
                       (push :recover events)
                       (when recovery-fails-p
                         (error failure))))
               (stub 'star.rabbit::make-document-consumer
                     (lambda (&key name &allow-other-keys)
                       (push (list :construct name) events)
                       (cdr (assoc name handles :test #'equal))))
               (stub 'star.rabbit::start-consumer
                     (lambda (consumer)
                       (push (list :start (car (rassoc consumer handles))) events)
                       :ignored-start-result))
               (if recovery-fails-p
                   (progn
                     (is (eq failure
                             (handler-case
                                 (progn (star.rabbit:start-consumers) nil)
                               (error (condition) condition))))
                     (is (equal '(:recover) (reverse events))
                         "Failed recovery must construct/start no intake"))
                   (progn
                     (is (equal (mapcar #'cdr handles) (star.rabbit:start-consumers))
                         "Retain the three original consumer handles")
                     (is (equal (append '(:recover)
                                        (mapcar (lambda (name) (list :construct name)) names)
                                        (mapcar (lambda (name) (list :start name)) names))
                                (reverse events))
                         "Recovery must precede construction and original start order"))))
          (dolist (entry saved)
            (sb-ext:without-package-locks
              (setf (symbol-function (car entry)) (cdr entry)))))))))

(defun make-running-test-consumer (&optional (name "runtime-test-consumer"))
  (let ((consumer
          (make-instance
           'star.consumers:consumer
           :name name
           :workers 1
           :stream nil)))
    (setf (star.consumers:consumer-state consumer) :running
          (star.consumers:consumer-running-p consumer) t)
    consumer))

(defun make-ready-test-runtime ()
  (let ((consumer (make-running-test-consumer)))
    (star.runtime::%make-star-runtime
     :state :running
     :actor-system t
     :consumers (list consumer)
     :event-consumer consumer
     :http-server t
     :kernel t
     :started-at (get-universal-time))))

(test consumer-stop-joins-only-owned-threads
  (let* ((consumer
           (make-running-test-consumer "owned-thread-test"))
         (owned-thread
           (bt:make-thread
            (lambda ()
              (loop until
                    (eq :stopping
                        (star.consumers:consumer-state consumer))
                    do (sleep 0.01)))
            :name "starintel-owned-test-thread"))
         (unrelated-thread
           (bt:make-thread
            (lambda () (sleep 0.5))
            :name "unrelated-test-thread")))
    (setf (star.consumers:consumer-threads consumer)
          (list owned-thread))
    (unwind-protect
         (progn
           (is (star.consumers::stop-consumer-and-wait
                consumer
                :timeout-seconds 1))
           (is (eq :stopped
                   (star.consumers:consumer-state consumer)))
           (is (not (star.consumers:consumer-running-p consumer)))
           (is (bt:thread-alive-p unrelated-thread)))
      (when (bt:thread-alive-p unrelated-thread)
        (bt:join-thread unrelated-thread)))))

(test runtime-readiness-fails-closed
  (let ((runtime (make-ready-test-runtime)))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly t))
          (star.runtime::*rabbit-readiness-probe* (constantly t)))
      (is (star.runtime:runtime-ready-p runtime)))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly nil))
          (star.runtime::*rabbit-readiness-probe* (constantly t)))
      (is (not (star.runtime:runtime-ready-p runtime))))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly t))
          (star.runtime::*rabbit-readiness-probe* (constantly nil)))
      (is (not (star.runtime:runtime-ready-p runtime))))))

;; issue #416: auth-required CouchDB fronts answer anonymous /_up with
;; 401 while the authed write path is healthy, so the default probe must
;; send the configured credential or readiness wedges at unavailable.
(test default-couchdb-readiness-probe-authenticates
  (let* ((expected (cons "probe-admin" "probe-secret"))
         (captured nil)
         (original-dex-get (and (fboundp 'dex:get)
                                (symbol-function 'dex:get))))
    (flet ((stubbed-dex-get (uri &rest args &key basic-auth &allow-other-keys)
             (declare (ignore uri args))
             (setf captured basic-auth)
             ;; auth-required front: authenticated -> 200, else -> 401
             (values nil (if (equal basic-auth expected) 200 401))))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (setf (symbol-function 'dex:get) #'stubbed-dex-get))
             (let ((star:*couchdb-user* "probe-admin")
                   (star:*couchdb-password* "probe-secret"))
               (is (star.runtime::default-couchdb-readiness-probe)
                   "authenticated /_up 200 must read as ready")
               (is (equal expected captured)
                   "probe must send the configured admin credential"))
             (let ((star:*couchdb-user* "wrong")
                   (star:*couchdb-password* "wrong"))
               (is (not (star.runtime::default-couchdb-readiness-probe))
                   "wrong credentials must stay unready (fail closed)")))
        (sb-ext:without-package-locks
          (setf (symbol-function 'dex:get) original-dex-get))))))

(test runtime-liveness-tracks-lifecycle-state
  (let ((runtime
          (star.runtime::%make-star-runtime
           :state :running
           :started-at (get-universal-time))))
    (is (star.runtime:runtime-live-p runtime))
    (setf (star.runtime:star-runtime-state runtime) :stopping)
    (is (star.runtime:runtime-live-p runtime))
    (setf (star.runtime:star-runtime-state runtime) :stopped)
    (is (not (star.runtime:runtime-live-p runtime)))
    (is (not (star.runtime:runtime-live-p nil)))))

(test operational-health-paths-are-public
  (is (member "/health" star:*auth-public-paths* :test #'string=))
  (is (member "/live" star:*auth-public-paths* :test #'string=))
  (is (member "/ready" star:*auth-public-paths* :test #'string=)))

(defun stub-star-actors-boot (recover-spy)
  "Install read-only stubs over every infrastructure start function.

No real actor system, producer, CouchDB actor, timer, or hook side effect
can occur under the stubs.  RECOVER-SPY substitutes for
recover-persisted-targets.  Restore with RESTORE-STAR-ACTORS-BOOT."
  (setf *boot-stub-saved-functions* nil)
  (flet ((stub (symbol function)
           ;; Wheel-timer symbols can be unbound in the unit-test image;
           ;; stub them anyway and restore by fmakunbound.
           (push (cons symbol
                       (and (fboundp symbol) (symbol-function symbol)))
                 *boot-stub-saved-functions*)
           (sb-ext:without-package-locks
             (setf (symbol-function symbol) function))))
    (dolist (replacement
              (list
               (cons 'star.actors::start-actor-system (constantly nil))
               (cons 'star.actors::make-producer-agent (constantly nil))
               (cons 'star.actors::start-couchdb-agent (constantly nil))
               (cons 'star.actors::start-actor-index (constantly nil))
               (cons 'star.actors::start-couchdb-gets (constantly nil))
               (cons 'star.actors::start-couchdb-inserts (constantly nil))
               (cons 'star.actors::start-target-timer (constantly nil))
               (cons 'star.actors::start-target-actor (constantly nil))
               (cons 'nhooks:run-hook
                     (lambda (&rest args) (declare (ignore args)) nil))
               (cons 'star.actors::recover-persisted-targets recover-spy)))
      (stub (car replacement) (cdr replacement)))
    (let ((wt-package (find-package :wt)))
      (dolist (name '("MAKE-WHEEL-TIMER" "SCHEDULE-RECURRING"))
        (let ((symbol (and wt-package (find-symbol name wt-package))))
          (when symbol
            (stub symbol
                  (lambda (&rest args)
                    (declare (ignore args))
                    nil))))))))

(defparameter *boot-stub-saved-functions* nil)

(defun restore-star-actors-boot ()
  (dolist (saved *boot-stub-saved-functions*)
    (sb-ext:without-package-locks
      (let ((symbol (car saved))
            (original (cdr saved)))
        (cond
          (original (setf (symbol-function symbol) original))
          ;; The symbol had no definition before stubbing.
          ((fboundp symbol) (fmakunbound symbol))))))
  (setf *boot-stub-saved-functions* nil))

;; issue #360: the operative start-actors (runtime-lifecycle) must rehydrate
;; durable recurring target schedules at boot; a restart without recovery
;; silently stops all recurring collection.
(test boot-start-actors-rehydrates-persisted-targets
  (let ((recover-calls 0)
        (original-producer star.actors::*producer-agent*)
        (original-gc-timer star.actors::*gc-timer*))
    (unwind-protect
         (progn
           (stub-star-actors-boot
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (incf recover-calls)
              '(:boot-recovery-stubbed)))
           (is (eq star.actors::*sys* (star.actors:start-actors)))
           (is (= 1 recover-calls)
               "boot must run recover-persisted-targets exactly once"))
      (restore-star-actors-boot)
      (setf star.actors::*producer-agent* original-producer
            star.actors::*gc-timer* original-gc-timer))))
