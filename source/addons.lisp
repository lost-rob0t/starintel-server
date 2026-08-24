(in-package :star)

(define-condition addon-error (error)
  ((name :initarg :name :reader addon-error-name)
   (message :initarg :message :reader addon-error-message)
   (cause :initarg :cause :initform nil :reader addon-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Add-on ~a: ~a"
             (addon-error-name condition)
             (addon-error-message condition)))))

(defstruct addon-definition
  name
  system
  start
  stop)

(defstruct addon-state
  name
  system
  (status :registered)
  (generation 0)
  started-at
  stopped-at
  last-error)

(defvar *addon-definitions* (make-hash-table :test #'equal))
(defvar *addon-states* (make-hash-table :test #'equal))
(defvar *addon-lock* (bt:make-lock "starintel-addon-registry"))
(defvar *addon-lifecycle-lock* (bt:make-lock "starintel-addon-lifecycle"))

(defun canonical-addon-name (name)
  (string-downcase
   (etypecase name
     (string name)
     (symbol (symbol-name name)))))

(defun canonical-addon-system (system)
  (canonical-addon-name system))

(defun register-addon (name &key system start stop)
  "Register a trusted StarIntel add-on lifecycle.

Add-on systems call this while ASDF loads them. START and STOP must be
zero-argument functions. Registration is metadata only; it does not start the
add-on and therefore remains safe to repeat during ASDF reload."
  (let* ((canonical-name (canonical-addon-name name))
         (canonical-system
           (canonical-addon-system (or system canonical-name))))
    (unless (or (null start) (functionp start))
      (error 'addon-error
             :name canonical-name
             :message "START must be a function or NIL"))
    (unless (or (null stop) (functionp stop))
      (error 'addon-error
             :name canonical-name
             :message "STOP must be a function or NIL"))
    (bt:with-lock-held (*addon-lock*)
      (setf (gethash canonical-system *addon-definitions*)
            (make-addon-definition
             :name canonical-name
             :system canonical-system
             :start start
             :stop stop))
      (unless (gethash canonical-system *addon-states*)
        (setf (gethash canonical-system *addon-states*)
              (make-addon-state
               :name canonical-name
               :system canonical-system))))
    canonical-name))

(defun addon-definition-for-system (system)
  (let ((canonical-system (canonical-addon-system system)))
    (bt:with-lock-held (*addon-lock*)
      (gethash canonical-system *addon-definitions*))))

(defun addon-state-for-system (system)
  (let ((canonical-system (canonical-addon-system system)))
    (bt:with-lock-held (*addon-lock*)
      (gethash canonical-system *addon-states*))))

(defun ensure-addon-definition (system)
  (or (addon-definition-for-system system)
      (error 'addon-error
             :name (canonical-addon-system system)
             :message "ASDF system loaded but did not register a StarIntel add-on")))

(defun update-addon-state (system &key status generation started-at stopped-at last-error)
  "Mutate registry state. Caller must hold *ADDON-LOCK*."
  (let* ((canonical-system (canonical-addon-system system))
         (state
           (or (gethash canonical-system *addon-states*)
               (setf (gethash canonical-system *addon-states*)
                     (make-addon-state
                      :name canonical-system
                      :system canonical-system)))))
    (when status (setf (addon-state-status state) status))
    (when generation (setf (addon-state-generation state) generation))
    (when started-at (setf (addon-state-started-at state) started-at))
    (when stopped-at (setf (addon-state-stopped-at state) stopped-at))
    (setf (addon-state-last-error state) last-error)
    state))

(defun invoke-addon-hook (definition hook-name)
  (let ((hook
          (ecase hook-name
            (:start (addon-definition-start definition))
            (:stop (addon-definition-stop definition)))))
    (when hook
      (funcall hook))))

(defun start-addon-definition (definition)
  (invoke-addon-hook definition :start)
  (bt:with-lock-held (*addon-lock*)
    (let* ((system (addon-definition-system definition))
           (state (update-addon-state system :status :active
                                     :started-at (get-universal-time)
                                     :last-error nil)))
      (incf (addon-state-generation state))
      (copy-addon-state state))))

(defun %load-addon (system)
  (let ((canonical-system (canonical-addon-system system)))
    (handler-case
        (progn
          (asdf:load-system canonical-system)
          (let ((definition (ensure-addon-definition canonical-system))
                (state (addon-status canonical-system)))
            (if (and state (eq :active (addon-state-status state)))
                state
                (start-addon-definition definition))))
      (error (condition)
        (bt:with-lock-held (*addon-lock*)
          (update-addon-state canonical-system
                              :status :failed
                              :last-error (princ-to-string condition)))
        (error 'addon-error
               :name canonical-system
               :message "load/start failed"
               :cause condition)))))

(defun load-addon (system)
  "Load SYSTEM through ASDF and start its registered StarIntel lifecycle.

This is the intended init.lisp experience:

  (load-addon :starintel-bixby)

Add-ons are trusted operator code, exactly like init.lisp; this is a lifecycle
and packaging boundary, not a sandbox or an authorization boundary. Lifecycle
operations are serialized so load/stop/reload cannot interleave."
  (bt:with-lock-held (*addon-lifecycle-lock*)
    (%load-addon system)))

(defun %unload-addon (system)
  (let* ((canonical-system (canonical-addon-system system))
         (definition (addon-definition-for-system canonical-system))
         (state (addon-status canonical-system)))
    (unless definition
      (error 'addon-error
             :name canonical-system
             :message "add-on is not registered"))
    (when (and state (eq :active (addon-state-status state)))
      (handler-case
          (progn
            (invoke-addon-hook definition :stop)
            (bt:with-lock-held (*addon-lock*)
              (update-addon-state canonical-system
                                  :status :stopped
                                  :stopped-at (get-universal-time)
                                  :last-error nil)))
        (error (condition)
          (bt:with-lock-held (*addon-lock*)
            (update-addon-state canonical-system
                                :status :failed
                                :last-error (princ-to-string condition)))
          (error 'addon-error
                 :name canonical-system
                 :message "stop failed"
                 :cause condition))))
    (addon-status canonical-system)))

(defun unload-addon (system)
  "Stop a loaded add-on without pretending Common Lisp code can be unloaded."
  (bt:with-lock-held (*addon-lifecycle-lock*)
    (%unload-addon system)))

(defun restore-addon-generation (definition system condition)
  (bt:with-lock-held (*addon-lock*)
    (setf (gethash system *addon-definitions*) definition))
  (handler-case
      (progn
        (invoke-addon-hook definition :start)
        (bt:with-lock-held (*addon-lock*)
          (update-addon-state system
                              :status :active
                              :started-at (get-universal-time)
                              :last-error
                              (format nil "reload rolled back after: ~a" condition))))
    (error (rollback-condition)
      (bt:with-lock-held (*addon-lock*)
        (update-addon-state system
                            :status :failed
                            :last-error
                            (format nil "reload failed: ~a; rollback failed: ~a"
                                    condition rollback-condition))))))

(defun %reload-addon (system)
  (let* ((canonical-system (canonical-addon-system system))
         (old-definition (ensure-addon-definition canonical-system))
         (old-state (addon-status canonical-system))
         (was-active (and old-state (eq :active (addon-state-status old-state)))))
    (when was-active
      (handler-case
          (invoke-addon-hook old-definition :stop)
        (error (condition)
          (bt:with-lock-held (*addon-lock*)
            (update-addon-state canonical-system
                                :status :failed
                                :last-error (princ-to-string condition)))
          (error 'addon-error
                 :name canonical-system
                 :message "reload stop phase failed"
                 :cause condition)))
      (bt:with-lock-held (*addon-lock*)
        (update-addon-state canonical-system
                            :status :reloading
                            :stopped-at (get-universal-time)
                            :last-error nil)))
    (handler-case
        (progn
          (asdf:load-system canonical-system :force t)
          (start-addon-definition (ensure-addon-definition canonical-system)))
      (error (condition)
        (if was-active
            (restore-addon-generation old-definition canonical-system condition)
            (bt:with-lock-held (*addon-lock*)
              (setf (gethash canonical-system *addon-definitions*) old-definition)
              (update-addon-state canonical-system
                                  :status :failed
                                  :last-error (princ-to-string condition))))
        (error 'addon-error
               :name canonical-system
               :message "reload failed"
               :cause condition)))))

(defun reload-addon (system)
  "Reload one add-on transactionally at the lifecycle boundary.

The old START/STOP function objects are retained until the replacement starts.
If loading or starting the replacement fails, the old definition and START
hook are restored when possible. ASDF dependencies own dependency ordering;
START/STOP hooks must not recursively invoke add-on lifecycle operations."
  (bt:with-lock-held (*addon-lifecycle-lock*)
    (%reload-addon system)))

(defun addon-status (system)
  (let ((canonical-system (canonical-addon-system system)))
    (bt:with-lock-held (*addon-lock*)
      (let ((state (gethash canonical-system *addon-states*)))
        (and state (copy-addon-state state))))))

(defun list-addons ()
  (bt:with-lock-held (*addon-lock*)
    (sort
     (loop for state being the hash-values of *addon-states*
           collect (copy-addon-state state))
     #'string<
     :key #'addon-state-system)))

(export '(addon-error
          addon-error-name
          addon-error-message
          addon-error-cause
          addon-state
          addon-state-name
          addon-state-system
          addon-state-status
          addon-state-generation
          addon-state-started-at
          addon-state-stopped-at
          addon-state-last-error
          register-addon
          load-addon
          unload-addon
          reload-addon
          addon-status
          list-addons)
        :star)
