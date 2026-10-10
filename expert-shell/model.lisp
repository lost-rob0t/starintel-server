(in-package :star.expert.shell)

(defclass shell-session ()
  ((client
    :initarg :client
    :reader shell-session-client)
   (engine
    :initarg :engine
    :reader shell-session-engine)
   (last-result
    :initform nil
    :accessor shell-session-last-result)
   (last-plan
    :initform nil
    :accessor shell-session-last-plan)
   (trace
    :initform nil
    :accessor shell-session-trace)))

(defclass shell-request ()
  ((verb
    :initarg :verb
    :reader shell-request-verb)
   (resource
    :initarg :resource
    :reader shell-request-resource)
   (qualifier
    :initarg :qualifier
    :initform nil
    :reader shell-request-qualifier)
   (args
    :initarg :args
    :initform nil
    :reader shell-request-args)
   (confirmed-p
    :initarg :confirmed-p
    :initform nil
    :reader shell-request-confirmed-p)
   (raw
    :initarg :raw
    :initform nil
    :reader shell-request-raw)))

(defclass shell-plan ()
  ((operation
    :initarg :operation
    :reader shell-plan-operation)
   (risk
    :initarg :risk
    :reader shell-plan-risk)
   (rule-name
    :initarg :rule-name
    :reader shell-plan-rule-name)
   (reason
    :initarg :reason
    :reader shell-plan-reason)
   (args
    :initarg :args
    :initform nil
    :reader shell-plan-args)
   (confirmed-p
    :initarg :confirmed-p
    :initform nil
    :reader shell-plan-confirmed-p)))

(defclass shell-result ()
  ((success-p
    :initarg :success-p
    :reader shell-result-success-p)
   (operation
    :initarg :operation
    :initform nil
    :reader shell-result-operation)
   (code
    :initarg :code
    :initform nil
    :reader shell-result-code)
   (value
    :initarg :value
    :initform nil
    :reader shell-result-value)
   (message
    :initarg :message
    :initform nil
    :reader shell-result-message)))

(defvar *current-session* nil)

(defun trace-event (event &rest fields)
  (when *current-session*
    (push (list* :event event fields)
          (shell-session-trace *current-session*))))

(defun finish-result (result)
  (unless *current-session*
    (error "No active StarIntel expert-shell session"))
  (setf (shell-session-last-result *current-session*) result)
  (trace-event :result
               :success-p (shell-result-success-p result)
               :operation (shell-result-operation result)
               :code (shell-result-code result))
  result)

(defun make-result (&key success-p operation code value message)
  (make-instance 'shell-result
                 :success-p success-p
                 :operation operation
                 :code code
                 :value value
                 :message message))
