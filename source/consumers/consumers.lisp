(in-package :star.consumers)


(defclass consumer ()
  ((name :initarg :name :accessor consumer-name :initform "")
   (predicate :initarg :test-fn :initform (lambda (self message)
                                            (not (null message)))
              :accessor consumer-filter)
   (fn :initarg :fn :accessor consumer-fn :initform (lambda (self message)
                                                      (print message)) :type function)
   (take :initarg :take :accessor consumer-take :type integer :initform 1)
   (worker-channel :initarg :consumer-channel :accessor consumer-channel :type lparallel:channel :initform (make-channel))
   (state :initarg :state :accessor consumer-state)
   (consumer-stream :initarg :stream :reader consumer-stream)
   (lock :initform (bt:make-lock) :accessor consumer-lock))
  (:documentation "Consumers process a stream of data."))

(defgeneric consumer-state (consumer)
  (:documentation "Return the consumer's state"))

(defgeneric consumer-update-state (consumer new-state)
  (:documentation "Update the consumer state"))

(defgeneric consumer-cleanup (consumer)
  (:documentation "Close any streams and de-init the consumer"))

(defmacro with-consumer-lock ((consumer) &body body)
  `(bt:with-lock-held ((consumer-lock ,consumer))
     ,@body))

(defmethod consumer-update ((consumer consumer) new-state)
  (setf (consumer-state consumer) new-state))

(defmethod consumer-read ((consumer consumer))
  (with-consumer-lock (consumer)
    (stream-read (consumer-stream consumer))))

(defmethod consume  ((consumer consumer) data)
  (submit-task (consumer-channel consumer) (lambda ()
                                             (if (funcall (consumer-filter consumer) data)
                                                 (funcall (consumer-fn consumer) consumer data)))))




(defmethod start-consumer ((consumer consumer))
  (bt:make-thread (lambda ()
                    (loop
                      for data = (consumer-read consumer)
                      do (consume consumer data)
                      do (receive-result (consumer-channel consumer))))
                  :name (consumer-name consumer)))






(defun make-consumer (&rest args)
  (apply #'make-instance 'consumer args))
