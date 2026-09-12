(uiop:define-package :star.actor-manifest-options
  (:use :cl)
  (:export
   #:invalid-actor-manifest
   #:invalid-actor-manifest-reason
   #:missing-required-target-option
   #:missing-required-target-option-actor
   #:missing-required-target-option-key
   #:actor-manifest-actor
   #:actor-manifest-target-options
   #:target-option-value
   #:merge-target-options-with-manifest))

(in-package :star.actor-manifest-options)

(define-condition invalid-actor-manifest (error)
  ((reason :initarg :reason :reader invalid-actor-manifest-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid actor manifest: ~a"
             (invalid-actor-manifest-reason condition)))))

(define-condition missing-required-target-option (error)
  ((actor :initarg :actor :reader missing-required-target-option-actor)
   (key :initarg :key :reader missing-required-target-option-key))
  (:report
   (lambda (condition stream)
     (format stream "Actor ~a requires target option ~a"
             (missing-required-target-option-actor condition)
             (missing-required-target-option-key condition)))))

(defun json-object-p (value)
  (and (consp value) (eq (first value) :obj)))

(defun array-values (value label)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t
     (error 'invalid-actor-manifest
            :reason (format nil "~a must be a JSON array" label)))))

(defun json-true-p (value)
  (or (eq value t) (eq value :true)))

(defun actor-manifest-actor (document)
  "Return the actor identity advertised by an actor-manifest document."
  (unless (string= "actor-manifest"
                   (or (star.documents:document-dtype document) ""))
    (error 'invalid-actor-manifest
           :reason "document dtype must be actor-manifest"))
  (let ((actor (star.documents:document-value document "actor" nil)))
    (unless (and (stringp actor) (plusp (length actor)))
      (error 'invalid-actor-manifest
             :reason "data.actor must be a non-empty string"))
    actor))

(defun actor-manifest-target-options (document)
  "Return the target option declarations from an actor-manifest document."
  (actor-manifest-actor document)
  (array-values
   (star.documents:document-value document "target_options" #())
   "data.target_options"))

(defun option-value (option key sentinel)
  (if (json-object-p option)
      (let ((descriptor-key
              (star.documents:object-value option "key" sentinel)))
        (cond
          ((and (not (eq descriptor-key sentinel))
                (stringp descriptor-key)
                (string= descriptor-key key)
                (star.documents:object-has-key-p option "value"))
           (values (star.documents:object-value option "value") t))
          ((star.documents:object-has-key-p option key)
           (values (star.documents:object-value option key) t))
          (t (values sentinel nil))))
      (values sentinel nil)))

(defun target-option-value (options key &optional default)
  "Read KEY from actor-defined target OPTIONS.

Both supported option forms are accepted: {\"key\": KEY, \"value\": VALUE}
and the compact {KEY: VALUE} object used by existing actors."
  (let ((sentinel (gensym "MISSING-OPTION-")))
    (dolist (option (array-values options "target options") default)
      (multiple-value-bind (value found-p)
          (option-value option key sentinel)
        (when found-p
          (return value))))))

(defun target-option-present-p (options key)
  (let ((sentinel (gensym "MISSING-OPTION-")))
    (not (eq sentinel (target-option-value options key sentinel)))))

(defun single-option (key value)
  (let ((object (jsown:empty-object)))
    (setf (jsown:val object key) value)
    object))

(defun manifest-option-default (spec sentinel)
  (cond
    ((star.documents:object-has-key-p spec "default")
     (values (star.documents:object-value spec "default") t))
    ((star.documents:object-has-key-p spec "value")
     (values (star.documents:object-value spec "value") t))
    (t (values sentinel nil))))

(defun merge-target-options-with-manifest (manifest options)
  "Merge actor-manifest defaults into OPTIONS and enforce required declarations.

The server deliberately does not interpret actor-specific values. It only
provides one generic contract so external RabbitMQ actors and local actors use
the same target option semantics. Explicit target options always win."
  (let* ((actor (actor-manifest-actor manifest))
         (merged (copy-list (array-values options "target options")))
         (sentinel (gensym "NO-DEFAULT-")))
    (dolist (spec (actor-manifest-target-options manifest))
      (unless (json-object-p spec)
        (error 'invalid-actor-manifest
               :reason "each target_options entry must be a JSON object"))
      (let ((key (star.documents:object-value spec "key" nil)))
        (when key
          (unless (and (stringp key) (plusp (length key)))
            (error 'invalid-actor-manifest
                   :reason "target option key must be a non-empty string"))
          (unless (target-option-present-p merged key)
            (multiple-value-bind (default has-default-p)
                (manifest-option-default spec sentinel)
              (cond
                (has-default-p
                 (setf merged
                       (append merged (list (single-option key default)))))
                ((json-true-p
                  (star.documents:object-value spec "required" nil))
                 (error 'missing-required-target-option
                        :actor actor
                        :key key))))))))
    (coerce merged 'vector)))
