(in-package :star.playbook)

(define-condition playbook-error (error)
  ((code :initarg :code :reader playbook-error-code)
   (message :initarg :message :reader playbook-error-message))
  (:report
   (lambda (condition stream)
     (format stream "Playbook ~a: ~a"
             (playbook-error-code condition)
             (playbook-error-message condition)))))

(defun fail-playbook (code control &rest arguments)
  (error 'playbook-error
         :code code
         :message (apply #'format nil control arguments)))

(defun non-empty-string (value field)
  (unless (and (stringp value) (plusp (length value)))
    (fail-playbook :invalid-field "~a must be a non-empty string" field))
  value)

(defun canonical-id (value field)
  (string-downcase (non-empty-string value field)))

(defun canonical-kind (value field)
  (etypecase value
    (string (string-downcase value))
    (symbol (string-downcase (symbol-name value)))
    (null (fail-playbook :invalid-field "~a is required" field))))

(defun digest-string (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defparameter +allowed-action-types+
  '(:assert-fact
    :actor-run
    :actor-message
    :target-create
    :document-query
    :document-save
    :timer
    :subplaybook
    :relation-emit)
  "Closed action vocabulary accepted from visual playbook graphs.")

(defstruct (playbook-flow
             (:constructor %make-playbook-flow
                 (&key id match-kind actions (salience 0))))
  id
  match-kind
  actions
  (salience 0 :type integer))

(defun action-type (action)
  (and (listp action) (getf action :type)))

(defun normalize-action (action)
  (unless (and (listp action) (evenp (length action)))
    (fail-playbook :invalid-action "Action must be a property list: ~s" action))
  (let ((type (action-type action)))
    (unless (member type +allowed-action-types+ :test #'eq)
      (fail-playbook :unsupported-action "Unsupported action type ~s" type))
    (case type
      (:assert-fact
       (canonical-kind (getf action :kind) :action-kind))
      ((:actor-run :actor-message :target-create)
       (non-empty-string (getf action :actor) :actor))
      (:subplaybook
       (non-empty-string (getf action :playbook-id) :playbook-id)))
    (copy-tree action)))

(defun make-playbook-flow (id &key match-kind actions (salience 0))
  (unless (<= -250 salience 250)
    (fail-playbook :invalid-salience "LISA salience must be between -250 and 250"))
  (%make-playbook-flow
   :id (canonical-id id :flow-id)
   :match-kind (canonical-kind match-kind :match-kind)
   :actions (mapcar #'normalize-action (or actions '()))
   :salience salience))

(defstruct (playbook-definition
             (:constructor %make-playbook-definition
                 (&key id version tenant-id flows digest system-name)))
  id
  version
  tenant-id
  flows
  digest
  system-name)

(defun flow-canonical-form (flow)
  (list :id (playbook-flow-id flow)
        :match-kind (playbook-flow-match-kind flow)
        :salience (playbook-flow-salience flow)
        :actions (copy-tree (playbook-flow-actions flow))))

(defun definition-canonical-form (id version tenant-id flows)
  (list :id id
        :version version
        :tenant-id tenant-id
        :flows (mapcar #'flow-canonical-form flows)))

(defun canonical-print (form)
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-case* :downcase)
          (*print-circle* nil))
      (prin1-to-string form))))

(defun slugify (text)
  (let ((result
          (with-output-to-string (out)
            (loop with dash-p = nil
                  for char across (string-downcase text)
                  do (cond
                       ((alphanumericp char)
                        (write-char char out)
                        (setf dash-p nil))
                       ((not dash-p)
                        (write-char #\- out)
                        (setf dash-p t)))))))
    (string-trim "-" result)))

(defun default-system-name (tenant-id id version)
  (format nil "star-playbook-~a-~a-~a"
          (slugify tenant-id)
          (slugify id)
          (slugify version)))

(defun make-playbook-definition (&key id version tenant-id flows system-name)
  (let* ((id (canonical-id id :playbook-id))
         (version (canonical-id version :version))
         (tenant-id (canonical-id tenant-id :tenant-id))
         (flows (sort (copy-list (or flows '())) #'string< :key #'playbook-flow-id)))
    (unless flows
      (fail-playbook :empty-playbook "Playbook must contain at least one flow"))
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (flow flows)
        (unless (typep flow 'playbook-flow)
          (fail-playbook :invalid-flow "Expected PLAYBOOK-FLOW, got ~s" flow))
        (when (gethash (playbook-flow-id flow) seen)
          (fail-playbook :duplicate-flow "Duplicate flow id ~a" (playbook-flow-id flow)))
        (setf (gethash (playbook-flow-id flow) seen) t)))
    (let* ((canonical (definition-canonical-form id version tenant-id flows))
           (digest (digest-string (canonical-print canonical))))
      (%make-playbook-definition
       :id id
       :version version
       :tenant-id tenant-id
       :flows flows
       :digest digest
       :system-name (or system-name (default-system-name tenant-id id version))))))

(defmacro flow (id &key when then (salience 0))
  `(make-playbook-flow ,id
                       :match-kind ,when
                       :actions ',then
                       :salience ,salience))

(defmacro define-playbook (variable (&key id version tenant-id system-name) &body flows)
  `(defparameter ,variable
     (make-playbook-definition
      :id ,id
      :version ,version
      :tenant-id ,tenant-id
      :system-name ,system-name
      :flows (list ,@flows))))

(defvar *playbook-registry* (make-hash-table :test #'equal))
(defvar *playbook-registry-lock* (bt:make-lock "star-playbook-registry"))

(defun playbook-key (tenant-id id version)
  (list (canonical-id tenant-id :tenant-id)
        (canonical-id id :playbook-id)
        (canonical-id version :version)))

(defun register-playbook (definition)
  (unless (typep definition 'playbook-definition)
    (fail-playbook :invalid-definition "Expected PLAYBOOK-DEFINITION"))
  (let ((key (playbook-key (playbook-definition-tenant-id definition)
                           (playbook-definition-id definition)
                           (playbook-definition-version definition))))
    (bt:with-lock-held (*playbook-registry-lock*)
      (setf (gethash key *playbook-registry*) definition))
    definition))

(defun unregister-playbook (tenant-id id version)
  (bt:with-lock-held (*playbook-registry-lock*)
    (remhash (playbook-key tenant-id id version) *playbook-registry*)))

(defun find-playbook (tenant-id id version)
  (bt:with-lock-held (*playbook-registry-lock*)
    (gethash (playbook-key tenant-id id version) *playbook-registry*)))

(defun list-playbooks (tenant-id)
  (let ((tenant-id (canonical-id tenant-id :tenant-id)))
    (bt:with-lock-held (*playbook-registry-lock*)
      (sort
       (loop for definition being the hash-values of *playbook-registry*
             when (string= tenant-id (playbook-definition-tenant-id definition))
               collect definition)
       #'string<
       :key (lambda (definition)
              (format nil "~a/~a"
                      (playbook-definition-id definition)
                      (playbook-definition-version definition)))))))

(defun clear-playbook-registry ()
  (bt:with-lock-held (*playbook-registry-lock*)
    (clrhash *playbook-registry*))
  t)

(defun graph-node-id (node)
  (canonical-id (getf node :id) :node-id))

(defun graph-node-type (node)
  (let ((type (getf node :type)))
    (unless (member type '(:input :rule :action) :test #'eq)
      (fail-playbook :unsupported-node "Unsupported graph node type ~s" type))
    type))

(defun edge-source (edge)
  (canonical-id (getf edge :source) :edge-source))

(defun edge-target (edge)
  (canonical-id (getf edge :target) :edge-target))

(defun internal-node-kind (node-id)
  (format nil "node:~a" node-id))

(defun validate-playbook-graph (graph)
  "Validate GRAPH as inert playbook IR. Returns GRAPH on success.

The normal tenant path accepts this data model, never arbitrary Lisp source."
  (unless (and (listp graph) (evenp (length graph)))
    (fail-playbook :invalid-graph "Graph must be a property list"))
  (canonical-id (getf graph :playbook-id) :playbook-id)
  (canonical-id (getf graph :version) :version)
  (canonical-id (getf graph :tenant-id) :tenant-id)
  (let* ((nodes (or (getf graph :nodes) '()))
         (edges (or (getf graph :edges) '()))
         (ids (make-hash-table :test #'equal)))
    (unless nodes
      (fail-playbook :empty-playbook "Graph has no nodes"))
    (dolist (node nodes)
      (unless (and (listp node) (evenp (length node)))
        (fail-playbook :invalid-node "Node must be a property list: ~s" node))
      (let ((id (graph-node-id node))
            (type (graph-node-type node)))
        (when (gethash id ids)
          (fail-playbook :duplicate-node "Duplicate node id ~a" id))
        (setf (gethash id ids) type)
        (ecase type
          (:input (canonical-kind (getf node :kind) :input-kind))
          (:rule (canonical-kind (getf node :match-kind) :match-kind))
          (:action nil))
        (dolist (action (or (getf node :actions) '()))
          (normalize-action action))))
    (dolist (edge edges)
      (unless (and (listp edge) (evenp (length edge)))
        (fail-playbook :invalid-edge "Edge must be a property list: ~s" edge))
      (let ((source (edge-source edge))
            (target (edge-target edge)))
        (unless (gethash source ids)
          (fail-playbook :unknown-node "Unknown edge source ~a" source))
        (unless (gethash target ids)
          (fail-playbook :unknown-node "Unknown edge target ~a" target))))
    graph))

(defun outgoing-targets (node-id edges)
  (sort
   (loop for edge in edges
         when (string= node-id (edge-source edge))
           collect (edge-target edge))
   #'string<))

(defun graph-node-flow (node edges)
  (let* ((id (graph-node-id node))
         (type (graph-node-type node))
         (match-kind
           (ecase type
             (:input (canonical-kind (getf node :kind) :input-kind))
             (:rule (canonical-kind (getf node :match-kind) :match-kind))
             (:action (internal-node-kind id))))
         (actions
           (append
            (mapcar #'normalize-action (or (getf node :actions) '()))
            (mapcar
             (lambda (target)
               (list :type :assert-fact
                     :kind (internal-node-kind target)
                     :payload nil))
             (outgoing-targets id edges)))))
    (make-playbook-flow
     id
     :match-kind match-kind
     :actions actions
     :salience (or (getf node :salience) 0))))

(defun graph-definition (graph)
  (validate-playbook-graph graph)
  (let ((edges (or (getf graph :edges) '())))
    (make-playbook-definition
     :id (getf graph :playbook-id)
     :version (getf graph :version)
     :tenant-id (getf graph :tenant-id)
     :flows (mapcar
             (lambda (node) (graph-node-flow node edges))
             (sort (copy-list (getf graph :nodes)) #'string< :key #'graph-node-id)))))

(defun package-name-for-definition (definition)
  (string-upcase
   (format nil "STAR.PLAYBOOKS.~a"
           (substitute #\. #\- (playbook-definition-system-name definition)))))

(defun emit-flow-source (stream flow)
  (format stream "    (star.playbook:flow ~s :when ~s :salience ~d :then ~s)~%"
          (playbook-flow-id flow)
          (playbook-flow-match-kind flow)
          (playbook-flow-salience flow)
          (playbook-flow-actions flow)))

(defun module-lisp-source (definition)
  (let ((package-name (package-name-for-definition definition))
        (system-name (playbook-definition-system-name definition)))
    (with-output-to-string (out)
      (format out "(defpackage #:~a (:use #:cl))~%" package-name)
      (format out "(in-package #:~a)~%~%" package-name)
      (format out "(star.playbook:define-playbook *playbook*~%")
      (format out "  (:id ~s :version ~s :tenant-id ~s :system-name ~s)~%"
              (playbook-definition-id definition)
              (playbook-definition-version definition)
              (playbook-definition-tenant-id definition)
              system-name)
      (dolist (flow (playbook-definition-flows definition))
        (emit-flow-source out flow))
      (format out ")~%~%")
      (format out "(defun start () (star.playbook:register-playbook *playbook*))~%")
      (format out "(defun stop ()~%  (star.playbook:unregister-playbook ~s ~s ~s))~%~%"
              (playbook-definition-tenant-id definition)
              (playbook-definition-id definition)
              (playbook-definition-version definition))
      (format out "(star:register-addon ~s :system ~s :start #'start :stop #'stop)~%"
              system-name system-name))))

(defun module-asdf-source (definition)
  (let ((system-name (playbook-definition-system-name definition)))
    (with-output-to-string (out)
      (format out "(asdf:defsystem #:~a~%" system-name)
      (format out "  :version ~s~%" (playbook-definition-version definition))
      (format out "  :depends-on (#:starintel-gserver)~%")
      (format out "  :serial t~%")
      (format out "  :components ((:file \"playbook\")))~%"))))

(defun compile-playbook-module (graph)
  "Compile inert GRAPH into canonical, human-readable Common Lisp module text.

Returns a plist containing the normalized definition, ASDF system name, Lisp
source, ASDF source, and a digest over both emitted files."
  (let* ((definition (graph-definition graph))
         (lisp-source (module-lisp-source definition))
         (asdf-source (module-asdf-source definition))
         (digest (digest-string (concatenate 'string asdf-source "\n" lisp-source))))
    (list :definition definition
          :system-name (playbook-definition-system-name definition)
          :lisp-source lisp-source
          :asdf-source asdf-source
          :digest digest)))
