(in-package :star.actors)

(defparameter +actor-registry-resource-kinds+ '("actor" "service"))
(defparameter +actor-runtime-statuses+
  '("online" "declared-offline" "degraded" "unavailable"))
(defparameter +actor-contract-keys+ '("targets" "documents" "messages"))
(defparameter +actor-manifest-max-values+ 128)
(defparameter +actor-manifest-max-string-length+ 512)

(define-condition invalid-actor-manifest (error)
  ((reason :initarg :reason :reader invalid-actor-manifest-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid actor manifest: ~a"
             (invalid-actor-manifest-reason condition))))
  (:documentation "An emitted actor/service manifest failed closed validation."))

(define-condition actor-registry-conflict (error)
  ((resource-uri :initarg :resource-uri
                 :reader actor-registry-conflict-resource-uri)
   (reason :initarg :reason :reader actor-registry-conflict-reason))
  (:report
   (lambda (condition stream)
     (format stream "Actor registry conflict for ~a: ~a"
             (actor-registry-conflict-resource-uri condition)
             (actor-registry-conflict-reason condition))))
  (:documentation "Two emitted manifests claim the same canonical STAR resource URI."))

(setf (documentation 'invalid-actor-manifest-reason 'function)
      "The stable validation reason for an invalid emitted manifest."
      (documentation 'actor-registry-conflict-resource-uri 'function)
      "The canonical STAR URI claimed by conflicting manifests."
      (documentation 'actor-registry-conflict-reason 'function)
      "The stable reason why registry construction rejected a duplicate claim.")

(defstruct (actor-registry-entry
            (:constructor %make-actor-registry-entry))
  resource-uri resource-kind semantic-name semantic-version semantic-digest
  accepts produces capabilities source-package operator-visible-p)

(defstruct (actor-runtime-observation
            (:constructor %make-actor-runtime-observation))
  status ready-p observed-at)

(defvar *actor-registry* (make-hash-table :test #'equal))
(defvar *actor-runtime-observations* (make-hash-table :test #'equal))
(defvar *actor-runtime-bindings* (make-hash-table :test #'equal))
(defvar *actor-registry-lock* (bt:make-lock "starintel-actor-registry"))

(defun invalid-actor-manifest (control &rest arguments)
  "Signal an =invalid-actor-manifest= with a formatted bounded reason."
  (error 'invalid-actor-manifest
         :reason (apply #'format nil control arguments)))

(defun manifest-object-p (value)
  (and (listp value) (eq (first value) :obj)))

(defun required-manifest-string (object key context)
  (let ((value (and (manifest-object-p object)
                    (jsown:val-safe object key))))
    (unless (and (stringp value)
                 (plusp (length value))
                 (<= (length value) +actor-manifest-max-string-length+))
      (invalid-actor-manifest "~a.~a must be a bounded non-empty string"
                              context key))
    value))

(defun validate-object-keys (object allowed required context)
  (unless (manifest-object-p object)
    (invalid-actor-manifest "~a must be an object" context))
  (dolist (key (jsown:keywords object))
    (unless (member key allowed :test #'string=)
      (invalid-actor-manifest "~a contains unknown field ~a" context key)))
  (dolist (key required)
    (unless (jsown:keyp object key)
      (invalid-actor-manifest "~a is missing required field ~a" context key)))
  object)

(defun validate-string-list (values context)
  (unless (and (listp values)
               (<= (length values) +actor-manifest-max-values+))
    (invalid-actor-manifest "~a must be a bounded array" context))
  (dolist (value values)
    (unless (and (stringp value)
                 (plusp (length value))
                 (<= (length value) +actor-manifest-max-string-length+))
      (invalid-actor-manifest "~a entries must be bounded non-empty strings"
                              context)))
  (sort (remove-duplicates (copy-list values) :test #'string=) #'string<))

(defun validate-contract (contract context)
  (validate-object-keys contract +actor-contract-keys+
                        +actor-contract-keys+ context)
  (jsown:new-js
    ("targets"
     (validate-string-list (jsown:val contract "targets")
                           (format nil "~a.targets" context)))
    ("documents"
     (validate-string-list (jsown:val contract "documents")
                           (format nil "~a.documents" context)))
    ("messages"
     (validate-string-list (jsown:val contract "messages")
                           (format nil "~a.messages" context)))))

(defun canonical-star-resource-uri-p (resource-uri resource-kind)
  (handler-case
      (multiple-value-bind (scheme userinfo host port path query fragment)
          (quri:parse-uri resource-uri)
        (let ((prefix (format nil "/~a/" resource-kind)))
          (and (string= scheme "star")
               (or (null userinfo) (string= userinfo ""))
               (stringp host)
               (plusp (length host))
               (null port)
               (stringp path)
               (> (length path) (length prefix))
               (string= prefix path :end2 (length prefix))
               (not (search "//" path))
               (not (search "/./" path))
               (not (search "/../" path))
               (or (null query) (string= query ""))
               (or (null fragment) (string= fragment ""))
               (string= resource-uri
                        (format nil "star://~a~a" host path)))))
    (error () nil)))

(defun actor-registry-entry-from-manifest (manifest)
  (validate-object-keys
   manifest
   '("resourceUri" "resourceKind" "semantic" "accepts" "produces"
     "capabilities" "operatorVisible" "provenance")
   '("resourceUri" "resourceKind" "semantic" "accepts" "produces"
     "capabilities" "operatorVisible" "provenance")
   "manifest")
  (let* ((resource-uri (required-manifest-string manifest "resourceUri" "manifest"))
         (resource-kind (required-manifest-string manifest "resourceKind" "manifest"))
         (semantic (jsown:val manifest "semantic"))
         (provenance (jsown:val manifest "provenance"))
         (visible (jsown:val manifest "operatorVisible")))
    (unless (member resource-kind +actor-registry-resource-kinds+ :test #'string=)
      (invalid-actor-manifest "unknown resource kind ~a" resource-kind))
    (unless (canonical-star-resource-uri-p resource-uri resource-kind)
      (invalid-actor-manifest "resourceUri is not a canonical STAR ~a URI"
                              resource-kind))
    (validate-object-keys semantic '("name" "version" "digest")
                          '("name" "version" "digest") "manifest.semantic")
    (validate-object-keys provenance '("sourcePackage") '("sourcePackage")
                          "manifest.provenance")
    (unless (member visible '(:true :false))
      (invalid-actor-manifest "manifest.operatorVisible must be explicit"))
    (%make-actor-registry-entry
     :resource-uri resource-uri
     :resource-kind resource-kind
     :semantic-name (required-manifest-string semantic "name" "manifest.semantic")
     :semantic-version (required-manifest-string semantic "version" "manifest.semantic")
     :semantic-digest (required-manifest-string semantic "digest" "manifest.semantic")
     :accepts (validate-contract (jsown:val manifest "accepts") "manifest.accepts")
     :produces (validate-contract (jsown:val manifest "produces") "manifest.produces")
     :capabilities
     (validate-string-list (jsown:val manifest "capabilities")
                           "manifest.capabilities")
     :source-package
     (required-manifest-string provenance "sourcePackage" "manifest.provenance")
     :operator-visible-p (eq visible :true))))

(defun add-actor-registry-entry (registry entry)
  (let* ((resource-uri (actor-registry-entry-resource-uri entry))
         (existing (gethash resource-uri registry)))
    (when existing
      (error 'actor-registry-conflict
             :resource-uri resource-uri
             :reason
             (if (string= (actor-registry-entry-semantic-digest existing)
                          (actor-registry-entry-semantic-digest entry))
                 "duplicate canonical resource claim"
                 "conflicting semantic digest")))
    (setf (gethash resource-uri registry) entry)
    registry))

(defun build-actor-registry (manifests)
  "Build a validated registry without starting actors or invoking callbacks."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (manifest manifests registry)
      (add-actor-registry-entry
       registry (actor-registry-entry-from-manifest manifest)))))

(defun install-actor-manifests (manifests)
  "Atomically replace the registry and discard stale runtime state."
  (let ((registry (build-actor-registry manifests)))
    (bt:with-lock-held (*actor-registry-lock*)
      (setf *actor-registry* registry
            *actor-runtime-observations* (make-hash-table :test #'equal)
            *actor-runtime-bindings* (make-hash-table :test #'equal)))
    registry))

(defun register-actor-manifest (manifest)
  "Register one emitted manifest at the trusted server/add-on boundary."
  (let ((entry (actor-registry-entry-from-manifest manifest)))
    (bt:with-lock-held (*actor-registry-lock*)
      (add-actor-registry-entry *actor-registry* entry))
    entry))

(defun record-actor-runtime-status
    (resource-uri status &key ready-p observed-at
                           (observations *actor-runtime-observations*))
  "Record a bounded server-owned runtime observation without creating a manifest entry."
  (unless (member status +actor-runtime-statuses+ :test #'string=)
    (error "Unknown actor runtime status ~s" status))
  (flet ((record ()
           (setf (gethash resource-uri observations)
                 (%make-actor-runtime-observation
                  :status status
                  :ready-p (not (null ready-p))
                  :observed-at observed-at))))
    (if (eq observations *actor-runtime-observations*)
        (bt:with-lock-held (*actor-registry-lock*) (record))
        (record))))

(defun bind-local-actor-runtime (resource-uri actor)
  "Bind a registered semantic resource to a private Sento runtime reference."
  (bt:with-lock-held (*actor-registry-lock*)
    (unless (gethash resource-uri *actor-registry*)
      (error "Cannot bind runtime for unknown actor resource ~a" resource-uri))
    (setf (gethash resource-uri *actor-runtime-bindings*) actor))
  actor)

(defun local-runtime-observation (actor)
  (handler-case
      (if (and actor (act-cell:running-p actor))
          (%make-actor-runtime-observation :status "online" :ready-p t)
          (%make-actor-runtime-observation :status "unavailable" :ready-p nil))
    (error ()
      (%make-actor-runtime-observation :status "unavailable" :ready-p nil))))

(defun effective-runtime-observation (resource-uri observations)
  (or (gethash resource-uri observations)
      (let ((actor (gethash resource-uri *actor-runtime-bindings*)))
        (and actor (local-runtime-observation actor)))
      (%make-actor-runtime-observation :status "unavailable" :ready-p nil)))

(defun copy-actor-contract (contract)
  (jsown:new-js
    ("targets" (copy-list (jsown:val contract "targets")))
    ("documents" (copy-list (jsown:val contract "documents")))
    ("messages" (copy-list (jsown:val contract "messages")))))

(defun actor-registry-entry-public-object (entry observation)
  (let ((object
          (jsown:new-js
            ("resourceUri" (actor-registry-entry-resource-uri entry))
            ("resourceKind" (actor-registry-entry-resource-kind entry))
            ("semantic"
             (jsown:new-js
               ("name" (actor-registry-entry-semantic-name entry))
               ("version" (actor-registry-entry-semantic-version entry))
               ("digest" (actor-registry-entry-semantic-digest entry))))
            ("accepts" (copy-actor-contract (actor-registry-entry-accepts entry)))
            ("produces" (copy-actor-contract (actor-registry-entry-produces entry)))
            ("capabilities" (copy-list (actor-registry-entry-capabilities entry)))
            ("operatorVisible" :true)
            ("provenance"
             (jsown:new-js
               ("sourcePackage" (actor-registry-entry-source-package entry))))
            ("status" (actor-runtime-observation-status observation))
            ("ready"
             (if (actor-runtime-observation-ready-p observation) :true :false)))))
    (when (actor-runtime-observation-observed-at observation)
      (setf (jsown:val object "observedAt")
            (actor-runtime-observation-observed-at observation)))
    object))

(defun actor-registry-public-entries
    (&optional (registry *actor-registry*)
               (observations *actor-runtime-observations*))
  "Return the deterministic, scrubbed operator-visible catalog projection."
  (sort
   (loop for entry being the hash-values of registry
         when (actor-registry-entry-operator-visible-p entry)
           collect
           (actor-registry-entry-public-object
            entry
            (effective-runtime-observation
             (actor-registry-entry-resource-uri entry) observations)))
   #'string< :key (lambda (object) (jsown:val object "resourceUri"))))

(defun actor-registry-document ()
  "Return the safe actor/service catalog envelope consumed by =GET /v1/actors=."
  (bt:with-lock-held (*actor-registry-lock*)
    (let ((actors (actor-registry-public-entries)))
      (jsown:new-js
        ("status" "ok")
        ("data"
         (jsown:new-js
           ("schema" "starintel-actor-registry-v1")
           ("actors" actors)
           ("count" (length actors))))))))
