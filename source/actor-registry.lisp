(in-package :star.actor-registry)

(defconstant +actor-registry-query-limit+ 256
  "Maximum number of actor discovery rows returned by one pure query.")

(defconstant +actor-registry-manifest-limit+ 1024)
(defconstant +actor-registry-actors-per-manifest-limit+ 1024)
(defconstant +actor-registry-contract-list-limit+ 256)
(defconstant +legacy-identity-status+ "legacy_star_service_uri_v1")

(define-condition actor-registry-error (error)
  ((code
    :initarg :code
    :reader actor-registry-error-code)
   (path
    :initarg :path
    :reader actor-registry-error-path)
   (detail
    :initarg :detail
    :reader actor-registry-error-detail))
  (:report
   (lambda (condition stream)
     (format stream "Actor registry rejected ~a at ~a: ~a"
             (actor-registry-error-code condition)
             (actor-registry-error-path condition)
             (actor-registry-error-detail condition))))
  (:documentation "A stable fail-closed portable-manifest rejection."))

(setf (documentation 'actor-registry-error-code 'function)
      "Stable machine-readable actor registry rejection code.")
(setf (documentation 'actor-registry-error-path 'function)
      "Manifest or query path at which registry validation failed.")

(defstruct (actor-registry-entry
            (:constructor make-actor-registry-entry)
            (:copier nil))
  service-uri
  name
  runtime
  accepts
  produces
  capabilities
  library-name
  library-version
  library-digest)

(defstruct (actor-registry
            (:constructor %make-actor-registry)
            (:copier nil))
  "Immutable public handle over URI-sorted actor registry entries."
  (entries #() :read-only t)
  (index (make-hash-table :test #'equal) :read-only t))

(defun reject-manifest (code path control &rest arguments)
  (error 'actor-registry-error
         :code code
         :path path
         :detail (apply #'format nil control arguments)))

(defun jsown-object-p (value)
  (and (consp value) (eq :obj (first value))))

(defun string-alist-p (value)
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry) (stringp (car entry))))
              value)))

(defun manifest-object-p (value)
  (or (jsown-object-p value)
      (string-alist-p value)
      (hash-table-p value)))

(defun object-entries (object)
  (cond
    ((jsown-object-p object) (rest object))
    ((string-alist-p object) object)
    ((hash-table-p object)
     (loop for key being the hash-keys of object using (hash-value value)
           collect (cons key value)))
    (t nil)))

(defun object-field (object key)
  (let ((entry
          (find key (object-entries object)
                :key #'car
                :test #'string=)))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun ensure-exact-object-keys (object allowed path)
  (dolist (entry (object-entries object))
    (unless (and (stringp (car entry))
                 (member (car entry) allowed :test #'string=))
      (reject-manifest "unknown_field" path
                       "Unknown field ~s." (car entry))))
  object)

(defun portable-json-data-p (value)
  (typecase value
    (null t)
    (string t)
    (number t)
    (keyword (member value '(:obj :true :false :null) :test #'eq))
    (vector (every #'portable-json-data-p value))
    (hash-table
     (loop for key being the hash-keys of value using (hash-value item)
           always (and (stringp key) (portable-json-data-p item))))
    (cons
     (and (portable-json-data-p (car value))
          (portable-json-data-p (cdr value))))
    (t nil)))

(defun ensure-portable-json-data (value)
  (unless (portable-json-data-p value)
    (reject-manifest "non_data_manifest" "manifests"
                     "Manifest contains a non-JSON runtime value."))
  value)

(defun require-object (value path)
  (unless (manifest-object-p value)
    (reject-manifest "invalid_object" path "Expected an object."))
  value)

(defun require-field (object key path)
  (multiple-value-bind (value present-p)
      (object-field object key)
    (unless present-p
      (reject-manifest "missing_field" path "Missing field ~a." key))
    value))

(defun require-non-empty-string (value path)
  (unless (and (stringp value) (plusp (length value)) (<= (length value) 512))
    (reject-manifest "invalid_string" path
                     "Expected a bounded non-empty string."))
  value)

(defun array-values (value path &key (limit +actor-registry-contract-list-limit+))
  (let ((items
          (cond
            ((vectorp value) (coerce value 'list))
            ((and (listp value) (not (manifest-object-p value))) value)
            (t
             (reject-manifest "invalid_array" path "Expected an array.")))))
    (when (> (length items) limit)
      (reject-manifest "array_limit_exceeded" path
                       "Array exceeds limit ~d." limit))
    items))

(defun bounded-string-array (value path)
  (let ((items (array-values value path)))
    (dolist (item items)
      (require-non-empty-string item path))
    (remove-duplicates (mapcar #'copy-seq items) :test #'string=)))

(defun lowercase-hex-character-p (character)
  (or (char<= #\0 character #\9)
      (char<= #\a character #\f)))

(defun valid-sha256-p (value)
  (and (stringp value)
       (= (length value) 71)
       (string= "sha256:" value :end2 7)
       (every #'lowercase-hex-character-p (subseq value 7))))

(defun valid-star-token-character-p (character)
  (or (char<= #\a character #\z)
      (char<= #\0 character #\9)
      (find character "-_." :test #'char=)))

(defun valid-star-token-p (value)
  (and (stringp value)
       (plusp (length value))
       (every #'valid-star-token-character-p value)))

(defun split-colon-tokens (value)
  (loop with start = 0
        for separator = (position #\: value :start start)
        collect (subseq value start separator)
        while separator
        do (setf start (1+ separator))))

(defun validate-legacy-service-uri (service-uri actor-name path)
  (unless (and (stringp service-uri)
               (> (length service-uri) 7)
               (string= "star://" service-uri :end2 7))
    (reject-manifest "invalid_service_uri" path
                     "Expected a StarLang star:// service URI."))
  (let ((tokens (split-colon-tokens (subseq service-uri 7))))
    (unless (and (= (length tokens) 3)
                 (every #'valid-star-token-p tokens))
      (reject-manifest "invalid_service_uri" path
                       "Expected star://domain:address:actor-name."))
    (unless (string= actor-name (third tokens))
      (reject-manifest "service_uri_name_mismatch" path
                       "Actor name ~a does not match URI actor name ~a."
                       actor-name (third tokens))))
  service-uri)

(defun validate-library (manifest manifest-index)
  (let* ((path (format nil "manifests[~d].library" manifest-index))
         (library (require-object
                   (require-field manifest "library" path)
                   path))
         (name (require-non-empty-string
                (require-field library "name" path)
                (format nil "~a.name" path)))
         (version (require-non-empty-string
                   (require-field library "version" path)
                   (format nil "~a.version" path)))
         (digest (require-non-empty-string
                  (require-field library "digest" path)
                  (format nil "~a.digest" path))))
    (ensure-exact-object-keys library '("name" "version" "digest") path)
    (unless (valid-sha256-p digest)
      (reject-manifest "invalid_semantic_digest"
                       (format nil "~a.digest" path)
                       "Expected sha256: followed by 64 lowercase hex digits."))
    (values name version digest)))

(defun validate-wire-shape (manifest manifest-index)
  (let ((path (format nil "manifests[~d]" manifest-index)))
    (require-object manifest path)
    (ensure-exact-object-keys
     manifest
     '("wireVersion" "library" "imports" "types" "predicates"
       "messages" "actors")
     path)
    (unless (eql 1 (require-field manifest "wireVersion" path))
      (reject-manifest "unsupported_wire_version"
                       (format nil "~a.wireVersion" path)
                       "Only StarLang wire version 1 is supported."))
    (dolist (field '("imports" "types" "predicates" "messages"))
      (array-values (require-field manifest field path)
                    (format nil "~a.~a" path field)
                    :limit +actor-registry-manifest-limit+))))

(defun validate-actor (actor library-name library-version library-digest
                       manifest-index actor-index)
  (let* ((path (format nil "manifests[~d].actors[~d]"
                       manifest-index actor-index))
         (actor (require-object actor path))
         (name (require-non-empty-string
                (require-field actor "name" path)
                (format nil "~a.name" path)))
         (runtime (require-non-empty-string
                   (require-field actor "runtime" path)
                   (format nil "~a.runtime" path)))
         (service-uri
           (validate-legacy-service-uri
            (require-field actor "serviceUri" path)
            name
            (format nil "~a.serviceUri" path)))
         (accepts (bounded-string-array
                   (require-field actor "accepts" path)
                   (format nil "~a.accepts" path)))
         (produces (bounded-string-array
                    (require-field actor "produces" path)
                    (format nil "~a.produces" path)))
         (capabilities (bounded-string-array
                        (require-field actor "capabilities" path)
                        (format nil "~a.capabilities" path))))
    (ensure-exact-object-keys
     actor
     '("name" "runtime" "protocol" "endpoint" "accepts" "produces"
       "capabilities" "serviceUri" "input" "body" "metadata")
     path)
    (unless (member runtime '("native" "external") :test #'string=)
      (reject-manifest "unknown_runtime"
                       (format nil "~a.runtime" path)
                       "Runtime must be native or external."))
    (make-actor-registry-entry
     :service-uri (copy-seq service-uri)
     :name (copy-seq name)
     :runtime (copy-seq runtime)
     :accepts accepts
     :produces produces
     :capabilities capabilities
     :library-name (copy-seq library-name)
     :library-version (copy-seq library-version)
     :library-digest (copy-seq library-digest))))

(defun manifest-entries (manifest manifest-index)
  (validate-wire-shape manifest manifest-index)
  (multiple-value-bind (library-name library-version library-digest)
      (validate-library manifest manifest-index)
    (let* ((path (format nil "manifests[~d].actors" manifest-index))
           (actors
             (array-values
              (require-field manifest "actors" path)
              path
              :limit +actor-registry-actors-per-manifest-limit+)))
      (loop for actor in actors
            for actor-index from 0
            collect (validate-actor
                     actor library-name library-version library-digest
                     manifest-index actor-index)))))

(defun register-entry (entry index entries)
  (let* ((service-uri (actor-registry-entry-service-uri entry))
         (existing (gethash service-uri index)))
    (when existing
      (if (string= (actor-registry-entry-library-digest existing)
                   (actor-registry-entry-library-digest entry))
          (reject-manifest "duplicate_service_uri" service-uri
                           "Duplicate actor service URI.")
          (reject-manifest "semantic_digest_conflict" service-uri
                           "Actor service URI has conflicting semantic digests.")))
    (setf (gethash service-uri index) entry)
    (push entry entries)
    entries))

(defun build-actor-registry (manifests)
  "Validate StarLang MANIFESTS and build a deterministic, side-effect-free registry.

The current StarLang colon-form service URI is preserved as compatibility
identity.  This function does not claim or synthesize the future RFC3986
registry resource identity."
  (let ((manifest-list
          (array-values manifests "manifests"
                        :limit +actor-registry-manifest-limit+))
        (index (make-hash-table :test #'equal))
        (entries nil))
    (ensure-portable-json-data manifests)
    (loop for manifest in manifest-list
          for manifest-index from 0
          do (dolist (entry (manifest-entries manifest manifest-index))
               (setf entries (register-entry entry index entries))))
    (let ((sorted
            (sort (copy-list entries) #'string<
                  :key #'actor-registry-entry-service-uri)))
      (%make-actor-registry
       :entries (coerce sorted 'vector)
       :index index))))

(defun actor-registry-count (registry)
  "Return the number of validated actor contracts in REGISTRY."
  (length (actor-registry-entries registry)))

(defun copy-string-vector (items)
  (coerce (mapcar #'copy-seq items) 'vector))

(defun entry-projection (entry)
  (list
   :service-uri (copy-seq (actor-registry-entry-service-uri entry))
   :name (copy-seq (actor-registry-entry-name entry))
   :kind "actor"
   :runtime (copy-seq (actor-registry-entry-runtime entry))
   :accepts (copy-string-vector (actor-registry-entry-accepts entry))
   :produces (copy-string-vector (actor-registry-entry-produces entry))
   :capabilities (copy-string-vector
                  (actor-registry-entry-capabilities entry))
   :library
   (list :name (copy-seq (actor-registry-entry-library-name entry))
         :version (copy-seq (actor-registry-entry-library-version entry))
         :digest (copy-seq (actor-registry-entry-library-digest entry)))
   :identity-status +legacy-identity-status+))

(defun actor-registry-find (registry service-uri)
  "Return the safe projection for exact compatibility SERVICE-URI, or NIL."
  (let ((entry (and (stringp service-uri)
                    (gethash service-uri (actor-registry-index registry)))))
    (and entry (entry-projection entry))))

(defun validate-query-limit (limit)
  (unless (and (integerp limit)
               (<= 0 limit +actor-registry-query-limit+))
    (reject-manifest "invalid_query_limit" "query.limit"
                     "Limit must be between 0 and ~d."
                     +actor-registry-query-limit+))
  limit)

(defun entry-matches-query-p (entry kind capability accepts produces)
  (and (or (null kind) (string= kind "actor"))
       (or (null capability)
           (member capability (actor-registry-entry-capabilities entry)
                   :test #'string=))
       (or (null accepts)
           (member accepts (actor-registry-entry-accepts entry)
                   :test #'string=))
       (or (null produces)
           (member produces (actor-registry-entry-produces entry)
                   :test #'string=))))

(defun query-actor-registry
    (registry &key kind capability accepts produces
                    (limit +actor-registry-query-limit+))
  "Return deterministic safe projections matching exact bounded predicates."
  (validate-query-limit limit)
  (dolist (pair `((,kind . "kind")
                  (,capability . "capability")
                  (,accepts . "accepts")
                  (,produces . "produces")))
    (when (car pair)
      (require-non-empty-string (car pair)
                                (format nil "query.~a" (cdr pair)))))
  (if (zerop limit)
      #()
      (coerce
       (loop for entry across (actor-registry-entries registry)
             when (entry-matches-query-p
                   entry kind capability accepts produces)
               collect (entry-projection entry) into matches
             when (= (length matches) limit)
               return matches
             finally (return matches))
       'vector)))

(defun actor-registry-list (registry &key (limit +actor-registry-query-limit+))
  "Return a bounded, URI-sorted vector of safe actor projections."
  (query-actor-registry registry :limit limit))
