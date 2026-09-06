(in-package :star.star-uri)

;;;; Legacy star://domain:address:actor-name compatibility input.
;;;;
;;;; Part of the temporary gserver STAR URI compatibility layer; see the
;;;; replacement marker in source/star-uri.lisp.  Normative source:
;;;; STAR-SERVER-041 ("Relationship to Legacy STAR-LANG-004") and
;;;; STAR-RESEARCH-041 ("Legacy StarLang URI is compatibility input only").
;;;;
;;;; Rules enforced here:
;;;;   * the legacy tuple is parseable ONLY through this explicit
;;;;     compatibility API; parse-star-uri rejects it;
;;;;   * there is no automatic mapping from the old tuple to a canonical
;;;;     identity.  In particular the legacy domain is NEVER reused as a
;;;;     canonical authority and no legacy.<domain> authority is invented;
;;;;   * migration requires an explicit canonical target URI or an explicit
;;;;     migration map supplied by the caller;
;;;;   * new code never emits the legacy tuple format.

(defstruct (legacy-star-actor-tuple
            (:constructor %make-legacy-star-actor-tuple (domain address actor-name))
            (:predicate legacy-star-actor-tuple-p)
            (:copier nil))
  (domain "" :type string :read-only t)
  (address "" :type string :read-only t)
  (actor-name "" :type string :read-only t))

(defun star-legacy-token-character-p (character)
  "Token profile of the historical StarLang parser: lowercase ASCII letters,
digits, '.', '_', and '-'."
  (or (char<= #\a character #\z)
      (char<= #\0 character #\9)
      (find character ".-_" :test #'char=)))

(defun star-legacy-token-p (value)
  (and (stringp value)
       (plusp (length value))
       (every #'star-legacy-token-character-p value)))

(defun star-legacy-token (value label)
  (unless (star-legacy-token-p value)
    (fail-invalid-star-uri
     "legacy STAR actor URI ~a must be non-empty lowercase ASCII using only letters, digits, '.', '_' or '-': ~s"
     label value))
  value)

(defun legacy-star-actor-tuple-string (tuple)
  "Re-serialize a parsed legacy tuple.  For migration-map lookup only; new
code must never emit this format as identity."
  (format nil "star://~a:~a:~a"
          (legacy-star-actor-tuple-domain tuple)
          (legacy-star-actor-tuple-address tuple)
          (legacy-star-actor-tuple-actor-name tuple)))

(defun parse-legacy-star-uri (value)
  "Parse the legacy StarLang actor tuple star://domain:address:actor-name
into a legacy-star-actor-tuple.  This is compatibility input parsing only:
the result is not a canonical STAR URI and cannot be used as one without an
explicit migration target."
  (unless (stringp value)
    (fail-invalid-star-uri
     "legacy STAR actor URI must be a string, got ~s" (type-of value)))
  (unless (star-uri-text-p value)
    (fail-invalid-star-uri
     "legacy STAR actor URIs use the star:// scheme: ~s" value))
  (let* ((body (subseq value 7))
         (first-separator (position #\: body))
         (second-separator
           (and first-separator
                (position #\: body :start (1+ first-separator))))
         (third-separator
           (and second-separator
                (position #\: body :start (1+ second-separator)))))
    (unless (and first-separator second-separator (null third-separator))
      (fail-invalid-star-uri
       "legacy STAR actor URI must have exactly domain:address:actor-name after star://: ~s"
       value))
    (%make-legacy-star-actor-tuple
     (star-legacy-token (subseq body 0 first-separator) "domain")
     (star-legacy-token
      (subseq body (1+ first-separator) second-separator) "address")
     (star-legacy-token (subseq body (1+ second-separator)) "actor-name"))))

(defun legacy-star-actor-uri-p (value)
  "True when VALUE is a legacy star://domain:address:actor-name string."
  (and (stringp value)
       (not (null (ignore-errors (parse-legacy-star-uri value))))))

(defun star-canonical-migration-target (target)
  (unless (canonical-star-uri-p target)
    (fail-invalid-star-uri
     "legacy STAR migration target must already be a canonical STAR URI: ~s"
     target))
  target)

(defun migrate-legacy-star-uri (legacy &key to map)
  "Migrate a legacy star://domain:address:actor-name input to a canonical
STAR identity.

The destination is always explicit: pass :to with an already-canonical
STAR URI, or :map with an alist whose keys are legacy tuple strings and
whose values are already-canonical STAR URIs.  There is intentionally no
automatic domain-to-authority rewrite (STAR-SERVER-041).  Returns the
canonical target string; signals invalid-star-uri otherwise."
  (let ((tuple (etypecase legacy
                 (legacy-star-actor-tuple legacy)
                 (string (parse-legacy-star-uri legacy)))))
    (cond
      (to (star-canonical-migration-target to))
      (map
       (let ((entry (assoc (legacy-star-actor-tuple-string tuple)
                           map :test #'string=)))
         (unless entry
           (fail-invalid-star-uri
            "no explicit migration map entry for legacy STAR actor URI ~s"
            (legacy-star-actor-tuple-string tuple)))
         (star-canonical-migration-target (cdr entry))))
      (t
       (fail-invalid-star-uri
        "migrating legacy STAR actor URI ~s requires an explicit :to canonical target or an explicit :map migration map"
        (legacy-star-actor-tuple-string tuple))))))
