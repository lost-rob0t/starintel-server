(in-package :star.star-uri)

;;;; Temporary canonical STAR URI compatibility layer for starintel-gserver.
;;;;
;;;; Normative source:
;;;;   - STAR-SERVER-041 "RFC 3986 STAR URI Authority Routing Extension"
;;;;     (roam/design/star-server/STAR-SERVER-041-star-uri-authority-routing-protocol.org)
;;;;   - STAR-RESEARCH-041 "Unified STAR URI Authority Routing and FediWatch
;;;;     RabbitMQ"
;;;;     (roam/research/star-server/STAR-RESEARCH-041-unified-star-uri-authority-routing-and-fediwatch-rabbitmq.org)
;;;;
;;;; *** REPLACEMENT MARKER *********************************************************
;;;; *** This entire package (source/star-uri.lisp and source/star-uri-legacy.lisp)
;;;; *** is a TEMPORARY gserver-local compatibility implementation.  It exists only
;;;; *** so gserver can speak canonical STAR identity for the BBPD migration before
;;;; *** the shared StarLang STAR URI library is consumable from gserver.
;;;; *** Once the shared StarLang / STAR URI library is consumable by gserver this
;;;; *** package MUST BE DELETED IN FULL and replaced by that shared library.
;;;; *** Actor and target routing must depend only on the narrow API exported by
;;;; *** the :star.star-uri package, never on this parser implementation, so the
;;;; *** replacement does not touch gserver actor or target routing code.
;;;; *** See STAR-SERVER-041 Phase A (reusable URI library) and STAR-LANG-004:
;;;; *** "StarLang MUST reuse the shared URI value model and conformance fixtures.
;;;; *** It MUST NOT maintain a second actor-specific canonicalizer."
;;;; *********************************************************************************

(define-condition invalid-star-uri (error)
  ((reason :initarg :reason :reader invalid-star-uri-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid STAR URI: ~a"
             (invalid-star-uri-reason condition)))))

(defun fail-invalid-star-uri (control &rest arguments)
  (error 'invalid-star-uri :reason (apply #'format nil control arguments)))

(defparameter +star-uri-implementation-replacement-marker+
  "TEMPORARY gserver-local STAR URI compatibility layer (STAR-SERVER-041 / STAR-RESEARCH-041). Delete and replace with the shared StarLang STAR URI library when it becomes consumable by gserver. Depend only on the :star.star-uri package API.")

;;;; Reviewed global resource-kind registry (STAR-SERVER-041).  Adding a kind
;;;; requires a reviewed protocol registry change.
(defparameter +star-uri-resource-kinds+
  '("actor" "service" "document" "target" "dataset" "relation" "job"))

;;;; Immutable value model recommended by STAR-SERVER-041: authority plus
;;;; hierarchical path segments, with resource-kind/resource-path views.
;;;; There are deliberately no domain/address/actor-name fields, and the
;;;; struct setters are not exported.
(defstruct (star-uri
            (:constructor %make-star-uri (authority path-segments))
            (:predicate star-uri-p)
            (:copier nil))
  (authority "" :type string :read-only t)
  (path-segments '() :type list :read-only t))

(defun star-uri-resource-kind (uri)
  "First path segment: the reviewed STAR resource class."
  (car (star-uri-path-segments uri)))

(defun star-uri-resource-path (uri)
  "Ordered segments following the resource kind.  Segments are in canonical
serialization form; percent escapes that must remain are uppercase."
  (cdr (star-uri-path-segments uri)))

;;;; RFC 3986 character profiles.

(defun star-unreserved-character-p (character)
  (or (char<= #\a character #\z)
      (char<= #\A character #\Z)
      (char<= #\0 character #\9)
      (find character "-._~" :test #'char=)))

(defun star-sub-delimiter-character-p (character)
  (find character "!$&'()*+,;=" :test #'char=))

(defun star-pchar-character-p (character)
  "RFC 3986 pchar profile minus pct-encoded, which is handled explicitly."
  (or (star-unreserved-character-p character)
      (star-sub-delimiter-character-p character)
      (char= character #\:)
      (char= character #\@)))

(defun star-hex-digit-character-p (character)
  (or (char<= #\0 character #\9)
      (char<= #\a character #\f)
      (char<= #\A character #\F)))

(defun star-dot-segment-p (segment)
  (or (string= segment ".") (string= segment "..")))

;;;; Percent-escape handling.

(defun star-percent-escape-code (text index)
  "Return (values code end-index) for the %HH escape starting at INDEX,
signaling invalid-star-uri when the escape is malformed."
  (let* ((length (length text))
         (high (and (< (1+ index) length) (char text (1+ index))))
         (low (and (< (+ index 2) length) (char text (+ index 2)))))
    (unless (and high low
                 (char= (char text index) #\%)
                 (star-hex-digit-character-p high)
                 (star-hex-digit-character-p low))
      (fail-invalid-star-uri
       "malformed percent escape in ~s at offset ~d" text index))
    (values
     (+ (* 16 (digit-char-p high 16)) (digit-char-p low 16))
     (+ index 3))))

(defun star-normalized-escape (text index)
  "Return (values normalized-string next-index) for the escape at INDEX.

Percent-encoded unreserved characters normalize to their unreserved
representation; everything else re-serializes with uppercase hex digits
(STAR-SERVER-041 canonicalization rules 6 and 7)."
  (multiple-value-bind (code end)
      (star-percent-escape-code text index)
    (values
     (if (and (< code 128)
              (star-unreserved-character-p (code-char code)))
         (string (code-char code))
         (format nil "%~2,'0X" code))
     end)))

;;;; Authority: RFC 3986 reg-name profile, canonical lowercase.

(defun star-normalize-authority (text)
  (when (zerop (length text))
    (fail-invalid-star-uri "authority is required"))
  (let ((out (make-string-output-stream))
        (index 0)
        (length (length text)))
    (loop while (< index length)
          for character = (char text index)
          do (cond
               ((char= character #\@)
                (fail-invalid-star-uri "userinfo is forbidden in STAR authority"))
               ((char= character #\:)
                (fail-invalid-star-uri
                 "transport ports are forbidden in canonical STAR authority"))
               ((char= character #\%)
                (multiple-value-bind (escape next)
                    (star-normalized-escape text index)
                  ;; Authority is case-insensitive: decoded unreserved
                  ;; literals fold to lowercase alongside literal characters.
                  (write-string
                   (if (= 1 (length escape))
                       (string (char-downcase (char escape 0)))
                       escape)
                   out)
                  (setf index next)))
               ((or (star-unreserved-character-p character)
                    (star-sub-delimiter-character-p character))
                (write-char (char-downcase character) out)
                (incf index))
               (t
                (fail-invalid-star-uri
                 "character ~s is outside the RFC 3986 reg-name profile for a STAR authority"
                 character))))
    (get-output-stream-string out)))

;;;; Path segments: RFC 3986 pchar plus percent-encoding rules.

(defun star-split-path (path-text)
  "Split the canonical path text (which starts with '/') into raw segments."
  (let ((segments '())
        (start 1)
        (length (length path-text)))
    (loop
      (let ((slash (position #\/ path-text :start start)))
        (cond
          (slash
           (push (subseq path-text start slash) segments)
           (setf start (1+ slash)))
          (t
           (push (subseq path-text start) segments)
           (return)))))
    (nreverse segments)))

(defun star-normalize-path-segment (segment original)
  "Validate and canonicalize one raw path segment.

Dot segments are rejected before percent normalization and again after it,
so %2E and %2E%2E can never normalize into accepted dot segments
(STAR-SERVER-041 canonicalization rule 9).  Resource-path case is
preserved (rule 8)."
  (when (zerop (length segment))
    (fail-invalid-star-uri "empty path segment in ~s" original))
  (when (star-dot-segment-p segment)
    (fail-invalid-star-uri
     "dot path segment ~s is forbidden in ~s" segment original))
  (let ((out (make-string-output-stream))
        (index 0)
        (length (length segment)))
    (loop while (< index length)
          for character = (char segment index)
          do (cond
               ((char= character #\%)
                (multiple-value-bind (escape next)
                    (star-normalized-escape segment index)
                  (write-string escape out)
                  (setf index next)))
               ((star-pchar-character-p character)
                (write-char character out)
                (incf index))
               (t
                (fail-invalid-star-uri
                 "character ~s is not an RFC 3986 pchar in path segment ~s of ~s"
                 character segment original))))
    (let ((normalized (get-output-stream-string out)))
      (when (star-dot-segment-p normalized)
        (fail-invalid-star-uri
         "path segment ~s normalizes to forbidden dot segment ~s in ~s"
         segment normalized original))
      normalized)))

;;;; Parsing, serialization, and canonicalization.

(defun star-uri-text-p (value)
  "True when VALUE is textual star:// input: a canonical STAR URI, a
canonicalizable variant, or legacy tuple input.  It does not assert
validity."
  (and (stringp value)
       (>= (length value) 7)
       (string-equal "star://" value :end1 7 :end2 7)))

(defun parse-star-uri (value)
  "Parse VALUE into a canonical star-uri value, applying the STAR-SERVER-041
v1 canonicalization rules.  Signals invalid-star-uri for any input outside
the profile."
  (unless (stringp value)
    (fail-invalid-star-uri "STAR URI must be a string, got ~s" (type-of value)))
  (unless (plusp (length value))
    (fail-invalid-star-uri "STAR URI must be a non-empty string"))
  (unless (star-uri-text-p value)
    (fail-invalid-star-uri "STAR URIs use the star:// scheme: ~s" value))
  (let* ((body (subseq value 7))
         (path-start (position #\/ body))
         (authority-text (if path-start (subseq body 0 path-start) body))
         (path-text (if path-start (subseq body path-start) ""))
         (authority (star-normalize-authority authority-text)))
    (when (zerop (length path-text))
      (fail-invalid-star-uri
       "resource kind is required: star://<authority>/<resource-kind>[/<resource-path>]: ~s"
       value))
    (let ((segments
            (mapcar (lambda (segment)
                      (star-normalize-path-segment segment value))
                    (star-split-path path-text))))
      (unless (member (car segments) +star-uri-resource-kinds+
                      :test #'string=)
        (fail-invalid-star-uri
         "unknown STAR resource kind ~s; reviewed kinds are ~{~a~^, ~}"
         (car segments) +star-uri-resource-kinds+))
      (%make-star-uri authority segments))))

(defun serialize-star-uri (uri)
  "Serialize a star-uri value in canonical v1 form."
  (unless (star-uri-p uri)
    (fail-invalid-star-uri "expected a star-uri value, got ~s" uri))
  (format nil "star://~a/~{~a~^/~}"
          (star-uri-authority uri)
          (star-uri-path-segments uri)))

(defun canonicalize-star-uri (value)
  "Return the canonical v1 serialization of VALUE."
  (serialize-star-uri (parse-star-uri value)))

(defun canonical-star-uri-p (value)
  "True when VALUE is a string that is already the exact canonical v1
serialization of a valid STAR URI.

This deliberately differs from valid-star-uri-p, which accepts any
canonicalizable input; conflating the two would let non-canonical identity
text masquerade as canonical (a defect flagged in the BBPD compatibility
module review)."
  (and (stringp value)
       (plusp (length value))
       (string= value (ignore-errors (canonicalize-star-uri value)))))

(defun valid-star-uri-p (value)
  "True when VALUE parses under the canonical STAR v1 profile, even if it
still requires canonicalization before it is canonical."
  (and (stringp value)
       (not (null (ignore-errors (parse-star-uri value))))))

(defun actor-star-uri-p (value)
  "True when VALUE (a star-uri value or a string) addresses an actor
resource with a non-empty actor resource path."
  (let ((uri (etypecase value
               (star-uri value)
               (string (ignore-errors (parse-star-uri value))))))
    (and (star-uri-p uri)
         (string= "actor" (star-uri-resource-kind uri))
         (star-uri-resource-path uri))))

;;;; Gserver authority ownership (STAR-SERVER-041: starintel-gserver owns one
;;;; configured stable logical authority).

(defparameter *gserver-star-authority*
  (let ((configured (uiop:getenv "STAR_GSERVER_AUTHORITY")))
    (and (stringp configured)
         (plusp (length configured))
         (string-downcase configured)))
  "The stable logical STAR authority owned by this gserver deployment.

STAR-SERVER-041: authority identity is a stable logical namespace.  It is
never derived from an HTTP origin, RabbitMQ host, ZeroMQ endpoint, machine
hostname, or process identity.  Configured through STAR_GSERVER_AUTHORITY.

NIL means this deployment owns no STAR authority: every canonical URI is
then external, and resolution boundaries that require local authority
ownership fail closed.")

(defun star-uri-owned-p (value)
  "True when VALUE (a star-uri value or a star:// string) belongs to the
STAR authority configured for this gserver deployment.  Distributed
authority discovery is StarRouter territory (STAR-SERVER-040/041) and is
deliberately not implemented here."
  (let ((uri (etypecase value
               (star-uri value)
               (string (ignore-errors (parse-star-uri value))))))
    (and (star-uri-p uri)
         *gserver-star-authority*
         (string-equal (star-uri-authority uri) *gserver-star-authority*))))
