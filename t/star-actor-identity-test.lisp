(in-package :star-server-tests)

;;;; STAR actor identity resolution tests for target dispatch.
;;;;
;;;; Conformance case required by the BBPD migration: a canonical external
;;;; actor identity such as star://bbpd.starintel.actor/actor/subfinder must
;;;; be accepted, parsed, kept distinct from the local actor registry key,
;;;; projected onto the existing external-actor RabbitMQ transport, and
;;;; carried across that projection with its canonical identity preserved.
;;;;
;;;; Normative source: STAR-SERVER-041, STAR-RESEARCH-041, STAR-LANG-004,
;;;; STAR-RESEARCH-060.  The parser behind star.star-uri is a temporary
;;;; compatibility layer that the shared StarLang STAR URI library replaces.

(def-suite star-actor-identity-tests
  :description "Canonical STAR actor identity resolution, BBPD interoperability, and legacy bare-name compatibility")

(in-suite star-actor-identity-tests)

(defparameter *identity-test-authority* "gserver.starintel.actor")

(defmacro with-gserver-authority (&body body)
  `(let ((star.star-uri:*gserver-star-authority* *identity-test-authority*))
     ,@body))

(defun make-identity-target-document (&key (actor "star://bbpd.starintel.actor/actor/subfinder")
                                        (id "identity-target-1")
                                        (delay 60))
  (jsown:new-js
    ("_id" id)
    ("dtype" "target")
    ("actor" actor)
    ("target" "example.org")
    ("delay" delay)
    ("recurring" :false)
    ("schedule_id" (format nil "schedule:~a" id))
    ("options" #())))

(defun capture-invalid-target-dispatch (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.actors:invalid-target-dispatch (condition) condition)))

;;;; ----------------------------------------------------------------------
;;;; Canonical STAR identity is accepted and parsed.
;;;; ----------------------------------------------------------------------

(test bbpd-canonical-actor-uri-is-a-valid-target-actor-identity
  (with-gserver-authority
    (is-true (star.actors:valid-target-actor-identity-p
              "star://bbpd.starintel.actor/actor/subfinder"))
    (let ((identity (star.actors:resolve-target-actor-identity
                     "star://bbpd.starintel.actor/actor/subfinder")))
      (is-true (star.actors:target-actor-identity-p identity))
      (let ((uri (star.actors:target-actor-identity-star-uri identity)))
        (is-true (star.star-uri:star-uri-p uri))
        (is (string= "bbpd.starintel.actor" (star.star-uri:star-uri-authority uri)))
        (is (string= "actor" (star.star-uri:star-uri-resource-kind uri)))
        (is (equal '("subfinder") (star.star-uri:star-uri-resource-path uri)))))))

(test bbpd-actor-uri-is-not-a-local-registry-key
  "An external authority URI must never be interpreted as the literal local
actor registry key star://bbpd.starintel.actor/actor/subfinder, and the local
registry is not consulted for foreign authorities at all."
  (with-gserver-authority
    (let ((identity
            (star.actors:resolve-target-actor-identity
             "star://bbpd.starintel.actor/actor/subfinder")))
      (is-false (star.actors:target-actor-identity-registry-key identity))
      (let ((seen nil))
        (star.actors:resolve-target-destination
         "star://bbpd.starintel.actor/actor/subfinder"
         :resolver (lambda (key) (declare (ignore key)) (setf seen t) nil))
        (is-false seen)))))

(test bbpd-actor-uri-projects-to-the-current-external-actor-transport
  (with-gserver-authority
    (let ((destination
            (star.actors:resolve-target-destination
             "star://bbpd.starintel.actor/actor/subfinder")))
      (is (eq :rabbit (star.actors:target-destination-handle-kind destination)))
      (is (string= "subfinder" (star.actors:target-destination-handle-name destination)))
      (is (string= "documents.target.dispatch.subfinder"
                   (star.actors:target-destination-handle-routing-key destination)))
      (is (equal '("actors.subfinder.new.target")
                 (star.actors:target-destination-handle-compatibility-routing-keys
                  destination)))
      (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                   (star.actors:target-destination-handle-star-uri destination))))))

(test bbpd-actor-uri-keeps-canonical-identity-across-the-projection
  "The canonical STAR actor URI is carried in document extensions and AMQP
metadata so bbpd.starintel.actor/subfinder stays distinguishable from any
other authority projecting the same transport token."
  (with-gserver-authority
    (let* ((record (star.actors:parse-target-record
                    (make-identity-target-document)))
           (envelope (star.actors:make-target-dispatch-envelope record))
           (dispatch-document (star.actors:target-dispatch-document envelope))
           (extensions (jsown:val dispatch-document "extensions"))
           (properties (star.actors:target-dispatch-remote-properties envelope))
           (headers (cdr (assoc :headers properties))))
      (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                   (jsown:val extensions "target_actor_uri")))
      (is-true (assoc "x-star-destination-uri" headers :test #'string=))
      (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                   (cdr (assoc "x-star-destination-uri" headers :test #'string=))))
      ;; The carried identity is canonical STAR, never the legacy tuple form.
      (is-false (star.star-uri:legacy-star-actor-uri-p
                 (jsown:val extensions "target_actor_uri"))))))

(test target-acceptance-records-the-canonical-actor-uri
  (with-gserver-authority
    (let* ((record (star.actors:parse-target-record
                    (make-identity-target-document)))
           (acceptance
             (star.actors:target-acceptance-document
              (star.actors:make-target-dispatch-envelope record))))
      (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                   (jsown:val acceptance "actor_star_uri"))))))

;;;; ----------------------------------------------------------------------
;;;; Local-authority actor URIs resolve through the registry adapter.
;;;; ----------------------------------------------------------------------

(test local-authority-actor-uri-resolves-into-the-local-registry
  (with-gserver-authority
    (let ((sentinel (gensym "LOCAL-ACTOR"))
          (calls nil))
      (let ((destination
              (star.actors:resolve-target-destination
               "star://gserver.starintel.actor/actor/user-hunt"
               :resolver (lambda (key)
                           (push key calls)
                           sentinel))))
        (is (eq :local (star.actors:target-destination-handle-kind destination)))
        (is (eq sentinel (star.actors:target-destination-handle-component destination)))
        (is (equal '("user-hunt") calls))
        (is (string= "user-hunt"
                     (star.actors:target-actor-identity-registry-key
                      (star.actors:resolve-target-actor-identity
                       "star://gserver.starintel.actor/actor/user-hunt"))))
        (is (string= "star://gserver.starintel.actor/actor/user-hunt"
                     (star.actors:target-destination-handle-star-uri destination)))))))

(test multi-segment-local-actor-uri-resolves-through-the-adapter
  (with-gserver-authority
    (let ((sentinel (gensym "LOCAL-ACTOR")))
      (let ((destination
              (star.actors:resolve-target-destination
               "star://gserver.starintel.actor/actor/quasar/user-hunt"
               :resolver (lambda (key) (if (string= key "quasar/user-hunt") sentinel nil)))))
        (is (eq :local (star.actors:target-destination-handle-kind destination)))
        (is (eq sentinel (star.actors:target-destination-handle-component destination)))))))

(test local-authority-actor-uri-without-a-local-actor-projects-externally
  (with-gserver-authority
    (let ((destination
            (star.actors:resolve-target-destination
             "star://gserver.starintel.actor/actor/domain-enricher"
             :resolver (lambda (key) (declare (ignore key)) nil))))
      (is (eq :rabbit (star.actors:target-destination-handle-kind destination)))
      (is (string= "domain-enricher"
                   (star.actors:target-destination-handle-name destination)))
      (is (string= "documents.target.dispatch.domain-enricher"
                   (star.actors:target-destination-handle-routing-key destination)))
      (is (string= "star://gserver.starintel.actor/actor/domain-enricher"
                   (star.actors:target-destination-handle-star-uri destination))))))

(test multi-segment-actor-uri-cannot-project-onto-the-flat-transport
  "The current external-actor RabbitMQ contract is flat; gserver fails closed
instead of inventing a flattened transport token for a two-segment resource."
  (with-gserver-authority
    (is-true (capture-invalid-target-dispatch
              (lambda ()
                (star.actors:resolve-target-destination
                 "star://gserver.starintel.actor/actor/quasar/user-hunt"
                 :resolver (lambda (key) (declare (ignore key)) nil)))))))

;;;; ----------------------------------------------------------------------
;;;; Fail-closed behavior for malformed and unsupported identities.
;;;; ----------------------------------------------------------------------

(test malformed-and-non-actor-star-uris-are-rejected-as-target-identities
  (with-gserver-authority
    (dolist (actor '("star://bbpd.starintel.actor:5672/actor/subfinder"
                     "star://bbpd.starintel.actor/actor/%2E%2E/subfinder"
                     "star://bbpd.starintel.actor/service/recon"
                     "star://bbpd.starintel.actor"
                     "star://quasar:localhost:user-hunt"
                     "star://"))
      (is-true (capture-invalid-target-dispatch
                (lambda ()
                  (star.actors:resolve-target-actor-identity actor)))
               actor)
      (is-false (star.actors:valid-target-actor-identity-p actor))
      (let* ((document (make-identity-target-document :actor actor))
             (outcome (star.actors:accept-target-record
                       (star.actors:parse-target-record document))))
        (is (eq :invalid (star.actors:target-dispatch-outcome-status outcome)))
        (is-true (star.actors:target-dispatch-outcome-reason outcome))))))

;;;; ----------------------------------------------------------------------
;;;; Bare actor names keep working during the migration.
;;;; ----------------------------------------------------------------------

(test bare-actor-name-remains-a-valid-identity-without-a-star-uri
  (let ((identity (star.actors:resolve-target-actor-identity "domain-enricher")))
    (is-false (star.actors:target-actor-identity-star-uri identity))
    (is (string= "domain-enricher"
                 (star.actors:target-actor-identity-registry-key identity))))
  (is-true (star.actors:valid-target-actor-identity-p "domain-enricher")))

(test bare-actor-name-local-routing-is-unchanged
  (let ((sentinel (gensym "LOCAL-ACTOR")))
    (let ((destination
            (star.actors:resolve-target-destination
             "domain-enricher"
             :resolver (lambda (key) (if (string= key "domain-enricher") sentinel nil)))))
      (is (eq :local (star.actors:target-destination-handle-kind destination)))
      (is (eq sentinel (star.actors:target-destination-handle-component destination)))
      (is-false (star.actors:target-destination-handle-star-uri destination)))))

(test bare-actor-name-external-routing-is-unchanged
  (let ((destination
          (star.actors:resolve-target-destination
           "subfinder"
           :resolver (lambda (key) (declare (ignore key)) nil))))
    (is (eq :rabbit (star.actors:target-destination-handle-kind destination)))
    (is (string= "documents.target.dispatch.subfinder"
                 (star.actors:target-destination-handle-routing-key destination)))
    (is (equal '("actors.subfinder.new.target")
               (star.actors:target-destination-handle-compatibility-routing-keys
                destination)))
    (is-false (star.actors:target-destination-handle-star-uri destination))))

(test bare-actor-name-dispatch-carries-no-star-identity-metadata
  (let* ((record (star.actors:parse-target-record
                  (make-identity-target-document :actor "subfinder")))
         (envelope (star.actors:make-target-dispatch-envelope record))
         (extensions
           (jsown:val (star.actors:target-dispatch-document envelope) "extensions"))
         (headers (cdr (assoc :headers
                              (star.actors:target-dispatch-remote-properties envelope)))))
    (is-false (jsown:keyp extensions "target_actor_uri"))
    (is-false (assoc "x-star-destination-uri" headers :test #'string=))))

;;;; ----------------------------------------------------------------------
;;;; Replacement seam: routing depends on the identity API, not the parser.
;;;; ----------------------------------------------------------------------

(test actor-routing-consumes-the-identity-interface-not-the-parser
  "STAR-SERVER-041 names one shared STAR URI implementation as the end state.
gserver actor routing must only depend on the star identity resolution
interface, so the temporary gserver-side parser can be swapped for the shared
StarLang STAR URI library without touching target routing."
  (let ((star.actors:*target-actor-identity-resolver*
           (lambda (actor)
             (declare (ignore actor))
             (star.actors:make-target-actor-identity
              :star-uri nil :registry-key "stubbed-actor"))))
    ;; "not even a plausible URI" still routes: resolution went through the
    ;; injected identity interface instead of the temporary parser.
    (let ((destination
            (star.actors:resolve-target-destination
             "not a star uri at all"
             :resolver (lambda (key) (declare (ignore key)) nil))))
      (is (eq :rabbit (star.actors:target-destination-handle-kind destination)))
      (is (string= "stubbed-actor"
                   (star.actors:target-destination-handle-name destination))))))

(test uri-parsing-stays-outside-the-actor-namespace
  (is-false (find-symbol "PARSE-STAR-URI" :star.actors))
  (is-false (find-symbol "SERIALIZE-STAR-URI" :star.actors))
  (is-true (find-symbol "PARSE-STAR-URI" :star.star-uri)))

;;;; ----------------------------------------------------------------------
;;;; HTTP target surfaces.
;;;; ----------------------------------------------------------------------

(test v1-target-create-accepts-canonical-star-actor-identity
  (let ((document
          (star.frontends.http-api::target-v1-document-from-request
           (jsown:new-js
             ("actor" "star://bbpd.starintel.actor/actor/subfinder")
             ("target" "example.org")
             ("dataset" "star-intel")
             ("delay" 1)
             ("recurring" :false)
             ("options" #())
             ("idempotency_key" "bbpd-migration-1"))
           "human:alice")))
    (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                 (jsown:val document "actor")))))

(test v1-target-create-rejects-invalid-star-actor-identity
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api::target-v1-document-from-request
              (jsown:new-js
                ("actor" "star://bbpd.starintel.actor:5672/actor/subfinder")
                ("target" "example.org")
                ("dataset" "star-intel")
                ("delay" 1)
                ("recurring" :false)
                ("options" #())
                ("idempotency_key" "bbpd-migration-2"))
              "human:alice")))))
    (is-true condition)
    (is (= 422 (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "invalid_target_actor"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test legacy-target-adapter-stays-bare-name-only
  "The historical /new/target/:actor route remains a bounded compatibility
adapter for bare actor names; canonical STAR identities must use the
versioned target API."
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api::ensure-legacy-target-adapter-actor
              "star://bbpd.starintel.actor/actor/subfinder")))))
    (is-true condition)
    (is (= 400 (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "canonical_actor_identity_unsupported"
                 (star.frontends.http-api:http-input-error-code condition))))
  (is-false (capture-http-input-error
             (lambda ()
               (star.frontends.http-api::ensure-legacy-target-adapter-actor
                "domain-enricher")))))
