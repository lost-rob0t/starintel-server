(in-package :star-server-tests)

;;;; Temporary canonical STAR URI conformance fixtures.
;;;;
;;;; Normative source: STAR-SERVER-041 (RFC 3986 STAR URI Authority Routing
;;;; Extension) and STAR-RESEARCH-041.  This suite pins the gserver-side
;;;; temporary compatibility implementation that will be replaced by the
;;;; shared StarLang STAR URI library (see source/star-uri.lisp).

(def-suite star-uri-tests
  :description "STAR-SERVER-041 canonical star:// URI parsing, canonicalization, and legacy compatibility")

(in-suite star-uri-tests)

(defun star-uri-condition-of (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.star-uri:invalid-star-uri (condition) condition)))

(test reviewed-resource-kinds-are-the-041-registry
  (is (equal '("actor" "service" "document" "target" "dataset" "relation" "job")
             star.star-uri:+star-uri-resource-kinds+)))

(test canonical-actor-uri-parses-into-authority-kind-and-path
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/user-hunt")))
    (is-true (star.star-uri:star-uri-p uri))
    (is (string= "gserver.starintel.actor" (star.star-uri:star-uri-authority uri)))
    (is (string= "actor" (star.star-uri:star-uri-resource-kind uri)))
    (is (equal '("user-hunt") (star.star-uri:star-uri-resource-path uri)))
    (is (string= "star://gserver.starintel.actor/actor/user-hunt"
                 (star.star-uri:serialize-star-uri uri)))))

(test multi-segment-actor-path-has-no-domain-field
  "STAR-RESEARCH-041: /actor/quasar/user-hunt is a two-segment resource path,
not a revived domain/address actor tuple."
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/quasar/user-hunt")))
    (is (equal '("quasar" "user-hunt") (star.star-uri:star-uri-resource-path uri)))
    (is-false (find-symbol "STAR-URI-DOMAIN" :star.star-uri))
    (is-false (find-symbol "STAR-URI-ADDRESS" :star.star-uri))
    (is-false (find-symbol "STAR-URI-ACTOR-NAME" :star.star-uri))))

(test every-reviewed-resource-kind-parses
  (dolist (kind star.star-uri:+star-uri-resource-kinds+)
    (let ((uri (star.star-uri:parse-star-uri
                (format nil "star://gserver.starintel.actor/~a/01Kresource" kind))))
      (is (string= kind (star.star-uri:star-uri-resource-kind uri))))))

(test scheme-and-authority-canonicalize-to-lowercase
  (let ((uri (star.star-uri:parse-star-uri
              "STAR://GServer.StarIntel.Actor/actor/User-Hunt")))
    (is (string= "star://gserver.starintel.actor/actor/User-Hunt"
                 (star.star-uri:serialize-star-uri uri)))))

(test resource-path-case-is-preserved
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/User-Hunt")))
    (is (equal '("User-Hunt") (star.star-uri:star-uri-resource-path uri)))))

(test resource-kind-is-case-sensitive
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://gserver.starintel.actor/Actor/user-hunt")))))

(test userinfo-is-forbidden
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://user@gserver.starintel.actor/actor/user-hunt")))))

(test transport-port-is-forbidden
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://gserver.starintel.actor:5672/actor/user-hunt")))))

(test query-is-forbidden
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://gserver.starintel.actor/actor/user-hunt?trace=1")))))

(test fragment-is-forbidden
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://gserver.starintel.actor/actor/user-hunt#frag")))))

(test literal-dot-segments-are-rejected
  (dolist (text '("star://gserver.starintel.actor/actor/./user-hunt"
                  "star://gserver.starintel.actor/actor/../user-hunt"
                  "star://gserver.starintel.actor/actor/user-hunt/."
                  "star://gserver.starintel.actor/actor/user-hunt/.."))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri text)))
             text)))

(test percent-encoded-dot-segments-are-rejected-after-normalization
  "Normalization must not turn %2E / %2E%2E into accepted dot segments."
  (dolist (text '("star://gserver.starintel.actor/actor/%2E/user-hunt"
                  "star://gserver.starintel.actor/actor/%2E%2E/user-hunt"
                  "star://gserver.starintel.actor/actor/%2e%2e/user-hunt"
                  "star://gserver.starintel.actor/actor/user-hunt/%2E"
                  "star://gserver.starintel.actor/actor/%2E./user-hunt"
                  "star://gserver.starintel.actor/actor/.%2E/user-hunt"))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri text)))
             text)))

(test percent-encoded-dots-inside-a-segment-do-not-create-dot-segments
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/user%2Ehunt")))
    (is (equal '("user.hunt") (star.star-uri:star-uri-resource-path uri)))))

(test percent-encoded-unreserved-characters-normalize
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/user%2Dhunt")))
    (is (equal '("user-hunt") (star.star-uri:star-uri-resource-path uri)))
    (is (string= "star://gserver.starintel.actor/actor/user-hunt"
                 (star.star-uri:serialize-star-uri uri)))))

(test lowercase-percent-hex-normalizes-to-uppercase-when-encoding-remains
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver.starintel.actor/actor/a%2fb%3fc")))
    (is (equal '("a%2Fb%3Fc") (star.star-uri:star-uri-resource-path uri)))
    (is (string= "star://gserver.starintel.actor/actor/a%2Fb%3Fc"
                 (star.star-uri:serialize-star-uri uri)))))

(test authority-percent-escapes-normalize-and-lowercase
  (let ((uri (star.star-uri:parse-star-uri
              "star://gserver%2Estarintel%2eACTOR/actor/user-hunt")))
    (is (string= "gserver.starintel.actor" (star.star-uri:star-uri-authority uri)))))

(test malformed-percent-escapes-are-rejected
  (dolist (text '("star://gserver.starintel.actor/actor/%"
                  "star://gserver.starintel.actor/actor/%2"
                  "star://gserver.starintel.actor/actor/%ZZ"
                  "star://gserver.starintel.actor/actor/%2G"
                  "star://gserver.starintel.actor/actor/user-hunt%"))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri text)))
             text)))

(test path-characters-outside-pchar-are-rejected
  (dolist (text '("star://gserver.starintel.actor/actor/user hunt"
                  "star://gserver.starintel.actor/actor/user\"hunt"
                  "star://gserver.starintel.actor/actor/user[hunt]"
                  "star://gserver.starintel.actor/actor/user<hunt"))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri text)))
             text)))

(test empty-authority-resource-kind-and-empty-segments-are-rejected
  (dolist (text '("star://"
                  "star:///actor/user-hunt"
                  "star://gserver.starintel.actor"
                  "star://gserver.starintel.actor/"
                  "star://gserver.starintel.actor//user-hunt"
                  "star://gserver.starintel.actor/actor/"
                  "star://gserver.starintel.actor/actor//user-hunt"))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri text)))
             text)))

(test unknown-resource-kind-is-rejected
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri
               "star://gserver.starintel.actor/widget/01K")))))

(test non-string-and-empty-input-is-rejected
  (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri nil))))
  (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri ""))))
  (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri "http://example.org/actor/x"))))
  (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-star-uri "star:////actor/x")))))

(test canonical-star-uri-p-distinguishes-canonical-from-canonicalizable
  (is-true (star.star-uri:canonical-star-uri-p
            "star://gserver.starintel.actor/actor/user-hunt"))
  (is-true (star.star-uri:canonical-star-uri-p
            "star://gserver.starintel.actor/actor/a%2Fb"))
  ;; Parseable but not already canonical.
  (is-false (star.star-uri:canonical-star-uri-p
             "STAR://GServer.StarIntel.Actor/actor/User-Hunt"))
  (is-false (star.star-uri:canonical-star-uri-p
             "star://gserver.starintel.actor/actor/user%2Dhunt"))
  (is-false (star.star-uri:canonical-star-uri-p
             "star://gserver.starintel.actor/actor/a%2fb"))
  ;; Not even parseable.
  (is-false (star.star-uri:canonical-star-uri-p
             "star://gserver.starintel.actor/actor/../user-hunt")))

(test valid-star-uri-p-accepts-canonicalizable-input
  (is-true (star.star-uri:valid-star-uri-p
            "star://gserver.starintel.actor/actor/user%2Dhunt"))
  (is-false (star.star-uri:valid-star-uri-p
             "star://gserver.starintel.actor:5672/actor/user-hunt")))

(test canonicalization-is-idempotent
  (let ((once (star.star-uri:canonicalize-star-uri
               "STAR://GServer.StarIntel.Actor/actor/user%2Dhunt")))
    (is (string= "star://gserver.starintel.actor/actor/user-hunt" once))
    (is (string= once (star.star-uri:canonicalize-star-uri once)))))

(test actor-star-uri-p-requires-actor-kind-and-resource-path
  (is-true (star.star-uri:actor-star-uri-p
            "star://bbpd.starintel.actor/actor/subfinder"))
  (is-true (star.star-uri:actor-star-uri-p
            (star.star-uri:parse-star-uri
             "star://gserver.starintel.actor/actor/quasar/user-hunt")))
  (is-false (star.star-uri:actor-star-uri-p
             "star://gserver.starintel.actor/service/starintel-mcp"))
  ;; Grammar permits a bare resource kind, but it names no actor resource.
  (is-false (star.star-uri:actor-star-uri-p
             "star://gserver.starintel.actor/actor")))

;;;; Legacy star://domain:address:actor-name compatibility input.

(test legacy-tuple-parses-through-the-compatibility-api-only
  (let ((legacy (star.star-uri:parse-legacy-star-uri
                 "star://quasar:localhost:user-hunt")))
    (is-true (star.star-uri:legacy-star-actor-tuple-p legacy))
    (is (string= "quasar" (star.star-uri:legacy-star-actor-tuple-domain legacy)))
    (is (string= "localhost" (star.star-uri:legacy-star-actor-tuple-address legacy)))
    (is (string= "user-hunt" (star.star-uri:legacy-star-actor-tuple-actor-name legacy)))))

(test legacy-tuple-syntax-is-validated
  (dolist (text '("star://quasar:user-hunt"
                  "star://quasar:localhost:user-hunt:extra"
                  "star://:localhost:user-hunt"
                  "star://quasar::user-hunt"
                  "star://quasar:localhost:"
                  "star://Quasar:localhost:user-hunt"
                  "star://quasar:local host:user-hunt"
                  "star://quasar:localhost:user/hunt"
                  "star://quasar:localhost:user-hunt/suffix"
                  "star://gserver.starintel.actor/actor/user-hunt"
                  "http://quasar:localhost:user-hunt"
                  ""))
    (is-true (star-uri-condition-of (lambda () (star.star-uri:parse-legacy-star-uri text)))
             text)))

(test legacy-tuple-is-not-a-canonical-star-uri
  (is-true (star.star-uri:legacy-star-actor-uri-p "star://quasar:localhost:user-hunt"))
  (is-false (star.star-uri:legacy-star-actor-uri-p
             "star://gserver.starintel.actor/actor/user-hunt"))
  (is-true (star-uri-condition-of
            (lambda ()
              (star.star-uri:parse-star-uri "star://quasar:localhost:user-hunt")))))

(test legacy-migration-requires-an-explicit-canonical-target
  (let ((legacy (star.star-uri:parse-legacy-star-uri
                 "star://quasar:localhost:user-hunt")))
    (is-true (star-uri-condition-of
              (lambda () (star.star-uri:migrate-legacy-star-uri legacy)))
            "no automatic mapping")
    (is-true (star-uri-condition-of
              (lambda ()
                (star.star-uri:migrate-legacy-star-uri
                 legacy
                 :to "star://gserver.starintel.actor/actor/user%2Dhunt")))
            "canonicalizable-but-not-canonical target")
    (is-true (star-uri-condition-of
              (lambda ()
                (star.star-uri:migrate-legacy-star-uri
                 legacy
                 :to "STAR://GServer.StarIntel.Actor/actor/user-hunt")))
            "non-canonical target")
    (is (string= "star://gserver.starintel.actor/actor/user-hunt"
                 (star.star-uri:migrate-legacy-star-uri
                  legacy
                  :to "star://gserver.starintel.actor/actor/user-hunt")))))

(test legacy-migration-accepts-an-explicit-migration-map
  (let ((legacy (star.star-uri:parse-legacy-star-uri
                 "star://bbp:localhost:nmap")))
    (is (string= "star://gserver.starintel.actor/actor/user-hunt"
                 (star.star-uri:migrate-legacy-star-uri
                  legacy
                  :map '(("star://bbp:localhost:nmap"
                          . "star://gserver.starintel.actor/actor/user-hunt")))))
    (is-true (star-uri-condition-of
              (lambda ()
                (star.star-uri:migrate-legacy-star-uri
                 legacy
                 :map '(("star://bbp:localhost:other"
                         . "star://gserver.starintel.actor/actor/other"))))))))

(test legacy-domain-is-never-mapped-to-an-authority
  "STAR-SERVER-041 forbids automatic domain->authority rewrites such as
legacy.<domain> or domain -> first path segment."
  (let ((legacy (star.star-uri:parse-legacy-star-uri
                 "star://quasar:localhost:user-hunt")))
    ;; The only permitted migration is the explicitly supplied target.
    (is (string= "star://bbpd.starintel.actor/actor/subfinder"
                 (star.star-uri:migrate-legacy-star-uri
                  legacy :to "star://bbpd.starintel.actor/actor/subfinder")))))

(test replacement-marker-is-present
  (is-true (search "StarLang" star.star-uri:+star-uri-implementation-replacement-marker+)))
