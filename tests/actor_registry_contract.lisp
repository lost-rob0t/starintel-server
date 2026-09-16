;;;; Dependency-free fresh-process proof for the pure actor registry.

(require :asdf)

(defun repository-path (relative)
  (merge-pathnames relative
                   (make-pathname :name nil :type nil
                                  :defaults *load-truename*)))

(load (repository-path "../source/actor-registry-package.lisp"))
(load (repository-path "../source/actor-registry.lisp"))

(defun check (truth control &rest arguments)
  (unless truth
    (error (apply #'format nil control arguments))))

(defun semantic-digest (character)
  (format nil "sha256:~a" (make-string 64 :initial-element character)))

(defun actor-contract (name &key (accepts #()) (produces #())
                                  (capabilities #("run")))
  `(("name" . ,name)
    ("runtime" . "external")
    ("protocol" . "rabbitmq")
    ("endpoint" . "private-runtime-endpoint")
    ("accepts" . ,accepts)
    ("produces" . ,produces)
    ("capabilities" . ,capabilities)
    ("serviceUri" . ,(format nil
                              "star://starintel-pro-actors:localhost:~a"
                              name))))

(defun portable-manifest (name actor &optional (digest (semantic-digest #\a)))
  `(("wireVersion" . 1)
    ("library" . (("name" . ,name)
                   ("version" . "0.9.1.2")
                   ("digest" . ,digest)))
    ("imports" . #())
    ("types" . #())
    ("predicates" . #())
    ("messages" . #())
    ("actors" . ,(vector actor))))

(defun rejected-with-code-p (code thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (star.actor-registry:actor-registry-error (condition)
      (string= code
               (star.actor-registry:actor-registry-error-code condition)))))

(let* ((mastodon
         (portable-manifest
          "mastodon"
          (actor-contract "mastodon"
                          :accepts #("starintel/instance@1"
                                     "starintel/profile@1")
                          :produces #("starintel/post@1"))))
       (fediwatch
         (portable-manifest
          "fediwatch"
          (actor-contract "fediwatch"
                          :accepts #("starintel/actor-config@0.9.2")
                          :produces #("starintel/post@1"))
          (semantic-digest #\b)))
       (left (star.actor-registry:build-actor-registry
              (vector mastodon fediwatch)))
       (right (star.actor-registry:build-actor-registry
               (vector fediwatch mastodon)))
       (left-list (star.actor-registry:actor-registry-list left))
       (right-list (star.actor-registry:actor-registry-list right))
       (mastodon-uri "star://starintel-pro-actors:localhost:mastodon")
       (projection (star.actor-registry:actor-registry-find left mastodon-uri))
       (bespoke '(("schema_version" . "1.0")
                  ("actor_id" . "mastodon"))))
  (check (= 2 (star.actor-registry:actor-registry-count left))
         "registry count changed")
  (check (equalp left-list right-list)
         "input order changed deterministic registry projection")
  (check (string= mastodon-uri (getf projection :service-uri))
         "exact service URI lookup failed")
  (check (string= "legacy_star_service_uri_v1"
                  (getf projection :identity-status))
         "compatibility identity status missing")
  (check (null (getf projection :endpoint))
         "private endpoint leaked into safe projection")
  (check (= 1 (length
               (star.actor-registry:query-actor-registry
                left :accepts "starintel/actor-config@0.9.2")))
         "accepted-type query failed")
  (check (null (star.actor-registry:actor-registry-find
                left "star://starintel-pro-actors:localhost:masto"))
         "lookup was not exact")
  (check
   (rejected-with-code-p
    "duplicate_service_uri"
    (lambda ()
      (star.actor-registry:build-actor-registry
       (vector mastodon mastodon))))
   "duplicate URI did not fail closed")
  (check
   (rejected-with-code-p
    "invalid_semantic_digest"
    (lambda ()
      (star.actor-registry:build-actor-registry
       (vector
        (portable-manifest
         "bad" (actor-contract "bad") "sha256:NOT-A-DIGEST")))))
   "invalid digest did not fail closed")
  (check
   (rejected-with-code-p
    "unknown_field"
    (lambda ()
      (star.actor-registry:build-actor-registry
       (vector bespoke))))
   "bespoke non-StarLang descriptor was accepted"))

(format t "ACTOR-REGISTRY-CONTRACT-OK~%")
