(defpackage #:star.request-audit
  (:use #:cl)
  (:export #:analyze-request #:lexical-signals #:default-policy #:valid-signals-p
           #:assessment #:assessment-local-signals #:assessment-model-signals
           #:assessment-flags #:assessment-rule-ids #:assessment-refuse-p
           #:assessment-status #:policy-result #:make-policy-result
           #:policy-result-flags #:policy-result-rule-ids #:policy-result-refuse-p
           #:make-local-classifier #:install #:audit-policy-engine
           #:*input-text* #:*subject-key* #:*request-tenant*))
(in-package #:star.request-audit)

(defparameter +signals+
  '("identity-linking" "sensitive-details" "location-tracking"
    "exposure-intent" "harassment-intent" "protective-context"))
(defparameter +flags+
  '("potential-doxxing-setup" "targeted-location-risk" "sensitive-data-exposure"
    "model-review" "assessment-unavailable" "custom-policy"))
(defparameter +maximum-input+ 8192)

(defstruct policy-result
  (flags nil :read-only t) (rule-ids nil :read-only t) (refuse-p nil :read-only t))
(defstruct assessment
  (local-signals nil :read-only t) (model-signals nil :read-only t)
  (flags nil :read-only t) (rule-ids nil :read-only t)
  (refuse-p nil :read-only t) (status "rules-only" :read-only t))

(defun bounded-list-p (value maximum)
  "Bound traversal even for malformed, dotted or circular callback results."
  (loop for tail = value then (cdr tail)
        for count from 0 to maximum
        when (null tail) return t
        unless (consp tail) return nil
        finally (return nil)))

(defun valid-signals-p (signals)
  (and (bounded-list-p signals (length +signals+))
       (every (lambda (x) (and (stringp x) (member x +signals+ :test #'string=))) signals)
       (= (length signals) (length (remove-duplicates signals :test #'string=)))))

(defun safe-rule-id-p (value)
  (and (stringp value) (<= 1 (length value) 64)
       (every (lambda (c) (or (find c "abcdefghijklmnopqrstuvwxyz0123456789-")
                              (char= c #\.))) value)))

(defun valid-result-p (result)
  (and (policy-result-p result)
       (member (policy-result-refuse-p result) '(nil t))
       (bounded-list-p (policy-result-flags result) 6)
       (every (lambda (x) (and (stringp x) (member x +flags+ :test #'string=)))
              (policy-result-flags result))
       (bounded-list-p (policy-result-rule-ids result) 16)
       (every #'safe-rule-id-p (policy-result-rule-ids result))
       (or (not (policy-result-refuse-p result)) (policy-result-rule-ids result))))

(defun includes-any-p (text phrases)
  (some (lambda (phrase) (search phrase text)) phrases))

(defun lexical-signals (text)
  "Conservative English intent hints, not a comprehensive abuse detector.
Do not feed social-post/document content to this function as user intent."
  (check-type text string)
  (when (> (length text) +maximum-input+) (error "Audit input limit exceeded"))
  (let ((text (string-downcase text)))
    (loop for (signal . phrases) in
          '(("identity-linking" "deanonymize" "de-anonymize" "real name behind"
                                "link this username" "identify the person behind")
            ("sensitive-details" "home address" "private phone" "personal phone"
                                 "family members" "private email")
            ("location-tracking" "track their location" "track where they live"
                                 "follow them home" "find where they live")
            ("exposure-intent" "publish their" "leak their" "post their address"
                               "expose their" "share their private")
            ("harassment-intent" "harass them" "intimidate them" "threaten them"
                                 "swat them" "send threats")
            ("protective-context" "remove my" "redact my" "protect my"
                                  "report doxxing" "prevent doxxing"))
          when (includes-any-p text phrases) collect signal)))

(defun signal-p (signals name) (not (null (member name signals :test #'string=))))

(defun default-policy (local model)
  "Model hints can flag a review; they cannot independently cause refusal.
Protective language is context, never an authorization/consent override."
  (let* ((all (union local model :test #'string=))
         (sensitive (signal-p all "sensitive-details"))
         (identity (signal-p all "identity-linking"))
         (location (signal-p all "location-tracking"))
         (hostile (or (signal-p all "exposure-intent")
                      (signal-p all "harassment-intent")))
         (local-hostile (or (signal-p local "exposure-intent")
                            (signal-p local "harassment-intent")))
         (local-personal (or (signal-p local "sensitive-details")
                             (signal-p local "location-tracking")
                             (signal-p local "identity-linking")))
         (flags nil) (rules nil))
    (when (and identity (or sensitive location))
      (push "potential-doxxing-setup" flags) (push "identity-plus-personal.v1" rules))
    (when (and location (or identity hostile))
      (push "targeted-location-risk" flags) (push "targeted-location.v1" rules))
    (when (and hostile (or sensitive identity location))
      (push "sensitive-data-exposure" flags) (push "personal-exposure.v1" rules))
    (when (set-difference model local :test #'string=)
      (push "model-review" flags) (push "unverified-model-signals.v1" rules))
    (when (and local-hostile local-personal)
      (pushnew "local-personal-abuse.v1" rules :test #'string=))
    (make-policy-result :flags (nreverse flags) :rule-ids (nreverse rules)
                        :refuse-p (and local-hostile local-personal))))

(defun unavailable-assessment (status &optional local)
  (make-assessment :status status :local-signals local
                   :flags '("assessment-unavailable")
                   :rule-ids '("assessment-unavailable.v1")))

(defun analyze-request (text &key classifier (policy #'default-policy))
  "Run bounded local logic and an optional evidence-only classifier.
CLASSIFIER must enforce its I/O deadline and return a closed list of signals.
Malformed/model failures are visible, never silently labeled safe."
  (unless (or (null text) (stringp text)) (error "Invalid audit input"))
  (unless (and (functionp policy) (or (null classifier) (functionp classifier)))
    (error "Invalid audit callback"))
  (when (or (null text) (zerop (length text)))
    (return-from analyze-request (make-assessment :status "no-input")))
  (when (> (length text) +maximum-input+)
    (return-from analyze-request (unavailable-assessment "input-too-large")))
  (let ((local (lexical-signals text)) (model nil)
        (status (if classifier "assessed" "rules-only")))
    (when classifier
      (handler-case
          (progn
            (setf model (funcall classifier text))
            (unless (valid-signals-p model) (error "Invalid model signals")))
        (error () (setf model nil status "unavailable"))))
    (handler-case
        (let ((result (funcall policy (mapcar #'copy-seq local) (mapcar #'copy-seq model))))
          (unless (valid-result-p result) (error "Invalid policy result"))
          (make-assessment
           :local-signals local :model-signals model :status status
           :flags (remove-duplicates
                   (append (policy-result-flags result)
                           (when (string= status "unavailable") '("assessment-unavailable")))
                   :test #'string=)
           :rule-ids (append (copy-list (policy-result-rule-ids result))
                             (when (string= status "unavailable") '("assessment-unavailable.v1")))
           :refuse-p (policy-result-refuse-p result)))
      (error () (unavailable-assessment "policy-error" local)))))
