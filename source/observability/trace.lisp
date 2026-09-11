(in-package :star.observability)

(defun random-bytes (length)
  "Return LENGTH cryptographically random bytes."
  (ironclad:random-data length))

(defun random-hex (length)
  "Return LENGTH random bytes as a lowercase hex string."
  (ironclad:byte-array-to-hex-string (random-bytes length)))

(defun make-trace-id ()
  "A valid W3C trace id: 16 random bytes, never all-zero."
  (let ((id (random-hex 16)))
    (if (string= id "00000000000000000000000000000000")
        (make-trace-id)
        id)))

(defun make-span-id ()
  "A valid W3C span id: 8 random bytes, never all-zero."
  (let ((id (random-hex 8)))
    (if (string= id "0000000000000000")
        (make-span-id)
        id)))

(defstruct trace-context
  trace-id
  span-id
  (parent-span-id nil)
  (trace-flags "01")
  (tracestate nil))

(defun make-root-context ()
  "Create a fresh root trace context."
  (make-trace-context :trace-id (make-trace-id) :span-id (make-span-id)))

(defun encode-traceparent (context)
  "Render CONTEXT as a W3C traceparent header value."
  (format nil "00-~a-~a-~a"
          (trace-context-trace-id context)
          (trace-context-span-id context)
          (trace-context-trace-flags context)))

(defun valid-hex-p (string length)
  (and (= (length string) length)
       (every (lambda (c) (or (digit-char-p c 16) (char= c #\a) (char= c #\b)
                              (char= c #\c) (char= c #\d) (char= c #\e)
                              (char= c #\f)))
              string)
       (not (every (lambda (c) (char= c #\0)) string))))

(defun parse-traceparent (value)
  "Parse a W3C traceparent header value into a trace-context, or NIL.
Rejects malformed versions, non-hex ids and all-zero ids."
  (when (and value (>= (length value) 55))
    (let* ((parts (uiop:split-string value :separator "-")))
      (when (and parts (= (length parts) 4))
        (let ((version (first parts))
              (trace-id (second parts))
              (span-id (third parts))
              (flags (fourth parts)))
          (when (and (or (string= version "00")
                         (not (string= version "ff")))
                     (valid-hex-p trace-id 32)
                     (valid-hex-p span-id 16)
                     (>= (length flags) 2))
            (make-trace-context
             :trace-id (string-downcase trace-id)
             :span-id (string-downcase span-id)
             :trace-flags (subseq (string-downcase flags) 0 2))))))))

(defvar *current-trace-context* nil
  "Dynamic current trace context. Bound per request/consumer thread.")

(defun current-context ()
  "The current trace context, creating a fresh root when none is bound."
  (or *current-trace-context* (make-root-context)))

(defun child-context (parent)
  "Derive a child context of PARENT with a fresh span id."
  (make-trace-context
   :trace-id (trace-context-trace-id parent)
   :span-id (make-span-id)
   :parent-span-id (trace-context-span-id parent)
   :trace-flags (trace-context-trace-flags parent)
   :tracestate (trace-context-tracestate parent)))
