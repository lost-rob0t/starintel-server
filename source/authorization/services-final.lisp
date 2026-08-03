(in-package :star.authorization)

(defun split-printed-view-key (text)
  "Decode CouchDB's non-JSON printed composite-key form.

Examples include [dataset-1 tenant-1]. This parser is intentionally narrow and
never invokes the Common Lisp reader."
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (size (length trimmed)))
    (when (and (> size 1)
               (char= #\[ (char trimmed 0))
               (char= #\] (char trimmed (1- size))))
      (remove-if
       (lambda (token)
         (zerop (length token)))
       (uiop:split-string
        (subseq trimmed 1 (1- size))
        :separator '(#\Space #\Tab #\Newline #\Return))))))

(defun normalize-view-key-sequence (value)
  (cond
    ((listp value)
     (mapcar #'princ-to-string value))
    ((vectorp value)
     (map 'list #'princ-to-string value))
    (t value)))

(defun json-encoded-view-key-p (key)
  "Return true when KEY looks like a JSON-encoded array/object of quoted
values rather than CouchDB's bare printed composite-key form
(e.g. [dataset-1 tenant-1]).

jsown's reader treats a bare leading ``t`` as JSON ``true`` and a bare
leading ``f``/``n`` as ``false``/``null``, so feeding it the printed form
yields spurious tokens (e.g. ``[dataset-1 tenant-1]`` -> ``(T)``).  Only
strings whose first value starts with a JSON string quote are safe to hand
to jsown."
  (declare (type string key))
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) key))
         (length (length trimmed)))
    (and (>= length 2)
         (find (char trimmed 0) "[{")
         (loop for index from 1 below length
               for char = (char trimmed index)
               while (find char " " :test #'char=)
               finally (return (and char (char= char #\")))))))

(defun decode-view-key (key)
  "Decode CouchDB composite keys without invoking the Lisp reader."
  (cond
    ((stringp key)
     (if (json-encoded-view-key-p key)
         (handler-case
             (normalize-view-key-sequence (jsown:parse key))
           (error ()
             (or (split-printed-view-key key)
                 (error "Failed to decode view key ~s" key))))
         (or (split-printed-view-key key)
             (error "Failed to decode view key ~s" key))))
    ((listp key)
     (normalize-view-key-sequence key))
    ((vectorp key)
     (normalize-view-key-sequence key))
    (t key)))

(defun lucene-quoted-escape (value)
  "Escape only syntax that is special inside a quoted Lucene term."
  (with-output-to-string (stream)
    (loop for character across value
          do (when (or (char= character #\\)
                       (char= character #\"))
               (write-char #\\ stream))
             (write-char character stream))))

(defun lucene-term (field value)
  (format nil "~a:\"~a\"" field (lucene-quoted-escape value)))
