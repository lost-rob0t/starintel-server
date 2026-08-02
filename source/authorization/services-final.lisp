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

(defun decode-view-key (key)
  "Decode CouchDB composite keys without invoking the Lisp reader."
  (typecase key
    (list
     (normalize-view-key-sequence key))
    (string
     (or
      (handler-case
          (normalize-view-key-sequence (jsown:parse key))
        (error () nil))
      (split-printed-view-key key)
      (error "Failed to decode view key ~s" key)))
    (vector
     (normalize-view-key-sequence key))
    (t key)))
