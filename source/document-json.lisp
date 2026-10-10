(in-package :star.documents)

(defun native-json-to-document (value)
  "Adapt the pinned exact JSON representation to existing JSOWN sequence contracts."
  (cond
    ((hash-table-p value)
     (let ((pairs nil))
       (maphash (lambda (key item) (push (cons key (native-json-to-document item)) pairs)) value)
       (cons :obj (nreverse pairs))))
    ((stringp value) value)
    ((vectorp value) (map 'list #'native-json-to-document value))
    ((eq value t) :true)
    ((null value) :false)
    ((eq value 'null) :null)
    (t value)))

(defun document-to-native-json (value)
  "Adapt JSON document values without a lossy serialization intermediate."
  (labels ((convert (item ancestors)
             (when (member item ancestors :test #'eq)
               (error "Cyclic document JSON"))
             (cond
               ((or (stringp item) (numberp item) (starintel:json-number-p item)) item)
               ((member item '(t :true :t)) t)
               ((member item '(:false :f)) nil)
               ((member item '(:null :n)) 'null)
               ((and (consp item) (eq (car item) :obj))
                (let ((object (make-hash-table :test #'equal)))
                  (dolist (pair (cdr item))
                    (unless (and (consp pair) (stringp (car pair)))
                      (error "Invalid document JSON object key"))
                    (when (nth-value 1 (gethash (car pair) object))
                      (error "Duplicate document JSON object key"))
                    (setf (gethash (car pair) object) (convert (cdr pair) (cons item ancestors))))
                  object))
               ((or (listp item) (vectorp item))
                (when (and (listp item) (not (integerp (list-length item))))
                  (error "Circular document JSON array"))
                (map 'vector (lambda (child) (convert child (cons item ancestors))) item))
               (t (error "Unsupported document JSON value ~s" item)))))
    (convert value nil)))

(defun parse-json-value (text)
  "Read exact JSON through the pinned StarIntel codec, retaining JSOWN arrays as lists."
  (native-json-to-document (starintel:parse-json text)))

(defun clone-json-value (value)
  "Copy a JSON value without rounding number tokens or collapsing literals."
  (native-json-to-document (document-to-native-json value)))

(defmethod jsown:to-json ((value starintel:json-number))
  "Write only the canonical library's exact-number type through its own authority."
  (starintel:stringify-json value))
