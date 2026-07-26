(in-package :star.actors)

(defmacro with-json (jobject &body body)
  "Bind safe document readers for actor code.

The object is parsed once. Dtype resolution uses the versioned class registry and
never interns request-controlled text."
  (let ((document (gensym "DOCUMENT")))
    `(let ((,document (star.documents:parse-document-object ,jobject)))
       (flet ((val (key &optional default)
                (star.documents:document-field-value ,document key default))
              (dataset ()
                (star.documents:document-dataset ,document))
              (date-added ()
                (star.documents:document-date-added ,document))
              (date-updated ()
                (star.documents:document-date-updated ,document))
              (dtype ()
                (star.documents:document-dtype ,document))
              (parse-doc ()
                (star.documents:decode-document ,document)))
         ,@body))))
