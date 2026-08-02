(in-package :star.databases.couchdb)

(defun checked-in-design-document-map ()
  "Return design documents embedded in the runtime image.

The packaged server does not retain the ASDF source tree, so runtime startup
validation must use the JSON definitions captured in STAR:*COUCHDB-VIEWS* at
build time instead of reopening source/views from disk."
  (let ((documents (make-hash-table :test #'equal)))
    (dolist (json star:*couchdb-views* documents)
      (let* ((document
               (jsown:with-injective-reader
                 (jsown:parse json)))
             (id (jsown:val-safe document "_id")))
        (unless (and (stringp id)
                     (uiop:string-prefix-p "_design/" id))
          (error 'view-registry-error
                 :reason
                 (format nil "embedded view document has invalid _id: ~s" id)))
        (setf (gethash (subseq id (length "_design/")) documents)
              document)))))
