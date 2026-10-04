(in-package :star.databases.couchdb)

(defparameter +geo-projection-kind+ "starintel.geo-projection.v1"
  "Server-internal rebuildable CouchDB JSON projection for explicit geography.")

(defun geo-json-object-p (value)
  (and (consp value) (eq (car value) :obj)))

(defun geo-json-document (value)
  (cond
    ((null value) nil)
    ((geo-json-object-p value) value)
    ((stringp value)
     (jsown:with-injective-reader
       (jsown:parse value)))
    (t nil)))

(defun geo-document-id (document)
  (or (star.documents:document-value document "_id" nil)
      (star.documents:document-value document "id" nil)))

(defun geo-document-tenant (document)
  (or (star.documents:document-value document "tenant_id" nil)
      (star.documents:document-value document "tenant" nil)
      "default"))

(defun geo-reference-id (value)
  (cond
    ((and (stringp value) (plusp (length value))) value)
    ((geo-json-object-p value)
     (or (jsown:val-safe value "id")
         (jsown:val-safe value "_id")
         (jsown:val-safe value "ref")))
    (t nil)))

(defun usable-geo-coordinate-p (value minimum maximum)
  (cond
    ((realp value) (<= minimum value maximum))
    ((stringp value)
     (handler-case
         (progn
           (decimal-coordinate value minimum maximum "coordinate")
           t)
       (invalid-geo-bbox () nil)))
    (t nil)))

(defun geo-point-document-p (document)
  (and document
       (string= "geo-point"
                (or (star.documents:document-value document "dtype" nil) ""))
       (usable-geo-coordinate-p
        (star.documents:document-value document "longitude" nil) -180 180)
       (usable-geo-coordinate-p
        (star.documents:document-value document "latitude" nil) -90 90)))

(defun fetch-geo-reference (fetch-fn reference)
  (let ((id (geo-reference-id reference)))
    (and id (geo-json-document (funcall fetch-fn id)))))

(defun resolve-explicit-geo-point (document fetch-fn &key (max-depth 4))
  "Resolve only explicit location/geometry/address references to one geo-point.

Returns the point and the exact document-id path. No fuzzy relation, proximity,
name, wireless, or media correlation participates in this function."
  (labels ((walk (current path depth visited)
             (when (and current (<= depth max-depth))
               (let ((id (geo-document-id current)))
                 (when (and id (gethash id visited))
                   (return-from walk (values nil nil)))
                 (when id (setf (gethash id visited) t))
                 (when (geo-point-document-p current)
                   (return-from walk
                     (values current
                             (if id (append path (list id)) path))))
                 (dolist (field '("geometry" "location" "address"))
                   (let* ((reference
                            (star.documents:document-value current field nil))
                          (referenced
                            (fetch-geo-reference fetch-fn reference)))
                     (when referenced
                       (multiple-value-bind (point point-path)
                           (walk referenced
                                 (if id (append path (list id)) path)
                                 (1+ depth)
                                 visited)
                         (when point
                           (return-from walk
                             (values point point-path)))))))
                 (values nil nil)))))
    (walk document nil 0 (make-hash-table :test #'equal))))

(defun geo-projection-id (subject)
  (let* ((subject-id (geo-document-id subject))
         (dataset
           (or (star.documents:document-value subject "dataset" nil) ""))
         (tenant (geo-document-tenant subject))
         (material
           (format nil "~d:~a|~d:~a|~d:~a"
                   (length tenant) tenant
                   (length dataset) dataset
                   (length subject-id) subject-id))
         (digest
           (ironclad:byte-array-to-hex-string
            (ironclad:digest-sequence
             :sha256
             (babel:string-to-octets material :encoding :utf-8)))))
    (format nil "geo-projection:~a" digest)))

(defun geo-projection-document-p (document)
  (and document
       (string=
        +geo-projection-kind+
        (or (star.documents:document-value document "kind" nil) ""))))

(defun build-geo-projection (subject fetch-fn)
  "Build one rebuildable projection for SUBJECT's explicit point anchor.

Direct geo-point documents need no projection because the CouchDB geo index
already sees their own coordinates. Returns NIL when no explicit point path
exists. SUBJECT is never mutated."
  (let ((subject (geo-json-document subject)))
    (unless subject
      (return-from build-geo-projection nil))
    (when (or (geo-projection-document-p subject)
              (geo-point-document-p subject))
      (return-from build-geo-projection nil))
    (let* ((subject-id (geo-document-id subject))
           (dataset (star.documents:document-value subject "dataset" nil)))
      (unless (and (stringp subject-id) (plusp (length subject-id))
                   (stringp dataset) (plusp (length dataset)))
        (return-from build-geo-projection nil))
      (multiple-value-bind (point path)
          (resolve-explicit-geo-point subject fetch-fn)
        (when point
          (let* ((point-id (geo-document-id point))
                 (projection
                   (jsown:new-js
                     ("_id" (geo-projection-id subject))
                     ("kind" +geo-projection-kind+)
                     ("projectionOnly" :true)
                     ("participationKind" "anchored")
                     ("geometrySource" "explicit_reference")
                     ("subjectId" subject-id)
                     ("subjectDtype"
                      (or (star.documents:document-value subject "dtype" nil)
                          "unknown"))
                     ("dataset" dataset)
                     ("tenant_id" (geo-document-tenant subject))
                     ("geometryId" point-id)
                     ("longitude"
                      (star.documents:document-value point "longitude" nil))
                     ("latitude"
                      (star.documents:document-value point "latitude" nil))
                     ("relationPath" path))))
            (let ((subject-revision
                    (star.documents:document-value subject "_rev" nil))
                  (geometry-revision
                    (star.documents:document-value point "_rev" nil)))
              (when subject-revision
                (setf (jsown:val projection "subjectRevision")
                      subject-revision))
              (when geometry-revision
                (setf (jsown:val projection "geometryRevision")
                      geometry-revision)))
            projection))))))

(defun same-geo-scope-p (projection subject)
  (and
   (string=
    (or (star.documents:document-value projection "dataset" nil) "")
    (or (star.documents:document-value subject "dataset" nil) ""))
   (string=
    (geo-document-tenant projection)
    (geo-document-tenant subject))))

(defun resolve-geo-search-projections (response fetch-fn)
  "Replace internal projection hits with their canonical subject documents.

Stale, missing, or scope-mismatched projection rows are dropped. This prevents
server-internal projection JSON from leaking through the public bbox API."
  (let* ((parsed (geo-json-document response))
         (rows (and parsed (jsown:val-safe parsed "rows")))
         (row-list
           (cond ((null rows) nil)
                 ((listp rows) rows)
                 ((vectorp rows) (coerce rows 'list))
                 (t nil)))
         (seen (make-hash-table :test #'equal))
         (resolved
           (loop for row in row-list
                 for document = (jsown:val-safe row "doc")
                 for projection-p = (geo-projection-document-p document)
                 for candidate =
                   (if projection-p
                       (let* ((subject-id
                                (star.documents:document-value
                                 document "subjectId" nil))
                              (subject
                                (and subject-id
                                     (geo-json-document
                                      (funcall fetch-fn subject-id)))))
                         (and subject
                              (same-geo-scope-p document subject)
                              subject))
                       document)
                 for candidate-id = (and candidate (geo-document-id candidate))
                 when (and candidate
                           candidate-id
                           (not (gethash candidate-id seen)))
                   collect
                   (progn
                     (setf (gethash candidate-id seen) t)
                     (setf (jsown:val row "doc") candidate)
                     row))))
    (when parsed
      (setf (jsown:val parsed "rows")
            (if (vectorp rows) (coerce resolved 'vector) resolved))
      (when (jsown:keyp parsed "total_rows")
        (setf (jsown:val parsed "total_rows") (length resolved))))
    (if (stringp response)
        (jsown:to-json parsed)
        parsed)))

(defun couchdb-fetch-geo-document (client database id)
  (handler-case
      (jsown:with-injective-reader
        (jsown:parse (cl-couch:get-document client database id)))
    (dexador:http-request-not-found () nil)))

(defun couchdb-save-geo-projection (client database projection)
  (let* ((projection-id (jsown:val projection "_id"))
         (existing
           (couchdb-fetch-geo-document client database projection-id)))
    (when existing
      (let ((revision (jsown:val-safe existing "_rev")))
        (when revision
          (setf (jsown:val projection "_rev") revision))))
    (cl-couch:create-document client database (jsown:to-json projection))
    projection))

(defun couchdb-delete-geo-projection (client database subject)
  (let* ((projection-id (geo-projection-id subject))
         (existing
           (couchdb-fetch-geo-document client database projection-id)))
    (when existing
      (let ((revision (jsown:val-safe existing "_rev")))
        (when revision
          (cl-couch:delete-document
           client database projection-id revision))))
    nil))

(defun couchdb-refresh-geo-projection (client database subject)
  "Create/update/delete SUBJECT's explicit geographic projection."
  (let ((subject (geo-json-document subject)))
    (when (and subject
               (geo-document-id subject)
               (not (geo-projection-document-p subject)))
      (let ((projection
              (build-geo-projection
               subject
               (lambda (id)
                 (couchdb-fetch-geo-document client database id)))))
        (if projection
            (couchdb-save-geo-projection client database projection)
            (couchdb-delete-geo-projection client database subject))))))

(defun couchdb-geo-dependent-documents (client database document-id)
  (let* ((response
           (query-view
            client database
            "geo" "references"
            :key document-id
            :include-docs t
            :reduce nil))
         (rows (or (jsown:val-safe response "rows") nil)))
    (loop for row in rows
          for document = (jsown:val-safe row "doc")
          when document collect document)))

(defun couchdb-refresh-geo-projections-for-change
    (client database document &key (max-depth 4))
  "Refresh DOCUMENT and explicitly dependent documents after one mutation.

The dependency walk is bounded and cycle-safe. It follows only the checked-in
CouchDB reference view, so proximity or similarity can never create an anchor."
  (let ((queue (list (cons (geo-json-document document) 0)))
        (visited (make-hash-table :test #'equal))
        (refreshed 0))
    (loop while queue
          for item = (pop queue)
          for current = (car item)
          for depth = (cdr item)
          for id = (and current (geo-document-id current))
          when (and current id (not (gethash id visited)))
            do
               (setf (gethash id visited) t)
               (couchdb-refresh-geo-projection client database current)
               (incf refreshed)
               (when (< depth max-depth)
                 (dolist
                     (dependent
                      (couchdb-geo-dependent-documents
                       client database id))
                   (push (cons dependent (1+ depth)) queue))))
    refreshed))

(defun couchdb-resolve-geo-search-projections (client database response)
  "Resolve projection hits in RESPONSE through canonical CouchDB documents."
  (resolve-geo-search-projections
   response
   (lambda (id)
     (couchdb-fetch-geo-document client database id))))
