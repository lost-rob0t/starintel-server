(in-package :star.databases.couchdb)

(defparameter +geo-search-design-document+ "geo")
(defparameter +geo-search-index+ "bbox")

(define-condition invalid-geo-bbox (error)
  ((reason
    :initarg :reason
    :reader invalid-geo-bbox-reason))
  (:report
   (lambda (condition stream)
     (format stream "~a" (invalid-geo-bbox-reason condition)))))

(defun fail-geo-bbox (format-control &rest arguments)
  (error 'invalid-geo-bbox
         :reason (apply #'format nil format-control arguments)))

(defun decimal-coordinate (text minimum maximum name)
  "Parse one untrusted decimal coordinate without invoking the Lisp reader."
  (unless (and (stringp text)
               (<= 1 (length text) 32)
               (cl-ppcre:scan "^[+-]?[0-9]+(\\.[0-9]+)?$" text))
    (fail-geo-bbox "~a must be a plain decimal number" name))
  (let* ((negative-p (char= (char text 0) #\-))
         (signed-p (or negative-p (char= (char text 0) #\+)))
         (start (if signed-p 1 0))
         (dot (position #\. text :start start))
         (whole-text (subseq text start (or dot (length text))))
         (fraction-text (and dot (subseq text (1+ dot))))
         (denominator (if fraction-text
                          (expt 10 (length fraction-text))
                          1))
         (whole (parse-integer whole-text))
         (fraction (if fraction-text
                       (parse-integer fraction-text)
                       0))
         (numerator (+ (* whole denominator) fraction))
         (value (/ (if negative-p (- numerator) numerator)
                   denominator))
         (normalized (if (and (plusp (length text))
                              (char= (char text 0) #\+))
                         (subseq text 1)
                         text)))
    (unless (<= minimum value maximum)
      (fail-geo-bbox "~a is outside [~a,~a]" name minimum maximum))
    (values value normalized)))

(defun geo-bbox-lucene-query (bbox)
  "Compile WEST,SOUTH,EAST,NORTH into a numeric Lucene range query.

WEST > EAST is interpreted as an antimeridian-crossing bounding box."
  (unless (stringp bbox)
    (fail-geo-bbox "bbox is required"))
  (let ((parts (uiop:split-string bbox :separator '(#\,))))
    (unless (= 4 (length parts))
      (fail-geo-bbox "bbox must contain west,south,east,north"))
    (destructuring-bind (west-text south-text east-text north-text) parts
      (multiple-value-bind (west west-wire)
          (decimal-coordinate west-text -180 180 "west")
        (multiple-value-bind (south south-wire)
            (decimal-coordinate south-text -90 90 "south")
          (multiple-value-bind (east east-wire)
              (decimal-coordinate east-text -180 180 "east")
            (multiple-value-bind (north north-wire)
                (decimal-coordinate north-text -90 90 "north")
              (unless (<= south north)
                (fail-geo-bbox "south must be less than or equal to north"))
              (if (<= west east)
                  (format nil
                          "(geo_lat:[~a TO ~a] AND geo_lon:[~a TO ~a])"
                          south-wire north-wire west-wire east-wire)
                  (format nil
                          "(geo_lat:[~a TO ~a] AND (geo_lon:[~a TO 180] OR geo_lon:[-180 TO ~a]))"
                          south-wire north-wire west-wire east-wire)))))))))

(defun geo-bbox-search (client database query &key (limit 50))
  "Execute a validated/scoped bounding-box query against CouchDB/Clouseau."
  (unless (and (integerp limit) (<= 1 limit 100))
    (error "Geo bbox limit must be an integer from 1 through 100"))
  (cl-couch:fts-search
   client
   (jsown:to-json
    (jsown:new-js
      ("q" query)
      ("limit" limit)
      ("include_docs" t)))
   database
   +geo-search-design-document+
   +geo-search-index+))
