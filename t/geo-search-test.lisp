(in-package :star-server-tests)

(def-suite geo-search-tests
  :description "CouchDB JSON geospatial bounding-box query tests")

(in-suite geo-search-tests)

(test bbox-query-uses-numeric-couchdb-fields
  (is
   (string=
    "(geo_lat:[39.8 TO 40.2] AND geo_lon:[-83.2 TO -82.8])"
    (star.databases.couchdb:geo-bbox-lucene-query
     "-83.2,39.8,-82.8,40.2"))))

(test bbox-query-supports-antimeridian-crossing
  (is
   (string=
    "(geo_lat:[-10 TO 10] AND (geo_lon:[170 TO 180] OR geo_lon:[-180 TO -170]))"
    (star.databases.couchdb:geo-bbox-lucene-query
     "170,-10,-170,10"))))

(test bbox-query-rejects-invalid-or-injected-coordinates
  (dolist (bbox
           '("-181,0,1,1"
             "0,-91,1,1"
             "0,10,1,9"
             "0,0,1"
             "0,0,1,1 OR *:*"
             "0,0,1,NaN"))
    (signals star.databases.couchdb:invalid-geo-bbox
      (star.databases.couchdb:geo-bbox-lucene-query bbox))))

(test geo-design-document-indexes-couchdb-json-coordinates
  (let* ((pathname
           (asdf:system-relative-pathname
            :starintel-gserver
            "views/geo.json"))
         (text (uiop:read-file-string pathname))
         (document (jsown:parse text))
         (index-function
           (jsown:val
            (jsown:val
             (jsown:val document "indexes")
             "bbox")
            "index")))
    (is (string= "_design/geo" (jsown:val document "_id")))
    (is (search "doc.longitude" index-function))
    (is (search "doc.latitude" index-function))
    (is (search "index('geo_lon'" index-function))
    (is (search "index('geo_lat'" index-function))
    (is-false (search "star-lang" (string-downcase index-function)))))


(defun geo-fixture-table-fetcher (pairs)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (setf (gethash (car pair) table) (cdr pair)))
    (lambda (id) (gethash id table))))

(test explicit-location-chain-builds-anchored-projection-without-writeback
  (let* ((subject
           (jsown:new-js
             ("_id" "photo-1")
             ("_rev" "1-subject")
             ("dtype" "picture")
             ("dataset" "dataset-a")
             ("tenant_id" "default")
             ("location" "location-1")))
         (location
           (jsown:new-js
             ("_id" "location-1")
             ("dtype" "location")
             ("dataset" "dataset-a")
             ("geometry" "point-1")))
         (point
           (jsown:new-js
             ("_id" "point-1")
             ("_rev" "1-point")
             ("dtype" "geo-point")
             ("dataset" "dataset-a")
             ("geometryType" "point")
             ("longitude" "-83.01")
             ("latitude" "40.01")))
         (before (jsown:to-json subject))
         (projection
           (star.databases.couchdb:build-geo-projection
            subject
            (geo-fixture-table-fetcher
             (list (cons "location-1" location)
                   (cons "point-1" point))))))
    (is-true projection)
    (is (string= "starintel.geo-projection.v1"
                 (jsown:val projection "kind")))
    (is (string= "photo-1" (jsown:val projection "subjectId")))
    (is (string= "point-1" (jsown:val projection "geometryId")))
    (is (string= "-83.01" (jsown:val projection "longitude")))
    (is (string= "40.01" (jsown:val projection "latitude")))
    (is (equal '("photo-1" "location-1" "point-1")
               (jsown:val projection "relationPath")))
    (is (eq :true (jsown:val projection "projectionOnly")))
    (is (string= before (jsown:to-json subject)))))

(test correlations-without-explicit-geo-reference-do-not-project
  (let ((subject
          (jsown:new-js
            ("_id" "wireless-observation-1")
            ("dtype" "observation")
            ("dataset" "dataset-a")
            ("nearbyDevice" "device-1")
            ("rssi" -42))))
    (is-false
     (star.databases.couchdb:build-geo-projection
      subject
      (lambda (id)
        (declare (ignore id))
        (error "fetch must not run for correlation-only input"))))))

(test projection-search-hit-resolves-to-canonical-subject
  (let* ((subject
           (jsown:new-js
             ("_id" "subject-1")
             ("dtype" "picture")
             ("dataset" "dataset-a")
             ("tenant_id" "default")))
         (projection
           (jsown:new-js
             ("_id" "geo-projection:test")
             ("kind" "starintel.geo-projection.v1")
             ("subjectId" "subject-1")
             ("dataset" "dataset-a")
             ("tenant_id" "default")
             ("longitude" "-83.0")
             ("latitude" "40.0")))
         (response
           (jsown:new-js
             ("total_rows" 1)
             ("rows"
              (list
               (jsown:new-js
                 ("id" "geo-projection:test")
                 ("doc" projection))))))
         (resolved
           (star.databases.couchdb:resolve-geo-search-projections
            response
            (lambda (id)
              (and (string= id "subject-1") subject))))
         (rows (jsown:val resolved "rows")))
    (is (= 1 (length rows)))
    (is (string= "subject-1"
                 (jsown:val (jsown:val (first rows) "doc") "_id")))
    (is (= 1 (jsown:val resolved "total_rows")))))

(test scope-mismatched-projection-is-dropped
  (let* ((subject
           (jsown:new-js
             ("_id" "subject-b")
             ("dtype" "picture")
             ("dataset" "dataset-b")
             ("tenant_id" "default")))
         (projection
           (jsown:new-js
             ("_id" "geo-projection:poison")
             ("kind" "starintel.geo-projection.v1")
             ("subjectId" "subject-b")
             ("dataset" "dataset-a")
             ("tenant_id" "default")
             ("longitude" "-83.0")
             ("latitude" "40.0")))
         (response
           (jsown:new-js
             ("total_rows" 1)
             ("rows"
              (list (jsown:new-js ("doc" projection))))))
         (resolved
           (star.databases.couchdb:resolve-geo-search-projections
            response
            (lambda (id)
              (declare (ignore id))
              subject))))
    (is (null (jsown:val resolved "rows")))
    (is (= 0 (jsown:val resolved "total_rows")))))

(test geo-design-document-indexes-explicit-reference-dependencies
  (let* ((pathname
           (asdf:system-relative-pathname
            :starintel-gserver
            "views/geo.json"))
         (document (jsown:parse (uiop:read-file-string pathname)))
         (view (jsown:val
                (jsown:val document "views")
                "references"))
         (map-source (jsown:val view "map")))
    (is (search "'location'" map-source))
    (is (search "'geometry'" map-source))
    (is (search "'address'" map-source))
    (is-false (search "nearby" (string-downcase map-source)))))
