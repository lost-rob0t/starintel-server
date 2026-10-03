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
