(in-package :star-server-tests)

(in-suite http-boundary-tests)

(test activity-v2-views-are-registered-and-backed
  (is-true (star.databases.couchdb:validate-view-registry))
  (let ((catalog
          (star.frontends.http-api::view-v2-catalog-document "activity")))
    (is (= 11 (jsown:val catalog "count"))))
  (dolist (name
           '(star.databases.couchdb::activity-added-1m
             star.databases.couchdb::activity-added-15m
             star.databases.couchdb::activity-added-2h
             star.databases.couchdb::activity-added-6h
             star.databases.couchdb::activity-added-1d
             star.databases.couchdb::activity-timeline
             star.databases.couchdb::activity-latest-updated))
    (is (not (null
              (star.databases.couchdb:registered-view-spec name))))))

(test activity-v2-default-buckets-match-watch-ranges
  (dolist (fixture
           '(("1h" 3600 60)
             ("6h" 21600 300)
             ("24h" 86400 900)
             ("7d" 604800 7200)
             ("30d" 2592000 21600)))
    (multiple-value-bind (bucket view)
        (star.frontends.http-api::activity-v2-select-bucket
         (second fixture)
         star.frontends.http-api::+activity-v2-max-points+)
      (declare (ignore view))
      (is (= (third fixture) bucket)))))

(test activity-v2-max-points-selects-a-bounded-coarser-bucket
  (multiple-value-bind (bucket view)
      (star.frontends.http-api::activity-v2-select-bucket 86400 24)
    (is (= 3600 bucket))
    (is (eq 'star.databases.couchdb::activity-added-1h view)))
  (multiple-value-bind (bucket view)
      (star.frontends.http-api::activity-v2-select-bucket 2592000 1)
    (is (= 2592000 bucket))
    (is (eq 'star.databases.couchdb::activity-added-30d view))))

(test activity-v2-samples-preserve-zero-gaps-and-spikes
  (let ((rows
          (list
           (jsown:new-js ("key" 60) ("value" 2))
           (jsown:new-js ("key" 180) ("value" 9)))))
    (multiple-value-bind (samples metric-values raw-counts)
        (star.frontends.http-api::activity-v2-samples-from-rows
         rows 60 180 60 "documents_added")
      (is (= 3 (length samples)))
      (is (equal '(2 0 9) raw-counts))
      (is (equal '(2 0 9) metric-values))
      (is (= 0 (jsown:val (second samples) "value"))))))

(test activity-v2-ingest-rate-is-derived-from-bucket-count
  (is (= 2.0d0
         (star.frontends.http-api::activity-v2-value
          "ingest_rate" 30 900))))

(test timeline-v2-range-keys-preserve-explicit-basis
  (multiple-value-bind (start-key end-key)
      (star.frontends.http-api::timeline-v2-range-keys
       "observed" 100 200 nil)
    (is (string= "observed" (first start-key)))
    (is (= 100 (second start-key)))
    (is (string= "observed" (first end-key)))
    (is (= 200 (second end-key))))
  (multiple-value-bind (start-key end-key)
      (star.frontends.http-api::timeline-v2-range-keys
       "collected" 100 200 t)
    (is (= 200 (second start-key)))
    (is (= 100 (second end-key)))))

(test activity-and-timeline-api-live-under-api-v1-and-require-views-read
  (dolist (path '("/api/v1/activity"
                  "/api/v1/activity/added-15m"
                  "/api/v1/timeline"
                  "/api/v1/views/activity/timeline"))
    (is (string= "views:read"
                 (star.frontends.http-api::route-action :get path))))
  (is (find "/api/v1/activity"
            star.frontends.http-api::*http-view-v2-route-matrix*
            :key #'first
            :test #'string=))
  (is (find "/api/v1/timeline"
            star.frontends.http-api::*http-view-v2-route-matrix*
            :key #'first
            :test #'string=)))

(test activity-v2-query-contract-rejects-unknown-range-and-metric
  (dolist (params '((("range" . "forever"))
                    (("metric" . "made_up"))))
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api::activity-v2-choice
                params
                (if (assoc "range" params :test #'string=)
                    "range"
                    "metric")
                (if (assoc "range" params :test #'string=)
                    (mapcar #'first
                            star.frontends.http-api::+activity-v2-ranges+)
                    star.frontends.http-api::+activity-v2-metrics+)
                "24h")))))
      (is (= 400
             (star.frontends.http-api:http-input-error-status
              condition))))))

(test activity-v2-http-contract-is-discoverable
  (dolist (operation-id
           '("activity.v2.get"
             "timeline.v2.get"
             "activity.v2.execute"))
    (let ((operation
            (star.http.contract:find-http-operation operation-id)))
      (is (not (null operation)))
      (is (eq :get
              (star.http.contract:http-operation-method operation)))
      (is (eq :authenticated
              (star.http.contract:http-operation-authority operation)))
      (is (equal '("views:read")
                 (star.http.contract:http-operation-scopes operation)))
      (is (uiop:string-prefix-p
           "/api/v1/"
           (star.http.contract:http-operation-path operation))))))

(test activity-v2-period-boundaries-are-utc
  (let ((now 1789187696))
    (is (= (* (floor now 60) 60)
           (star.frontends.http-api::activity-v2-utc-period-start
            :minute now)))
    (is (= (* (floor now 3600) 3600)
           (star.frontends.http-api::activity-v2-utc-period-start
            :hour now)))
    (is (<
         (star.frontends.http-api::activity-v2-utc-period-start
          :year now)
         (star.frontends.http-api::activity-v2-utc-period-start
          :month now)
         now))))

(test activity-v2-is-advertised-by-capabilities
  (let* ((data (star.frontends.http-api::capabilities-data))
         (features (jsown:val data "features"))
         (activity (jsown:val features "activity"))
         (timeline (jsown:val features "timeline"))
         (limits (jsown:val data "limits"))
         (endpoints (jsown:val data "endpoints")))
    (is (eq :true (jsown:val activity "available")))
    (is (= 120 (jsown:val activity "max_points")))
    (is (member "documents_added"
                (jsown:val activity "metrics")
                :test #'string=))
    (is (member "observed"
                (jsown:val timeline "time_bases")
                :test #'string=))
    (is (= 120 (jsown:val limits "activity_max_points")))
    (is (= 200 (jsown:val limits "timeline_max_events")))
    (is (find "/api/v1/activity"
              endpoints
              :key (lambda (endpoint)
                     (jsown:val endpoint "path"))
              :test #'string=))
    (is (find "/api/v1/timeline"
              endpoints
              :key (lambda (endpoint)
                     (jsown:val endpoint "path"))
              :test #'string=))))
