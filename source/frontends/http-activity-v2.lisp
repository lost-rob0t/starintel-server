(in-package :star.frontends.http-api)

(defparameter +activity-v2-max-points+ 120
  "Maximum number of samples returned by the high-level activity API.")
(defparameter +activity-v2-unix-universal-offset+ 2208988800
  "Seconds between the Common Lisp universal and Unix epochs.")

(defparameter +activity-v2-ranges+
  '(("1h" 3600)
    ("6h" 21600)
    ("24h" 86400)
    ("7d" 604800)
    ("30d" 2592000))
  "Allowed activity/timeline ranges and their durations in seconds.")

(defparameter +activity-v2-buckets+
  '((60 star.databases.couchdb::activity-added-1m)
    (300 star.databases.couchdb::activity-added-5m)
    (900 star.databases.couchdb::activity-added-15m)
    (3600 star.databases.couchdb::activity-added-1h)
    (7200 star.databases.couchdb::activity-added-2h)
    (21600 star.databases.couchdb::activity-added-6h)
    (86400 star.databases.couchdb::activity-added-1d)
    (604800 star.databases.couchdb::activity-added-7d)
    (2592000 star.databases.couchdb::activity-added-30d))
  "Bucket width and registered reduced view ordered finest to coarsest.")

(defparameter +activity-v2-metrics+
  '("documents_added" "ingest_rate")
  "Metrics accepted by the high-level activity API.")

(defparameter +timeline-v2-bases+
  '("added" "updated" "observed" "collected"
    "valid_from" "valid_to" "generated")
  "Explicit timestamp bases accepted by the timeline API.")

(defun activity-v2-unix-now ()
  "Return the current UTC time as Unix epoch seconds."
  (- (get-universal-time) +activity-v2-unix-universal-offset+))

(defun activity-v2-choice (params name allowed default)
  "Read a bounded string choice from PARAMS."
  (let ((value (string-downcase (or (query-value params name) default))))
    (unless (member value allowed :test #'string=)
      (signal-http-input-error
       400
       "invalid_query_parameter"
       (format nil "Query parameter ~a has an unsupported value" name)
       (jsown:new-js
         ("parameter" name)
         ("received" value)
         ("allowed" allowed))))
    value))

(defun activity-v2-range-seconds (range)
  "Return the exact configured duration for RANGE."
  (or (second (assoc range +activity-v2-ranges+ :test #'string=))
      (signal-http-input-error
       400 "invalid_query_parameter" "Unsupported activity range")))

(defun activity-v2-select-bucket (duration max-points)
  "Choose the finest registered bucket that stays within MAX-POINTS."
  (dolist (entry +activity-v2-buckets+)
    (when (<= (ceiling duration (first entry)) max-points)
      (return-from activity-v2-select-bucket
        (values (first entry) (second entry)))))
  (let ((entry (car (last +activity-v2-buckets+))))
    (values (first entry) (second entry))))

(defun activity-v2-aligned-window (now duration bucket-seconds)
  "Return first and last bucket starts inside the requested UTC window."
  (let* ((requested-start (- now duration))
         (first (* (ceiling requested-start bucket-seconds)
                   bucket-seconds))
         (last (* (floor (max 0 (1- now)) bucket-seconds)
                  bucket-seconds)))
    (values first last requested-start)))

(defun activity-v2-value (metric count bucket-seconds)
  "Convert one raw document count into the requested metric."
  (if (string= metric "ingest_rate")
      (/ (float count 1.0d0)
         (/ (float bucket-seconds 1.0d0) 60.0d0))
      count))

(defun activity-v2-samples-from-rows
    (rows first last bucket-seconds metric)
  "Expand sparse reduced rows into deterministic zero-filled UTC buckets."
  (let ((counts (make-hash-table :test #'eql))
        (samples nil)
        (metric-values nil)
        (raw-counts nil))
    (dolist (row rows)
      (let ((key (jsown:val-safe row "key"))
            (value (or (jsown:val-safe row "value") 0)))
        (when (numberp key)
          (setf (gethash key counts) value))))
    (when (<= first last)
      (loop for timestamp from first to last by bucket-seconds
            for count = (or (gethash timestamp counts) 0)
            for value = (activity-v2-value metric count bucket-seconds)
            do (push count raw-counts)
               (push value metric-values)
               (push (jsown:new-js
                       ("ts" timestamp)
                       ("value" value))
                     samples)))
    (values (nreverse samples)
            (nreverse metric-values)
            (nreverse raw-counts))))

(defun activity-v2-series-rows
    (client view first last)
  "Read one bounded grouped activity series from a registered reduced view."
  (if (> first last)
      nil
      (view-v2-result-rows
       (star.databases.couchdb:execute-registered-view
        view
        client
        star:*couchdb-default-database*
        :limit +activity-v2-max-points+
        :start-key first
        :end-key last
        :reduce t
        :include-docs nil
        :group t))))

(defun activity-v2-reduced-count
    (client view start end)
  "Return the reduced document count for an inclusive UTC key range."
  (let* ((result
           (star.databases.couchdb:execute-registered-view
            view
            client
            star:*couchdb-default-database*
            :limit 1
            :start-key start
            :end-key end
            :reduce t
            :include-docs nil
            :group nil))
         (rows (view-v2-result-rows result))
         (row (first rows)))
    (if row
        (or (jsown:val-safe row "value") 0)
        0)))

(defun activity-v2-utc-period-start (period now)
  "Return the UTC Unix boundary for PERIOD containing NOW."
  (let ((universal (+ now +activity-v2-unix-universal-offset+)))
    (multiple-value-bind
          (second minute hour day month year day-of-week)
        (decode-universal-time universal 0)
      (declare (ignore second minute hour))
      (labels ((unix (value)
                 (- value +activity-v2-unix-universal-offset+))
               (midnight (d m y)
                 (unix (encode-universal-time 0 0 0 d m y 0))))
        (ecase period
          (:minute (* (floor now 60) 60))
          (:hour (* (floor now 3600) 3600))
          (:day (midnight day month year))
          (:week (- (midnight day month year)
                    (* day-of-week 86400)))
          (:month (midnight 1 month year))
          (:year (midnight 1 1 year)))))))

(defun activity-v2-period-counts (client now)
  "Return real UTC-boundary document-add counters."
  (flet ((count-range (view period)
           (activity-v2-reduced-count
            client
            view
            (activity-v2-utc-period-start period now)
            now)))
    (jsown:new-js
      ("minute"
       (count-range
        'star.databases.couchdb::activity-added-1m :minute))
      ("hour"
       (count-range
        'star.databases.couchdb::activity-added-1m :hour))
      ("today"
       (count-range
        'star.databases.couchdb::activity-added-1h :day))
      ("week"
       (count-range
        'star.databases.couchdb::activity-added-1d :week))
      ("month"
       (count-range
        'star.databases.couchdb::activity-added-1d :month))
      ("year"
       (count-range
        'star.databases.couchdb::activity-added-1d :year)))))

(defun activity-v2-summary
    (metric metric-values raw-counts)
  "Return current, average, peak, and document delta for one series."
  (declare (ignore metric))
  (let* ((count (length metric-values))
         (current (if metric-values (car (last metric-values)) 0))
         (average
           (if (zerop count)
               0
               (/ (reduce #'+ metric-values :initial-value 0.0d0)
                  (float count 1.0d0))))
         (peak
           (if metric-values
               (reduce #'max metric-values)
               0))
         (delta (reduce #'+ raw-counts :initial-value 0)))
    (values current average peak delta)))

(defun activity-v2-document-with-client (client params &optional now)
  "Build the bounded activity response consumed by compact clients."
  (let* ((metric
           (activity-v2-choice
            params "metric" +activity-v2-metrics+ "documents_added"))
         (range
           (activity-v2-choice
            params "range" (mapcar #'first +activity-v2-ranges+) "24h"))
         (duration (activity-v2-range-seconds range))
         (max-points
           (view-v2-integer
            params "max_points"
            :default +activity-v2-max-points+
            :minimum 1
            :maximum +activity-v2-max-points+))
         (generated-at (or now (activity-v2-unix-now))))
    (multiple-value-bind (bucket-seconds view)
        (activity-v2-select-bucket duration max-points)
      (multiple-value-bind (first last requested-start)
          (activity-v2-aligned-window
           generated-at duration bucket-seconds)
        (let ((rows
                (activity-v2-series-rows
                 client view first last)))
          (multiple-value-bind
                (samples metric-values raw-counts)
              (activity-v2-samples-from-rows
               rows first last bucket-seconds metric)
            (multiple-value-bind
                  (current average peak delta)
                (activity-v2-summary
                 metric metric-values raw-counts)
              (jsown:new-js
                ("status" "ok")
                ("data"
                 (jsown:new-js
                   ("generated_at" generated-at)
                   ("metric" metric)
                   ("unit"
                    (if (string= metric "ingest_rate")
                        "documents_per_minute"
                        "documents"))
                   ("time_basis" "added")
                   ("range" range)
                   ("requested_start" requested-start)
                   ("start" first)
                   ("end" generated-at)
                   ("bucket_seconds" bucket-seconds)
                   ("max_points" max-points)
                   ("samples" samples)
                   ("current" current)
                   ("average" average)
                   ("peak" peak)
                   ("delta" delta)
                   ("delta_unit" "documents")
                   ("period_counts"
                    (activity-v2-period-counts client generated-at))
                   ("partial"
                    (if (or (> first requested-start)
                            (not (zerop
                                  (mod generated-at bucket-seconds))))
                        :true
                        :false))))))))))))

(defun activity-v2-document (params)
  "Execute the authenticated high-level activity endpoint."
  (couchdb-handler (client *couchdb-pool*)
    (jsown:to-json
     (activity-v2-document-with-client client params))))

(defun timeline-v2-sequence-value (sequence index)
  "Read INDEX from a decoded JSON array represented as a list or vector."
  (cond
    ((vectorp sequence)
     (when (< index (length sequence))
       (aref sequence index)))
    ((listp sequence)
     (nth index sequence))
    (t nil)))

(defun timeline-v2-event-from-row (row)
  "Convert one indexed timeline row into the stable compact event shape."
  (let* ((key (jsown:val-safe row "key"))
         (value (jsown:val-safe row "value"))
         (basis (timeline-v2-sequence-value key 0))
         (timestamp (timeline-v2-sequence-value key 1)))
    (jsown:new-js
      ("ts" (or timestamp :null))
      ("time_basis" (or basis :null))
      ("id" (or (and value (jsown:val-safe value "id")) :null))
      ("dataset" (or (and value (jsown:val-safe value "dataset")) :null))
      ("dtype" (or (and value (jsown:val-safe value "dtype")) :null))
      ("label" (or (and value (jsown:val-safe value "label")) :null)))))

(defun timeline-v2-range-keys
    (basis start end descending)
  "Return CouchDB composite range keys for a bounded timeline query."
  (let ((low (list basis start))
        (high (list basis end (jsown:empty-object))))
    (if descending
        (values high low)
        (values low high))))

(defun timeline-v2-document-with-client (client params &optional now)
  "Build a bounded chronological timeline from explicit timestamp bases."
  (let* ((basis
           (activity-v2-choice
            params "basis" +timeline-v2-bases+ "added"))
         (range
           (activity-v2-choice
            params "range" (mapcar #'first +activity-v2-ranges+) "24h"))
         (duration (activity-v2-range-seconds range))
         (limit
           (view-v2-integer
            params "limit" :default 50 :minimum 1 :maximum 200))
         (descending
           (if (view-v2-query-present-p params "descending")
               (view-v2-boolean params "descending")
               t))
         (generated-at (or now (activity-v2-unix-now)))
         (start (- generated-at duration))
         (end generated-at))
    (multiple-value-bind (start-key end-key)
        (timeline-v2-range-keys basis start end descending)
      (let* ((result
               (star.databases.couchdb:execute-registered-view
                'star.databases.couchdb::activity-timeline
                client
                star:*couchdb-default-database*
                :limit (1+ limit)
                :start-key start-key
                :end-key end-key
                :descending descending
                :reduce nil
                :include-docs nil))
             (rows (view-v2-result-rows result))
             (partial (> (length rows) limit))
             (bounded
               (if partial
                   (subseq rows 0 limit)
                   rows))
             (events (mapcar #'timeline-v2-event-from-row bounded)))
        (jsown:new-js
          ("status" "ok")
          ("data"
           (jsown:new-js
             ("generated_at" generated-at)
             ("time_basis" basis)
             ("range" range)
             ("start" start)
             ("end" end)
             ("descending" (if descending :true :false))
             ("limit" limit)
             ("event_count" (length events))
             ("events" events)
             ("partial" (if partial :true :false)))))))))

(defun timeline-v2-document (params)
  "Execute the authenticated high-level timeline endpoint."
  (couchdb-handler (client *couchdb-pool*)
    (jsown:to-json
     (timeline-v2-document-with-client client params))))

(install-view-v2-route
 "/api/v1/activity"
 #'activity-v2-document)

(install-view-v2-route
 "/api/v1/timeline"
 #'timeline-v2-document)

(install-view-v2-route
 "/api/v1/activity/:view"
 (lambda (params)
   (execute-http-view-v2
    (view-v2-resolve-name
     (view-v2-route-view-name params) "activity")
    params)))

(pushnew '("/api/v1/activity" activity)
         *http-view-v2-route-matrix*
         :test #'equal)
(pushnew '("/api/v1/activity/:view" activity-view)
         *http-view-v2-route-matrix*
         :test #'equal)
(pushnew '("/api/v1/timeline" timeline)
         *http-view-v2-route-matrix*
         :test #'equal)
