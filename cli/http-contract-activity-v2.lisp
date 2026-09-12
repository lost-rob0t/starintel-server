(in-package :star.http.contract)

(register-view-v2-http-operation
 "activity.v2.get"
 "activity-v2"
 "/api/v1/activity"
 "Return bounded document-add or ingest-rate activity history"
 '("views" "activity"))

(register-view-v2-http-operation
 "timeline.v2.get"
 "timeline-v2"
 "/api/v1/timeline"
 "Return a bounded chronological intelligence timeline"
 '("views" "activity" "timeline"))

(register-view-v2-http-operation
 "activity.v2.execute"
 "activity-v2-execute"
 "/api/v1/activity/:view"
 "Execute a registered activity projection"
 '("views" "activity"))
