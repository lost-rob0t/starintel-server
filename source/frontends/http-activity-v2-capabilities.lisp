(in-package :star.frontends.http-api)

(defvar *activity-v2-base-capabilities-data* nil
  "Capabilities builder wrapped by the activity/timeline extension.")

(unless *activity-v2-base-capabilities-data*
  (setf *activity-v2-base-capabilities-data*
        (symbol-function 'capabilities-data)))

(defun capabilities-data ()
  "Return base capabilities extended with bounded activity and timeline reads."
  (let* ((data (funcall *activity-v2-base-capabilities-data*))
         (features (jsown:val data "features"))
         (limits (jsown:val data "limits"))
         (endpoints (jsown:val data "endpoints")))
    (setf
     (jsown:val features "activity")
     (jsown:new-js
       ("available" :true)
       ("metrics" +activity-v2-metrics+)
       ("ranges" (mapcar #'first +activity-v2-ranges+))
       ("max_points" +activity-v2-max-points+))
     (jsown:val features "timeline")
     (jsown:new-js
       ("available" :true)
       ("time_bases" +timeline-v2-bases+))
     (jsown:val limits "activity_max_points")
     +activity-v2-max-points+
     (jsown:val limits "timeline_max_events")
     200
     (jsown:val data "endpoints")
     (append
      endpoints
      (list
       (capability-endpoint
        "activity" "GET" "/api/v1/activity"
        :scopes '("views:read"))
       (capability-endpoint
        "timeline" "GET" "/api/v1/timeline"
        :scopes '("views:read")))))
    data))
