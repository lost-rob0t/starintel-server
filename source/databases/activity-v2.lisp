(in-package :star.databases.couchdb)

(unless (assoc "activity" +v2-view-categories+ :test #'string=)
  (setf +v2-view-categories+
        (append +v2-view-categories+ '(("activity" . "activity-")))))

(defun register-activity-v2-view-specs ()
  "Register bounded activity series and explicit-basis timeline projections."
  (dolist (entry
           '((activity-added-1m "added_1m")
             (activity-added-5m "added_5m")
             (activity-added-15m "added_15m")
             (activity-added-1h "added_1h")
             (activity-added-2h "added_2h")
             (activity-added-6h "added_6h")
             (activity-added-1d "added_1d")
             (activity-added-7d "added_7d")
             (activity-added-30d "added_30d")))
    (register-view-spec
     (first entry) "activity_v2" (second entry)
     :reducer-p t
     :default-reduce t
     :default-include-docs nil))
  (register-view-spec
   'activity-timeline "activity_v2" "timeline"
   :default-include-docs nil)
  (register-view-spec
   'activity-latest-updated "activity_v2" "latest_updated"
   :default-include-docs nil)
  t)

(register-activity-v2-view-specs)
