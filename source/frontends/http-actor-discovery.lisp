(in-package :star.frontends.http-api)

(defun handle-actor-discovery-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (set-cache-control "no-store")
    (jsown:to-json (star.actors:actor-registry-document))))

(mount-http-operation "actors.list" #'handle-actor-discovery-route)
