(in-package :star.frontends.http-api)

(defparameter *actor-manifest-provider-urls*
  (star::split-comma-setting (uiop:getenv "STAR_ACTOR_MANIFEST_URLS"))
  "Trusted read-only remote actor manifest endpoints, comma separated via STAR_ACTOR_MANIFEST_URLS.")

(defun actor-manifest-string (object key &optional default)
  (let ((value
          (and object
               (handler-case
                   (jsown:val-safe object key)
                 (error () nil)))))
    (if (stringp value) value default)))

(defun local-actor-manifest (actor-name)
  (jsown:new-js
    ("schema" "starintel-actor-deployment-manifest-v1")
    ("id" actor-name)
    ("label" actor-name)
    ("description" "Local Common Lisp StarIntel actor.")
    ("version" 1)
    ("available" :true)
    ("service"
     (jsown:new-js
       ("id" "starintel-gserver")
       ("kind" "starintel-server")
       ("language" "common-lisp")))
    ("runtime"
     (jsown:new-js
       ("location" "local")
       ("transport" "sento")))
    ("dispatch"
     (jsown:new-js
       ("dtype" "target")
       ("actor" actor-name)))))

(defun local-actor-manifests-from-registry (registry)
  (sort
   (loop for actor-name being the hash-keys of registry
         when (stringp actor-name)
           collect (local-actor-manifest actor-name))
   #'string<
   :key (lambda (manifest) (jsown:val manifest "id"))))

(defun local-actor-manifests ()
  (if (null star.actors:*actor-index-agent*)
      nil
      (sento.agent:agent-get
       star.actors:*actor-index-agent*
       #'local-actor-manifests-from-registry)))

(defun normalize-remote-actor-manifest (manifest)
  (handler-case
      (let ((id (actor-manifest-string manifest "id")))
        (when (and id (plusp (length id)))
          (let* ((service (jsown:val-safe manifest "service"))
                 (runtime (jsown:val-safe manifest "runtime"))
                 (dispatch (jsown:val-safe manifest "dispatch"))
                 (label (actor-manifest-string manifest "label" id))
                 (description
                   (actor-manifest-string
                    manifest "description" "Remote StarIntel actor."))
                 (service-id
                   (actor-manifest-string service "id" "remote-actor-service"))
                 (service-kind (actor-manifest-string service "kind" "remote"))
                 (language (actor-manifest-string service "language" "unknown"))
                 (transport (actor-manifest-string runtime "transport" "rabbitmq"))
                 (routing-key (actor-manifest-string runtime "routing_key" ""))
                 (queue (actor-manifest-string runtime "queue" ""))
                 (dispatch-actor (actor-manifest-string dispatch "actor" id)))
            (jsown:new-js
              ("schema" "starintel-actor-deployment-manifest-v1")
              ("id" id)
              ("label" label)
              ("description" description)
              ("version" (or (jsown:val-safe manifest "version") 1))
              ("available" :true)
              ("service"
               (jsown:new-js
                 ("id" service-id)
                 ("kind" service-kind)
                 ("language" language)))
              ("runtime"
               (jsown:new-js
                 ("location" "remote")
                 ("transport" transport)
                 ("routing_key" routing-key)
                 ("queue" queue)))
              ("dispatch"
               (jsown:new-js
                 ("dtype" "target")
                 ("actor" dispatch-actor)))))))
    (error () nil)))

(defun provider-actor-manifests-from-document (document)
  (handler-case
      (let* ((data (jsown:val-safe document "data"))
             (actors (and data (jsown:val-safe data "actors"))))
        (when (listp actors)
          (remove-if
           #'null
           (mapcar #'normalize-remote-actor-manifest actors))))
    (error () nil)))

(defun fetch-actor-manifest-provider (url)
  (handler-case
      (multiple-value-bind (body status)
          (dex:get url :connect-timeout 2 :read-timeout 2)
        (if (and (integerp status) (<= 200 status 299))
            (let ((actors
                    (provider-actor-manifests-from-document
                     (jsown:parse body))))
              (values
               actors
               (jsown:new-js
                 ("url" url)
                 ("status" "ok")
                 ("actor_count" (length actors)))))
            (values
             nil
             (jsown:new-js
               ("url" url)
               ("status" "error")
               ("http_status" (or status 0))))))
    (error (condition)
      (log:warn "Actor manifest provider ~a unavailable: ~a" url condition)
      (values
       nil
       (jsown:new-js
         ("url" url)
         ("status" "unavailable")))))))

(defparameter *actor-manifest-provider-fetcher*
  #'fetch-actor-manifest-provider
  "Fetcher used for configured remote actor deployment-manifest providers.")

(defun remote-actor-manifests ()
  (let ((actors nil)
        (providers nil))
    (dolist (url *actor-manifest-provider-urls*)
      (multiple-value-bind (provider-actors provider-status)
          (funcall *actor-manifest-provider-fetcher* url)
        (setf actors (append actors provider-actors))
        (push provider-status providers)))
    (values actors (nreverse providers))))

(defun merge-actor-manifests (local remote)
  "Merge LOCAL and REMOTE actor deployments using actual router precedence.
A registered in-process actor wins when the same actor id is also advertised by
a remote provider because the target router resolves the local actor first."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (manifest local)
      (let ((id (jsown:val manifest "id")))
        (setf (gethash id seen) t)
        (push manifest result)))
    (dolist (manifest remote)
      (let ((id (jsown:val manifest "id")))
        (unless (gethash id seen)
          (setf (gethash id seen) t)
          (push manifest result))))
    (sort (nreverse result)
          #'string<
          :key (lambda (manifest) (jsown:val manifest "id")))))

(defun actor-discovery-document ()
  (multiple-value-bind (remote providers)
      (remote-actor-manifests)
    (jsown:new-js
      ("status" "ok")
      ("data"
       (jsown:new-js
         ("schema" "starintel-actor-deployment-manifest-list-v1")
         ("actors"
          (merge-actor-manifests
           (local-actor-manifests)
           remote))
         ("providers" providers))))))

(defun handle-actor-discovery-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (set-cache-control "no-store")
    (jsown:to-json (actor-discovery-document))))

(mount-http-operation "actors.list" #'handle-actor-discovery-route)
