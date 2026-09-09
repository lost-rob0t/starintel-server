(in-package :star-server-tests)

(in-suite http-boundary-tests)

(defun actor-discovery-test-remote-manifest (id &key (service "star-bbpd"))
  (jsown:new-js
    ("schema" "starintel-actor-deployment-manifest-v1")
    ("id" id)
    ("label" id)
    ("description" "remote test actor")
    ("version" 1)
    ("available" :true)
    ("service"
     (jsown:new-js
       ("id" service)
       ("kind" "star-bbpd")
       ("language" "python")))
    ("runtime"
     (jsown:new-js
       ("location" "remote")
       ("transport" "rabbitmq")
       ("routing_key" (format nil "actors.~a.new.target" id))
       ("queue" (format nil "bbp.~a.targets" id))))
    ("dispatch"
     (jsown:new-js
       ("dtype" "target")
       ("actor" id)))))

(test actor-discovery-local-registry-produces-lisp-manifests
  (let* ((registry (serapeum:dict "user-hunt" :fake-actor))
         (actors
           (star.frontends.http-api::local-actor-manifests-from-registry
            registry))
         (actor (first actors))
         (service (jsown:val actor "service"))
         (runtime (jsown:val actor "runtime")))
    (is (= 1 (length actors)))
    (is (string= "user-hunt" (jsown:val actor "id")))
    (is (string= "common-lisp" (jsown:val service "language")))
    (is (string= "local" (jsown:val runtime "location")))
    (is (string= "sento" (jsown:val runtime "transport")))))

(test actor-discovery-local-runtime-wins-name-collision
  (let* ((local (list (star.frontends.http-api::local-actor-manifest "shared")))
         (remote
           (list
            (star.frontends.http-api::normalize-remote-actor-manifest
             (actor-discovery-test-remote-manifest "shared"))
            (star.frontends.http-api::normalize-remote-actor-manifest
             (actor-discovery-test-remote-manifest "subfinder"))))
         (merged
           (star.frontends.http-api::merge-actor-manifests local remote))
         (shared
           (find "shared" merged
                 :key (lambda (actor) (jsown:val actor "id"))
                 :test #'string=)))
    (is (= 2 (length merged)))
    (is (string= "local"
                 (jsown:val (jsown:val shared "runtime") "location")))))

(test actor-discovery-provider-document-is-normalized
  (let* ((document
           (jsown:new-js
             ("status" "ok")
             ("data"
              (jsown:new-js
                ("actors"
                 (list
                  (actor-discovery-test-remote-manifest "httpx")))))))
         (actors
           (star.frontends.http-api::provider-actor-manifests-from-document
            document))
         (actor (first actors)))
    (is (= 1 (length actors)))
    (is (string= "httpx" (jsown:val actor "id")))
    (is (string= "remote"
                 (jsown:val (jsown:val actor "runtime") "location")))
    (is (string= "python"
                 (jsown:val (jsown:val actor "service") "language")))))

(test actor-discovery-v1-contract-is-authenticated
  (let ((operation (star.http.contract:find-http-operation "actors.list")))
    (is (eq :get (star.http.contract:http-operation-method operation)))
    (is (string= "/api/v1/actors"
                 (star.http.contract:http-operation-path operation)))
    (is (eq :authenticated
            (star.http.contract:http-operation-authority operation)))
    (is (equal '("actors:read")
               (star.http.contract:http-operation-scopes operation)))))

(test actor-discovery-is-advertised-by-capabilities
  (let* ((document (star.frontends.http-api::capabilities-document))
         (data (jsown:val document "data"))
         (features (jsown:val data "features"))
         (endpoints (jsown:val data "endpoints"))
         (endpoint
           (find "actors" endpoints
                 :key (lambda (item) (jsown:val item "id"))
                 :test #'string=)))
    (is (eq :true (jsown:val features "actors")))
    (is (not (null endpoint)))
    (is (string= "/api/v1/actors" (jsown:val endpoint "path")))
    (is (string= "authenticated" (jsown:val endpoint "authority")))))
