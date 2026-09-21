(in-package :star.frontends.http-api)

(mount-http-operation "documents.lifecycle.get"
                      #'handle-document-lifecycle-get-route)
(mount-http-operation "documents.lifecycle.update"
                      #'handle-document-lifecycle-put-route)
