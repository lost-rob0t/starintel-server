(in-package :star.databases.couchdb)

;;; Final view-request normalization layer.
;;;
;;; CouchDB uses inclusive_end=true by default.  Keep the default explicit in
;;; our request builder so callers can safely request half-open ranges without
;;; bypassing the typed view-query API.

(defun couchdb-view-query-parameters
    (limit skip descending include-docs reduce update inclusive-end
     key start-key end-key group group-level)
  (append
   `(("limit" . ,limit)
     ("skip" . ,skip)
     ("descending" . ,(if descending "true" "false"))
     ("include_docs" . ,(if include-docs "true" "false"))
     ("reduce" . ,(if reduce "true" "false"))
     ("update" . ,update)
     ("inclusive_end" . ,(if inclusive-end "true" "false")))
   (when key
     `(("key" . ,(couchdb-json-query-value key))))
   (when start-key
     `(("startkey" . ,(couchdb-json-query-value start-key))))
   (when end-key
     `(("endkey" . ,(couchdb-json-query-value end-key))))
   (when group
     '(("group" . "true")))
   (when group-level
     `(("group_level" . ,group-level)))))

(defun build-couchdb-view-request
    (client database design-document view-name
     &key
       (limit 50)
       (skip 0)
       (descending nil)
       (include-docs nil)
       (reduce nil)
       (update t)
       (inclusive-end t)
       key
       keys
       start-key
       end-key
       (group nil)
       group-level)
  "Build one validated CouchDB view request without performing network I/O.

INCLUSIVE-END follows CouchDB's inclusive_end query parameter and defaults to
T, matching CouchDB's native behavior."
  (validate-view-query-options
   limit skip descending include-docs reduce group group-level
   key keys start-key end-key)
  (validate-view-boolean "inclusive-end" inclusive-end)
  (let* ((normalized-update (normalize-view-update update))
         (parameters
           (couchdb-view-query-parameters
            limit skip descending include-docs reduce normalized-update
            inclusive-end key start-key end-key group group-level))
         (uri
           (couchdb-view-uri
            client database design-document view-name parameters)))
    (if keys
        (make-couchdb-view-request
         :post uri (jsown:to-json (jsown:new-js ("keys" keys))))
        (make-couchdb-view-request :get uri nil))))

(defun query-view
    (client database design-document view-name
     &rest arguments
     &key
       (limit 50)
       (skip 0)
       (descending nil)
       (include-docs nil)
       (reduce nil)
       (update t)
       (inclusive-end t)
       key
       keys
       start-key
       end-key
       (group nil)
       group-level
     &allow-other-keys)
  "Execute a validated view query against a CouchDB design document.

INCLUSIVE-END is part of the validated public query surface.  All validation
still occurs before the configured transport is invoked."
  (declare (ignore limit skip descending include-docs reduce update
                   inclusive-end key keys start-key end-key group group-level))
  (let* ((request
           (apply #'build-couchdb-view-request
                  client database design-document view-name arguments))
         (response (funcall *couchdb-view-transport* client request)))
    (jsown:parse response)))
