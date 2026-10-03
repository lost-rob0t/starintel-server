(in-package :star.authorization)

(defun split-printed-view-key (text)
  "Decode CouchDB's non-JSON printed composite-key form.

Examples include [dataset-1 tenant-1]. This parser is intentionally narrow and
never invokes the Common Lisp reader."
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (size (length trimmed)))
    (when (and (> size 1)
               (char= #\[ (char trimmed 0))
               (char= #\] (char trimmed (1- size))))
      (remove-if
       (lambda (token)
         (zerop (length token)))
       (uiop:split-string
        (subseq trimmed 1 (1- size))
        :separator '(#\Space #\Tab #\Newline #\Return))))))

(defun normalize-view-key-sequence (value)
  (cond
    ((listp value)
     (mapcar #'princ-to-string value))
    ((vectorp value)
     (map 'list #'princ-to-string value))
    (t value)))

(defun json-encoded-view-key-p (key)
  "Return true when KEY looks like a JSON-encoded array/object of quoted
values rather than CouchDB's bare printed composite-key form
(e.g. [dataset-1 tenant-1]).

jsown's reader treats a bare leading ``t`` as JSON ``true`` and a bare
leading ``f``/``n`` as ``false``/``null``, so feeding it the printed form
yields spurious tokens (e.g. ``[dataset-1 tenant-1]`` -> ``(T)``).  Only
strings whose first value starts with a JSON string quote are safe to hand
to jsown."
  (declare (type string key))
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) key))
         (length (length trimmed)))
    (and (>= length 2)
         (find (char trimmed 0) "[{")
         (loop for index from 1 below length
               for char = (char trimmed index)
               while (find char " " :test #'char=)
               finally (return (and char (char= char #\")))))))

(defun decode-view-key (key)
  "Decode CouchDB composite keys without invoking the Lisp reader."
  (cond
    ((stringp key)
     (if (json-encoded-view-key-p key)
         (handler-case
             (normalize-view-key-sequence (jsown:parse key))
           (error ()
             (or (split-printed-view-key key)
                 (error "Failed to decode view key ~s" key))))
         (or (split-printed-view-key key)
             (error "Failed to decode view key ~s" key))))
    ((listp key)
     (normalize-view-key-sequence key))
    ((vectorp key)
     (normalize-view-key-sequence key))
    (t key)))

(defun lucene-quoted-escape (value)
  "Escape only syntax that is special inside a quoted Lucene term."
  (with-output-to-string (stream)
    (loop for character across value
          do (when (or (char= character #\\)
                       (char= character #\"))
               (write-char #\\ stream))
             (write-char character stream))))

(defun lucene-term (field value)
  (format nil "~a:\"~a\"" field (lucene-quoted-escape value)))

(defparameter +search-query-audit-text-limit+ 16384
  "Maximum UTF-8 query text retained in one search audit event. Every query
still emits an event; oversized text is truncated and the SHA-256 digest plus
original byte length are retained so operators can identify the exact input.")

(defun search-query-audit-digest (text)
  "Return a stable SHA-256 digest for search audit correlation."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun search-query-audit-text (query)
  "Return QUERY text bounded for the telemetry queue plus truncation metadata."
  (let* ((octets (babel:string-to-octets query :encoding :utf-8))
         (byte-length (length octets))
         (truncated-p (> byte-length +search-query-audit-text-limit+)))
    (values
     (if truncated-p
         (babel:octets-to-string
          (subseq octets 0 +search-query-audit-text-limit+)
          :encoding :utf-8
          :errorp nil)
         query)
     byte-length
     truncated-p)))

(defun emit-search-query-audit (query principal requested-dataset requested-tenant metadata)
  "Emit one structured audit event for every search query entering authorization.

The raw user query is recorded because this is the operator-requested query
audit trail. It is bounded to +SEARCH-QUERY-AUDIT-TEXT-LIMIT+ bytes, while a
SHA-256 digest and original byte length identify oversized inputs. Credentials,
headers, cookies, request bodies and backend-expanded scoped queries are never
recorded here. Emission is a no-op unless observability logs are explicitly
enabled."
  (multiple-value-bind (text byte-length truncated-p)
      (search-query-audit-text query)
    (star.observability:emit-log-event
     :info
     "search query"
     :attributes
     (remove
      nil
      (list
       (cons "event.name" "search.query")
       (cons "query.text" text)
       (cons "query.sha256" (search-query-audit-digest query))
       (cons "query.bytes" byte-length)
       (cons "query.truncated" (if truncated-p "true" "false"))
       (cons "principal.id" (principal-id principal))
       (cons "starintel.tenant.id" requested-tenant)
       (and requested-dataset
            (cons "starintel.dataset.id" requested-dataset))
       (and metadata (getf metadata :route)
            (cons "http.route" (getf metadata :route)))
       (and metadata (getf metadata :method)
            (cons "http.request.method"
                  (string-upcase
                   (princ-to-string (getf metadata :method)))))
       (and metadata (getf metadata :correlation-id)
            (cons "starintel.operation.id"
                  (getf metadata :correlation-id))))))
    (star.observability:record-counter
     "starintel_search_queries_total"
     1
     :attributes
     (list (cons "query.truncated" (if truncated-p "true" "false"))))))

(defun authorized-search-query (query
                                &key principal requested-dataset
                                  (requested-tenant "default") metadata)
  "Audit and return a backend-scoped Clouseau query, never an unscoped
post-filter query. Every call emits a structured =search.query= event when
observability logging is enabled, including calls later rejected by policy."
  (let* ((candidate (candidate-principal principal))
         (scopes (principal-scopes candidate)))
    (emit-search-query-audit
     query candidate requested-dataset requested-tenant metadata)
    (authorize!
     "search:read"
     :principal candidate
     :metadata metadata)
    (let* ((wild-dataset
             (or (administrator-scopes-p scopes)
                 (member "*" (scope-values scopes "dataset:")
                         :test #'string=)))
           (wild-tenant
             (or (administrator-scopes-p scopes)
                 (member "*" (scope-values scopes "tenant:")
                         :test #'string=)))
           (datasets
             (restricted-values scopes "dataset:" requested-dataset))
           (tenants
             (restricted-values scopes "tenant:" requested-tenant)))
      (unless wild-dataset
        (require-search-dimension
         datasets "dataset_scope_required" "search:read" candidate metadata))
      (unless wild-tenant
        (require-search-dimension
         tenants "tenant_scope_required" "search:read" candidate metadata))
      (let ((clauses
              (remove nil
                      (list
                       (format nil "(~a)" query)
                       (and (not wild-dataset)
                            (lucene-scope-clause "dataset" datasets))
                       (and (not wild-tenant)
                            (lucene-scope-clause "tenant_id" tenants))))))
        (format nil "~{~a~^ AND ~}" clauses)))))
