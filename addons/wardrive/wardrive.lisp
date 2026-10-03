(in-package :star.addons.wardrive)

(defparameter *dataset* "warstar")
(defparameter +max-batch+ 100)

(defun reject-input (message)
  (star.frontends.http-api::signal-http-input-error
   422 "invalid_wardrive_observation" message))

(defun field (object key)
  (jsown:val-safe object key))

(defun required-string (object key &optional (max-length 128))
  (let ((value (field object key)))
    (unless (and (stringp value) (<= 1 (length value) max-length))
      (reject-input (format nil "~a is required" key)))
    value))

(defun required-number (object key low high)
  (let ((value (field object key)))
    (unless (and (realp value) (<= low value high))
      (reject-input (format nil "~a is out of range" key)))
    value))

(defun sample-fields (sample)
  (unless (star.frontends.http-api:json-object-p sample)
    (reject-input "Every observation must be an object"))
  (unless (integerp (required-number sample "id" 1 most-positive-fixnum))
    (reject-input "id must be an integer"))
  (let ((address (required-string sample "address" 64)))
    (unless (cl-ppcre:scan "^[A-Za-z0-9:._-]+$" address)
      (reject-input "address contains unsupported characters")))
  (when (and (field sample "radio") (not (stringp (field sample "radio"))))
    (reject-input "radio must be a string"))
  (required-number sample "level" -200 100)
  (required-number sample "latitude" -90 90)
  (required-number sample "longitude" -180 180)
  (unless (integerp (required-number sample "time" 1 most-positive-fixnum))
    (reject-input "time must be an integer"))
  sample)

(defun schema-ready-p ()
  (string= "0.10.1" starintel:+starintel-doc-version+))

(defun schema-required ()
  (unless (schema-ready-p)
    (star.frontends.http-api::signal-http-input-error
     503 "starintel_0_10_1_required"
     "Wardrive ingest requires the Star Language generated 0.10.1 server contract")))

(defun decimal-string (number)
  (format nil "~,8f" (coerce number 'double-float)))

(defun base-document (id dtype millis source-kind)
  (jsown:new-js
   ("id" id)
   ("dataset" *dataset*)
   ("dtype" dtype)
   ("schemaVersion" "0.10.1")
   ("observedAt" (floor millis 1000))
   ("collector" "star:v1:collector:wireless")
   ("sourceKinds" (vector source-kind))))

(defun security-type (description)
  (let ((value (string-upcase (if (stringp description) description ""))))
    (cond
      ((search "WPA3" value) "wpa3-psk")
      ((search "WPA2" value) "wpa2-psk")
      ((search "WPA" value) "wpa-psk")
      ((search "WEP" value) "wep")
      ((search "ESS" value) "open")
      (t "unknown"))))

(defun sample-documents (device-id sample)
  (sample-fields sample)
  (let* ((base (format nil "star:wardrive:~a:~d" device-id (field sample "id")))
         (geo-id (format nil "~a:geo" base))
         (radio (string-upcase (or (field sample "radio") "")))
         (wifi (member radio '("W" "WIFI") :test #'string=))
         (source-kind (if (member (field sample "source")
                                  '("wigle-csv" "wigle-import") :test #'equal)
                          "import" "sensor"))
         (geo (base-document geo-id "geo-point" (field sample "time") source-kind))
         (network (base-document base (if wifi "wireless-network" "network-device")
                                 (field sample "time") source-kind)))
    (jsown:extend-js geo
      ("geometryType" "point")
      ("latitude" (decimal-string (field sample "latitude")))
      ("longitude" (decimal-string (field sample "longitude"))))
    (when (realp (field sample "accuracy"))
      (setf (jsown:val geo "accuracyMeters")
            (decimal-string (field sample "accuracy"))))
    (if wifi
        (progn
          (jsown:extend-js network
            ("bssid" (field sample "address"))
            ("security" (security-type (field sample "security")))
            ("signalDbm" (field sample "level"))
            ("observations" 1)
            ("location" (jsown:new-js
                         ("schema" "org.starintel/core@1/geo-point")
                         ("id" geo-id))))
          (when (stringp (field sample "name"))
            (setf (jsown:val network "ssid") (field sample "name")))
          (when (and (integerp (field sample "frequency"))
                     (> (field sample "frequency") 0))
            (setf (jsown:val network "frequencyMhz")
                  (field sample "frequency"))))
        (jsown:extend-js network
          ("deviceId" (field sample "address"))
          ("deviceClass" "unknown")
          ("extensions" (jsown:new-js
                         ("radio" radio)
                         ("geoPointId" geo-id)
                         ("signalDbm" (field sample "level"))))))
    (setf (jsown:val network "raw") sample)
    (list geo network)))

(defun handle-wardrive-observations (params)
  (declare (ignore params))
  (star.frontends.http-api::with-http-boundary ()
    (schema-required)
    (let* ((body (star.frontends.http-api::require-json-object
                  (star.frontends.http-api::parse-json-request)))
           (device-id (required-string body "device_id" 36))
           (samples (star.frontends.http-api::require-json-array
                     (field body "observations"))))
      (unless (and (= 36 (length device-id))
                   (cl-ppcre:scan
                    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
                    device-id)
                   (<= 1 (length samples) +max-batch+))
        (reject-input "Expected an installation UUID and 1–100 observations"))
      (let ((documents
              (loop for sample in samples
                    append (sample-documents device-id sample))))
        ;; Validate every generated 0.10.1 document before publishing any.
        ;; The normal StarIntel ingest pipeline owns persistence and routing.
        (dolist (document documents)
          (star.frontends.http-api:validate-document-input document))
        (dolist (document documents)
          (star.frontends.http-api::publish-document document))
        (setf (lack.response:response-status *response*) 202)
        (jsown:to-json
         (jsown:new-js
          ("status" "accepted")
          ("observations" (length samples))
          ("documents" (length documents))))))))

(setf (ningle:route star.frontends.http-api::*app* "/warstar/observations" :method :post)
      #'handle-wardrive-observations)

(defun start-wardrive-addon ()
  (log:info "Wardrive ingest route registered; canonical 0.10.1 readiness: ~a"
            (schema-ready-p))
  t)

(defun stop-wardrive-addon () t)

(star:register-addon :starintel-wardrive
                     :system :starintel-wardrive
                     :start #'start-wardrive-addon
                     :stop #'stop-wardrive-addon)
