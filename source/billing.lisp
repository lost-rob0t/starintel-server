;; SPDX-License-Identifier: GPL-3.0-or-later
(uiop:define-package :star.billing
  (:use :cl)
  (:export #:make-billing-preview)
  (:documentation
   "Pure, non-charging billing example helpers for StarIntel API clients."))

(in-package :star.billing)

(defparameter +max-billing-preview-items+ 64)
(defparameter +max-billing-description-length+ 256)
(defparameter +max-billing-quantity+ 1000000)
(defparameter +max-billing-unit-price-micros+ 1000000000000)
(defparameter +max-billing-total-micros+ 9000000000000000000)

(defun billing-unix-now ()
  (- (get-universal-time) 2208988800))

(defun valid-currency-code-p (value)
  (and (stringp value)
       (= 3 (length value))
       (every #'alpha-char-p value)))

(defun normalize-currency-code (value)
  (unless (valid-currency-code-p value)
    (error "Currency must be a three-letter alphabetic code"))
  (string-upcase value))

(defun billing-line-item (item index)
  (unless (and (consp item) (eq :obj (first item)))
    (error "Billing item ~d must be a JSON object" index))
  (let ((description (jsown:val-safe item "description"))
        (quantity (jsown:val-safe item "quantity"))
        (unit-price (jsown:val-safe item "unit_price_micros")))
    (unless (and (stringp description)
                 (<= 1 (length description) +max-billing-description-length+))
      (error "Billing item ~d description is invalid" index))
    (unless (and (integerp quantity)
                 (<= 1 quantity +max-billing-quantity+))
      (error "Billing item ~d quantity is outside the allowed range" index))
    (unless (and (integerp unit-price)
                 (<= 0 unit-price +max-billing-unit-price-micros+))
      (error "Billing item ~d unit_price_micros is outside the allowed range"
             index))
    (let ((line-total (* quantity unit-price)))
      (when (> line-total +max-billing-total-micros+)
        (error "Billing item ~d total is too large" index))
      (jsown:new-js
        ("description" description)
        ("quantity" quantity)
        ("unit_price_micros" unit-price)
        ("line_total_micros" line-total)))))

(defun make-billing-preview
    (principal-id currency items &key (created-at (billing-unix-now)))
  "Build a deterministic, non-mutating billing example for PRINCIPAL-ID.

CURRENCY is normalized to an upper-case three-letter code. ITEMS is a bounded
list of JSON objects containing =description=, positive integer =quantity=, and
non-negative integer =unit_price_micros=. The result is integer-only and marks
itself as a preview with =charged=false=. No account balance, payment provider,
or usage ledger is read or mutated."
  (unless (and (stringp principal-id) (plusp (length principal-id)))
    (error "Principal id must be a non-empty string"))
  (unless (and (listp items)
               (<= 1 (length items) +max-billing-preview-items+))
    (error "Billing preview requires between 1 and ~d items"
           +max-billing-preview-items+))
  (unless (and (integerp created-at) (not (minusp created-at)))
    (error "created-at must be a non-negative integer"))
  (let* ((normalized-currency (normalize-currency-code currency))
         (normalized-items
           (loop for item in items
                 for index from 0
                 collect (billing-line-item item index)))
         (total
           (reduce #'+ normalized-items
                   :key (lambda (item)
                          (jsown:val item "line_total_micros"))
                   :initial-value 0)))
    (when (> total +max-billing-total-micros+)
      (error "Billing preview total is too large"))
    (jsown:new-js
      ("principal_id" principal-id)
      ("currency" normalized-currency)
      ("items" normalized-items)
      ("total_micros" total)
      ("preview" :true)
      ("charged" :false)
      ("created_at" created-at)
      ("license" star.http.contract:+software-license+)
      ("license_scope" "server-software")
      ("source_repository" star.http.contract:+source-repository+))))
