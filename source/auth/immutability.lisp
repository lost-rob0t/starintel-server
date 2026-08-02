(in-package :star.auth)

(defparameter *raw-request-principal-id-reader*
  (symbol-function 'request-principal-id))
(defparameter *raw-request-principal-type-reader*
  (symbol-function 'request-principal-type))
(defparameter *raw-request-principal-scopes-reader*
  (symbol-function 'request-principal-scopes))
(defparameter *raw-request-principal-credential-id-reader*
  (symbol-function 'request-principal-credential-id))
(defparameter *raw-request-security-context-correlation-id-reader*
  (symbol-function 'request-security-context-correlation-id))
(defparameter *raw-service-call-context-principal-id-reader*
  (symbol-function 'service-call-context-principal-id))
(defparameter *raw-service-call-context-principal-type-reader*
  (symbol-function 'service-call-context-principal-type))
(defparameter *raw-service-call-context-credential-id-reader*
  (symbol-function 'service-call-context-credential-id))
(defparameter *raw-service-call-context-scopes-reader*
  (symbol-function 'service-call-context-scopes))
(defparameter *raw-service-call-context-correlation-id-reader*
  (symbol-function 'service-call-context-correlation-id))

(defun copy-string-or-nil (value)
  (and value (copy-seq value)))

(defun defensive-request-principal-id (principal)
  (copy-string-or-nil
   (funcall *raw-request-principal-id-reader* principal)))

(defun defensive-request-principal-type (principal)
  (copy-string-or-nil
   (funcall *raw-request-principal-type-reader* principal)))

(defun defensive-request-principal-scopes (principal)
  (mapcar #'copy-string-or-nil
          (funcall *raw-request-principal-scopes-reader* principal)))

(defun defensive-request-principal-credential-id (principal)
  (copy-string-or-nil
   (funcall *raw-request-principal-credential-id-reader* principal)))

(defun defensive-request-security-context-correlation-id (context)
  (copy-string-or-nil
   (funcall *raw-request-security-context-correlation-id-reader* context)))

(defun defensive-service-call-context-principal-id (context)
  (copy-string-or-nil
   (funcall *raw-service-call-context-principal-id-reader* context)))

(defun defensive-service-call-context-principal-type (context)
  (copy-string-or-nil
   (funcall *raw-service-call-context-principal-type-reader* context)))

(defun defensive-service-call-context-credential-id (context)
  (copy-string-or-nil
   (funcall *raw-service-call-context-credential-id-reader* context)))

(defun defensive-service-call-context-scopes (context)
  (mapcar #'copy-string-or-nil
          (funcall *raw-service-call-context-scopes-reader* context)))

(defun defensive-service-call-context-correlation-id (context)
  (copy-string-or-nil
   (funcall *raw-service-call-context-correlation-id-reader* context)))

(eval-when (:load-toplevel :execute)
  (setf (symbol-function 'request-principal-id)
        #'defensive-request-principal-id
        (symbol-function 'request-principal-type)
        #'defensive-request-principal-type
        (symbol-function 'request-principal-scopes)
        #'defensive-request-principal-scopes
        (symbol-function 'request-principal-credential-id)
        #'defensive-request-principal-credential-id
        (symbol-function 'request-security-context-correlation-id)
        #'defensive-request-security-context-correlation-id
        (symbol-function 'service-call-context-principal-id)
        #'defensive-service-call-context-principal-id
        (symbol-function 'service-call-context-principal-type)
        #'defensive-service-call-context-principal-type
        (symbol-function 'service-call-context-credential-id)
        #'defensive-service-call-context-credential-id
        (symbol-function 'service-call-context-scopes)
        #'defensive-service-call-context-scopes
        (symbol-function 'service-call-context-correlation-id)
        #'defensive-service-call-context-correlation-id))
