(uiop:define-package :star.hot-reload
  (:use :cl)
  (:export
   #:hot-reload-error
   #:hot-reload-error-code
   #:hot-reload-error-message
   #:*enabled-p*
   #:*directory*
   #:*poll-seconds*
   #:*max-patch-bytes*
   #:apply-patch-file
   #:start-watcher
   #:stop-watcher
   #:watcher-running-p
   #:image-id
   #:image-generation
   #:image-marker
   #:status-json))
