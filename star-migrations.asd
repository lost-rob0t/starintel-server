;;;; Root source-registry shim for the migration library.
;;;; Keep the actual system definition beside its source files.
(load
 (merge-pathnames
  #p"source/migrations/star-migrations.asd"
  (make-pathname :name nil :type nil :defaults *load-truename*)))
