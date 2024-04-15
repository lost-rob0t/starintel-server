(uiop:define-package   :starintel-gserver
  (:nicknames :star)
  (:use       :cl :cl-rabbit :star.settings)
  (:export
   #:init-db))

(uiop:define-package   :starintel-gserver.rabbit
  (:nicknames :star.rabbit)
  (:use       :cl :star.settings)
  (:documentation "Rabitmq namespace"))
