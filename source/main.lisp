;; [[file:../source.org::*Main Entry][Main Entry:1]]
(in-package :starintel-gserver)
(defun main ()
  (sento-user::start-actors)
  (star.rabbit:start-rabbit-document-thread)
  (star.frontends.http-api::start-http-api)
  (loop))
;; Main Entry:1 ends here
