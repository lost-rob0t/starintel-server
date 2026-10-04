(in-package :star-server-tests)

(def-suite event-store-tests
  :description "Tek9 event-source append, conflict, ordering, and restart tests")

(in-suite event-store-tests)

(defun make-event-store-test-path ()
  (merge-pathnames
   (format nil "starintel-event-store-test-~36R/"
           (random (expt 36 10)))
   (uiop:temporary-directory)))

(defmacro with-event-store-fixture ((store path) &body body)
  `(let ((,path (make-event-store-test-path)))
     (unwind-protect
          (progn
            (ignore-errors (star.event-store:close-event-store))
            (let ((,store
                    (star.event-store:open-event-store
                     :path ,path
                     :name "event-store-test")))
              ,@body))
       (ignore-errors (star.event-store:close-event-store))
       (ignore-errors
         (uiop:delete-directory-tree
          ,path
          :validate t
          :if-does-not-exist :ignore)))))

(test event-store-appends-and-replays-in-stream-order
  (with-event-store-fixture (store path)
    (declare (ignore path))
    (let ((first
            (star.event-store:append-event
             "event-1" "actor/hackmode" "actor.started"
             "{\"step\":1}"
             :metadata '(:generation 1)
             :store store))
          (second nil))
      (is (eq :appended
              (star.event-store:event-store-append-result-status first)))
      (is (= 0
             (star.event-store:event-store-append-result-sequence first)))
      (setf second
            (star.event-store:append-event
             "event-2" "actor/hackmode" "tool.invoked"
             "{\"step\":2}"
             :metadata '(:generation 1)
             :store store
             :expected-sequence 1
             :expected-predecessor "event-1"))
      (is (= 1
             (star.event-store:event-store-append-result-sequence second)))
      (let ((events
              (star.event-store:replay-event-stream
               "actor/hackmode" :store store)))
        (is (= 2 (length events)))
        (is (equal '("event-1" "event-2")
                   (mapcar #'star.event-store:event-store-record-event-id
                           events)))
        (is (null
             (star.event-store:event-store-record-predecessor-id
              (first events))))
        (is (string=
             "event-1"
             (star.event-store:event-store-record-predecessor-id
              (second events)))))
      (let ((head
              (star.event-store:verify-event-stream
               "actor/hackmode" :store store)))
        (is (= 1 (getf head :sequence)))
        (is (string= "event-2" (getf head :event-id)))))))

(test identical-event-id-is-idempotent-after-stream-advance
  (with-event-store-fixture (store path)
    (declare (ignore path))
    (star.event-store:append-event
     "event-1" "actor/hackmode" "actor.started" "same"
     :metadata '(:generation 1)
     :store store)
    (star.event-store:append-event
     "event-2" "actor/hackmode" "tool.invoked" "later"
     :store store)
    (let ((replayed
            (star.event-store:append-event
             "event-1" "actor/hackmode" "actor.started" "same"
             :metadata '(:generation 1)
             :store store)))
      (is (eq :replayed
              (star.event-store:event-store-append-result-status replayed)))
      (is (= 0
             (star.event-store:event-store-append-result-sequence replayed)))
      (is (= 1
             (getf (star.event-store:event-store-stream-head
                    "actor/hackmode" :store store)
                   :sequence))))))

(test changed-content-under-existing-event-id-conflicts
  (with-event-store-fixture (store path)
    (declare (ignore path))
    (star.event-store:append-event
     "event-1" "actor/hackmode" "actor.started" "original"
     :store store)
    (signals star.event-store:event-store-conflict
      (star.event-store:append-event
       "event-1" "actor/hackmode" "actor.started" "changed"
       :store store))))

(test stream-version-and-predecessor-checks-fail-closed
  (with-event-store-fixture (store path)
    (declare (ignore path))
    (star.event-store:append-event
     "event-1" "actor/hackmode" "actor.started" "one"
     :store store
     :expected-sequence 0
     :expected-predecessor nil)
    (signals star.event-store:event-store-version-conflict
      (star.event-store:append-event
       "event-2" "actor/hackmode" "tool.invoked" "two"
       :store store
       :expected-sequence 0))
    (signals star.event-store:event-store-version-conflict
      (star.event-store:append-event
       "event-2" "actor/hackmode" "tool.invoked" "two"
       :store store
       :expected-sequence 1
       :expected-predecessor "wrong-predecessor"))
    (is (= 0
           (getf (star.event-store:event-store-stream-head
                  "actor/hackmode" :store store)
                 :sequence)))))

(test event-store-survives-close-and-reopen
  (with-event-store-fixture (store path)
    (star.event-store:append-event
     "event-1" "actor/hackmode" "actor.started" "one"
     :store store)
    (star.event-store:append-event
     "event-2" "actor/hackmode" "tool.invoked" "two"
     :store store)
    (star.event-store:close-event-store store)
    (let* ((reopened
             (star.event-store:open-event-store
              :path path
              :name "event-store-test"))
           (events
             (star.event-store:replay-event-stream
              "actor/hackmode" :store reopened)))
      (is (= 2 (length events)))
      (is (string= "event-2"
                   (star.event-store:event-store-record-event-id
                    (second events)))))))
