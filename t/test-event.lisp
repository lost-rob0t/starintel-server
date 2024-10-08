(in-package :star.tests)

(def-suite events :description "Event log testing.")

(in-suite events)

(def-test make-actor-event ()
  (let* ((actor-name "test-actor")
         (event-type "test-event")
         (details "test event")
         (source-id "foobar")
         (event (star.actors:make-actor-event :actor-name actor-name
                                              :event-type event-type
                                              :details details
                                              :source-id source-id)))
    (is (typep event 'star.actors:actor-event))
    (is (string= (star.actors:event-actor-name event) actor-name))
    (is (string= (star.actors:event-type event) event-type))
    (is (string= (star.actors:event-details event) details))
    (is (string= (star.actors:event-source-document event) source-id))
    (is (numberp (star.actors:event-timestamp event)))
    (is (stringp (star.actors:event-id event)))))
