;; [[file:../source.org::*Get Documents][Get Documents:1]]
(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)

          (let ((document-id  (cdr (assoc :id params :test #'string=))))

            document-id)))
;; Get Documents:1 ends here
