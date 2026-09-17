(actor document-storage
  (:runtime native
   :service-uri "star://starintel:localhost:document-storage"
   :accepts (org.starintel/storage-command@1)
   :produces (org.starintel/storage-result@1)
   :handler document-storage-handler
   :restart permanent
   :mailbox (bounded 256)
   :metadata ((domain "starintel")
              (subsystem "document-storage")
              (blocking-io "pinned-dispatcher"))))
