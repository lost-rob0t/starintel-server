(in-package :star-server-tests)

(in-suite document-storage-tests)

(test s3-sigv4-matches-aws-published-get-object-vector
  ;; AWS S3 SigV4 documentation: GET /test.txt with Range bytes=0-9,
  ;; timestamp 20130524T000000Z, and the public EXAMPLE credentials.
  ;; Expected signature: f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41
  (let* ((backend
           (make-instance
            'star.storage::s3-storage-backend
            :name "s3-test"
            :endpoint "https://examplebucket.s3.amazonaws.com"
            :region "us-east-1"
            :bucket "examplebucket"
            :access-key-id "AKIAIOSFODNN7EXAMPLE"
            :secret-access-key
            "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"))
         (payload-hash
           "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
         (headers
           (list
            (cons "host" "examplebucket.s3.amazonaws.com")
            (cons "range" "bytes=0-9")
            (cons "x-amz-content-sha256" payload-hash)
            (cons "x-amz-date" "20130524T000000Z")))
         (authorization
           (star.storage::s3-authorization-header
            backend
            :get
            "/test.txt"
            payload-hash
            "20130524T000000Z"
            "20130524"
            headers)))
    (is (search
         "Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
         authorization))))
