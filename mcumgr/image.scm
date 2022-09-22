(define-module (mcumgr image)
  #:use-module (guix records)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (gcrypt hash)
  #:use-module (mcumgr)
  #:use-module (cbor)
  #:export (image
	    make-image
	    image?
	    image-slot
	    image-version
	    image-hash
	    image-pending?
	    image-confirmed?
	    image-active?

	    image-group
	    image-list
	    image-erase
	    image-upload))

(define-record-type* <image>
  image make-image
  image?
  (slot image-slot)
  (version image-version)
  (hash image-hash)
  (pending? image-pending? (default #f))
  (confirmed? image-confirmed? (default #f))
  (active? image-active? (default #f)))

(define image-group 1)

(define image-hash-algorithm (hash-algorithm sha256))

(define (image-list smp-connection)
  (let* ((smp (smp-frame (op 0)
			 (group image-group)
			 (command 0)))
	 (res (cbor->scm (smp-data (smp-connection smp)))))
    (map (lambda (x)
	   (image
	    (slot (assoc-ref x "slot"))
	    (version (assoc-ref x "version"))
	    (hash (format #f "~{~X~}" (bytevector->u8-list
				       (assoc-ref x "hash"))))
	    (pending? (assoc-ref x "pending"))
	    (active? (assoc-ref x "active"))
	    (confirmed? (assoc-ref x "confirmed")) ))
	 (array->list (assoc-ref res "images")))))

(define (image-erase smp-connection)
  (cbor->scm (smp-data
	      (smp-connection (smp-frame
			       (op 2)
			       (group image-group)
			       (command 5))))))


(define %chunk-size 512)

(define* (image-upload smp-connection file #:key (chunk-size %chunk-size))

  (define hash (file-hash image-hash-algorithm file))

  (define (chunkify port acc offset)
    (let ((bv (get-bytevector-n port chunk-size)))
      (if (eof-object? bv)
	  (reverse acc)
	  (chunkify port
		    (cons (smp-frame
			   (op 2)
			   (group image-group)
			   (command 1)
			   (data (scm->cbor `(("len" . ,(bytevector-length bv))
					      ("off" . ,offset)
					      ("sha" . ,hash)
					      ("data" . ,bv)))))
			  acc)
		    (+ offset (bytevector-length bv))))))

  (let ((smp-frames (call-with-input-file file
		      (lambda (port)
			(chunkify port '() 0)))))
    (map (lambda (x) (cbor->scm (smp-data x)))
	 (map smp-connection smp-frames))))
