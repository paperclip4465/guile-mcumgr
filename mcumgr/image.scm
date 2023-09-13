(define-module (mcumgr image)
  #:use-module (guix records)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
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
	    image-upload
	    image-confirm))

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

(define (smp-image-resp->images resp)
  (map (lambda (x)
	 (image
	  (slot (assoc-ref x "slot"))
	  (version (assoc-ref x "version"))
	  (hash (format #f "~{~X~}" (bytevector->u8-list
				     (assoc-ref x "hash"))))
	  (pending? (assoc-ref x "pending"))
	  (active? (assoc-ref x "active"))
	  (confirmed? (assoc-ref x "confirmed")) ))
       (array->list (assoc-ref resp "images"))))

(define (image-list smp-connection)
  (let ((smp (smp-frame (op 0)
			(group image-group)
			(command 0))))
    (smp-image-resp->images
     (cbor->scm (smp-data (smp-connection smp))))))

(define (image-erase smp-connection)
  (match (cbor->scm (smp-data
		     (smp-connection (smp-frame
				      (op 2)
				      (group image-group)
				      (command 5)
				      (data (scm->cbor '()))))))
    ((("rc" . 0)) #t)
    ((("rc" . code))
     (raise-exception (make-smp-exception (smp-error-from-code code))))))


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
    (and-map (lambda (x)
	       (match (cbor->scm (smp-data (smp-connection x)))
		 ((("rc" . 0) ("off" . off))
		  #t)
		 ((("rc" . code) ("off" . off))
		  (values #f
			  off))
		 ((("rc" . code))
		  (raise-exception (make-smp-exception (smp-error-from-code code))))))
	     smp-frames)))


(define* (image-confirm smp-connection hash #:optional (confirm #t))
  (let* ((smp (smp-frame
	       (op 2)
	       (group image-group)
	       (command 0)
	       (data (scm->cbor `(,@(if confirm
					'()
					`(("hash" . ,hash)))
				  ("confirm" . ,confirm))))))
	 (resp (cbor->scm (smp-data (smp-connection smp)))))
    (match resp
      ((("rc" . code))
       (raise-exception (make-smp-exception (smp-error-from-code code))))
      (_ (smp-image-resp->images resp)))))
