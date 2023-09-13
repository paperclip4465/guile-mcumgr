(define-module (mcumgr os)
  #:use-module (ice-9 match)
  #:use-module (mcumgr)
  #:use-module (cbor)
  #:export (os-group
	    echo
	    reset))

(define os-group 0)

(define (echo smp-connection string)
  "Echo string over SMP-CONNECTION. Useful for testing connectivity.
SMP-CONNECTION should be a thunk which accepts a single <smp-frame> argument."
  (let ((resp (smp-connection (smp-frame
			       (group os-group)
			       (command 0)
			       (op 0)
			       (data
				(scm->cbor `(("d" . ,string))))))))
    (match (cbor->scm (smp-data resp))
      ((("rc" . code))
       (raise-exception (make-smp-exception (smp-error-from-code code))))
      ((("r" . msg)) msg))))

(define (reset smp-connection)
  "Reset device"
  (let* ((resp (smp-connection (smp-frame
				(group os-group)
				(command 5)
				(op 2)))))
    (match (cbor->scm (smp-data resp))
      ((("rc" . code))
       (raise-exception (make-smp-exception (smp-error-from-code code))))
      (_
       #t))))
