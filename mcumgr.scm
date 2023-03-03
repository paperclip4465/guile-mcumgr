(define-module (mcumgr)
  #:use-module (cbor)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (guix records)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (smp-udp-connection
	    smp-frame
	    smp-frame?
	    smp-result
	    smp-group
	    smp-op
	    smp-command
	    smp-sequence-num
	    smp-data
	    smp-error-from-code
	    smp-error-code))

(define-record-type* <smp-frame>
  smp-frame make-smp-frame
  smp-frame?
  (result smp-result
	  (default 0))
  (op smp-op)
  (group smp-group)
  (command smp-command)
  (sequence-num smp-sequence-num
		(default 0))
  (data smp-data
	(default #vu8())))

(define %status-codes
  '((#:ok . 0)
    (#:unknown . 1)
    (#:no-memory . 2)
    (#:invalid . 3)
    (#:timeout . 4)
    (#:no-entry . 5)
    (#:bad-state . 6)
    (#:response-too-long . 7)
    (#:not-supported . 8)
    (#:corrupted-payload . 9)
    (#:Device-busy . 10)
    (#:use-error-base . 256)))

(define (smp-error-code result)
  (assoc-ref %status-codes result))

(define (smp-error-from-code code)
  (car
   (find (lambda (x)
	   (= code (cdr x)))
	 %status-codes)))


;; Seconds before timeout condition is raised
(define %smp-timeout 5)

(define* (serialize-smp frame #:key
			(additional-commands '())
			(additional-ret-codes '()))
  (let ((bv (make-bytevector 8)))
    (bytevector-u8-set! bv 0 (smp-op frame))
    (bytevector-u8-set! bv 1 0)
    (bytevector-u16-set! bv 2 (bytevector-length (smp-data frame))
			 (endianness big))
    (bytevector-u16-set! bv 4 (smp-group frame) (endianness big))
    (bytevector-u8-set! bv 6 (smp-sequence-num frame))
    (bytevector-u8-set! bv 7 (smp-command frame))
    (u8-list->bytevector
     (append (bytevector->u8-list bv)
	     (bytevector->u8-list (smp-data frame))))))

(define (read-smp port)
  (let ((bv (get-bytevector-n port 8)))
    (smp-frame
     (result (ash (bytevector-u8-ref bv 0) -3))
     (op (logand (bytevector-u8-ref bv 0) 7))
     (group (bytevector-u16-ref bv 4 (endianness big)))
     (sequence-num (bytevector-u8-ref bv 6))
     (command (bytevector-u8-ref bv 7))
     (data (get-bytevector-n port
			     (bytevector-u16-ref bv 2 (endianness big)))))))


(define (send-udp-smp smp address port)
  (let ((s (socket AF_INET SOCK_DGRAM 0))
	(timed-out #f))
    (sendto s (serialize-smp smp) AF_INET
	    (inet-pton AF_INET address) port)
    (let* ((bv (make-bytevector 512))
	   (fd-list (car (select (list s) '() '() %smp-timeout))))
      (if (nil? fd-list)
	  (smp-frame
	   (result 0)
	   (op 2)
	   (group 0)
	   (sequence-num 0)
	   (command 0)
	   (data (scm->cbor `(("rc" . ,(smp-error-code #:timeout))))))
	  (begin
	    (recvfrom! s bv)
	    (call-with-input-bytevector bv read-smp))))))

(define (smp-udp-connection address port)
  (cut send-udp-smp <> address port))
