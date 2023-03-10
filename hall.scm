(hall-description
  (name "mcumgr")
  (prefix "guile")
  (version "0.0.1")
  (author "Mitchell Schmeisser")
  (copyright (2022))
  (synopsis
    "The mcumgr smp protocol is used to manage microcontrollers.")
  (description
    "guile implementation of the mcumgr protocol.")
  (home-page "")
  (license gpl3+)
  (dependencies
    `(("guile-cbor" (cbor) ,guile-cbor)
      ("guile-gcrypt" (gcrypt hash) ,guile-gcrypt)))
  (skip ())
  (files (libraries
	   ((directory
	     "mcumgr"
	     ((compiled-scheme-file "image")
	      (compiled-scheme-file "os")))
	    (directory
	     "guix"
	     ((scheme-file "records")))
	    (scheme-file "mcumgr")))
	 (tests ((directory "tests" ())))
	 (programs ((directory "scripts" ((in-file "mcumgr-cli")))))
	 (documentation
	   ((directory "doc" ((texi-file "mcumgr")))
	    (text-file "COPYING")
	    (text-file "HACKING")
	    (symlink "README" "README.org")
	    (org-file "README")))
	 (infrastructure
	   ((scheme-file "hall")
	    (text-file ".gitignore")
	    (scheme-file "guix")))))
