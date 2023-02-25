(use-modules (guix packages)
	     ((guix licenses) #:prefix license:)
	     (guix download)
	     (guix build-system gnu)
	     (gnu packages)
	     (gnu packages autotools)
	     (gnu packages guile)
	     (gnu packages guile-xyz)
	     (gnu packages gnupg)
	     (gnu packages pkg-config)
	     (gnu packages texinfo))

(define* (fix-guile guile-package #:optional (guile guile-3.0))
  "Adds GUILE to native-inputs of GUILE-PACKAGE so it can cross-compile."
  (package (inherit guile-package)
    (native-inputs
     (cons `("guile" ,guile)
	   (package-native-inputs guile-package)))))

(package
 (name "guile-mcumgr")
 (version "0.0.1")
 (source "./guile-mcumgr-0.0.1.tar.gz")
 (build-system gnu-build-system)
 (arguments
  `(#:modules
    ((ice-9 match)
     (ice-9 ftw)
     ,@%gnu-build-system-modules)
    #:phases
    (modify-phases
     %standard-phases
     (add-after
      'install
      'hall-wrap-binaries
      (lambda* (#:key inputs outputs #:allow-other-keys)
	(let* ((compiled-dir
		(lambda (out version)
		  (string-append
		   out
		   "/lib/guile/"
		   version
		   "/site-ccache")))
	       (uncompiled-dir
		(lambda (out version)
		  (string-append
		   out
		   "/share/guile/site"
		   (if (string-null? version) "" "/")
		   version)))
	       (dep-path
		(lambda (env modules path)
		  (list env
			":"
			'prefix
			(cons modules
			      (map (lambda (input)
				     (string-append
				      (assoc-ref inputs input)
				      path))
				   ,''("guile-cbor" "guile-gcrypt"))))))
	       (out (assoc-ref outputs "out"))
	       (bin (string-append out "/bin/"))
	       (site (uncompiled-dir out "")))
	  (match (scandir site)
	    (("." ".." version)
	     (for-each
	      (lambda (file)
		(wrap-program
		 (string-append bin file)
		 (dep-path
		  "GUILE_LOAD_PATH"
		  (uncompiled-dir out version)
		  (uncompiled-dir "" version))
		 (dep-path
		  "GUILE_LOAD_COMPILED_PATH"
		  (compiled-dir out version)
		  (compiled-dir "" version))))
	      ,''("mcumgr-cli"))
	     #t))))))))
 (native-inputs
  (list autoconf
	 automake
	 pkg-config
	 texinfo
	 guile-3.0
	 guile-config
	 guile-cbor
	 guile-gcrypt))
 (inputs `(("guile" ,guile-3.0)))
 (propagated-inputs
  (map fix-guile
       (list guile-config
	     guile-cbor
	     guile-gcrypt)))
 (synopsis
  "The mcumgr smp protocol is used to manage microcontrollers.")
 (description
  "guile implementation of the mcumgr protocol.")
 (home-page "")
 (license license:gpl3+))
