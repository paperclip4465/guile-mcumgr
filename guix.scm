(use-modules
  (guix packages)
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

(package
  (name "guile-mcumgr")
  (version "0.0.1")
  (source "./guile-mcumgr-0.0.1.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs
    `(("guile-cbor" ,guile-cbor)
      ("guile-gcrypt" ,guile-gcrypt)))
  (synopsis
    "The mcumgr smp protocol is used to manage microcontrollers.")
  (description
    "guile implementation of the mcumgr protocol.")
  (home-page "")
  (license license:gpl3+))
