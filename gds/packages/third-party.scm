(define-module (gds packages third-party)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages fontutils)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download))

(define-public phantomjs
  (package
    (name "phantomjs")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-" version "-linux-x86_64.tar.bz2"))
       (file-name (string-append name "-" version ".tar.bz2"))
       (sha256
        (base32 "0bqd8r97inh5f682m3cykg76s7bwjkqirxn9hhd5zr5fyi5rmpc6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source   (assoc-ref %build-inputs "source"))
               (out      (assoc-ref %outputs "out"))
               (bzip2    (assoc-ref %build-inputs "bzip2"))
               (tar      (assoc-ref %build-inputs "tar"))
               (patchelf (string-append
                          (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
               (ld-so (string-append (assoc-ref %build-inputs "libc")
                                     ,(glibc-dynamic-linker))))
           (mkdir-p out)
           (setenv "PATH" (string-append bzip2 "/bin"))
           (chdir out)
           (zero? (system* (string-append tar "/bin/tar") "xf" source "--strip" "1"))
           (system* patchelf "--set-interpreter" ld-so (string-append out "/bin/phantomjs"))
           (let*
               ((libs '("gcc:lib" "zlib" "fontconfig" "freetype" "libstdc++"))
                (ld-library-path
                 (map
                  (lambda (lib)
                    (string-append
                     (assoc-ref %build-inputs lib)
                     "/lib"))
                  libs)))
             (setenv "PATH" (string-append
                             (assoc-ref %build-inputs "bash")
                             "/bin"))
             (wrap-program
                 (string-append out "/bin/phantomjs")
               `("LD_LIBRARY_PATH" = (,(string-join
                                        ld-library-path
                                        ":")))))))))
    (native-inputs
     `(("tar" ,tar)
       ("bzip2" ,bzip2)
       ("patchelf" ,patchelf)
       ("libc" ,glibc)))
    (inputs
     `(("zlib" ,zlib)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libstdc++" ,libstdc++-4.9)
       ("gcc:lib" ,gcc-4.9 "lib")
       ("bash" ,bash)))
    (home-page "http://phantomjs.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))
