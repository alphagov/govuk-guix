(define-module (gds data tar-extract)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:export (<tar-extract>
            tar-extract
            tar-extract?
            tar-extract-name
            tar-extract-archive
            tar-extract-member))

(define-record-type* <tar-extract>
  tar-extract make-tar-extract
  tar-extract?
  (name tar-extract-name)
  (archive tar-extract-archive)
  (member tar-extract-member))

(define-gexp-compiler (tar-extract-compiler
                       (tar-extract <tar-extract>) system target)
  (match tar-extract
    (($ <tar-extract> name archive member)
     (mlet %store-monad ((guile (package->derivation (default-guile) system)))
       (define build
         (with-imported-modules `((guix build utils))
           #~(let ((tar-command
                    (list
                     (string-append #$tar "/bin/tar")
                     "--extract"
                     "--file" #$archive
                     (string-append "--directory=" "/gnu/store");;#$output)
                     "-x"
                     #$member)))
               (display #$output)
               (setenv "PATH" (string-append #$gzip "/bin"))
               (simple-format #t "running ~A\n" (string-join tar-command))
               (force-output)
               (let ((status (apply system* tar-command)))
                 (unless (zero? status)
                   (exit 1))
                 (rename-file (string-append "/gnu/store/" #$member) #$output)))))

       (gexp->derivation name build
                         #:system system
                         #:local-build? #t
                         #:recursive? #t
                         #:guile-for-build guile)))))
