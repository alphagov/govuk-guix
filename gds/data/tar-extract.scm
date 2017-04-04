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

       (define inputs (list gzip bzip2))

       (define build
         (with-imported-modules `((guix build utils))
           #~(let* ((tmp-directory (tmpnam))
                    (tar-command
                     (list
                      (string-append #$tar "/bin/tar")
                      "--extract"
                      "--wildcards"
                      "--file" #$archive
                      (string-append "--directory=" tmp-directory)
                      "-x"
                      #$member)))
               (use-modules (srfi srfi-1)
                            (srfi srfi-26)
                            (ice-9 ftw)
                            (guix build utils))
               (set-path-environment-variable
                "PATH" '("bin" "sbin") '#+inputs)

               (simple-format #t "\nrunning ~A\n" (string-join tar-command))
               (force-output)
               (mkdir tmp-directory)
               (let ((status (apply system* tar-command)))
                 (unless (zero? status)
                   (exit 1))
                 (let ((extracted-files
                        (find-files tmp-directory)))
                   (simple-format #t "extracted ~A\n" extracted-files)
                   (if (eq? 1 (length extracted-files))
                       (begin
                         (let ((file (first extracted-files)))
                           (rename-file file #$output)
                           (simple-format #t "tar-extract of ~A produced ~A\n"
                                          #$archive #$output)))
                       (begin
                         (display "\nMore than one file extracted:\n")
                         (for-each (cut (simple-format #t " - ~A\n" <>)) extracted-files)
                         (error "More than one file extracted"))))))))

       (gexp->derivation name build
                         #:system system
                         #:local-build? #t
                         #:recursive? #t
                         #:guile-for-build guile)))))
