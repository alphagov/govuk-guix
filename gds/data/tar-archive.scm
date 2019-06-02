(define-module (gds data tar-archive)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:export (<tar-archive>
            tar-archive
            tar-archive?
            tar-archive-name
            tar-archive-contents))

(define-record-type* <tar-archive>
  tar-archive make-tar-archive
  tar-archive?
  (name     tar-archive-name)
  (contents tar-archive-contents))

(define-gexp-compiler (tar-extract-compiler
                       (tar-extract <tar-archive>) system target)
  (match tar-extract
    (($ <tar-archive> name contents)
     (mlet %store-monad ((guile (package->derivation (default-guile) system)))

       (define inputs (list gzip xz))

       (define build
         (with-imported-modules `((guix build utils))
           #~(let* ((tar-command
                     (list
                      (string-append #$tar "/bin/tar")
                      "--checkpoint=20000"
                      "--checkpoint-action=echo='%ds: %{read,wrote}T'"
                      "--create"
                      "--verbose"
                      "--auto-compress"
                      "--file"
                      #$output
                      "--directory"
                      #$contents
                      ".")))
               (use-modules (srfi srfi-1)
                            (ice-9 ftw)
                            (guix build utils))
               (setenv "XZ_OPT" "-9 -T0")
               (set-path-environment-variable
                "PATH" '("bin" "sbin") '#+inputs)

               (simple-format #t "\nrunning ~A\n" (string-join tar-command))
               (force-output)
               (let ((status (apply system* tar-command)))
                 (unless (zero? status)
                   (exit 1))))))

       (gexp->derivation name build
                         #:system system
                         #:local-build? #t
                         #:recursive? #t
                         #:guile-for-build guile)))))
