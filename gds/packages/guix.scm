(define-module (gds packages guix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module ((gnu packages package-management) #:prefix gnu:))

(define-public guix
  (let ((select? (delay (git-predicate
                         (getenv "GDS_GNU_GUIX_PATH"))))
        (local-source (string? (getenv "GDS_GNU_GUIX_PATH"))))
    (if (and local-source
             (not (file-exists? (getenv "GDS_GNU_GUIX_PATH"))))
        (error "GDS_GNU_GUIX_PATH directory does not exist"))
    (package
      (inherit gnu:guix)
      (name "guix-gds")
      (version (if local-source
                   "local"
                   "release_23"))
      (arguments
       (if local-source
           (ensure-keyword-arguments
            (package-arguments gnu:guix)
            ;; Run the tests if using a tagged release, but not when
            ;; using Guix through GDS_GNU_GUIX_PATH, as this slows down
            ;; development.
            '(#:tests? #f))
           (package-arguments gnu:guix)))
      (source
       (if local-source
           (local-file (getenv "GDS_GNU_GUIX_PATH") "guix-gds"
                       #:recursive? #t
                       #:select? (force select?))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://git.cbaines.net/gds/gnu-guix")
                   (commit version)))
             (sha256
              (base32 "0sdki14msknqnrd6biw0vrhqkd69mn9hwbb6lirpv4f3gwyf7i7h"))
             (file-name (string-append "guix-" version "-checkout"))))))))

(define-public guix-no-tests
  (package
    (inherit guix)
    (name "guix-gds-no-tests")
    (arguments
     (ensure-keyword-arguments
      (package-arguments guix)
      '(#:tests? #f)))))
