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
                   "release_20"))
      (arguments
       (substitute-keyword-arguments (package-arguments gnu:guix)
         ;; Run the tests if using a tagged release, but not when
         ;; using Guix through GDS_GNU_GUIX_PATH, as this slows down
         ;; development.
         ((#:tests? tests) (not local-source))))
      (source
       (if local-source
           (local-file (getenv "GDS_GNU_GUIX_PATH") "guix-gds"
                       #:recursive? #t
                       #:select? (force select?))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "http://git.cbaines.net/gds/gnu-guix")
                   (commit version)))
             (sha256
              (base32 "116fnz7j9k191a6cgk11bf4jv5vf2mrh6xg791cd54nfi1iszg3m"))
             (file-name (string-append "guix-" version "-checkout"))))))))
