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
  ;; Note that when changing the treeish, you will need to change the
  ;; sha256 hash such that Guix thinks that it has not built this
  (let ((treeish "release_3")
        (select? (delay (git-predicate
                         (getenv "GDS_GNU_GUIX_PATH"))))
        (local-source (string? (getenv "GDS_GNU_GUIX_PATH"))))
    (package
      (inherit gnu:guix)
      (name "guix-gds")
      (arguments
       (substitute-keyword-arguments (package-arguments gnu:guix)
         ((#:tests? tests)
          #f)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check)))))
      (source
       (if local-source
           (local-file (getenv "GDS_GNU_GUIX_PATH") "guix-gds"
                       #:recursive? #t
                       #:select? (force select?))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/alphagov/gnu-guix.git")
                   (commit treeish)))
             (sha256
              (base32
               "10kcjdlzc56siz64qjzsqjpnrrlnj94d163w1ars0hgj1y9y5bb7"))
             (file-name (string-append
                         "guix-"
                         (package-version gnu:guix)
                         "-checkout"))))))))
