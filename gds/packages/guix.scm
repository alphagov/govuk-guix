(define-module (gds packages guix)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((gnu packages package-management) #:prefix gnu:))

(define-public guix
  ;; Note that when changing the treeish, you will need to change the
  ;; sha256 hash such that Guix thinks that it has not built this
  (let ((treeish "spike-end-to-end-test-system"))
    (package
      (inherit gnu:guix)
      (name "guix-gds")
      (version (string-append "0.11.0"))
      (arguments
       (substitute-keyword-arguments (package-arguments gnu:guix)
         ((#:tests? tests)
          #f)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check)))))
      (source
       (or (getenv "GDS_GNU_GUIX_PATH")
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/alphagov/gnu-guix.git")
                   (commit treeish)))
             (sha256
              (base32
               "1lfp1y8jjqbd73w45h421b550ca0lqbl6w5yypxxqff8r0mi9i07"))
             (file-name (string-append "guix-" version "-checkout"))))))))
