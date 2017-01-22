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

(define (make-git-predicate directory)
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  Upon Git failure, return #f instead of a predicate."
  (define (parent-directory? thing directory)
    ;; Return #t if DIRECTORY is the parent of THING.
    (or (string-suffix? thing directory)
        (and (string-index thing #\/)
             (parent-directory? (dirname thing) directory))))

  (let* ((pipe        (with-directory-excursion directory
                        (open-pipe* OPEN_READ "git" "ls-files")))
         (files       (let loop ((lines '()))
                        (match (read-line pipe)
                          ((? eof-object?)
                           (reverse lines))
                          (line
                           (loop (cons line lines))))))
         (status      (close-pipe pipe)))
    (and (zero? status)
         (lambda (file stat)
           ;(simple-format #t "~A\n" file)
           (let ((foo
                  (match (stat:type stat)
             ('directory
              ;; 'git ls-files' does not list directories, only regular files,
              ;; so we need this special trick.
              (any (cut parent-directory? <> file) files))
             ((or 'regular 'symlink)
              (any (cut string-suffix? <> file) files))
             (_
              #f))))
             ;(simple-format #t "~A\n" foo)
             foo)
                  ))))

(define-public guix
  ;; Note that when changing the treeish, you will need to change the
  ;; sha256 hash such that Guix thinks that it has not built this
  (let ((treeish "spike-end-to-end-test-system")
        (select? (delay (make-git-predicate
                         (getenv "GDS_GNU_GUIX_PATH"))))
        (local-source (string? (getenv "GDS_GNU_GUIX_PATH"))))
    (package
      (inherit gnu:guix)
      (name "guix-gds")
      (version (if local-source
                   "local"
                   "0.11.0"))
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
               "1lfp1y8jjqbd73w45h421b550ca0lqbl6w5yypxxqff8r0mi9i07"))
             (file-name (string-append "guix-" version "-checkout"))))))))
