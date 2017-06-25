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

(define (git-predicate directory)
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  Upon Git failure, return #f instead of a predicate.

The returned predicate takes two arguments FILE and STAT where FILE is an
absolute file name and STAT is the result of 'lstat'."
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
         (inodes      (map (lambda (file)
                             (let ((stat
                                    (lstat (string-append directory "/" file))))
                               (cons (stat:dev stat) (stat:ino stat))))
                           files))
         (status      (close-pipe pipe)))
    (and (zero? status)
         (lambda (file stat)
           (match (stat:type stat)
             ('directory
              ;; 'git ls-files' does not list directories, only regular files,
              ;; so we need this special trick.
              (any (lambda (f) (parent-directory? f file))
                   files))
             ((or 'regular 'symlink)
              ;; Comparing file names is always tricky business so we rely on
              ;; inode numbers instead
              (member (cons (stat:dev stat) (stat:ino stat))
                      inodes))
             (_
              #f))))))

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
      (version "release_8")
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
                   ;; Note that when changing the treeish, you will
                   ;; need to change the sha256 hash such that Guix
                   ;; thinks that it has not built this
                   (commit version)))
             (sha256
              (base32 "1hq80kk4in6c381w2zh70z8byqg8nxdi2f9qnxd4d9m0fg2q5i35"))
             (file-name (string-append "guix-" version "-checkout"))))))))
