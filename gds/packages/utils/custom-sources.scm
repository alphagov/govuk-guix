(define-module (gds packages utils custom-sources)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix ui)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (gds utils)
  #:use-module (gds packages utils bundler)
  #:export (github-url-regex
            environment-variable-commit-ish-regex
            environment-variable-path-regex
            get-package-source-config-list-from-environment
            custom-github-archive-source-for-package
            log-package-path-list
            log-package-commit-ish-list
            correct-source-of
            validate-custom-source-data))

(define github-url-regex
  (make-regexp
   "https:\\/\\/github\\.com\\/[^\\/]*\\/[^\\/]*\\/archive\\/([^\\/]*)\\.tar\\.gz"))

(define environment-variable-commit-ish-regex
  (make-regexp
   "GDS_GUIX_([A-Z0-9_]*)_COMMIT_ISH=(.*)"))

(define environment-variable-path-regex
  (make-regexp
   "GDS_GUIX_([A-Z0-9_]*)_PATH=(.*)"))

(define (package-name->custom-source-path-environment-variable name)
  (string-append "GDS_GUIX_"
                 (string-map
                  (lambda (c)
                    (if (eq? c #\-) #\_ c))
                  (string-upcase name))
                 "_PATH"))

(define (package-name->custom-source-commit-ish-environment-variable name)
  (string-append "GDS_GUIX_"
                 (string-map
                  (lambda (c)
                    (if (eq? c #\-) #\_ c))
                  (string-upcase name))
                 "_COMMIT_ISH"))

(define (get-package-source-config-list-from-environment regex)
  (map
   (lambda (name-value-match)
     (cons
      (string-map
       (lambda (c)
         (if (eq? c #\_) #\- c))
       (string-downcase
        (match:substring name-value-match 1)))
      (match:substring name-value-match 2)))
   (filter
    regexp-match?
    (map
     (lambda (name-value)
       (regexp-exec regex name-value))
     (environ)))))

(define* (custom-github-archive-source-for-package
          app-package
          commit-ish)
  (let*
      ((old-url
        (origin-uri (package-source app-package)))
       (regexp-match
        (regexp-exec github-url-regex old-url)))
    (if (not (regexp-match? regexp-match))
        (error "No match"))
    (with-store store
      (download-to-store
       store
       (string-replace
        old-url
        commit-ish
        (match:start regexp-match 1)
        (match:end regexp-match 1))
       #:recursive? #t))))

(define (log-package-path-list package-path-list)
  (for-each
   (match-lambda
     ((package . path)
      (simple-format
       #t
       "Using path \"~A\" for the ~A package\n"
       path
       package)))
   package-path-list))

(define (log-package-commit-ish-list package-commit-ish-list)
  (for-each
   (match-lambda
     ((package . commit-ish)
      (simple-format
       #t
       "Using commit-ish \"~A\" for the ~A package\n"
       commit-ish
       package)))
   package-commit-ish-list))

(define (validate-custom-source-data package-path-list package-commit-ish-list
                                     packages)
  (define (check-list list-to-check environment-variable-generator)
    (let* ((package-names (map package-name packages))
           (unknown-package-names
            (filter-map
             (lambda (package-name)
               (if (member package-name package-names)
                   #f
                   package-name))
             (map car list-to-check))))
      (if (not (null? unknown-package-names))
          (for-each
           (lambda (unknown-package-name)
             (let
                 ((similarly-named-packages
                   (find-similar-strings unknown-package-name package-names)))
               (if (null? similarly-named-packages)
                   (leave (G_ "custom-sources: No package called \"~A\", set through ~A")
                          unknown-package-name
                          (environment-variable-generator unknown-package-name))
                   (leave (G_ "custom-sources: No package called \"~A\", set through ~A, did you mean \"~A\" (~A)?")
                          unknown-package-name
                          (environment-variable-generator unknown-package-name)
                          (first similarly-named-packages)
                          (environment-variable-generator
                           (first similarly-named-packages))))))
           unknown-package-names)
          #t)))

  (check-list package-path-list
              package-name->custom-source-path-environment-variable)
  (check-list package-commit-ish-list
              package-name->custom-source-commit-ish-environment-variable))

(define (correct-source-of package-path-list package-commit-ish-list pkg)
  (define (update-inputs source inputs)
    (map
     (match-lambda
       ((name value rest ...)
        (if (string=? name "bundle-install")
            (cons*
             name
             (let ((bundle-pkg
                    (bundle-package
                     (inherit (package-source value))
                     (source source))))
               (package
                 (inherit value)
                 (source (bundle-package-to-store bundle-pkg))))
             rest)
            (cons* name value rest))))
     inputs))

  (let*
      ((custom-path (assoc-ref package-path-list
                               (package-name pkg)))
       (custom-commit-ish (assoc-ref package-commit-ish-list
                                     (package-name pkg)))
       (custom-source
        (cond
         ((and custom-commit-ish custom-path)
          (error "cannot specify custom-commit-ish and custom-path"))
         (custom-commit-ish
          (custom-github-archive-source-for-package
           pkg
           custom-commit-ish))
         (custom-path
          (local-file
           custom-path
           #:recursive? #t
           #:select? (git-predicate custom-path)))
         (else #f))))
    (if custom-source
        (package
          (inherit pkg)
          (source custom-source)
          (inputs (update-inputs custom-source (package-inputs pkg))))
        pkg)))
