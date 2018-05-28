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
            get-service-package-source-config-list-from-environment
            custom-github-archive-source-for-package
            log-service-package-path-list
            log-service-package-commit-ish-list
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

(define (service-name->custom-source-path-environment-variable name)
  (string-append "GDS_GUIX_"
                 (string-map
                  (lambda (c)
                    (if (eq? c #\-) #\_ c))
                  (string-upcase name))
                 "_PATH"))

(define (service-name->custom-source-commit-ish-environment-variable name)
  (string-append "GDS_GUIX_"
                 (string-map
                  (lambda (c)
                    (if (eq? c #\-) #\_ c))
                  (string-upcase name))
                 "_COMMIT_ISH"))

(define (get-service-package-source-config-list-from-environment regex)
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

(define (log-service-package-path-list service-path-list)
  (for-each
   (match-lambda
     ((service . path)
      (simple-format
       #t
       "Using path \"~A\" for the ~A service\n"
       path
       service)))
   service-path-list))

(define (log-service-package-commit-ish-list service-commit-ish-list)
  (for-each
   (match-lambda
     ((service . commit-ish)
      (simple-format
       #t
       "Using commit-ish \"~A\" for the ~A service\n"
       commit-ish
       service)))
   service-commit-ish-list))

(define (validate-custom-source-data service-revisions
                                     service-path-list
                                     service-commit-ish-list
                                     service-names)
  (define (check-list list-to-check environment-variable-generator)
    (let* ((unknown-service-names
            (filter-map
             (lambda (service-name)
               (if (member service-name service-names)
                   #f
                   service-name))
             (map car list-to-check))))
      (if (not (null? unknown-service-names))
          (for-each
           (lambda (unknown-service-name)
             (let
                 ((similarly-named-services
                   (find-similar-strings unknown-service-name service-names)))
               (if (null? similarly-named-services)
                   (leave (G_ "custom-sources: No services called \"~A\", set through ~A")
                          unknown-service-name
                          (environment-variable-generator unknown-service-name))
                   (leave (G_ "custom-sources: No service called \"~A\", set through ~A, did you mean \"~A\" (~A)?")
                          unknown-service-name
                          (environment-variable-generator unknown-service-name)
                          (first similarly-named-services)
                          (environment-variable-generator
                           (first similarly-named-services))))))
           unknown-service-names)
          #t)))

  (check-list service-path-list
              service-name->custom-source-path-environment-variable)
  (check-list service-commit-ish-list
              service-name->custom-source-commit-ish-environment-variable))

(define (correct-source-of service-name
                           pkg
                           custom-path
                           custom-revision)
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

  (let
      ((custom-source
        (cond
         ((and custom-revision custom-path)
          (error "cannot specify custom-commit-ish and custom-path"))
         (custom-revision
          (custom-github-archive-source-for-package
           pkg
           custom-revision))
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
