(define-module (gds build-jobs govuk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix config)
  #:use-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix discovery)
  #:use-module (gnu packages)
  #:export (govuk-packages-jobs))

(define (package-metadata package)
  `((#:description . ,(package-synopsis package))
    (#:long-description . ,(package-description package))
    (#:home-page . ,(package-home-page package))
    (#:maintainers . ("govuk-developers"))
    (#:max-silent-time . ,(or (assoc-ref (package-properties package)
                                         'max-silent-time)
                              3600))      ;1 hour by default
    (#:timeout . ,(or (assoc-ref (package-properties package) 'timeout)
                      72000))))           ;20 hours by default

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  `((#:job-name . ,(string-append (symbol->string job-name) "." system))
    (#:derivation . ,(derivation-file-name
                      (parameterize ((%graft? #f))
                                    (package-derivation store package system
                                                        #:graft? #f))))
    ,@(package-metadata package)))

(define job-name
  ;; Return the name of a package's job.
  (compose string->symbol package-full-name))

(define (package->job store package system)
  (package-job store (job-name package) package system))

(define (govuk-packages-jobs store)
  (parameterize ((%graft? #f))
    (let* ((modules
            (filter (lambda (module)
                      (match (module-name module)
                        (('gds 'packages 'govuk rest ...) #t)
                        (_ #f)))
                    (all-modules
                     (list
                      (string-append
                       (getenv "GOVUK_GUIX_ROOT")
                       "/.guix-package-path")))))
           (pkgs
            (fold-packages cons
                           '()
                           modules)))
      (filter-map (lambda (pkg)
                    (package->job store pkg "x86_64-linux"))
                  pkgs))))
