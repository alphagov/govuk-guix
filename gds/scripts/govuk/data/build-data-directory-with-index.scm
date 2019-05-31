(define-module (gds scripts govuk data build-data-directory-with-index)
  #:use-module (srfi srfi-19)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (gds data govuk sources govuk-puppet-aws)
  #:use-module (gds data govuk sources data-directory-with-index)
  #:export (build-data-directory-with-index))

(define* (build-data-directory-with-index services data-extracts
                                          #:key dry-run? verbose? max-jobs)
  (with-store store
    (set-build-options store #:max-build-jobs max-jobs)

    (let ((derivation
           ((lower-object (data-directory-with-index services data-extracts))
            store)))
      (if dry-run?
          (simple-format #t "Would build derivation ~A" derivation)
          (begin
            (simple-format #t "Building derivations ~A" derivation)
            (build-derivations store (list derivation))))
      (simple-format #t "\n~A\n" (derivation->output-path derivation)))))
