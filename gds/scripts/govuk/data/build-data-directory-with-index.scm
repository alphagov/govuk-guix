(define-module (gds scripts govuk data build-data-directory-with-index)
  #:use-module (srfi srfi-19)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (gds data govuk sources govuk-puppet-aws)
  #:use-module (gds data govuk sources data-directory-with-index)
  #:export (build-data-directory-with-index))

(define* (build-data-directory-with-index services data-extracts #:key dry-run?)
  (with-store store
    (let ((derivation
           ((lower-object (data-directory-with-index services data-extracts))
            store)))
      (simple-format #t "Building derivations ~A" derivation)
      (build-derivations store (list derivation))
      (simple-format #t "\n~A\n" (derivation->output-path derivation)))))
