(define-module (gds data govuk)
  #:use-module (srfi srfi-1)
  #:use-module (gds data data-source)
  #:use-module (gds data govuk sources data-directory-with-index)
  #:use-module (gds data govuk sources govuk-puppet)
  #:use-module (gds data govuk sources govuk-puppet-aws)
  #:export (data-sources
            all-extracts))

(define data-sources
  (list data-directory-with-index-data-source
        govuk-puppet-aws-data-source
        govuk-puppet-data-source))

(define (all-extracts)
  (append-map
   (lambda (list-extracts-thunk) (list-extracts-thunk))
   (map data-source-list-extracts data-sources)))
