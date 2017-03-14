(define-module (gds data data-source)
  #:use-module (guix records)
  #:export (<data-source>
            data-source
            data-source?
            data-source-list-extracts))

(define-record-type* <data-source>
  data-source make-data-source
  data-source?
  (list-extracts data-source-list-extracts))
