(define-module (gds data data-source)
  #:use-module (guix records)
  #:export (<data-source>
            data-source
            data-source?
            data-source-name
            data-source-list-extracts
            data-source-data-directory-with-index))

(define-record-type* <data-source>
  data-source make-data-source
  data-source?
  (name                      data-source-name)
  (list-extracts             data-source-list-extracts)
  (data-directory-with-index data-source-data-directory-with-index
                             (default #f)))
