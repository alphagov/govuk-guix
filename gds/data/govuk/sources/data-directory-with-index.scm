(define-module (gds data govuk sources data-directory-with-index)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (gnu services)
  #:use-module (gnu packages guile)
  #:use-module (gds data data-source)
  #:use-module (gds data data-extract)
  #:use-module (gds data govuk sources govuk-puppet)
  #:export (data-directory-with-index))

(define (all-extracts data-sources)
  (concatenate
   (filter-map
    (lambda (data-source)
      ((data-source-list-extracts data-source)))
    data-sources)))

(define (store-item-size item)
  (with-store store
    (let* ((derivation
            ((lower-object item) store))
           (output-path (derivation->output-path derivation)))
      (if (build-derivations store (list derivation))
          (path-info-nar-size
           (query-path-info store output-path))
          (error "Unable to build " item)))))

(define data-extract->details-list
  (match-lambda
   (($ <data-extract> file datetime database services)
    ;; G-expressions handle lists, so construct a list from the record
    ;; fields
    (list file
          (date->string datetime "~Y-~m-~d")
          database
          (map service-type-name services)
          (store-item-size file)))))

(define* (data-extracts->data-directory-with-index
          data-extracts
          #:key (name "govuk-data-extracts"))
  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (add-to-load-path #$(file-append guile-json
                                           "/share/guile/site/"
                                           (effective-version)))
          (use-modules (json)
                       (ice-9 match)
                       (srfi srfi-1)
                       (guix build utils))

          (let* ((data-extract-details-lists
                  '#$(map data-extract->details-list
                          data-extracts))
                 (data-extract-destinations
                  (map (match-lambda
                        ((file datetime database services size)
                         ;; Create filenames like:
                         ;; datetime/database/file
                         (string-join
                          (list
                           datetime
                           database
                           ;; Drop the hash and dash prefix
                           (string-drop (basename file)
                                        33))
                          "/")))
                       data-extract-details-lists)))

            (for-each (lambda (destination file)
                        (let ((full-destination
                               (string-append #$output "/" destination)))
                          (mkdir-p (dirname full-destination))
                          (symlink file full-destination)))
                      data-extract-destinations
                      (map first data-extract-details-lists))

            (mkdir-p #$output)
            (call-with-output-file (string-append #$output "/index.json")
              (lambda (port)
                (display
                 (scm->json-string
                  `((extracts
                     . ,(map (match-lambda*
                              (((file date database services size)
                                url)
                               `((date . ,date)
                                 (database . ,database)
                                 (services . ,services)
                                 (size . ,size)
                                 ;; This URL is relative to the
                                 ;; location of this index.json file.
                                 (url . ,url))))
                             data-extract-details-lists
                             data-extract-destinations)))
                  #:pretty #t)
                 port)))))))

  (computed-file name build))

(define (data-directory-with-index data-sources)
  (computed-file
   "data-directory-with-index"
   #~(begin
       (mkdir #$output)
       (symlink #$(data-extracts->data-directory-with-index
                   (all-extracts data-sources))
                (string-append #$output
                               "/data-extracts"))

       (mkdir (string-append #$output "/data-sources"))
       (for-each
        (lambda (source-name directory)
          (symlink directory
                   (string-append #$output
                                  "/data-sources/"
                                  source-name)))
        '#$(map data-source-name data-sources)
        '#$(map (cut apply <> '())
                (map data-source-data-directory-with-index data-sources))))))

(define (build-data-directory-with-index)
  (with-store store
    (let ((derivation
           ((lower-object (data-directory-with-index
                           (list govuk-puppet-data-source)))
            store)))
      (build-derivations store (list derivation))
      (derivation->output-path derivation))))

;; Useful snippit for testing the above function
;; (simple-format #t "\n\n~A\n" (build-data-directory-with-index))
