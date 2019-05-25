(define-module (gds data govuk sources data-directory-with-index)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (guix gexp)
  #:use-module (guix base16)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (gnu services)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases)
  #:use-module (gds data data-source)
  #:use-module (gds data data-extract)
  #:use-module (gds data tar-archive)
  #:use-module (gds data s3)
  #:use-module (gds data govuk sources govuk-puppet)
  #:use-module (gds data govuk sources govuk-puppet-aws)
  #:export (data-directory-with-index
            data-directory-with-index-data-source))

(define (all-extracts data-sources)
  (concatenate
   (filter-map
    (lambda (data-source)
      ((data-source-list-extracts data-source)))
    data-sources)))

(define (store-item-path-info item)
  (with-store store
    (let* ((derivation
            ((lower-object item) store))
           (output-path (if (derivation? derivation)
                            (derivation->output-path derivation)
                            derivation)))
      (simple-format #t "store-item-path-info: building: ~A\n"
                     derivation)
      (if (build-derivations store (list derivation))
          (query-path-info store output-path)
          (error "Unable to build " item)))))

(define (data-extract->details-list services data-extract)
  (match data-extract
    (($ <data-extract> name file datetime database extract-service-types
                       data-source variant-name variant-label
                       variant-properties directory?)
     ;; G-expressions handle lists, so construct a list from the record
     ;; fields
     (list name
           (if directory?
               (tar-archive
                (name (string-append name "-" variant-name ".tar.xz"))
                (contents file))
               file)
           (date->string datetime "~Y-~m-~d")
           database
           (map (lambda (service-type)
                  (cons
                   (service-type-name service-type)
                   (database-connection-config->alist
                    (database-connection-config-from-service-for-extract
                     (find (lambda (service)
                             (eq? (service-kind service) service-type))
                           services)
                     data-extract))))
                extract-service-types)
           variant-name
           variant-label
           variant-properties
           directory?))))

(define* (data-extracts->data-directory-with-index
          services
          data-extracts
          #:key (name "govuk-data-extracts"))
  (define build
    (with-imported-modules '((guix base16)
                             (guix build utils))
      #~(begin
          (add-to-load-path #$(file-append guile-json
                                           "/share/guile/site/"
                                           (effective-version)))
          (add-to-load-path #$(file-append guile-gcrypt
                                           "/share/guile/site/"
                                           (effective-version)))
          (use-modules (json)
                       (ice-9 match)
                       (srfi srfi-1)
                       (gcrypt hash)
                       (guix base16)
                       (guix build utils))

          (let* ((data-extract-details-lists
                  '#$(map (lambda (extract)
                            (data-extract->details-list services extract))
                          data-extracts))
                 (data-extract-destinations
                  (map (match-lambda
                        ((name file datetime database services
                               variant-name variant-label variant-properties
                               directory?)
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
                      (map second data-extract-details-lists))

            (mkdir-p #$output)
            (call-with-output-file (string-append #$output "/index.json")
              (lambda (port)
                (display
                 (scm->json-string
                  `((extracts
                     . ,(map (match-lambda*
                               (((name file date database services
                                  variant-name variant-label
                                  variant-properties directory?) url)
                                `((name . ,name)
                                  (date . ,date)
                                  (database . ,database)
                                  (services . ,services)
                                  (variant
                                   . ((name . ,variant-name)
                                      (label . ,variant-label)
                                      (properties . ,variant-properties)))
                                  (size . ,(stat:size (stat file)))
                                  (sha256-hash . ,(bytevector->base16-string
                                                   (call-with-input-file
                                                       file port-sha256)))
                                  ;; This URL is relative to the
                                  ;; location of this index.json file.
                                  (url . ,url)
                                  (directory? . ,directory?))))
                             data-extract-details-lists
                             data-extract-destinations)))
                  #:pretty #t)
                 port)))))))

  (computed-file name build))

(define* (data-directory-with-index services data-extracts
                                    #:key (data-source-specific-filters '()))
  (let* ((data-sources
          (list govuk-puppet-aws-data-source))
         (data-sources-with-data-directories
          (filter data-source-data-directory-with-index
                  data-sources)))

    (computed-file
     "data-directory-with-index"
     #~(begin
         (mkdir #$output)
         (symlink #$(data-extracts->data-directory-with-index services data-extracts)
                  (string-append #$output "/data-extracts"))

         (mkdir (string-append #$output "/data-sources"))

         (for-each
          (lambda (source-name directory)
            (symlink directory
                     (string-append #$output
                                    "/data-sources/"
                                    source-name)))
          '#$(map data-source-name data-sources-with-data-directories)
          '#$(map (lambda (data-source)
                    (let ((filters (or (assq-ref data-source-specific-filters
                                                 data-source)
                                       '())))
                      (apply (data-source-data-directory-with-index data-source)
                             filters)))
                  data-sources-with-data-directories))))))

;;;
;;; data-directory-with-index-data-source
;;;

(define (get-base-url)
  (or (getenv "GOVUK_GUIX_DATA_DIRECTORY_BASE_URL")
      "s3://govuk-development-data-test"))

(define (cache-directory)
  (string-append (or (getenv "XDG_CACHE_HOME")
                     (string-append (getenv "HOME") "/.cache"))
                 "/govuk-guix"))

(define (with-index-file url function)
  (let ((uri (string->uri url)))
    (cond
     ((eq? 'file (uri-scheme uri))
      (if (file-exists? (uri-path uri))
          (call-with-input-file (uri-path uri) function)
          '()))
     ((eq? 's3 (uri-scheme uri))
      (let ((target (string-append (cache-directory)
                                   "/data-extracts/index.json")))

        (if (or (not (file-exists? target))
                (> (- (time-second (current-time)) (stat:ctime (stat target)))
                   ;; 1 hour (in seconds)
                   (* 60 60)))
            (let ((download-command
                   `("govuk" "aws"
                     ,@(if (getenv "AWS_ACCESS_KEY_ID")
                           '()
                           '("--profile" "govuk-integration"))
                     "--"
                     "aws" "s3" "cp"
                     ,url
                     ,target)))
              (apply system* download-command)))

        (if (file-exists? target)
            (call-with-input-file target function)
            (begin
              (display "warning: unable to fetch index.json\n")
              #f))))
     (else (error "Unrecognised scheme" (uri-scheme uri))))))

(define (list-extracts)
  (let ((base-url (get-base-url)))
    (define (override-data-source extracts)
      (map (lambda (extract)
             (data-extract
              (inherit extract)
              (data-source data-directory-with-index-data-source)))
           extracts))

    ;; Currently unused, as the source-data separation isn't really
    ;; useful anymore
    (define (data-extracts-from-source-data)
      (append-map
       (match-lambda
         (($ <data-source> name list-extracts
                           list-extracts-from-data-directory-index
                           data-directory-with-index)
          (if list-extracts-from-data-directory-index
              (override-data-source
               (or (let* ((data-source-base-url
                           (string-append base-url "/data-sources/" name "/"))
                          (data-source-index
                           (string-append data-source-base-url "index.json")))

                     (with-index-file
                      data-source-index
                      (cut list-extracts-from-data-directory-index
                        <>
                        data-source-base-url)))
                   '()))
              '())))
       (list govuk-puppet-data-source)))

    (define (data-extracts)
      (with-index-file
       (string-append base-url "/data-extracts/index.json")
       (lambda (data-extracts-index)
         (map
          (lambda (data-extract-data)
            (data-extract
             (name (hash-ref data-extract-data "name"))
             (file (if (string-prefix? "file:///gnu/store/" base-url)
                       (readlink
                        (string-append (uri-path (string->uri base-url))
                                       "/data-extracts/"
                                       (hash-ref data-extract-data "url")))
                       (origin
                         (method (let ((scheme (uri-scheme (string->uri base-url))))
                                   (cond ((eq? scheme 'file)
                                          url-fetch)
                                         ((eq? scheme 's3)
                                          (s3-fetch-with-access-key-or-profile
                                           "govuk-integration"))
                                         (else (error "Unrecognised scheme"
                                                      scheme)))))
                         (uri (string-append base-url
                                             "/data-extracts/"
                                             (hash-ref data-extract-data
                                                       "url")))
                         (file-name (string-append
                                     (hash-ref data-extract-data "date")
                                     "_"
                                     (basename (hash-ref data-extract-data "url"))))
                         (sha256 (base16-string->bytevector
                                  (hash-ref data-extract-data "sha256-hash"))))))
             (datetime (string->date
                        (hash-ref data-extract-data "date") "~Y-~m-~d"))
             (database (hash-ref data-extract-data "database"))
             (services (filter-map
                        (lambda (service-name)
                          (find (lambda (service-type)
                                  (string=? (symbol->string
                                             (service-type-name service-type))
                                            service-name))
                                (map service-kind govuk-services)))
                        (hash-map->list (lambda (key value)
                                          key)
                                        (hash-ref data-extract-data "services"))))
             (data-source data-directory-with-index-data-source)
             (variant-name (hash-ref (hash-ref data-extract-data "variant")
                                     "name"))
             (variant-label (hash-ref (hash-ref data-extract-data "variant")
                                      "label"))
             (variant-properties (hash-map->list
                                  (lambda (key value)
                                    (cons (string->symbol key) value))
                                  (hash-ref (hash-ref data-extract-data "variant")
                                            "properties")))))
          (hash-ref (json->scm data-extracts-index) "extracts")))))

    (if (string=? base-url "")
        '()
        (or (data-extracts)
            '()))))

(define data-directory-with-index-data-source
  (data-source
   (name "data-directory-with-index")
   (list-extracts list-extracts)
   (priority 2)))
