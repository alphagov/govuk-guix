(define-module (gds scripts govuk data list)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds data govuk)
  #:use-module (gds data data-extract)
  #:use-module (gds data data-source)
  #:use-module (gds data transformations)
  #:use-module (gds data transformations postgresql)
  #:use-module (gds data transformations mysql)
  #:export (list-available-extracts))

(define* (list-available-extracts services data-extracts
                                  #:key dry-run? verbose? max-jobs)
  (for-each
   (match-lambda
     ((service-type . data-extracts)
      (if (member service-type (map service-kind services))
          (begin
            (simple-format #t "service: ~A\n" (service-type-name
                                               service-type))
            (for-each
             (match-lambda
               ((database . data-extracts)
                (simple-format #t "  database: ~A\n" database)
                (for-each
                 (match-lambda
                   ((date . data-extracts)
                    (simple-format
                     #t "    - ~A\n"
                     (date->string date "~d/~m/~Y"))
                    (if (or verbose?
                            (> (length data-extracts) 1))
                        (for-each
                         (lambda (data-extract priority-ordering)
                           (let ((data-source
                                  (data-extract-data-source
                                   data-extract)))
                             (format
                              #t
                              "      - ~A: ~A ~A (~:r priority)\n"
                              (data-extract-variant-name data-extract)
                              (data-extract-variant-label data-extract)
                              (data-source-name data-source)
                              priority-ordering)))
                         ;; Highest priority first, so reverse the sort order
                         (reverse (sort-extracts data-extracts))
                         (iota (length data-extracts) 1)))))
                 (group-extracts data-extract-datetime data-extracts))))
             (group-extracts data-extract-database data-extracts))))))
   (stable-sort
    (group-extracts data-extract-services
                    (sort-extracts data-extracts))
    (match-lambda* (((service-type1 . data-extracts1)
                     (service-type2 . data-extracts2))
                    (string<? (symbol->string
                               (service-type-name service-type1))
                              (symbol->string
                               (service-type-name service-type2))))))))
