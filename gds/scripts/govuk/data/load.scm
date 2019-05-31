(define-module (gds scripts govuk data load)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds data govuk)
  #:use-module (gds data data-extract)
  #:use-module (gds data data-source)
  #:use-module (gds data transformations)
  #:use-module (gds data transformations postgresql)
  #:use-module (gds data transformations mysql)
  #:export (load-extracts))

(define (show-selected-extracts extracts-and-database-connection-configs)
  (display "Extracts selected:\n")
  (map
   (match-lambda
     ((data-extract . database-connection-configs)
      (for-each
       (lambda (database-connection-config)
         (match data-extract
           (($ <data-extract> name file datetime database
                              services data-source)
            (simple-format
             #t " - ~A extract from ~A (~A) into the ~A database for"
             database
             (date->string datetime "~d/~m/~Y")
             (data-source-name data-source)
             (match database
               ("elasticsearch" "elasticsearch")
               (else
                (database-connection-config->database-name
                 database-connection-config))))
            (if (eq? 1 (length services))
                (simple-format #t " the ~A service\n"
                               (service-type-name (first services)))
                (begin
                  (display ":\n")
                  (for-each
                   (lambda (service)
                     (simple-format #t "   - ~A\n"
                                    (service-type-name service)))
                   services))))))
       database-connection-configs)))
   extracts-and-database-connection-configs)
  (display "\n"))

(define* (load-selected-extracts
          extracts-and-database-connection-configs
          #:key dry-run?)
  (map
   (match-lambda
     ((data-extract . database-connection-configs)
      (for-each
       (lambda (database-connection-config)
         (match data-extract
           (($ <data-extract> name file datetime database services)
            (simple-format
             #t "Importing extract from ~A in to ~A\n"
             (date->string datetime "~d/~m/~Y")
             (match database
               ("elasticsearch" "elasticsearch")
               (else (database-connection-config->database-name
                      database-connection-config))))))
         (display "\n")
         (load-extract data-extract database-connection-config
                       #:dry-run? dry-run?
                       #:use-local-files-directly? #t))
       database-connection-configs)))
   extracts-and-database-connection-configs))

(define* (load-extracts services data-extracts
                        #:key dry-run? verbose? max-jobs)
  (let ((extracts-and-database-connection-configs
         (get-extracts-and-database-connection-configs services
                                                       data-extracts)))
    (show-selected-extracts extracts-and-database-connection-configs)
    (load-selected-extracts extracts-and-database-connection-configs
                            #:dry-run? dry-run?)))
