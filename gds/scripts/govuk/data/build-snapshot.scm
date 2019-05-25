(define-module (gds scripts govuk data build-snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds systems govuk base)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds data govuk)
  #:use-module (gds data data-extract)
  #:use-module (gds data transformations)
  #:use-module (gds data transformations postgresql)
  #:use-module (gds data transformations mysql)
  #:use-module (gds data transformations mongodb)
  #:export (build-snapshot))

(define (postgresql-load-extracts* extracts-and-database-connection-configs)
  (define postgresql-service
    (find (lambda (service)
            (eq? (service-kind service)
                 postgresql-service-type))
          optional-services))

  (postgresql-load-extracts postgresql-service
                            extracts-and-database-connection-configs))

(define* (snapshot-data-transformations all-data-extracts
                                        #:key dry-run?)
  (define (display-data-extracts database data-extracts)
    (define (services-string services)
      (string-join
       (map (lambda (service)
              (symbol->string (service-type-name service)))
            services)
       ", "))

    (begin
      (simple-format #t "  ~A:\n" database)
      (for-each (lambda (data-extract)
                  (simple-format #t "    - ~A (~A)\n"
                                 (services-string (data-extract-services
                                                   data-extract))
                                 (date->string (data-extract-datetime data-extract)
                                               "~d/~m/~Y")))
                data-extracts)))

  (simple-format #t "\nsnapshot:\n")
  (filter-map
   (match-lambda
     ((database . data-extracts)
      (and=> (assoc-ref `(("postgresql" . ,postgresql-load-extracts*)
                          ("mysql" . ,mysql-load-extracts)
                          ("mongo" . ,mongodb-load-extracts))
                        database)
             (lambda (load-extracts)
               (let ((extracts-and-database-connection-configs
                      (get-extracts-and-database-connection-configs govuk-services
                                                                    data-extracts)))

                 (display-data-extracts
                  database
                  (map car extracts-and-database-connection-configs))

                 (data-transformation
                  (output-name (string-append database "-snapshot.tar.gz"))
                  (operation
                   #~(begin
                       #$(load-extracts extracts-and-database-connection-configs)

                       (invoke #$(file-append tar "/bin/tar")
                               "--checkpoint=1000"
                               "--checkpoint-action=echo='%ds: %{read,wrote}T'"
                               (string-append "--use-compress-program="
                                              #$(file-append pigz "/bin/pigz"))
                               "--create"
                               "--file" #$output
                               #$database)))))))))
   (let ((data-extracts-by-database
          (group-extracts data-extract-database all-data-extracts)))
     (fold (lambda (database result)
             (if (assoc database result)
                 result
                 (cons (cons database '())
                       result)))
           data-extracts-by-database
           '("postgresql" "mysql" "mongo")))))

(define (snapshot-manifest all-data-extracts)

  (define (data-extracts-for-database-sexp data-extracts)
    (let ((extracts-and-database-connection-configs
           (get-extracts-and-database-connection-configs govuk-services
                                                         data-extracts)))

      (map (match-lambda
             ((data-extract . database-connection-configs)

              `((datetime . ,(date->string (data-extract-datetime data-extract)
                                           "~d/~m/~Y"))
                (services . ,(map service-type-name
                                  (data-extract-services data-extract)))
                (database-connection-configs
                 . ,(map (match-lambda
                           (($ <postgresql-connection-config>
                               port user host database)
                            `((user . ,user)
                              (database . ,database)))
                           (($ <mysql-connection-config>
                               host user port database)
                            `((user . ,user)
                              (database . ,database)))
                           (($ <mongodb-connection-config>
                               port database)
                            `((database . ,database))))
                         database-connection-configs)))))

           extracts-and-database-connection-configs)))

  (define extracts-sexp
    (filter-map (match-lambda
                  ((database . data-extracts)
                   (if (member database
                               '("postgresql" "mysql" "mongo"))
                       (cons database
                             (data-extracts-for-database-sexp data-extracts))
                       #f)))
                (group-extracts data-extract-database all-data-extracts)))

  (scm->json-string
   `((extracts . ,extracts-sexp))
   #:pretty #t))

(define* (build-snapshot services data-extracts
                         #:key dry-run? verbose?)
  (define data-transformations
    (snapshot-data-transformations data-extracts
                                   #:dry-run? dry-run?))

  (define snapshot-union-gexp
    (with-imported-modules '((guix build utils))
      #~(let ((output-var-lib
               (string-append #$output "/var/lib")))

          (use-modules (guix build utils))
          (mkdir-p output-var-lib)

          (call-with-output-file (string-append #$output "/manifest.json")
            (lambda (port)
              (display #$(snapshot-manifest data-extracts)
                       port)))

          #$@(map (lambda (data-transformation)
                    #~(symlink #$data-transformation
                               (string-append
                                output-var-lib "/"
                                #$(car (string-split
                                        (data-transformation-output-name
                                         data-transformation)
                                        #\-)))))
                  data-transformations)

          (exit 0))))


  (define (build-snapshot-union)
    (with-store store
      (set-build-options store
                         #:max-build-jobs 1
                         #:max-silent-time 14400) ;; Wait up to 4 hours

      (run-with-store store
        (mlet %store-monad
            ((derivation (gexp->derivation
                          "snapshot"
                          snapshot-union-gexp)))
          (mbegin %store-monad
            (built-derivations (list derivation))
            (return (derivation->output-path derivation)))))))

  (unless dry-run?
    (simple-format #t "\n~A\n" (build-snapshot-union))))
