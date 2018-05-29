(define-module (gds scripts govuk data build-snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds data govuk)
  #:use-module (gds data data-extract)
  #:use-module (gds data transformations)
  #:use-module (gds data transformations postgresql)
  #:use-module (gds data transformations mysql)
  #:export (build-snapshot))

(define (postgresql-load-extracts extracts-and-database-connection-configs)
  (define operation
    (with-imported-modules '((gds data transformations build postgresql))
      #~(lambda _
          #$@(append-map
              (match-lambda
                ((data-extract . database-connection-configs)
                 (map
                  (match-lambda
                    (($ <postgresql-connection-config> port user host database)
                     #~(let ((database #$database)
                             (user #$user))
                         (invoke "createuser" user)
                         (invoke "createdb" database "-O" user)
                         (ungzip-file-and-pipe-to-psql
                          #$(data-extract-file data-extract)
                          database))))
                  database-connection-configs)))
              extracts-and-database-connection-configs))))

  #~(begin
      #$(with-postgresql
         (service postgresql-service-type)
         operation)

      (invoke #$(file-append tar "/bin/tar")
              "--create"
              "--file" #$output
              "postgresql")))

(define (mysql-load-extracts extracts-and-database-connection-configs)
  (define operation
    (with-imported-modules '((gds data transformations build mysql))
      #~(lambda _
          (use-modules (gds data transformations build mysql))
          #$@(append-map
              (match-lambda
                ((data-extract . database-connection-configs)
                 (map
                  (lambda (database-connection-config)
                    #~(let ((database #$(mysql-connection-config-database
                                         database-connection-config)))
                        (invoke
                         "mysql" "--user=root" "-e"
                         (string-append
                          "CREATE DATABASE " database ";"
                          "GRANT ALL ON " database ".* TO ''@'localhost';"))

                        (ungzip-file-and-pipe-to-mysql
                         #$(data-extract-file data-extract)
                         database)))
                  database-connection-configs)))
              extracts-and-database-connection-configs))))

  #~(begin
      #$(with-mysql
         (service mysql-service-type)
         operation)

      (invoke #$(file-append tar "/bin/tar")
              "--create"
              "--file" #$output
              "mysql")))

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
      (simple-format #t "~A:\n" database)
      (for-each (lambda (data-extract)
                  (simple-format #t "  - ~A (~A)\n"
                                 (services-string (data-extract-services
                                                   data-extract))
                                 (date->string (data-extract-datetime data-extract)
                                               "~d/~m/~Y")))
                data-extracts)))

  (filter-map
   (match-lambda
     ((database . data-extracts)
      (and=> (assoc-ref `(("postgresql" . ,postgresql-load-extracts)
                          ("mysql" . ,mysql-load-extracts))
                        database)
             (lambda (load-extracts)
               (let ((extracts-and-database-connection-configs
                      (get-extracts-and-database-connection-configs govuk-services
                                                                    data-extracts)))

                 (if dry-run? (display-data-extracts
                               database
                               (map car extracts-and-database-connection-configs)))

                 (data-transformation
                  (output-name (string-append database "-snapshot.tar.gz"))
                  (operation (load-extracts
                              extracts-and-database-connection-configs))))))))
   (group-extracts data-extract-database all-data-extracts)))

(define* (build-snapshot services data-extracts
                         #:key dry-run?)
  (define data-transformations
    (snapshot-data-transformations data-extracts
                                   #:dry-run? dry-run?))

  (define snapshot-union-gexp
    (with-imported-modules '((guix build utils))
      #~(let ((output-var-lib
               (string-append #$output "/var/lib")))

          (use-modules (guix build utils))
          (mkdir-p output-var-lib)

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
