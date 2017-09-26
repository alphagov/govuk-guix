(define-module (gds data data-extract)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (gds utils)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases elasticsearch)
  #:export (<data-extract>
            data-extract
            data-extract?
            data-extract-file
            data-extract-datetime
            data-extract-database
            data-extract-services

            filter-extracts
            group-extracts
            sort-extracts
            load-extract))

(define-record-type* <data-extract>
  data-extract make-data-extract
  data-extract?
  (file       data-extract-file)
  (datetime   data-extract-datetime)
  (database   data-extract-database)
  (services   data-extract-services))

(define* (filter-extracts extracts
                          #:optional #:key
                          service-types
                          databases
                          before-date
                          after-date)
  (filter
   (lambda (extract)
     (and
      (let ((services (data-extract-services extract)))
        (if services
            (any (lambda (service-type)
                   (member service-type (data-extract-services extract)))
                 service-types)
            #t))
      (if databases
          (member (data-extract-database extract) databases)
          #t)
      (if before-date
          (time<? (date->time-utc (data-extract-datetime extract))
                   (date->time-utc before-date))
          #t)
      (if after-date
          (time>? (date->time-utc (data-extract-datetime extract))
                   (date->time-utc after-date)))))
   extracts))

(define (group-extracts field extracts)
  (fold (lambda (extract result)
          (let ((key (field extract)))
            (fold (lambda (key result)
                    (if (list? key) (error "key is a list"))
                    (alist-add key extract result))
                  result
                  (if (list? key)
                      key
                      (list key)))))
        '()
        extracts))

(define (sort-extracts extracts)
  (stable-sort extracts
               (lambda (a b)
                 (time<? (date->time-utc (data-extract-datetime a))
                         (date->time-utc (data-extract-datetime b))))))

(define* (load-extract extract database-connection-config
                       #:key dry-run?)
  (let* ((load-gexp
          (match extract
            (($ <data-extract> file datetime "postgresql" services)
             (postgresql-import-gexp
              (postgresql-connection-config
               (inherit database-connection-config)
               (user "postgres")
               (database "postgres"))
              file
              #:dry-run? dry-run?))
            (($ <data-extract> file datetime "mongodb" services)
             (mongodb-restore-gexp
              database-connection-config
              file
              #:dry-run? dry-run?))
            (($ <data-extract> file datetime "mysql" services)
             (mysql-run-file-gexp
              database-connection-config
              file
              #:dry-run? dry-run?))
            (($ <data-extract> file datetime "elasticsearch" services)
             (elasticsearch-restore-gexp
              database-connection-config
              (simple-format
               #f "govuk-~A"
               (date->string datetime "~d-~m-~Y"))
              file
              #:alias "govuk"
              #:overrides "{\"settings\":{\"index\":{\"number_of_replicas\":\"0\",\"number_of_shards\":\"1\"}}}"
              #:batch-size 250
              #:dry-run? dry-run?))))
         (script
          (with-store store
            (run-with-store store
              (mlet* %store-monad
                  ((script (gexp->script
                            "load-extract"
                            #~(begin (exit (#$load-gexp))))))
                (mbegin %store-monad
                  (built-derivations (list script))
                  (return (derivation->output-path script))))))))
    (simple-format #t "running script ~A\n\n" script)
    (system* script)))
