(define-module (gds data data-extract)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases mysql)
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

(define* (filter-extracts extracts #:optional #:key service-type)
  (filter
   (lambda (extract)
     (let ((services (data-extract-services extract)))
       (and services
            (member service-type (data-extract-services extract)))))
   extracts))

(define (group-extracts field extracts)
  (define (alist-add key value alist)
    (if (null? alist)
        (list (cons key (list value)))
        (if (equal? (caar alist) key)
            (cons (cons key
                        (cons value
                              (cdr (first alist))))
                  (cdr alist))
            (cons (car alist)
                  (alist-add key value (cdr alist))))))

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
  extracts)

(define (load-extract extract database-connection-config)
  (let* ((load-gexp
          (match (data-extract-database extract)
            ("postgresql"
             (postgresql-import-gexp
              (postgresql-connection-config
               (inherit database-connection-config)
               (user "postgres")
               (database "postgres"))
              (data-extract-file extract)))
            ("mongo"
             (mongodb-restore-gexp
              database-connection-config
              (data-extract-file extract)))
            ("mysql"
             (mysql-run-file-gexp
              database-connection-config
              (data-extract-file extract)))))
         (script
          (with-store store
            (run-with-store store
              (mlet* %store-monad
                  ((script (gexp->script
                            "foo"
                            #~(begin (exit (#$load-gexp))))))
                (mbegin %store-monad
                  (built-derivations (list script))
                  (return (derivation->output-path script))))))))
    (simple-format #t "running script ~A\n\n" script)
    (system* script)))
