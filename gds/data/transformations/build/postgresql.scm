(define-module (gds data transformations build postgresql)
  #:export (pg-restore
            pg-dump
            ungzip-file-and-pipe-to-psql))

(define* (pg-restore file)
  (let ((command
         `("pg_restore" ,file)))
    (simple-format #t "pg-restore running:\n  ~A\n"
                   (string-join command " "))
    (force-output)
    (or
     (zero? (apply system* command))
     (error "pg_restore failed"))))

(define* (pg-dump database-name output-path
                  #:key format)
  (let ((command
         `("pg_dump"
           ,database-name
           ,(string-append "--file=" output-path)
           ,@(if format
                 `(,(simple-format #f "--format=~A" format))
                 '()))))
    (simple-format #t "running:\n  ~A\n"
                   (string-join command " "))
    (force-output)
    (or
     (zero? (apply system* command))
     (error "pg_dump failed"))))

(define* (ungzip-file-and-pipe-to-psql file database)
  (let ((command
         (string-join
          `("set -eo pipefail;"
            "pv" "--force" ,file "|"
            "gzip" "-d" "|"
            "psql" "--no-psqlrc" "--quiet" ,database)
          " ")))
    (simple-format #t "ungzip-file-and-pipe-to-psql running:\n  ~A\n"
                   command)
    (force-output)
    (or (zero? (system command))
        (error "ungzip-file-and-pipe-to-psql failed"))))
