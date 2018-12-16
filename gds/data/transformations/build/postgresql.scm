(define-module (gds data transformations build postgresql)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:export (pg-restore
            pg-dump
            decompress-file-and-pipe-to-psql
            run-with-psql-port
            pg-dump-parallel-compression))

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
           "--verbose"
           ,@(if (string=? format "directory")
                 '("--jobs=8")
                 '())
           ,@(if format
                 `(,(simple-format #f "--format=~A" format))
                 '()))))
    (simple-format #t "running:\n  ~A\n"
                   (string-join command " "))
    (force-output)
    (or
     (zero? (apply system* command))
     (error "pg_dump failed"))))

(define (pg-dump-parallel-compression database-name output-path)
  (let ((command
         `("pg_dump"
           ,database-name
           ,(string-append "--file=" output-path)
           "--verbose"
           "--jobs=8"
           "--compress=0"
           "--format=directory")))
    (simple-format #t "running:\n  ~A\n"
                   (string-join command " "))
    (force-output)
    (or
     (zero? (apply system* command))
     (error "pg_dump failed")))

  (for-each
   (lambda (file)
     (or (zero? (system* "pigz" "-9" "--verbose"
                         (string-append output-path "/" file)))
         (error "pigz failed")))
   (scandir output-path
            (lambda (name)
              (and (not (string-prefix? "toc" name))
                   (not (string-prefix? "." name)))))))

(define (run-with-psql-port database-name user operations)
  (let ((p (open-pipe*
            OPEN_WRITE "psql"
            (string-append "--user=" user)
            "-a"
            "--no-psqlrc"
            database-name)))
    (for-each
     (lambda (o) (o p))
     (if (list? operations)
         operations
         (list operations)))
    (close-pipe p)))

(define* (decompress-file-and-pipe-to-psql file database)
  (define decompressor
    (assoc-ref '(("gz" . "gzip")
                 ("xz" . "xz"))
               (last (string-split file #\.))))

  (let ((command
         (string-join
          `("set -eo pipefail;"
            "pv" "--force" ,file "|"
            ,@(if decompressor
                  `(,decompressor "-d" "|")
                  '())
            "psql" "--no-psqlrc" "--quiet" ,database)
          " ")))
    (simple-format #t "ungzip-file-and-pipe-to-psql running:\n  ~A\n"
                   command)
    (force-output)
    (setenv "XZ_OPT" "-T0")
    (or (zero? (system command))
        (error "ungzip-file-and-pipe-to-psql failed"))))
