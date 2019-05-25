(define-module (gds data transformations postgresql)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services databases)
  #:use-module (gds data data-extract)
  #:use-module (gds data transformations)
  #:use-module (gds services utils databases postgresql)
  #:export (with-postgresql

            postgresql-load-extracts

            postgresql-multi-output-data-transformation))

(define* (with-postgresql
          postgresql-service
          gexp-to-run
          #:key (base-directory #~(string-append
                                   (getcwd)
                                   "/postgresql")))
  (let ((shepherd-service
         (first
          ((service-extension-compute
            (find (lambda (service-extension)
                    (eq? shepherd-root-service-type
                         (service-extension-target service-extension)))
                  (service-type-extensions (service-kind postgresql-service))))
           (service-parameters postgresql-service))))
        (postgresql-config
         (postgresql-config-file
          (extra-config
           '(("wal_level" "minimal")
             ("archive_mode" "off")
             ("fsync" "off")
             ("full_page_writes" "off")
             ("autovacuum" "off")
             ("max_wal_senders" "0")
             ("shared_buffers" "2GB")
             ("work_mem" "1GB")
             ("maintenance_work_mem" "2GB")))))
        (postgresql
         ((@@ (gnu services databases)
              postgresql-configuration-postgresql)
          (service-parameters postgresql-service))))
    (with-imported-modules '((guix build utils)
                             (gds data transformations build postgresql))
      #~(begin
          (add-to-load-path #$(file-append shepherd
                                           "/share/guile/site/"
                                           (effective-version)))

          (use-modules (shepherd service)
                       (guix build utils)
                       (gds data transformations build postgresql))

          (define base-directory #$base-directory)

          (setenv "PATH"
                  (list->search-path-as-string
                   (cons*
                    (string-append
                     #$((@@ (gnu services databases)
                            postgresql-configuration-postgresql)
                        (service-parameters postgresql-service))
                     "/bin")
                    (string-append #$pv "/bin")
                    (string-append #$gzip "/bin")
                    (string-append #$pigz "/bin")
                    (string-append #$glibc "/bin")
                    (string-append #$xz "/bin")
                    (search-path-as-string->list (getenv "PATH")))
                   ":"))
          (setenv "GUIX_LOCPATH" #$(file-append glibc-locales "/lib/locale"))
          (setenv "LC_ALL" "en_GB.UTF-8")
          (setenv "PGUSER" "postgres")

          (define data-directory
            (string-append base-directory "/data"))

          (define (activate)
            (simple-format #t "activating PostgreSQL\n")
            (mkdir-p data-directory)
            (chmod data-directory #o700)
            (invoke "initdb"
                    "--locale=en_GB.UTF-8"
                    "-U" "postgres"
                    "-D" data-directory))

          (define (pg_ctl . args)
            (let ((command
                   `(,(string-append #$postgresql "/bin/pg_ctl")
                     "-D" ,data-directory
                     "-o" ,(string-append "--config-file=" #$postgresql-config)
                     ,@args)))
              (simple-format #t "running: ~A\n" (string-join command " "))
              (apply invoke command)))

          (activate)
          (pg_ctl "start"
                  "-w") ; wait for PostgreSQL to start
          (let ((result
                 (#$gexp-to-run '())))

            (pg_ctl "stop")

            result)))))

(define (postgresql-load-extracts postgresql-service
                                  extracts-and-database-connection-configs)
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
                         #$(let ((format
                                  (assoc-ref
                                   (data-extract-variant-properties data-extract)
                                   'format)))
                             (cond
                              ((string=? format "plain")
                               #~(decompress-file-and-pipe-to-psql
                                  #$(data-extract-file data-extract)
                                  database))
                              ((string=? format "directory")
                               #~(let ((tmp-dir
                                        #$(string-append
                                           "tmp-"
                                           (data-extract-name data-extract))))
                                   (mkdir tmp-dir)
                                   (tar-extract #$(file-append tar "/bin/tar")
                                                #$(data-extract-file data-extract)
                                                tmp-dir)

                                   (pg-restore tmp-dir database)

                                   (use-modules (ice-9 ftw))
                                   (make-file-writable tmp-dir)
                                   (for-each (lambda (file)
                                               (make-file-writable
                                                (string-append tmp-dir "/" file)))
                                             (scandir tmp-dir
                                                      (lambda (file)
                                                        (not (member
                                                              file
                                                              '("." ".."))))))
                                   (delete-file-recursively tmp-dir)))
                              (else
                               (error "Unknown format")))))))
                  database-connection-configs)))
              extracts-and-database-connection-configs))))

  #~(begin
      #$(with-postgresql
         postgresql-service
         (if (null? extracts-and-database-connection-configs)
             #~(lambda _ #f)
             operation))))

(define* (postgresql-multi-output-data-transformation
          postgresql-service
          base-extract
          database-connection-config
          variant-details
          #:key (initial-superuser-sql '()))
  (define operation
    (with-imported-modules '((gds data transformations build postgresql))
      #~(lambda _
          (define database-name
            #$(postgresql-connection-config-database database-connection-config))

          (define user
            #$(postgresql-connection-config-user database-connection-config))

          (define initial-superuser-sql (list #$@initial-superuser-sql))

          (invoke "createuser" user)
          (invoke "createdb" database-name "-O" user)
          (decompress-file-and-pipe-to-psql #$(data-extract-file base-extract)
                                            database-name)

          (unless (null? initial-superuser-sql)
            (run-with-psql-port
             database-name "postgres"
             (lambda (port)
               (for-each (lambda (statement)
                           (simple-format #t "initial-sql: ~A;\n" statement)
                           (display (string-append statement ";")
                                    port))
                         initial-superuser-sql))))

          #$@(map
              (match-lambda
                ((variant-name variant-description variant-sql)
                 #~(let ((variant-name #$variant-name)
                         (variant-sql (list #$@variant-sql)))
                     ;; Run the SQL for this variant
                     (run-with-psql-port
                      database-name user
                      (lambda (port)
                        (for-each (lambda (statement)
                                    (display (string-append statement ";")
                                             port))
                                  variant-sql)))

                     ;; Then dump out the state at this point during the
                     ;; transformation
                     (pg-dump-parallel-compression
                      database-name (ungexp output variant-name)))))
              variant-details))))

  (multi-output-data-transformation
   (name "postgresql-incremental-transformation")
   (outputs (map (match-lambda
                   ((variant-name variant-description variant-sql)
                    (list variant-name variant-description)))
                 variant-details))
   (operation #~(begin
                  #$(with-postgresql
                     postgresql-service
                     operation)))))
