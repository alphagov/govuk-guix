(define-module (gds services utils databases postgresql)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
  #:export (<postgresql-connection-config>
            postgresql-connection-config
            postgresql-connection-config?
            postgresql-connection-config-port
            postgresql-connection-config-user
            postgresql-connection-config-host
            postgresql-connection-config-database

            run-with-psql-port
            postgresql-ensure-user-exists-gexp
            postgresql-create-database-gexp
            postgresql-list-databases-gexp
            postgresql-import-gexp
            postgresql-create-user-for-database-connection
            postgresql-create-user-and-database-for-database-connection))

(define-record-type* <postgresql-connection-config>
  postgresql-connection-config make-postgresql-connection-config
  postgresql-connection-config?
  (host postgresql-connection-config-host
        (default "localhost"))
  (user postgresql-connection-config-user)
  (port postgresql-connection-config-port
        (default 5432))
  (database postgresql-connection-config-database))

(define (run-with-psql-port database-connection operations)
  (match database-connection
    (($ <postgresql-connection-config> host user port database)
     #~(lambda ()
         (use-modules (ice-9 popen))
         (let
             ((psql (string-append #$postgresql "/bin/psql")))
           (let ((p (open-pipe*
                     OPEN_WRITE psql
                     (string-append "--user=" #$user)
                     "-a"
                     "--no-psqlrc"
                     "-p" (number->string #$port))))
             (for-each
              (lambda (o) (o p))
              (list #$@operations))
             (close-pipe p)
             #t))))))

(define (postgresql-list-databases-gexp database-connection)
  (match database-connection
    (($ <postgresql-connection-config> host user port database)
     #~(lambda ()
         (use-modules (ice-9 popen)
                      (ice-9 rdelim)
                      (srfi srfi-1))
         (let* ((command `(,(string-append #$postgresql "/bin/psql")
                           ,(string-append "--user=" #$user)
                           "-p" ,(number->string #$port)
                           "--no-psqlrc"
                           "-lqt"))
                (p (apply open-pipe* OPEN_READ command))
                (lines (let loop ((lines '())
                                  (line (read-line p)))
                         (if (eof-object? line)
                             (reverse lines)
                             (loop (cons line lines)
                                   (read-line p))))))
           (and (let ((status (close-pipe p)))
                  (if (zero? status)
                      #t
                      (begin
                        (simple-format #t
                                       "command: ~A\n"
                                       (string-join command))
                        (error "listing databases failed, status ~A\n"
                               status))))
                (filter-map
                 (lambda (line)
                   (let ((name
                          (string-trim-both
                           (first (string-split line #\|)))))
                     (if (eq? (string-length name) 0)
                         #f
                         name)))
                 lines)))))))

(define (postgresql-ensure-user-exists-gexp user)
  #~(lambda (port)
      (simple-format port "
DO
$body$
BEGIN
   IF NOT EXISTS (
      SELECT *
      FROM   pg_catalog.pg_user
      WHERE  usename = '~A') THEN

      CREATE ROLE \"~A\" LOGIN CREATEDB;
   END IF;
END
$body$;
" #$user #$user)))

(define (postgresql-create-database-gexp database owner)
  #~(lambda (port)
      (simple-format port "
CREATE DATABASE \"~A\" WITH OWNER \"~A\";" #$database #$owner)))

(define* (postgresql-import-gexp database-connection file
                                 #:key dry-run?)
  (match database-connection
    (($ <postgresql-connection-config> host user port database)
     #~(lambda _
         (let*
             ((psql (string-append #$postgresql "/bin/psql"))
              (gzip (string-append #$gzip "/bin/gzip"))
              (pv (string-append #$pv "/bin/pv"))
              (command
               (string-join
                `(,pv
                  ,#$file
                  "|"
                  ,gzip
                  "-d"
                  "|"
                  ,psql
                  ,(simple-format #f "postgres://~A@~A:~A/~A" #$user #$host #$port #$database)
                  "--no-psqlrc"
                  "--quiet")
                " ")))
           #$@(if dry-run?
                  '((simple-format #t "Would run command: ~A\n"
                                   command))
                  '((simple-format #t "Running command: ~A\n" command)
                    (zero? (system command)))))))))

(define (postgresql-create-user-for-database-connection
         database-connection)
  (let ((database-connection-with-postgres-user
         (postgresql-connection-config
          (inherit database-connection)
          (user "postgres")))) ;; The user in the database connection
                               ;; might not exist, so use postgres
                               ;; instead
    (run-with-psql-port
     database-connection-with-postgres-user
     (match database-connection
       (($ <postgresql-connection-config> host user port database)
        (list
         (postgresql-ensure-user-exists-gexp user)))))))

(define (postgresql-create-user-and-database-for-database-connection
         database-connection)
  (let ((database-connection-with-postgres-user
         (postgresql-connection-config
          (inherit database-connection)
          (user "postgres")))) ;; The user in the database connection
                               ;; might not exist, so use postgres
                               ;; instead
    (run-with-psql-port
     database-connection-with-postgres-user
     (match database-connection
       (($ <postgresql-connection-config> host user port database)
        (list
         (postgresql-ensure-user-exists-gexp user)
         #~(lambda (port)
             (if (member
                  #$database
                  (map car (#$(postgresql-list-databases-gexp
                               database-connection-with-postgres-user))))
                 #t
                 (#$(postgresql-create-database-gexp database user) port)))))))))
