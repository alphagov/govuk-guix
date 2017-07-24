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
            postgresql-import-gexp
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
     (with-imported-modules '((ice-9 popen))
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
               #t)))))))

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

(define (postgresql-import-gexp database-connection file)
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
           (simple-format #t "Running command: ~A\n" command)
           (zero? (system command)))))))

(define (postgresql-create-user-and-database-for-database-connection
         database-connection)
  (run-with-psql-port
   (postgresql-connection-config
    (inherit database-connection)
    (user "postgres")) ;; The user in the database connection might
                       ;; not exist, so use postgres instead
   (match database-connection
     (($ <postgresql-connection-config> host user port database)
      (list
       (postgresql-ensure-user-exists-gexp user)
       (postgresql-create-database-gexp database user))))))
