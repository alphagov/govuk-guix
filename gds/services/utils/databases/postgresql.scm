(define-module (gds services utils databases postgresql)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:export (<postgresql-connection-config>
            postgresql-connection-config
            postgresql-connection-config?
            postgresql-connection-config-port
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
           (let
               ((pid (primitive-fork))
                (postgres-user (getpwnam "postgres"))
                (psql (string-append #$postgresql "/bin/psql")))
             (if
              (= 0 pid)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid postgres-user))
                  (setuid (passwd:uid postgres-user))
                  (let ((p (open-pipe* OPEN_WRITE psql "-a" "-p" (number->string #$port))))
                    (for-each
                     (lambda (o) (o p))
                     (list #$@operations))
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (zero? (cdr (waitpid pid))))))))))

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
  (match-lambda
    (($ <postgresql-connection-config> host user port database)
     (with-imported-modules '((ice-9 popen))
       #~(lambda ()
           (let
               ((pid (primitive-fork))
                (postgres-user (getpwnam "postgres"))
                (psql (string-append #$postgresql "/bin/psql")))
             (if
              (= 0 pid)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid postgres-user))
                  (setuid (passwd:uid postgres-user))
                  (system* psql "-f" #$file "--quiet")
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (waitpid pid))))))))

(define (postgresql-create-user-and-database-for-database-connection
         database-connection)
  (run-with-psql-port
   database-connection
   (match database-connection
     (($ <postgresql-connection-config> host user port database)
      (list
       (postgresql-ensure-user-exists-gexp user)
       (postgresql-create-database-gexp database user))))))
