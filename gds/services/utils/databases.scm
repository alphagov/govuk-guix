(define-module (gds services utils databases)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow sudo))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (guix gcrypt)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:use-module (gds packages mongodb)
  #:export (<postgresql-connection-config>
            postgresql-connection-config
            postgresql-connection-config?
            postgresql-connection-config-port
            postgresql-connection-config-database

            <mongodb-connection-config>
            mongodb-connection-config
            mongodb-connection-config?
            mongodb-connection-config-port
            mongodb-connection-config-database

            <mysql-connection-config>
            mysql-connection-config
            mysql-connection-config?
            mysql-connection-config-port
            mysql-connection-config-database

            <redis-connection-config>
            redis-connection-config
            redis-connection-config?
            redis-connection-config-port
            redis-connection-config-namespace

            database-connection-config?
            database-connection-config->environment-variables))


(define-record-type* <postgresql-connection-config>
  postgresql-connection-config make-postgresql-connection-config
  postgresql-connection-config?
  (host postgresql-connection-config-host
        (default "localhost"))
  (user postgresql-connection-config-user)
  (port postgresql-connection-config-port
        (default 5432))
  (database postgresql-connection-config-database))

(define-record-type* <mysql-connection-config>
  mysql-connection-config make-mysql-connection-config
  mysql-connection-config?
  (host mysql-connection-config-host
        (default "localhost"))
  (user mysql-connection-config-user)
  (port mysql-connection-config-port
        (default 3306))
  (database mysql-connection-config-database)
  (password mysql-connection-config-password))

(define-record-type* <mongodb-connection-config>
  mongodb-connection-config make-mongodb-connection-config
  mongodb-connection-config?
  (user mongodb-connection-config-user)
  (password mongodb-connection-config-password)
  (host mongodb-connection-config-host
        (default "127.0.0.1"))
  (port mongodb-connection-config-port
        (default 27017))
  (database mongodb-connection-config-database))

(define-record-type* <redis-connection-config>
  redis-connection-config make-redis-connection-config
  redis-connection-config?
  (host redis-connection-config-host
        (default "localhost"))
  (port redis-connection-config-port
        (default 6379))
  (db-number redis-connection-config-db-number
             (default 0))
  (namespace redis-connection-config-namespace
             (default #f)))

(define (database-connection-config? config)
  (or (postgresql-connection-config? config)
      (mysql-connection-config? config)
      (mongodb-connection-config? config)
      (redis-connection-config? config)))

(define database-connection-config->environment-variables
  (match-lambda
    (($ <postgresql-connection-config> host user port database)
     `(("DATABASE_URL" .
        ,(simple-format
          #f
          "postgres://~A:~A/~A"
          host
          port
          database))))
    (($ <mysql-connection-config> host user port database)
     `(("DATABASE_URL" .
        ,(simple-format
          #f
          "mysql2://~A:~A/~A"
          host
          port
          database))))
    (($ <mongodb-connection-config> user password host port database)
     `(("MONGO_URL" . ,host)
       ("MONGO_DB" . ,database)
       ("MONGODB_URI" .
        ,(simple-format
          #f
          "mongodb://~A:~A/~A"
          host
          port
          database))))
    (($ <redis-connection-config> host port db-number namespace)
     `(("REDIS_URL" .
        ,(simple-format
          #f
          "redis://~A:~A/~A"
          host
          port
          db-number))
       ("REDIS_HOST" . ,host)
       ("REDIS_PORT" . ,(number->string port))
       ,@(if namespace `("REDIS_NAMESPACE" . ,namespace))))
    (unmatched
     (error "get-database-environment-variables no match for ~A"
            unmatched))))

(define (make-database-setup-thunk config)
  (cond
   ((postgresql-connection-config? config)
    (postgresql-create-user-and-database config))
   ((mongodb-connection-config? config)
    (mongodb-create-user-and-database config))
   ((mysql-connection-config? config)
    (mysql-create-user-and-database config))
   ((redis-connection-config? config)
    #~(lambda () #t))
   (else
    (error
     "make-database-setup: Unknown database configuration ~A"
     config))))

(define (run-with-psql-port database-connection . operations)
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
                  (let ((p (open-pipe* OPEN_WRITE psql "-a" "-p" (number->string #$port))))
                    (for-each
                     (lambda (o) (o p))
                     '#$operations)
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (waitpid pid))))))))

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

(define mongodb-create-user-and-database
  (match-lambda
    (($ <mongodb-connection-config> user password host port database)
     (with-imported-modules '((ice-9 popen))
       #~(lambda ()
           (let
               ((pid (primitive-fork))
                (mongodb-user (getpwnam "mongodb"))
                (mongo (string-append #$mongodb "/bin/mongo")))
             (if
              (= 0 pid)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid mongodb-user))
                  (setuid (passwd:uid mongodb-user))
                  (let ((p (open-pipe* OPEN_WRITE mongo "--port" (number->string #$port))))
                    (display "\nChecking if user exists:\n")
                    (simple-format p "
use ~A
db.createUser(
  {
    user: \"~A\",
    pwd: \"~A\",
    roles: [
       { role: \"readWrite\", db: \"~A\" }
    ]
  }
)
" #$database #$user #$password #$database)
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (waitpid pid))))))))

(define mysql-create-user-and-database
  (match-lambda
    (($ <mysql-connection-config> host user port database password)
     (with-imported-modules '((ice-9 popen))
       #~(lambda ()
           (let*
               ((pid (primitive-fork))
                (root (getpwnam "root"))
                (mysql (string-append #$mariadb "/bin/mysql"))
                (command `(,mysql "-h" #$host "-u" "root" "--password=''" "-P" ,(number->string #$port))))
             (if
              (= 0 pid)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (define (log-and-write p str . args)
                    (display (apply simple-format #f str args))(display "\n")
                    (apply simple-format p str args))

                  (setgid (passwd:gid root))
                  (setuid (passwd:uid root))
                  ;(apply system* (append command '("-e" "\"SHOW DATABASES;\"")))
                  (display "\n")
                  (display (string-join command " "))
                  (display "\n")
                  (let ((p (open-pipe (string-join command " ") OPEN_WRITE)))
                    (display "\nChecking if user exists:\n")
                    (log-and-write p "
CREATE USER IF NOT EXISTS '~A'@'localhost' IDENTIFIED BY '~A';\n
" #$user #$password)
                    (display "\nChecking if the database exists:\n")
                    (log-and-write p "
CREATE DATABASE ~A;\n" #$database)
                    (display "\nGRANT\n")
                    (log-and-write p "
GRANT ALL ON ~A.* TO '~A'@'localhost';\n" #$database #$user)
                    (log-and-write p "EXIT\n")
                    (primitive-exit
                     (status:exit-val (close-pipe p)))))
                (lambda ()
                  (primitive-exit 1)))
              (or
               (let ((result (waitpid pid)))
                 (display "result\n") ;; TODO: Fix error handling
                 (display result)
                 (display "\n")
                 (display (status:exit-val (cdr result)))
                 (display "\n")
                 (status:exit-val (cdr result)))
               (error "Error initialising mysql")))))))))
