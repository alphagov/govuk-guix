(define-module (gds services utils databases mysql)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
  #:export (<mysql-connection-config>
            mysql-connection-config
            mysql-connection-config?
            mysql-connection-config-host
            mysql-connection-config-user
            mysql-connection-config-port
            mysql-connection-config-database
            mysql-connection-config-password

            run-with-mysql-port
            mysql-ensure-user-exists-gexp
            mysql-create-database-gexp
            mysql-create-user-and-database-for-database-connection
            mysql-run-file-gexp))

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

(define (run-with-mysql-port database-connection operations)
  (match database-connection
    (($ <mysql-connection-config> host user port database)
     (with-imported-modules '((ice-9 popen))
       #~(lambda ()
           (let
               ((pid (primitive-fork))
                (root (getpwnam "root"))
                (command `(,(string-append #$mariadb "/bin/mysql")
                           "-h" #$host
                           "-u" "root"
                           "--password=''"
                           "-P" ,(number->string #$port))))
             (if
              (= 0 pid)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid root))
                  (setuid (passwd:uid root))
                  (let ((p (apply open-pipe* OPEN_WRITE command)))
                    (for-each
                     (lambda (o) (o p))
                     (list #$@operations))
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (zero? (cdr (waitpid pid))))))))))

(define (mysql-run-file-gexp database-connection file)
  (match database-connection
    (($ <mysql-connection-config> host user port database)
     (with-imported-modules '((ice-9 popen))
       #~(lambda ()
           (let
               ((command `(,(string-append #$pv "/bin/pv")
                           ,#$file
                           "|"
                           ,(string-append #$pbzip2 "/bin/pbzip2")
                           "-d"
                           "|"
                           ,(string-append #$mariadb "/bin/mysql")
                           "-h" #$host
                           "-u" "root"
                           "--password=''"
                           "-P" ,(number->string #$port))))
             (simple-format #t "Running command: ~A\n\n" (string-join command))
             (zero?
              (system (string-join command)))))))))

(define (mysql-ensure-user-exists-gexp user password)
  #~(lambda (port)
      (define (log-and-write p str . args)
        (display (apply simple-format #f str args))(display "\n")
        (apply simple-format p str args))

      (log-and-write port "
CREATE USER IF NOT EXISTS '~A'@'localhost' IDENTIFIED BY '~A';\n
" #$user #$password)))

(define (mysql-create-database-gexp database user)
  #~(lambda (port)
      (define (log-and-write p str . args)
        (display (apply simple-format #f str args))(display "\n")
        (apply simple-format p str args))

      (log-and-write port "
CREATE DATABASE ~A;\n" #$database)
      (log-and-write port "
GRANT ALL ON ~A.* TO '~A'@'localhost';\n" #$database #$user)
      (log-and-write port "EXIT\n")))

(define (mysql-create-user-and-database-for-database-connection
         database-connection)
  (run-with-mysql-port
   database-connection
   (match database-connection
     (($ <mysql-connection-config> host user port database password)
      (list
       (mysql-ensure-user-exists-gexp user password)
       (mysql-create-database-gexp database user))))))
