(define-module (gds services utils databases mongodb)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gds packages mongodb)
  #:export (<mongodb-connection-config>
            mongodb-connection-config
            mongodb-connection-config?
            mongodb-connection-config-port
            mongodb-connection-config-database

            mongodb-restore-gexp
            run-with-mongodb-port
            mongodb-create-user-for-database-connection))

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


(define mongodb-create-user-for-database-connection
  (match-lambda
    (($ <mongodb-connection-config> user password host port database)
     #~(lambda (port)
         (simple-format p "
var database = \"~A\";
var username = \"~A\";

db = db.getSiblingDB(database);

var profile = {
  pwd: \"~A\",
  roles: [
     { role: \"readWrite\", db: database }
  ]
};

if (db.getUser(username) === null) {
  db.createUser(
    Object.assign(
      {
        user: username,
      },
      profile
    )
  );
} else {
  db.updateUser(
    username,
    profile
  );
}
" #$database #$user #$password)))))

(define (run-with-mongodb-port database-connection operations)
  (match database-connection
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
                    (for-each
                     (lambda (o) (o p))
                     (list #$@operations))
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (zero? (cdr (waitpid pid))))))))))

(define (mongodb-restore-gexp database-connection file)
  (match database-connection
    (($ <mongodb-connection-config> user password host port database)
     #~(lambda ()
         (simple-format #t "Running mongorestore with ~A" #$file)
         (zero?
          (system*
           (string-append #$mongo-tools "/bin/mongorestore")
           "--host" (simple-format #f "~A:~A" #$host (number->string #$port))
           "-u" #$user
           "-p" #$password
           "-d" #$database
           #$file))))))
