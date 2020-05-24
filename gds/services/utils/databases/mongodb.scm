(define-module (gds services utils databases mongodb)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
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
  (user mongodb-connection-config-user
        (default #f))
  (password mongodb-connection-config-password
            (default #f))
  (host mongodb-connection-config-host
        (default "127.0.0.1"))
  (port mongodb-connection-config-port
        (default 27017))
  (database mongodb-connection-config-database))

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
     #~(lambda ()
         (let
             ((mongo (string-append #$mongodb "/bin/mongo")))
           (let ((p (open-pipe* OPEN_WRITE mongo "--port" (number->string #$port))))
             (for-each
              (lambda (o) (o p))
              (list #$@operations))
             (simple-format p "exit")
             (zero?
              (status:exit-val
                 (close-pipe p)))))))))

(define* (mongodb-restore-gexp database-connection file
                               #:key dry-run?)
  (match database-connection
    (($ <mongodb-connection-config> user password host port database)
     #~(lambda ()
         (use-modules (srfi srfi-1))
         (if (string-suffix? ".mongo.xz" #$file)
             (let* ((pv (string-append #$pv "/bin/pv"))
                    (decompressor
                     (assoc-ref '(("gz" . #$(file-append gzip "/bin/gzip"))
                                  ("xz" . #$(file-append xz "/bin/xz")))
                                (last (string-split #$file #\.))))
                    (command
                     (string-join
                      `(,pv
                        ,#$file
                        "|"
                        ,decompressor
                        "-d"
                        "|"
                        ,(string-append #$mongo-tools "/bin/mongorestore")
                        "--quiet"
                        "--host" ,(simple-format #f "~A:~A"
                                                 #$host
                                                 (number->string #$port))
                        #$@(if user `("-u" #$user) '())
                        #$@(if password `("-p" #$password) '())
                        "-d" #$database
                        "--drop" ;; TODO: Make this optional
                        "--archive"
                      " "))))
               #$@(if dry-run?
                      '((simple-format #t "Would run command: ~A\n"
                                       command))
                      '((simple-format #t "Running command: ~A\n" command)
                        (zero? (system command)))))
             (let ((command
                    `(,(string-append #$mongo-tools "/bin/mongorestore")
                      "--host" ,(simple-format #f "~A:~A"
                                               #$host
                                               (number->string #$port))
                      #$@(if user `("-u" #$user) '())
                      #$@(if password `("-p" #$password) '())
                      "-d" #$database
                      "--drop" ;; TODO: Make this optional
                      #$file)))
               #$@(if dry-run?
                      '((simple-format
                         #t "Would run command: ~A\n"
                         (string-join command " ")))
                      '((simple-format #t "Running command: ~A\n" (string-join command " "))
                        (zero?
                         (apply system* command))))))))))
