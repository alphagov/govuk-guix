(define-module (gds services mongodb)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gds packages mongodb)
  #:export (mongodb-configuration
            mongodb-configuration?
            mongodb-service
            mongodb-service-type))

(define-record-type* <mongodb-configuration>
  mongodb-configuration make-mongodb-configuration
  mongodb-configuration?
  (mongodb     mongodb-configuration-mongodb
               (default mongodb))
  (port        mongodb-congiguration-port
               (default 27017))
  (config-file    mongodb-configuration-file)
  (data-directory mongodb-configuration-data-directory))

(define %default-mongodb-config
  (mixed-text-file "mongodb.conf"
                   "dbpath=/var/lib/mongodb\n"
                   "logpath=/var/lib/mongodb/mongodb.log\n"
                   "logappend=true\n"
                   "bind_ip = 127.0.0.1\n"
                   "#port = 27017\n"
                   "journal=true\n"))

(define %mongodb-accounts
  (list (user-group (name "mongodb") (system? #t))
        (user-account
         (name "mongodb")
         (group "mongodb")
         (system? #t)
         (comment "Mongodb server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define mongodb-activation
  (match-lambda
    (($ <mongodb-configuration> mongodb port config-file data-directory)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "mongodb")))
           ;; Create db state directory.
           (mkdir-p #$data-directory)
           (chown #$data-directory (passwd:uid user) (passwd:gid user)))))))

(define mongodb-shepherd-service
  (match-lambda
    (($ <mongodb-configuration> mongodb port config-file data-directory)
     (let ((start-script
            ;; Wrapper script that switches to the 'mongodb' user before
            ;; launching daemon.
            (program-file "start-mongodb"
                          #~(let ((user (getpwnam "mongodb"))
                                  (mongod (string-append #$mongodb
                                                         "/bin/mongod")))
                              (setgid (passwd:gid user))
                              (setuid (passwd:uid user))
                              (system* mongod
                                       (string-append "--config="
                                                      #$config-file)
                                       "--port" (number->string #$port))))))
       (list (shepherd-service
              (provision '(mongodb))
              (documentation "Run the Mongodb daemon.")
              (requirement '(user-processes syslogd))
              (start #~(make-forkexec-constructor #$start-script))
              (stop #~(make-kill-destructor))))))))

(define mongodb-service-type
  (service-type (name 'mongodb)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          mongodb-shepherd-service)
                       (service-extension activation-service-type
                                          mongodb-activation)
                       (service-extension account-service-type
                                          (const %mongodb-accounts))))))

(define* (mongodb-service #:key
                          (mongodb mongodb)
                          (port 27017)
                          (config-file %default-mongodb-config)
                          (data-directory "/var/lib/mongodb"))
  (service mongodb-service-type
           (mongodb-configuration
            (mongodb mongodb)
            (port port)
            (config-file config-file)
            (data-directory data-directory))))
