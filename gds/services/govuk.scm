(define-module (gds services govuk)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:use-module (gds services sidekiq)
  #:use-module (gds services govuk signon)
  #:use-module (gds packages mongodb)
  #:export (ports
            port-for

            govuk-content-schemas-service-type
            govuk-content-schemas-service

            rails-app-config
            rails-app-config?
            rails-app-service
            rails-app-service-type

            signon-config
            signon-config?
            signon-config-rails-app-config
            signon-config-applications

            postgresql-connection-config
            postgresql-connection-config?
            postgresql-connection-config-port
            postgresql-connection-config-database

            mongodb-connection-config
            mongodb-connection-config?
            mongodb-connection-config-port
            mongodb-connection-config-database

            publishing-api-service
            content-store-service
            draft-content-store-service
            specialist-publisher-service
            publishing-e2e-tests-service
            draft-content-store-service))

(define ports
  (make-parameter
   `((postgresql . 5432)
     (mongodb . 27017)
     (publishing-api . 3039)
     (content-store . 3000)
     (draft-content-store . 3001)
     (specialist-publisher . 3064))))

(define-record-type* <router-api-config>
  router-api-config make-router-api-config
  router-api-config?
  (router-nodes router-api-config-router-nodes
                (default '())))

;;;
;;; GOV.UK Content Schemas
;;;

(define govuk-content-schemas-service-type
  (shepherd-service-type
   'govuk-content-schemas
   (lambda (package)
     (shepherd-service
      (provision (list 'govuk-content-schemas))
      (documentation "Ensure /var/lib/govuk-content-schemas exists")
      (start
       #~(lambda _
           (use-modules (guix build utils))

           (if (not (file-exists? "/var/lib/govuk-content-schemas"))
               (begin
                 (mkdir-p "/var/lib")
                 (symlink #$package
                          "/var/lib/govuk-content-schemas")))
           #t))
   (stop #~(lambda _
             #f))
   (respawn? #f)))))

(define* (govuk-content-schemas-service
          #:optional #:key
          (package govuk-content-schemas))
  (service govuk-content-schemas-service-type package))


(define-record-type* <service-startup-config>
  service-startup-config make-service-startup-config
  service-startup-config?
  (pre-startup-script service-startup-config-pre-startup-script))

(define-record-type* <rails-app-config>
  rails-app-config make-rails-app-config
  rails-app-config?
  (environment rails-app-config-environment
               (default "production"))
  (secret-key-base rails-app-config-secret-key-base
                   (default #f))
  (secret-token rails-app-config-secret-token
                (default #f)))

(define (update-rails-app-config-environment environment config)
  (rails-app-config
   (inherit config)
   (environment environment)))

(define (update-rails-app-config-with-random-secret-token config)
  (rails-app-config
   (inherit config)
   (secret-token
    (or (rails-app-config-secret-token config)
        (random-base16-string 30)))))

(define (update-rails-app-config-with-random-secret-key-base config)
  (rails-app-config
   (inherit config)
   (secret-key-base
    (or (rails-app-config-secret-key-base config)
        (random-base16-string 30)))))

(define (rails-app-config->environment-variables config)
  (filter
   (lambda (pair) (cdr pair))
   (list
    (cons "RAILS_ENV" (rails-app-config-environment config))
    (cons "SECRET_KEY_BASE" (rails-app-config-secret-key-base config))
    (cons "SECRET_TOKEN" (rails-app-config-secret-token config)))))

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
             (default 0)))

(define (database-connection-config? config)
  (or (postgresql-connection-config? config)
      (mysql-connection-config? config)
      (mongodb-connection-config? config)
      (redis-connection-config? config)))

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
     `(("MONGODB_URI" .
        ,(simple-format
          #f
          "mongodb://~A:~A/~A"
          host
          port
          database))))
    (($ <redis-connection-config> host port db-number)
     `(("REDIS_URL" .
        ,(simple-format
          #f
          "redis://~A:~A/~A"
          host
          port
          db-number))
       ("REDIS_HOST" . ,host)
       ("REDIS_PORT" . ,(number->string port))))
    (unmatched
     (error "get-database-environment-variables no match for ~A"
            unmatched))))

(define postgresql-create-user-and-database
  (match-lambda
    (($ <postgresql-connection-config> user port database)
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
                    (display "\nChecking if user exists:\n")
                    (simple-format p "
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
" #$user #$user)
                    (display "\nChecking if the database exists:\n")
                    (simple-format p "
CREATE DATABASE \"~A\" WITH OWNER \"~A\";" #$database #$user)
                    (close-pipe p))
                  (primitive-exit 0))
                (lambda ()
                  (primitive-exit 1)))
              (waitpid pid))))))))

(define mongodb-create-user-and-database
  (match-lambda
    (($ <mongodb-connection-config> user port database)
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
" #$database #$user "password" #$database)
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

;;;
;;; Generic Rails App Service
;;;

(define (generic-rails-app-service-environment-variables name root-directory bundle-path-base . parameters)
  (apply
   append
   `(("PATH" . ,(string-append root-directory "/bin"))
     ("BUNDLE_PATH" .
      ,(string-append bundle-path-base name "-FAKE_HASH/BUNDLE_PATH"))
     ("BUNDLE_APP_CONFIG" .
      ,(string-append root-directory "/.bundle"))
     ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/lib/govuk-content-schemas"))
   (map
    (lambda (parameter)
      (cond
       ((plek-config? parameter)
        (plek-config->environment-variables parameter))
       ((rails-app-config? parameter)
        (rails-app-config->environment-variables parameter))
       ((database-connection-config? parameter)
        (database-connection-config->environment-variables parameter))
       (else '())))
    parameters)))

(define (generic-rails-app-start-script
         name
         package
         rails-app-config
         plek-config
         .
         rest)
  (let*
      ((string-name (symbol->string name))
       (string-port
        (number->string
         (service-port-from-plek-config plek-config name)))
       (root-directory
        (string-append "/var/lib/" string-name))
       (bundler
        (car
         (assoc-ref (package-propagated-inputs package)
                    "bundler")))
       (bundle-path-base "/tmp/guix/")
       (service-startup-config
        (find service-startup-config? rest))
       (pre-startup-scripts
        (if service-startup-config
            (let ((pre-startup-script
                   (service-startup-config-pre-startup-script
                    service-startup-config)))
              (if (list? pre-startup-script)
                  pre-startup-script
                  (list pre-startup-script)))
            '()))
       (database-connection-configs
        (filter database-connection-config? rest))
       (database-connection-thunks
        (map make-database-setup-thunk database-connection-configs))
       (run-rake-db-setup?
        (not (null? database-connection-configs)))
       (environment-variables
        (apply
         generic-rails-app-service-environment-variables
         string-name
         root-directory
         bundle-path-base
         rails-app-config
         plek-config
         rest)))
    (program-file
     (string-append "start-" string-name)
     (with-imported-modules '((guix build utils)
                              (ice-9 popen))
       #~(let ((user (getpwnam #$string-name))
               (bundle (string-append #$root-directory "/bin/bundle"))
               (rake (string-append #$root-directory "/bin/rake"))
               (rails (string-append #$root-directory "/bin/rails")))
           (use-modules (guix build utils)
                        (ice-9 popen))

           (for-each
            (lambda (t) (t))
            (list #$@database-connection-thunks))

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))
           (for-each
            (lambda (env-var)
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)
           (chdir #$root-directory)
           (and
            (if #$run-rake-db-setup?
                (zero? (system* rake "db:setup"))
                #t)
            (for-each
             (lambda (t) (t))
             (list #$@pre-startup-scripts))
            (zero? (system* rails "server" "-p" #$string-port))))))))



(define (generic-rails-app-activation
         name
         .
         rest)
  (let*
      ((rails-app-config (find rails-app-config? rest))
       (package (find package? rest))
       (string-name (symbol->string name))
       (root-directory (app-name->root-directory string-name))
       (environment-variables
        (apply
         generic-rails-app-service-environment-variables
         root-directory
         rails-app-config
         rest)))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match)
                     (ice-9 ftw)
                     (srfi srfi-26))
        (let* ((string-name (symbol->string '#$name))
               (user (getpwnam string-name))
               (bundle (string-append #$root-directory "/bin/bundle")))
          (if
           (not (file-exists? #$root-directory))
           (begin
             (mkdir-p #$root-directory)
             (chown #$root-directory (passwd:uid user) (passwd:gid user))
             (for-each
              (lambda (file)
                (copy-recursively
                 (string-append #$package "/" file)
                 (string-append #$root-directory "/" file)
                 #:log (%make-void-port "w")))
              (scandir
               #$package
               (negate
                (cut member <> '("." ".." "tmp" "log" "spec" "doc")))))

             (for-each
              (lambda (file)
                (mkdir-p file)
                (chown file (passwd:uid user) (passwd:gid user)))
              (map
               (lambda (dir)
                 (string-append #$root-directory "/" dir))
               '("tmp" "log" "public"))))
           (begin
             (copy-recursively
              (string-append #$package "/bin")
              (string-append #$root-directory "/bin")
              #:log (%make-void-port "w")
              #:follow-symlinks? #f)
             (mkdir-p (string-append #$root-directory "/.bundle"))
             (copy-file (string-append #$package "/.bundle/config")
                        (string-append #$root-directory "/.bundle/config"))
             (for-each
              (lambda (name)
                (let ((target
                       (string-append #$root-directory "/vendor/" name)))
                  (if (file-exists? target)
                      (delete-file target))
                  (symlink (string-append #$package "/vendor/" name)
                           target)))
              '("cache" "bundle"))))

          (call-with-output-file (string-append #$root-directory "/bin/env.sh")
            (lambda (port)
              (for-each
               (lambda (env-var)
                 (simple-format port "export ~A=\"~A\"\n" (car env-var) (cdr env-var)))
               '#$environment-variables)))

          (mkdir-p (string-append #$root-directory "/tmp/pids"))
          (chmod (string-append #$root-directory "/tmp/pids") #o777)

          (substitute* (find-files (string-append #$root-directory "/bin"))
            (("/usr/bin/env") (which "env")))
          (let*
              ((target
                (string-append "exec -a \"\\$0\" \"" #$package))
               (replacement
                (string-append "exec -a \"$0\" \"" #$root-directory)))
            (substitute* (find-files (string-append #$root-directory "/bin"))
              ((target)
               replacement)))))))

(define (generic-rails-app-shepherd-service
         name
         requirements
         start-script)
  (list
   (shepherd-service
    (provision (list name))
    (documentation
     (simple-format #f "~A rails app" name))
    (requirement requirements)
    (respawn? #f)
    (start #~(make-forkexec-constructor #$start-script))
    (stop #~(make-kill-destructor)))))

(define (generic-rails-app-service-account
         username)
  (list
   (user-account
    (name username)
    (group "nogroup")
    (system? #t)
    (home-directory "/var/empty")
    (shell #~(string-append #$shadow "/sbin/nologin")))))

(define (make-rails-app-service-type config)
  (service-type
   (name (rails-app-config-name config))
   (extensions
    (append
     (list
      (service-extension shepherd-root-service-type
                         (match-lambda
                           (($ <rails-app-config> name package requirements ports root-directory database-connection-configs)
                            (generic-rails-app-shepherd-service
                             name
                             requirements
                             (generic-rails-app-start-script
                              name
                              package
                              ports
                              root-directory
                              database-connection-configs)))))
      (service-extension activation-service-type
                         (match-lambda
                           (($ <rails-app-config> name package requirements ports root-directory database-connection-configs)
                            (generic-rails-app-activation
                             name
                             package
                             root-directory
                             database-connection-configs))))
      (service-extension account-service-type
                         (match-lambda
                           (($ <rails-app-config> name)
                            (generic-rails-app-service-account
                             (symbol->string name))))))
     (let ((signon-application
            (rails-app-config-signon-application config)))
       (if signon-application
           (list
            (service-extension signon-service-type
                               (const signon-application)))
           '()))))))

(define (rails-app-service config)
  (service
   (make-rails-app-service-type config)
   config))

(define (make-rails-app-using-signon-service-type name . rest)
  (let ((base-service-type
         (apply make-rails-app-service-type name rest)))
    (service-type
     (inherit base-service-type)
     (extensions
      (cons
       (service-extension signon-service-type
                          (lambda (parameters)
                            (filter
                             signon-application?
                             parameters)))
       (service-type-extensions base-service-type))))))

;;;
;;; Signon
;;;

(define-record-type* <signon-config>
  signon-config make-signon-config
  signon-config?
  (applications signon-config-applications
                (default '())))

(define signon-service-type
  (service-type
   (inherit
    (make-rails-app-service-type 'signon))
   (compose list)
   (extend (lambda (parameters applications)
             (match parameters
               ((plek-config rails-app-config package config rest ...)
                (cons*
                 plek-config
                 rails-app-config
                 package
                 (signon-config
                  (inherit config)
                  (applications (append
                                 (signon-config-applications config)
                                 applications)))
                 rest)))))))

(define default-signon-database-connection-configs
  (list
   (mysql-connection-config
    (host "localhost")
    (user "halberd")
    (port "-")
    (database "signon-production")
    (password ""))
   (redis-connection-config)))

(define signon-service
  (service
   signon-service-type
   (cons* (plek-config) (rails-app-config) signonotron2
          (signon-config) (sidekiq-config) default-signon-database-connection-configs)))

;;
;; Publishing E2E Tests
;;

(define (make-publishing-e2e-tests-start-script environment-variables package)
  (let*
      ((bundle-path-base "/tmp/guix/")
       (bundle-path
        (string-append bundle-path-base "publishing-e2e-tests-FAKE_HASH/BUNDLE_PATH"))
       (bundle-bin-path
        (string-append bundle-path "/bin"))
       (environment-variables
        (append
         environment-variables
         `(("SECRET_KEY_BASE" . "t0a")
           ("BUNDLE_PATH" . ,bundle-path)
           ("BUNDLE_APP_CONFIG" .
            "/var/lib/publishing-e2e-tests/.bundle")
           ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/lib/govuk-content-schemas")))))
    (program-file
     (string-append "start-publishing-e2e-tests")
     (with-imported-modules '((guix build utils)
                              (ice-9 popen))
       #~(let ((user (getpwnam "nobody"))
               (bundle (string-append #$package "/bin/bundle"))
               (rspec (string-append #$bundle-bin-path "/rspec")))
           (use-modules (guix build utils)
                        (ice-9 popen))

           (mkdir-p "/var/lib/publishing-e2e-tests")
           (chown "/var/lib/publishing-e2e-tests"
                  (passwd:uid user) (passwd:gid user))

           (mkdir-p #$bundle-path-base)
           (chmod #$bundle-path-base #o777)

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))
           (display "\n")
           (for-each
            (lambda (env-var)
              (display "export ")(display (car env-var))(display "=")(display "\"")(display (cdr env-var))(display "\"\n")
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)
           (display "\n")
           (chdir #$package)
           (and
            (zero? (system* bundle "install"))
            (zero? (system* bundle "exec" "rspec"))))))))

(define publishing-e2e-tests-service-type
  (service-type
   (name 'publishing-e2e-tests-service)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           (match-lambda
            ((plek-config package)
             (let* ((start-script
                     (make-publishing-e2e-tests-start-script
                      (plek-config->environment-variables plek-config)
                      package)))
               (list
                (shepherd-service
                 (provision (list 'publishing-e2e-tests))
                 (documentation "publishing-e2e-tests")
                 (requirement '(specialist-publisher))
                 (respawn? #f)
                 (start #~(make-forkexec-constructor #$start-script))
                 (stop #~(make-kill-destructor))))))))))))

(define publishing-e2e-tests-service
  (service
   publishing-e2e-tests-service-type
   (list (plek-config) publishing-e2e-tests)))

(define (port-for service)
  (assq-ref (ports) service))

(define* (publishing-api-service
          #:optional #:key
          (name 'publishing-api)
          (package publishing-api)
          (requirements '(content-store postgres))
          (ports (ports))
          (root-directory "/var/lib/publishing-api")
          (database-connection-configs
           (list
            (postgresql-connection-config
             (user "publishing-api")
             (port (port-for 'postgresql))
             (database "publishing_api_production"))))
          (signon-application
           (signon-application
            (name (symbol->string name))
            (description "")
            (redirect-uri "")
            (home-uri "")
            (uid "uid")
            (secret "secret"))))
  (rails-app-service
   (rails-app-config
    (name name)
    (package package)
    (requirements requirements)
    (ports ports)
    (root-directory root-directory)
    (database-connection-configs
     database-connection-configs)
    (signon-application
     signon-application))))

(define* (content-store-service
          #:optional #:key
          (name 'content-store)
          (package content-store)
          (requirements '(mongodb))
          (ports (ports))
          (root-directory "/var/lib/content-store")
          (database-connection-configs
           (list
            (mongodb-connection-config
             (user "content-store")
             (port (port-for 'mongodb))
             (database "content-store")))))
  (rails-app-service
   (rails-app-config
    (name name)
    (package package)
    (requirements requirements)
    (ports ports)
    (root-directory root-directory)
    (database-connection-configs
     database-connection-configs))))

(define* (draft-content-store-service
          #:optional #:key
          (name 'draft-content-store)
          (root-directory "/var/lib/draft-content-store")
          (database-connection-configs
           (list
            (mongodb-connection-config
             (user "draft-content-store")
             (port (port-for 'mongodb))
             (database "draft-content-store"))))
          #:allow-other-keys
          #:rest rest)
  (apply
   content-store-service
   #:name name
   #:root-directory root-directory
   #:database-connection-configs database-connection-configs
   rest))

(define* (specialist-publisher-service
          #:optional #:key
          (name 'specialist-publisher)
          (package specialist-publisher)
          (requirements '(publishing-api))
          (ports (ports))
          (root-directory "/var/lib/specialist-publisher")
          (database-connection-configs '())
          (signon-application
           (signon-application
            (name (symbol->string name))
            (description "")
            (redirect-uri "")
            (home-uri "")
            (uid (random-base16-string 32))
            (secret (random-base16-string 32)))))
  (rails-app-service
   (rails-app-config
    (name name)
    (package package)
    (requirements requirements)
    (ports ports)
    (root-directory root-directory)
    (database-connection-configs
     database-connection-configs)
    (signon-application
     signon-application))))

(define* (static-service
          #:optional #:key
          (name 'static)
          (package static)
          (requirements '())
          (ports (ports))
          (root-directory "/var/lib/static"))
  (rails-app-service
   (rails-app-config
    (name name)
    (package package)
    (requirements requirements)
    (ports ports)
    (root-directory root-directory))))
