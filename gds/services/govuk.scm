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

            govuk-content-schemas-service-type
            govuk-content-schemas-service

            rails-app-config
            rails-app-config?

            update-rails-app-config-environment
            update-rails-app-config-with-random-secret-key-base
            update-rails-app-config-with-random-secret-token

            rails-app-service
            rails-app-service-type

            signon-config
            signon-config?
            signon-config-rails-app-config
            signon-config-applications

            router-config
            router-config?
            router-config-public-port
            router-config-api-port

            router-api-config
            router-api-config?
            router-api-nodes

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

(define govuk-content-schemas-service
  (service govuk-content-schemas-service-type govuk-content-schemas))


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

;;;
;;; Generic Rails App Service
;;;

(define (generic-rails-app-service-environment-variables name root-directory . parameters)
    (apply
     append
     `(("PATH" . ,(simple-format #f "~A/bin" root-directory))
       ;; GOVUK_APP_NAME is primarily for Slimmer
       ("GOVUK_APP_NAME" . ,name)
       ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/lib/govuk-content-schemas"))
     (map
      (lambda (parameter)
        (cond
         ((plek-config? parameter)
          (plek-config->environment-variables parameter))
         ((router-api-config? parameter)
          (list (cons "ROUTER_NODES" (string-join
                                      (router-api-config-router-nodes parameter)
                                      ","))))
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



(define (gemrc ruby)
  (mixed-text-file "gemrc"
                   "custom_shebang: " ruby "/bin/ruby\n"))

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

(define (generic-rails-app-shepherd-services
         name
         requirements
         plek-config
         rails-app-config
         package
         .
         rest)
  (cons
   (let
       ((start-script
         (apply
          generic-rails-app-start-script
          name
          package
          rails-app-config
          plek-config
          rest)))
     (shepherd-service
      (provision (list name))
      (documentation
       (simple-format #f "~A rails app" name))
      (requirement requirements)
      (respawn? #f)
      (start #~(make-forkexec-constructor #$start-script))
      (stop #~(make-kill-destructor))))
   (let ((sidekiq-config (find sidekiq-config? rest)))
     (if sidekiq-config
         (list
          (let
              ((sidekiq-start-script
                (apply
                 generic-sidekiq-start-script
                 name
                 package
                 sidekiq-config
                 rails-app-config
                 plek-config
                 rest)))
            (shepherd-service
             (provision (list (symbol-append name '-sidekiq)))
             (documentation
              (simple-format #f "~A sidekiq service" name))
             (requirement requirements)
             (respawn? #f)
             (start #~(make-forkexec-constructor #$sidekiq-start-script))
             (stop #~(make-kill-destructor)))))
         '()))))

(define (generic-rails-app-service-account
         username)
  (list
   (user-account
    (name username)
    (group "nogroup")
    (system? #t)
    (home-directory "/var/empty")
    (shell #~(string-append #$shadow "/sbin/nologin")))))

(define* (make-rails-app-service-type name
                                      #:optional #:key
                                      (requirements '()))
  (service-type
   (name name)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        (lambda (parameters)
                          (apply generic-rails-app-shepherd-services name requirements parameters)))
     (service-extension activation-service-type
                        (lambda (parameters)
                          (apply generic-rails-app-activation name parameters)))
     (service-extension account-service-type
                        (const
                         (generic-rails-app-service-account
                          (symbol->string name))))))))

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
      ((environment-variables
        (append
         environment-variables
         `(("SECRET_KEY_BASE" . "t0a")
           ("CAPYBARA_SAVE_PATH" . "/tmp/guix/")
           ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/lib/govuk-content-schemas")))))
    (program-file
     (string-append "start-publishing-e2e-tests")
     (with-imported-modules '((guix build utils)
                              (ice-9 popen))
       #~(let ((user (getpwnam "nobody"))
               (bundle (string-append #$package "/bin/bundle")))
           (use-modules (guix build utils)
                        (ice-9 popen))

           (mkdir-p "/var/lib/publishing-e2e-tests")
           (chown "/var/lib/publishing-e2e-tests"
                  (passwd:uid user) (passwd:gid user))

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))
           (for-each
            (lambda (env-var)
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)
           (chdir #$package)
           (and
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

;;;
;;; Publishing API Service
;;;

(define default-publishing-api-database-connection-configs
  (list
   (postgresql-connection-config
    (user "publishing-api")
    (port "5432")
    (database "publishing_api_production"))))

(define default-publishing-api-signon-application
  (signon-application
   (name "publishing-api")
   (description "")
   (redirect-uri "")
   (home-uri "")
   (uid "uid")))

(define publishing-api-service-type
  (make-rails-app-service-type
   'publishing-api
   #:requirements '(content-store draft-content-store)))

(define publishing-api-service
  (service
   publishing-api-service-type
   (cons* (plek-config) (rails-app-config) publishing-api
          default-publishing-api-signon-application
          (sidekiq-config)
          default-publishing-api-database-connection-configs)))

;;;
;;; Content store
;;;

(define default-content-store-database-connection-configs
  (list
   (mongodb-connection-config
    (user "content-store")
    (password (random-base16-string 30))
    (database "content-store"))))

(define content-store-service-type
  (make-rails-app-using-signon-service-type
   'content-store
   #:requirements '(mongodb)))

(define content-store-service
  (service
   content-store-service-type
   (cons* (plek-config) (rails-app-config) content-store
          default-content-store-database-connection-configs)))

(define default-draft-content-store-database-connection-configs
  (list
   (mongodb-connection-config
    (user "draft-content-store")
    (password (random-base16-string 30))
    (database "draft-content-store"))))

(define draft-content-store-service-type
  (make-rails-app-service-type 'draft-content-store))

(define draft-content-store-service
  (service
   draft-content-store-service-type
   (cons* (plek-config) (rails-app-config) content-store
          default-draft-content-store-database-connection-configs)))

;;;
;;; Specialist Publisher
;;;

(define default-specialist-publisher-database-connection-configs
  (list
   (mongodb-connection-config
    (user "specialist-publisher")
    (password (random-base16-string 30))
    (database "specialist_publisher"))))

(define default-specialist-publisher-service-startup-config
  (service-startup-config
   (pre-startup-scripts
    (list
     (run-command "rake" "db:seed")
     (run-command "rake" "publishing_api:publish_finders")
     (run-command "rake" "permissions:grant[David Heath]")))))

(define specialist-publisher-service-type
  (make-rails-app-using-signon-service-type
   'specialist-publisher
   #:requirements '(publishing-api)))

(define specialist-publisher-service
  (service
   specialist-publisher-service-type
   (cons* (plek-config) (rails-app-config) specialist-publisher
          default-specialist-publisher-service-startup-config
          default-specialist-publisher-database-connection-configs)))

;;;
;;; Specialist Frontend
;;;

(define specialist-frontend-service-type
  (make-rails-app-using-signon-service-type
   'specialist-frontend
   #:requirements '(content-store)))

(define specialist-frontend-service
  (service
   specialist-frontend-service-type
   (list (plek-config) (rails-app-config) specialist-frontend)))

;;;
;;; Router
;;;

(define-record-type* <router-config>
  router-config make-router-config
  router-config?
  (public-port router-config-public-port
               (default 8080))
  (api-port router-config-api-port
            (default 8081))
  (debug? router-config-debug
          (default #f)))

(define router-config->environment-variables
  (match-lambda
    (($ <router-config> public-port api-port debug?)
     (append
      (list
       (cons "ROUTER_PUBADDR" (simple-format #f ":~A" public-port))
       (cons "ROUTER_APIADDR" (simple-format #f ":~A" api-port)))
      (if debug?
          (list (cons "DEBUG" "true"))
          '())))))

(define (make-router-start-script environment-variables package . rest)
  (let*
      ((database-connection-configs
        (filter database-connection-config? rest))
       (environment-variables
        (map
         (match-lambda
           ((name . value)
            (cond
             ((equal? name "MONGO_DB")
              (cons "ROUTER_MONGO_DB" value))
             ((equal? name "MONGODB_URI")
              (cons "ROUTER_MONGO_URL" value))
             (else
              (cons name value)))))
         (append
          environment-variables
          (concatenate
           (map database-connection-config->environment-variables
                database-connection-configs))))))
    (program-file
     (string-append "start-router")
     (with-imported-modules '((guix build utils)
                              (ice-9 popen))
       #~(let ((user (getpwnam "nobody")))
           (use-modules (guix build utils)
                        (ice-9 popen))

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))

           (display "\n")
           (for-each
            (lambda (env-var)
              (simple-format
               #t
               "export ~A=~A\n"
               (car env-var)
               (cdr env-var))
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)
           (display "\n")

           (chdir #$package)
           (and
            (zero? (system* (string-append #$package "/bin/router")))))))))

(define (make-router-shepherd-service name)
  (match-lambda
    ((router-config package rest ...)
     (let* ((start-script
             (apply
              make-router-start-script
              (router-config->environment-variables router-config)
              package
              rest)))
       (list
        (shepherd-service
         (provision (list name))
         (documentation (symbol->string name))
         (requirement '())
         (respawn? #f)
         (start #~(make-forkexec-constructor #$start-script))
         (stop #~(make-kill-destructor))))))))

(define (make-router-service-type name)
  (service-type
   (name name)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (make-router-shepherd-service name))))))

(define default-router-database-connection-configs
  (list
   (mongodb-connection-config
    (user "router")
    (password (random-base16-string 30))
    (database "router"))))

(define router-service-type
  (make-router-service-type 'router))

(define router-service
  (service
   router-service-type
   (cons* (router-config) router
          default-router-database-connection-configs)))

(define default-draft-router-database-connection-configs
  (list
   (mongodb-connection-config
    (user "draft-router")
    (password (random-base16-string 30))
    (database "draft-router"))))

(define draft-router-service-type
  (make-router-service-type 'draft-router))

(define draft-router-service
  (service
   draft-router-service-type
   (cons* (router-config) router
          default-draft-router-database-connection-configs)))

;;;
;;; Router API
;;;

(define router-api-service-type
  (make-rails-app-using-signon-service-type
   'router-api
   #:requirements '(router)))

(define router-api-service
  (service
   router-api-service-type
   (cons* (plek-config) (rails-app-config) router-api
          (router-api-config)
          default-router-database-connection-configs)))

(define draft-router-api-service-type
  (make-rails-app-using-signon-service-type
   'draft-router-api
   #:requirements '(draft-router)))

(define draft-router-api-service
  (service
   draft-router-api-service-type
   (cons* (plek-config) (rails-app-config) router-api
          (router-api-config)
          default-draft-router-database-connection-configs)))

;;;
;;; Maslow
;;;

(define default-maslow-database-connection-configs
  (list
   (mongodb-connection-config
    (user "maslow")
    (password (random-base16-string 30))
    (database "maslow"))))

(define maslow-service-type
  (make-rails-app-using-signon-service-type
   'maslow
   #:requirements '(publishing-api)))

(define maslow-service
  (service
   maslow-service-type
   (cons* (plek-config) (rails-app-config) maslow
          default-maslow-database-connection-configs)))

;;;
;;; Need API
;;;

(define default-need-api-database-connection-configs
  (list
   (mongodb-connection-config
    (user "need-api")
    (password (random-base16-string 30))
    (database "govuk_needs_development"))))

(define need-api-service-type
  (make-rails-app-using-signon-service-type
   'need-api
   #:requirements '(publishing-api)))

(define need-api-service
  (service
   need-api-service-type
   (cons* (plek-config) (rails-app-config) need-api
          default-need-api-database-connection-configs)))

;;;
;;; Static service
;;;

(define static-service-type
  (make-rails-app-service-type 'static))

(define static-service
  (service
   static-service-type
   (list (plek-config) (rails-app-config) static)))
