(define-module (gds services rails)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gds data tar-archive)
  #:use-module (gds services)
  #:use-module (gds services sidekiq)
  #:use-module (gds services delayed-job)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:export (<rails-app-config>
            rails-app-config
            rails-app-config?
            rails-app-config-environment
            rails-app-config-secret-key-base
            rails-app-config-secret-token
            rails-app-config-run-with
            rails-app-config-assets?
            rails-app-config-read-bundle-install-input-as-tar-archive?

            update-rails-app-config-environment
            update-rails-app-config-with-random-secret-key-base
            update-rails-app-config-with-random-secret-token

            rails-app-config->environment-variables

            generic-rails-app-service-environment-variables
            generic-rails-app-start-script
            generic-rails-app-activation
            generic-rails-app-shepherd-services
            generic-rails-app-service-account
            make-rails-app-service-type

            rails-run-db:setup
            update-rails-app-config-environment-for-service
            run-db:setup-if-postgresql-or-mysql-is-used
            update-rails-app-config-with-random-secret-key-base-for-services
            update-rails-app-set-read-bundle-install-input-as-tar-archive

            standard-rails-service-type-extensions))

(define-record-type* <rails-app-config>
  rails-app-config make-rails-app-config
  rails-app-config?
  (port        rails-app-config-port
               (default #f))
  (environment rails-app-config-environment
               (default "production"))
  (secret-key-base rails-app-config-secret-key-base
                   (default #f))
  (secret-token rails-app-config-secret-token
                (default #f))
  (run-with     rails-app-config-run-with
                (default 'unicorn))
  (assets?      rails-app-config-assets?
                (default #t))
  (precompiled-assets-are-environment-specific?
   rails-app-config-precompiled-assets-are-environment-specific?
   (default #t))
  (read-bundle-install-input-as-tar-archive?
   rails-app-config-read-bundle-install-input-as-tar-archive?
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
    (cons "SSL_CERT_FILE" "/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
    (cons "RAILS_ENV" (rails-app-config-environment config))
    (cons "SECRET_KEY_BASE" (rails-app-config-secret-key-base config))
    (cons "SECRET_TOKEN" (rails-app-config-secret-token config)))))

(define (app-name->root-directory name)
  (string-append "/var/apps/" name))

(define (tweak-package-with-environment pkg parameters)
  (let ((environment-variables
         (cons
          (cons "RAILS_ENV" (rails-app-config-environment
                             (find rails-app-config? parameters)))
          ;; TODO: This isn't great, as this module is mostly generic,
          ;; and not GOV.UK specific, apart from this. Just using the
          ;; service-startup-config means that the secrets get used as
          ;; well, which is worse.
          (filter (match-lambda
                    ((name . value)
                     (or (string-prefix? "PLEK_" name)
                         (string-prefix? "GOVUK_" name))))
                  (service-startup-config-environment-variables
                   (find service-startup-config? parameters))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'precompile-rails-assets 'set-environment
               (lambda _
                 (simple-format
                  #t "Setting environment variables in the ~A package from the service configuration, as the assets contained within this package are environment specific:\n" ,(package-name pkg))
                 (for-each
                  (lambda (var)
                    (let ((key (car var))
                          (value (cdr var)))
                      (simple-format #t "  ~A=~A\n" key value)
                      (setenv key value)))
                  ',environment-variables)
                 #t)))))))))

(define (package-from-parameters parameters)
  (let ((rails-app-config (find rails-app-config?
                                parameters))
        (pkg (find package?
                   parameters)))
    (if (and rails-app-config
             (rails-app-config-assets? rails-app-config)
             (rails-app-config-precompiled-assets-are-environment-specific?
              rails-app-config))
        (tweak-package-with-environment pkg parameters)
        pkg)))

;;;
;;; Generic Rails App Service
;;;

(define (generic-rails-app-service-environment-variables
         root-directory . parameters)
  `(("PATH" . ,(simple-format #f "~A/bin" root-directory))
    ("HOME" . ,root-directory)
    ,@(let ((rails-app-config
             (find rails-app-config? parameters)))
        (if rails-app-config
            (rails-app-config->environment-variables
             rails-app-config)
            '()))
    ,@(let ((service-startup-config
             (find service-startup-config? parameters)))
        (if service-startup-config
            (service-startup-config-environment-variables
             service-startup-config)
            '()))
    ,@(concatenate
       (map
        database-connection-config->environment-variables
        (filter
         database-connection-config?
         parameters)))))

(define (generic-rails-app-start-script
         name
         .
         rest)
  (let*
      ((rails-app-config (find rails-app-config? rest))
       (package (package-from-parameters rest))
       (string-name (symbol->string name))
       (string-port
        (number->string (rails-app-config-port rails-app-config)))
       (root-directory
        (app-name->root-directory string-name))
       (service-startup-config
        (or (find service-startup-config? rest)
            (error "Missing service-startup-config for ~A\n" name)))
       (run-pre-startup-scripts-program
        (if (null?
             (service-startup-config-pre-startup-scripts
              service-startup-config))
            #f
            (program-file
             (string-append "start-" string-name "-pre-startup-scripts")
             #~(exit #$(run-pre-startup-scripts-gexp
                        name
                        (service-startup-config-pre-startup-scripts
                         service-startup-config))))))
       (run-root-pre-startup-scripts
        (run-pre-startup-scripts-gexp
         name
         (if service-startup-config
             (service-startup-config-root-pre-startup-scripts
              service-startup-config)
             '())
         #:home "root"))
       (database-connection-configs
        (filter database-connection-config? rest))
       (environment-variables
        (map
         (match-lambda
           ((key . value)
            (string-append key "=" value)))
         (apply
          generic-rails-app-service-environment-variables
          root-directory
          rails-app-config
          rest)))
       (pid-file
        (if (memq (rails-app-config-run-with rails-app-config)
                  '(unicorn rails))
            (string-append "/tmp/" string-name ".pid")
            #f))
       (start-command
        (match (rails-app-config-run-with rails-app-config)
          ('unicorn
           (list
            (string-append root-directory "/bin/bundle")
            "exec"
            "unicorn"
            "-p" string-port
            "-P" pid-file))
          ('rails
           (list
            "bin/rails"
            "server"
            "-p" string-port
            "-P" pid-file))
          ((and command string)
           (list command)))))
    (with-imported-modules '((gds build utils))
    #~(lambda args
        (let ((user (getpwnam #$string-name))
              (environment-variables '#$environment-variables))

          (use-modules (guix build utils))

          (and
           #$run-root-pre-startup-scripts
           (or
            (eq? #$run-pre-startup-scripts-program #f)
            (let
                ((pid
                  (fork+exec-command
                   (list #$run-pre-startup-scripts-program)
                   #:user (passwd:uid user)
                   #:directory #$root-directory
                   #:environment-variables environment-variables)))
              (let ((exit-code (cdr (waitpid pid))))
                (if (eq? exit-code 0)
                    #t
                    (begin
                      (simple-format
                       #t "~A: pre-startup-scripts failed (exit-code ~A)\n"
                       #$string-name
                       exit-code)
                      #f)))))
           (begin (simple-format #t "starting ~A: ~A\n"
                                 '#$name '#$start-command)
                  #t)
           ((make-forkexec-constructor
             '#$start-command
             #:user (passwd:uid user)
             #:directory #$root-directory
             #:pid-file #$pid-file
             #:pid-file-timeout 60
             #:log-file #$(string-append "/var/log/" string-name ".log")
             #:environment-variables environment-variables))))))))

(define (gemrc ruby)
  (mixed-text-file "gemrc"
                   "custom_shebang: " ruby "/bin/ruby\n"))

(define (read-bundle-install-input-as-tar-archive package)
  (define bundle-install-input
    (any (match-lambda
           ((name package rest ...)
            (if (string=? name "bundle-install")
                package
                #f)))
         (package-inputs package)))

  (with-imported-modules (source-module-closure
                          '((guix build syscalls)
                            (gnu build file-systems)))
    #~(let* ((cache-directory "/var/cache/gems")
             (bundle-install #$bundle-install-input)
             (bundle-install-cache-directory
              (string-append cache-directory
                             (string-drop bundle-install
                                          #$(string-length (%store-prefix))))))
        (use-modules (gnu build file-systems))

        (mkdir-p cache-directory)
        (unless (file-exists? bundle-install-cache-directory)
          (simple-format #t "Caching ~A ...\n" bundle-install)
          (mkdir bundle-install-cache-directory)
          (system* #$(file-append tar "/bin/tar")
                   "--extract"
                   "--file" #$(tar-archive
                               (name (string-append (package-name package)
                                                    "-bundle-install.tar"))
                               (contents bundle-install-input))
                   (string-append "--directory=" bundle-install-cache-directory))
          (bind-mount bundle-install-cache-directory bundle-install)
          (simple-format #t "Finished caching ~A\n" bundle-install)))))

(define (generic-rails-app-activation
         name
         .
         rest)
  (let*
      ((rails-app-config (find rails-app-config? rest))
       (package (package-from-parameters rest))
       (string-name (symbol->string name))
       (root-directory (app-name->root-directory string-name))
       (environment-variables
        (apply
         generic-rails-app-service-environment-variables
         root-directory
         rails-app-config
         rest)))

    #~(begin
        #$@(if (rails-app-config-read-bundle-install-input-as-tar-archive?
                rails-app-config)
               (list (read-bundle-install-input-as-tar-archive package))
               '())

        #$(setup-app-directory string-name package)

        (let* ((dir "/tmp/env.d/")
               (file (string-append dir #$string-name)))
          (mkdir-p dir)
          (call-with-output-file file
            (lambda (port)
              (for-each
               (lambda (env-var)
                 (simple-format port "export ~A=\"~A\"\n"
                                (car env-var)
                                (cdr env-var)))
               '#$environment-variables)))))))

(define (generic-rails-app-shepherd-services
         name
         .
         rest)
  (let*
      ((package (package-from-parameters rest))
       (root-directory
        (app-name->root-directory (symbol->string name)))
       (ss (or (find shepherd-service? rest)
               (error "Missing shepherd service for ~A\n" name)))
       (rails-app-config (find rails-app-config? rest)))
    (delete
     #f
     (list
      (let
          ((start-script
            (apply
             generic-rails-app-start-script
             name
             package
             rails-app-config
             rest)))
        (shepherd-service
         (inherit ss)
         (documentation
          (simple-format #f "~A rails app" name))
         (respawn? #f)
         (start start-script)
         (stop #~(make-kill-destructor))))
      (and=> (find delayed-job-config? rest)
             (lambda (delayed-job-config)
               (delayed-job-worker-shepherd-service
                (simple-format #f "~A-delayed-job-worker" name)
                delayed-job-config
                (shepherd-service-requirement ss)
                root-directory
                (symbol->string name)
                (apply generic-rails-app-service-environment-variables
                       root-directory
                       rails-app-config
                       rest))))
      (and=> (find sidekiq-config? rest)
             (lambda (sidekiq-config)
               (sidekiq-shepherd-service
                (simple-format #f "~A-sidekiq"
                               (first (shepherd-service-provision ss)))
                sidekiq-config
                (cons* (first (shepherd-service-provision ss))
                       (shepherd-service-requirement ss))
                (app-name->root-directory (symbol->string name))
                (symbol->string name)
                (apply generic-rails-app-service-environment-variables
                       root-directory
                       rails-app-config
                       rest))))))))

(define (generic-rails-app-service-account
         username)
  (list
   (user-account
    (name username)
    (group "nogroup")
    (system? #t)
    (home-directory "/var/empty")
    (create-home-directory? #f)
    (shell #~(string-append #$shadow "/sbin/nologin")))))

(define (standard-rails-service-type-extensions name)
  (list
   (service-extension shepherd-root-service-type
                      (lambda (parameters)
                        (apply
                         generic-rails-app-shepherd-services
                         name
                         parameters)))
   (service-extension activation-service-type
                      (lambda (parameters)
                        (apply
                         generic-rails-app-activation
                         name
                         parameters)))
   (service-extension account-service-type
                      (const
                       (generic-rails-app-service-account
                        (symbol->string name))))))

(define (rails-run-db:setup s)
  (define (rails-setup-or-migrate parameters)
    (let* ((postgresql-or-mysql-connection-config
            (find (lambda (parameter)
                    (or (postgresql-connection-config? parameter)
                        (mysql-connection-config? parameter)))
                  parameters))
           (database-already-exists?
            (match postgresql-or-mysql-connection-config
                   (#f
                    #~#f)
                   (($ <postgresql-connection-config> host user port database)
                    #~(member
                       #$database
                       (#$(postgresql-list-databases-gexp
                           postgresql-or-mysql-connection-config))))
                   (($ <mysql-connection-config> host user port database)
                    #~(member
                       #$database
                       (#$(mysql-list-databases-gexp
                           (mysql-connection-config
                            (inherit postgresql-or-mysql-connection-config)
                            (user "root")
                            (password ""))))))
                   (_ #~#f))))
      (with-imported-modules '((gds build utils))
        #~(lambda ()
            (use-modules (gds build utils))
            (let ((run-task
                   (lambda args
                     (cond
                      ((and (file-exists? "bin/rake")
                            ;; When spring is used, rake seems
                            ;; to need to be run with bundle
                            ;; exec
                            (not (file-exists? "bin/spring")))
                       (apply run-command "rake" args))
                      ((file-exists? "bin/bundle")
                       (apply run-command "bundle" "exec" "rake" args))
                      (else
                       (apply run-command "bin/rails" args))))))
              (if #$database-already-exists?
                  (let ((schema-value (getenv "SCHEMA"))
                        (result
                         (begin
                           ;; Trick rails in to writing to
                           ;; /dev/null, rather than the
                           ;; schema that could be readonly
                           (setenv "SCHEMA" "tmp/schema")
                           (run-task "db:migrate"))))
                    (setenv "SCHEMA" schema-value)
                    result)
                  (run-task "db:setup")))))))

  (let
      ((parameters (service-parameters s)))
    (if (not (list? parameters))
        s
        (service
         (service-kind s)
         (map
          (lambda (parameter)
            (if (service-startup-config? parameter)
                (service-startup-config-add-pre-startup-scripts
                 parameter
                 `((rails-db:setup
                    .
                    ,(rails-setup-or-migrate parameters))))
                parameter))
          parameters)))))

(define (update-rails-app-config-with-random-secret-key-base-for-services
         services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        rails-app-config?
        update-rails-app-config-with-random-secret-key-base))))
   services))

(define (update-rails-app-config-environment-for-service environment service)
  (update-service-parameters
   service
   (list
    (cons
     rails-app-config?
     (lambda (config)
       (update-rails-app-config-environment
        environment
        (update-rails-app-config-with-random-secret-key-base config)))))))

(define (update-rails-app-set-read-bundle-install-input-as-tar-archive
         bundle-install-input-as-tar-archive? services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        rails-app-config?
        (lambda (config)
          (rails-app-config
           (inherit config)
           (read-bundle-install-input-as-tar-archive?
            bundle-install-input-as-tar-archive?)))))))
   services))

(define (run-db:setup-if-postgresql-or-mysql-is-used service)
  (if (and
       (list? (service-parameters service))
       (find rails-app-config? (service-parameters service))
       (any
        (lambda (parameter)
          (or (postgresql-connection-config? parameter)
              (mysql-connection-config? parameter)))
        (service-parameters service)))
      (rails-run-db:setup service)
      service))
