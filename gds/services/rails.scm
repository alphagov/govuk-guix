(define-module (gds services rails)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gds services)
  #:use-module (gds services sidekiq)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:export (<rails-app-config>
            rails-app-config
            rails-app-config?
            rails-app-config-environment
            rails-app-config-secret-key-base
            rails-app-config-secret-token

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

            rails-run-db:setup))

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

(define (app-name->root-directory name)
  (string-append "/var/apps/" name))

;;;
;;; Generic Rails App Service
;;;

(define (generic-rails-app-service-environment-variables
         root-directory . parameters)
  `(("PATH" . ,(simple-format #f "~A/bin" root-directory))
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

(define (run-pre-startup-scripts-gexp pre-startup-scripts)
  (let
      ((script-gexps
        (map
         (match-lambda
           ((key . script)
            #~(lambda ()
                (simple-format #t "Running pre-startup-script ~A\n" '#$key)
                (let
                    ((result
                      (catch
                        #t
                        #$script
                        (lambda (key . args) (cons key args)))))
                  (or (eq? result #t)
                      (begin
                        (simple-format #t "pre-startup-script ~A failed\n" '#$key)
                        (if (list? result)
                            (simple-format #t "result: ~A\n" result))
                        #f))))))
         pre-startup-scripts)))
    #~(let run ((scripts (list #$@script-gexps)))
        (if (null? scripts)
            #t
            (let
                ((result ((car scripts))))
              (if (eq? result #t)
                  (run (cdr scripts))
                  #f))))))

(define (generic-rails-app-start-script
         name
         .
         rest)
  (let*
      ((rails-app-config (find rails-app-config? rest))
       (package (find package? rest))
       (string-name (symbol->string name))
       (string-port
        (number->string (rails-app-config-port rails-app-config)))
       (root-directory
        (app-name->root-directory string-name))
       (service-startup-config
        (find service-startup-config? rest))
       (run-pre-startup-scripts-program
        (if (null?
             (service-startup-config-pre-startup-scripts
              service-startup-config))
            #f
            (program-file
             (string-append "start-" string-name "-pre-startup-scripts")
             (let
                 ((foo
                   (run-pre-startup-scripts-gexp
                    (service-startup-config-pre-startup-scripts
                     service-startup-config))))
               #~(begin
                   (exit #$foo))))))
       (run-root-pre-startup-scripts
        (run-pre-startup-scripts-gexp
         (if service-startup-config
             (service-startup-config-root-pre-startup-scripts
              service-startup-config)
             '())))
       (database-connection-configs
        (filter database-connection-config? rest))
       (run-rake-db-setup?
        (not (null? database-connection-configs)))
       (environment-variables
        (map
         (match-lambda
           ((key . value)
            (string-append key "=" value)))
         (apply
          generic-rails-app-service-environment-variables
          root-directory
          rails-app-config
          rest))))
    #~(lambda args
        (let ((user (getpwnam #$string-name))
              (rails (string-append #$root-directory "/bin/rails"))
              (pid-file (string-append
                         #$root-directory
                         "/tmp/pids/server.pid")))

          (use-modules (guix build utils)
                       (ice-9 popen))

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
                   #:environment-variables '#$environment-variables)))
              (if (zero? (cdr (waitpid pid)))
                  #t
                  (begin
                    (simple-format #t "~A: pre-startup-scripts failed\n"
                                   #$string-name)
                    #f)))))
           ((make-forkexec-constructor
             (list rails
                   "server"
                   "-p" #$string-port
                   "-P" pid-file)
             #:user (passwd:uid user)
             #:directory #$root-directory
             #:pid-file pid-file
             #:pid-file-timeout 10
             #:environment-variables '#$environment-variables))))))

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
                  (mkdir-p (string-append #$root-directory "/vendor"))
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
         .
         rest)
  (let*
      ((package (find package? rest))
       (root-directory
        (app-name->root-directory (symbol->string name)))
       (ss (find shepherd-service? rest))
       (rails-app-config (find rails-app-config? rest))
       (sidekiq-config (find sidekiq-config? rest))
       (sidekiq-service-name
        (symbol-append
         (first (shepherd-service-provision ss))
         '-sidekiq)))
    (cons
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
        (requirement
         `(,@(shepherd-service-requirement ss)
           ,@(if sidekiq-config
                 (list sidekiq-service-name)
                 '())))
        (respawn? #f)
        (start start-script)
        (stop #~(make-kill-destructor))))
     (if sidekiq-config
         (list
          (let*
              ((environment-variables
                (map
                 (match-lambda
                  ((key . value)
                   (string-append key "=" value)))
                 (apply
                  generic-rails-app-service-environment-variables
                  root-directory
                  rails-app-config
                  rest)))
               (config-file (sidekiq-config-file sidekiq-config))
               (string-name (symbol->string name))
               (root-directory (app-name->root-directory string-name))
               (pidfile (string-append
                         root-directory
                         "/tmp/pids/sidekiq.pid")))
            (shepherd-service
             (inherit ss)
             (provision (list sidekiq-service-name))
             (documentation
              (simple-format #f "~A sidekiq service" name))
             (respawn? #f)
             (start #~(make-forkexec-constructor
                       `(,(string-append #$root-directory "/bin/bundle")
                         "exec"
                         "sidekiq"
                         ,@(if #$config-file '("-C" #$config-file) '())
                         "--pidfile" #$pidfile)
                       #:user #$string-name
                       #:pid-file #$pidfile
                       #:pid-file-timeout 30
                       #:log-file (string-append
                                   "/var/log/"
                                   (symbol->string '#$sidekiq-service-name)
                                   ".log")
                       #:directory #$root-directory
                       #:environment-variables '#$environment-variables))
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

(define (make-rails-app-service-type name)
  (service-type
   (name name)
   (extensions
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
                          (symbol->string name))))))))

(define (rails-run-db:setup s)
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
                 (find service-startup-config? parameters)
                 `((rails-db:setup
                    .
                    ,(run-command "rake" "db:setup"))))
                parameter))
          parameters)))))
