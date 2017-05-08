(define-module (gds services rails)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gds services)
  #:use-module (gds services sidekiq)
  #:use-module (gds services delayed-job)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:export (<rails-app-config>
            rails-app-config
            rails-app-config?
            rails-app-config-environment
            rails-app-config-secret-key-base
            rails-app-config-secret-token
            rails-app-config-run-with

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
                (default #f))
  (run-with     rails-app-config-run-with
                (default 'unicorn)))

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
    ("HOME" . "/nonexistent")
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

(define (run-pre-startup-scripts-gexp name pre-startup-scripts)
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
                        (simple-format #t "result: ~A\n" result)
                        #f))))))
         pre-startup-scripts)))
    (if (null? script-gexps)
        #~#t
        (with-imported-modules '((gds build utils))
        #~(begin
            (use-modules (gds build utils))
            (simple-format
             #t
             "Running ~A startup scripts for ~A\n"
             #$(length script-gexps)
             '#$name)
            (for-each
             (lambda (key) (simple-format #t "  - ~A\n" key))
             '#$(map car pre-startup-scripts))
            (let run ((scripts (list #$@script-gexps)))
              (if (null? scripts)
                  #t
                  (let
                      ((result ((car scripts))))
                    (if (eq? result #t)
                        (run (cdr scripts))
                        #f)))))))))

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
        (or (find service-startup-config? rest)
            (error "Missing service-startup-config for ~A\n" name)))
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
                    name
                    (service-startup-config-pre-startup-scripts
                     service-startup-config))))
               #~(begin
                   (exit #$foo))))))
       (run-root-pre-startup-scripts
        (run-pre-startup-scripts-gexp
         name
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
          rest)))
       (pid-file
        (if (eq? (rails-app-config-run-with rails-app-config)
                 'unicorn)
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
          ((and command string)
           (list command)))))
    (with-imported-modules '((gds build utils))
    #~(lambda args
        (let ((user (getpwnam #$string-name))
              (environment-variables '#$environment-variables))

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
                   #:environment-variables environment-variables)))
              (if (zero? (cdr (waitpid pid)))
                  #t
                  (begin
                    (simple-format #t "~A: pre-startup-scripts failed\n"
                                   #$string-name)
                    #f))))
           (begin (simple-format #t "starting ~A: ~A\n"
                                 '#$name '#$start-command)
                  #t)
           ((make-forkexec-constructor
             '#$start-command
             #:user (passwd:uid user)
             #:directory #$root-directory
             #:pid-file #$pid-file
             #:pid-file-timeout 10
             #:log-file #$(string-append "/var/log/" string-name ".log")
             #:environment-variables environment-variables))))))))

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
    (with-imported-modules `((guix build syscalls)
                             (guix build bournish)
                             (gnu build file-systems))
     #~(begin
         (use-modules (guix build utils)
                      (gnu build file-systems)
                      (guix build syscalls)
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
              (bind-mount #$package #$root-directory)

              (for-each
               (lambda (file)
                 (if (file-exists? file)
                     (mount "tmpfs" file "tmpfs")))
               (map
                (lambda (dir)
                  (string-append #$root-directory "/" dir))
                '("log" "public"))))
            (begin
              (mount "tmpfs" (string-append #$root-directory "/bin") "tmpfs")
              (copy-recursively
               (string-append #$package "/bin")
               (string-append #$root-directory "/bin")
               #:log (%make-void-port "w")
               #:follow-symlinks? #f)
              (substitute* (find-files (string-append #$root-directory "/bin")
                                       (lambda (name stat)
                                         (access? name X_OK)))
                           (((string-append #$package "/bin"))
                            "${BASH_SOURCE%/*}"))
              (for-each
               (lambda (path)
                 (mkdir-p (string-append #$root-directory path))
                 (chmod (string-append #$root-directory path) #o777))
               '("/tmp" "/log"))

              (for-each
               (cut chmod <> #o666)
               (find-files (string-append #$root-directory "/log")
                           #:directories? #f))))

           (if (file-exists? (string-append #$root-directory "/tmp"))
               (mount "tmpfs" (string-append #$root-directory "/tmp") "tmpfs"))

           (let* ((dir (string-append "/tmp/env.d/"))
                  (file (string-append dir string-name)))
             (mkdir-p dir)
             (call-with-output-file file
               (lambda (port)
                 (for-each
                  (lambda (env-var)
                    (simple-format port "export ~A=\"~A\"\n" (car env-var) (cdr env-var)))
                  '#$environment-variables)))))))))

(define (generic-rails-app-shepherd-services
         name
         .
         rest)
  (let*
      ((package (find package? rest))
       (root-directory
        (app-name->root-directory (symbol->string name)))
       (ss (or (find shepherd-service? rest)
               (error "Missing shepherd service for ~A\n" name)))
       (rails-app-config (find rails-app-config? rest))
       (sidekiq-config (find sidekiq-config? rest))
       (sidekiq-service-name
        (symbol-append
         (first (shepherd-service-provision ss))
         '-sidekiq)))
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
      (if sidekiq-config
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
                         "/tmp/" (symbol->string sidekiq-service-name) ".pid"))
               (start-command
                `(,(string-append root-directory "/bin/bundle")
                  "exec"
                  "sidekiq"
                  ,@(if config-file `("-C" ,config-file) '())
                  "--pidfile" ,pidfile)))
            (shepherd-service
             (inherit ss)
             (provision (list sidekiq-service-name))
             (documentation
              (simple-format #f "~A sidekiq service" name))
             (requirement
              `(,@(shepherd-service-requirement ss)
                ,(first (shepherd-service-provision ss))))
             (respawn? #f)
             (start #~(lambda args
                        (display
                         #$(simple-format #f "starting ~A sidekiq service: ~A\n"
                                          name (string-join start-command)))
                        (apply
                         #$#~(make-forkexec-constructor
                              '#$start-command
                              #:user #$string-name
                              #:pid-file #$pidfile
                              #:pid-file-timeout 5
                              #:log-file #$(string-append
                                            "/var/log/"
                                            (symbol->string sidekiq-service-name)
                                            ".log")
                              #:directory #$root-directory
                              #:environment-variables '#$environment-variables)
                         args)))
             (stop #~(make-kill-destructor))))
          #f)))))

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
                    ,(with-imported-modules '((gds build utils))
                      #~(lambda ()
                          (use-modules (gds build utils))
                          (if (and (file-exists? "bin/rake")
                                   ;; When spring is used, rake seems
                                   ;; to need to be run with bundle
                                   ;; exec
                                   (not (file-exists? "bin/spring")))
                              (run-command "rake" "db:setup")
                              (run-command "bundle" "exec" "rake" "db:setup")))))))
                parameter))
          parameters)))))
