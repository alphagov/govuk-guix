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
            make-rails-app-service-type))

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

;;;
;;; Generic Rails App Service
;;;

(define (generic-rails-app-service-environment-variables
         name root-directory . parameters)
  `(("PATH" . ,(simple-format #f "~A/bin" root-directory))
    ,@(let ((rails-app-config
             (find rails-app-config? parameters)))
        (if rails-app-config
            (rails-app-config->environment-variables
             rails-app-config)
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
       (package (find package? rest))
       (string-name (symbol->string name))
       (string-port
        (number->string (rails-app-config-port rails-app-config)))
       (root-directory
        (string-append "/var/lib/" string-name))
       (ruby
        (car
         (assoc-ref (package-inputs package)
                    "ruby")))
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
       (run-rake-db-setup?
        (not (null? database-connection-configs)))
       (environment-variables
        (apply
         generic-rails-app-service-environment-variables
         string-name
         root-directory
         rails-app-config
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

           (display "\n\nstarting ")(display '#$name)(display "\n\n")

           (for-each
            (lambda (env-var)
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)
           (chdir #$root-directory)

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))
           (and
            ;; Run the root-pre-startup-scripts before switching user
            (let run ((scripts
                       (list #$@run-root-pre-startup-scripts)))
              (if (null? scripts)
                  #t
                  (and ;; Stop if any script fails
                   ((car scripts))
                   (run (cdr scripts)))))
            (begin
              ;; Start the service
              (setgid (passwd:gid user))
              (setuid (passwd:uid user))
              #t)
            (let run ((scripts
                       (list #$@run-pre-startup-scripts)))
              (if (null? scripts)
                  #t
                  (and ;; Stop if any script fails
                   ((car scripts))
                   (run (cdr scripts)))))
            (zero? (system* rails "server" "--daemon" "-p" #$string-port))))))))

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
       (root-directory (string-append "/var/lib/" string-name))
       (environment-variables
        (apply
         generic-rails-app-service-environment-variables
         string-name
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
          rest)))
     (shepherd-service
      (inherit (find shepherd-service? rest))
      (provision (list name))
      (documentation
       (simple-format #f "~A rails app" name))
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
                 rest)))
            (shepherd-service
             (inherit (find shepherd-service? rest))
             (provision (list (symbol-append name '-sidekiq)))
             (documentation
              (simple-format #f "~A sidekiq service" name))
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
