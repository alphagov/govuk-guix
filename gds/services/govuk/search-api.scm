(define-module (gds services govuk search-api)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases elasticsearch)
  #:use-module (gds services utils databases rabbitmq)
  #:use-module (gds services sidekiq)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk signon)
  #:export (search-api-service-type))

(define (queue-listener-shepherd-services package environment-variables)
  (map
   (match-lambda
     ((service task documentation)
      (shepherd-service (documentation documentation)
                        (provision (list service))
                        (requirement '(redis rabbitmq))
                        (respawn? #f)
                        (start #~(make-forkexec-constructor
                                  '("/var/apps/search-api/bin/bundle"
                                    "exec"
                                    "rake"
                                    #$(string-append "message_queue:" task))
                                  #:user (passwd:uid (getpwnam "search-api"))
                                  #:directory "/var/apps/search-api"
                                  #:log-file #$(string-append
                                                "/var/log/"
                                                (symbol->string service)
                                                ".log")
                                  #:environment-variables
                                  (list #$@environment-variables)))

                        (stop #~(make-kill-destructor)))))

   '((search-api-publishing-queue-listener
      "listen_to_publishing_queue"
      "Search API publishing queue listener")

     (search-api-publishing-api-queue-listener
      "insert_data_into_govuk"
      "Search API Publishing API queue listener")

     (search-api-bulk-reindex-queue-listener
      "bulk_insert_data_into_govuk"
      "Search API bulk-reindex queue listener"))))

(define search-api-govuk-dependencies
  '(publishing-api
    signon))

(define (environment-variable-pairs->strings environment-variables)
  (map
   (match-lambda
     ((key . value)
      #~(string-append #$key "=" #$value)))
   environment-variables))

(define (search-api-environment service-startup-config
                              plek-config
                              package
                              database-connection-configs)
  (append (service-startup-config-environment-variables
           service-startup-config)

          (plek-config->environment-variables
           plek-config
           #:service-name-whitelist search-api-govuk-dependencies)

          `(("PATH" . ,(file-append package "/bin"))
            ;; To satisfy pry-byebug
            ("HOME" . "/var/apps/search-api")
            ("SSL_CERT_FILE" .
             "/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"))

          (append-map database-connection-config->environment-variables
                      database-connection-configs)))

(define (search-api-web-start-gexp environment-variables
                                 plek-config
                                 package
                                 run-pre-startup-scripts-program
                                 run-root-pre-startup-scripts)
  (let* ((pid-file "/tmp/search-api.pid")
         (start-command
          `("/var/apps/search-api/bin/bundle"
            "exec"
            "unicorn"
            "-p" ,(number->string
                   (service-port-from-plek-config plek-config 'search-api))
            "-P" ,pid-file)))

    #~(lambda args
        (let ((environment-variables
               (list #$@environment-variables))
              (start-command '#$start-command))

          (use-modules (guix build utils))

          (and #$run-root-pre-startup-scripts
               (or
                (eq? #$run-pre-startup-scripts-program #f)
                (let
                    ((pid
                      (fork+exec-command
                       (list #$run-pre-startup-scripts-program)
                       #:user (passwd:uid (getpwnam "search-api"))
                       #:directory "/var/apps/search-api"
                       #:environment-variables environment-variables)))
                  (if (zero? (cdr (waitpid pid)))
                      #t
                      (begin
                        (display "search-api: pre-startup-scripts failed\n")
                        #f))))
               (begin (simple-format #t "starting search-api: ~A\n"
                                     start-command)
                      #t)

               ((make-forkexec-constructor
                 start-command
                 #:user (passwd:uid (getpwnam "search-api"))
                 #:directory "/var/apps/search-api"
                 #:pid-file #$pid-file
                 #:pid-file-timeout 10
                 #:log-file "/var/log/search-api.log"
                 #:environment-variables environment-variables)))))))

(define (search-api-shepherd-services parameters)
  (let* ((service-startup-config
          (or (find service-startup-config? parameters)
              (error "Missing service-startup-config for search-api")))
         (run-pre-startup-scripts-program
          (if (null?
               (service-startup-config-pre-startup-scripts
                service-startup-config))
              #f
              (program-file
               (string-append "start-search-api-pre-startup-scripts")
               #~(exit #$(run-pre-startup-scripts-gexp
                          'search-api
                          (service-startup-config-pre-startup-scripts
                           service-startup-config))))))
         (run-root-pre-startup-scripts
          (run-pre-startup-scripts-gexp
           'search-api
           (service-startup-config-root-pre-startup-scripts
            service-startup-config)
           #:home "/root"))
         (plek-config (find plek-config? parameters))
         (package (find package? parameters))
         (environment-variables (environment-variable-pairs->strings
                                 (search-api-environment
                                  service-startup-config
                                  plek-config
                                  package
                                  (filter database-connection-config?
                                          parameters)))))

    (cons*
     (shepherd-service (documentation "Search API web service")
                       (provision '(search-api))
                       (requirement '(elasticsearch
                                      signon))
                       (respawn? #f)
                       (start (search-api-web-start-gexp
                               environment-variables
                               plek-config
                               package
                               run-pre-startup-scripts-program
                               run-root-pre-startup-scripts))
                       (stop #~(make-kill-destructor)))

     (shepherd-service (documentation "Search API Sidekiq workers")
                       (provision '(search-api-sidekiq))
                       (requirement (cons* 'redis 'elasticsearch
                                           search-api-govuk-dependencies))
                       (respawn? #f)
                       (start (let* ((sidekiq-config (find sidekiq-config?
                                                           parameters))
                                     (config-file (sidekiq-config-file
                                                   sidekiq-config))
                                     (pid-file "/tmp/search-api-sidekiq.pid"))
                                #~(make-forkexec-constructor
                                   '("/var/apps/search-api/bin/bundle"
                                     "exec"
                                     "sidekiq"
                                     #$@(if config-file `("-C" ,config-file) '())
                                     "--pidfile" #$pid-file)
                                   #:user (passwd:uid (getpwnam "search-api"))
                                   #:directory "/var/apps/search-api"
                                   #:pid-file #$pid-file
                                   #:pid-file-timeout 5
                                   #:log-file "/var/log/search-api-sidekiq.log"
                                   #:environment-variables
                                   (list #$@environment-variables))))
                       (stop #~(make-kill-destructor)))

     (queue-listener-shepherd-services package
                                       environment-variables))))

(define (search-api-activation parameters)
  (define package (find package? parameters))

  (define environment-variables
    (search-api-environment (find service-startup-config?
                                parameters)
                          (find plek-config? parameters)
                          package
                          (filter database-connection-config?
                                  parameters)))

  #~(let* ((dir (string-append "/tmp/env.d/"))
           (file (string-append dir "search-api")))

      #$(setup-app-directory "search-api" package)

      (mkdir-p dir)
      (call-with-output-file file
        (lambda (port)
          (for-each
           (lambda (string) (display string port))
           (list #$@(map (lambda (pair)
                           #~(string-append "export "
                                            #$(car pair)
                                            "=\""
                                            #$(cdr pair)
                                            "\"\n"))
                         environment-variables)))))))

(define search-api-accounts
  (const
   (list (user-account (name "search-api")
                       (group "nogroup")
                       (system? #t)
                       (home-directory "/var/empty")
                       (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define search-api-service-type
  (service-type
   (name 'search-api)
   (extensions
    (modify-service-extensions-for-signon
     name
     (list
      (service-extension shepherd-root-service-type
                         search-api-shepherd-services)
      (service-extension activation-service-type
                         search-api-activation)
      (service-extension account-service-type
                         search-api-accounts))))
   (default-value
     (list (service-startup-config-add-pre-startup-scripts
            (service-startup-config)
            `((create-queues
               . ,#~(lambda ()
                      (run-command
                       "bundle" "exec"
                       "rake" "message_queue:create_queues")))
              (publish-special-routes
               . ,#~(lambda ()
                      (run-command
                       "bundle" "exec"
                       "rake" "publishing_api:publish_special_routes")))
              (publish-supergroup-finders
               . ,#~(lambda ()
                      (run-command
                       "bundle" "exec"
                       "rake" "publishing_api:publish_supergroup_finders")))))
           (redis-connection-config)
           (signon-application
            (name "Search API")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Search API")
            (email "search-api@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config)
           (sidekiq-config (file "config/sidekiq.yml"))
           (elasticsearch-connection-config)
           (rabbitmq-connection-config (user "search-api")
                                       (password "search-api"))
           search-api))))
