(define-module (gds services govuk rummager)
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
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services sidekiq)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk signon)
  #:export (rummager-service-type))

(define (queue-listener-shepherd-services package environment-variables)
  (map
   (match-lambda
     ((service task documentation)
      (shepherd-service (documentation documentation)
                        (provision (list service))
                        (requirement '(redis rabbitmq))
                        (respawn? #f)
                        (start #~(make-forkexec-constructor
                                  '("/var/apps/rummager/bin/bundle"
                                    "exec"
                                    "rake"
                                    #$(string-append "message_queue:" task))
                                  #:user (passwd:uid (getpwnam "rummager"))
                                  #:directory "/var/apps/rummager"
                                  #:log-file #$(string-append
                                                "/var/log/"
                                                (symbol->string service)
                                                ".log")
                                  #:environment-variables
                                  (list #$@environment-variables)))

                        (stop #~(make-kill-destructor)))))

   '((rummager-publishing-queue-listener
      "listen_to_publishing_queue"
      "Rummager publishing queue listener")

     (rummager-publishing-api-queue-listener
      "insert_data_into_govuk"
      "Rummager Publishing API queue listener")

     (rummager-bulk-reindex-queue-listener
      "bulk_insert_data_into_govuk"
      "Rummager bulk-reindex queue listener"))))

(define rummager-govuk-dependencies
  '(publishing-api))

(define (environment-variable-pairs->strings environment-variables)
  (map
   (match-lambda
     ((key . value)
      #~(string-append #$key "=" #$value)))
   environment-variables))

(define (rummager-environment service-startup-config
                              plek-config
                              package
                              database-connection-configs)
  (append (service-startup-config-environment-variables
           service-startup-config)

          (plek-config->environment-variables
           plek-config
           #:service-name-whitelist rummager-govuk-dependencies)

          `(("PATH" . ,(file-append package "/bin")))

          (append-map database-connection-config->environment-variables
                      database-connection-configs)))

(define (rummager-web-start-gexp environment-variables
                                 plek-config
                                 package
                                 run-pre-startup-scripts-program
                                 run-root-pre-startup-scripts)
  (let* ((pid-file "/tmp/rummager.pid")
         (start-command
          `("/var/apps/rummager/bin/bundle"
            "exec"
            "unicorn"
            "-p" ,(number->string
                   (service-port-from-plek-config plek-config 'rummager))
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
                       #:user (passwd:uid (getpwnam "rummager"))
                       #:directory "/var/apps/rummager"
                       #:environment-variables environment-variables)))
                  (if (zero? (cdr (waitpid pid)))
                      #t
                      (begin
                        (display "rummager: pre-startup-scripts failed\n")
                        #f))))
               (begin (simple-format #t "starting rummager: ~A\n"
                                     start-command)
                      #t)

               ((make-forkexec-constructor
                 start-command
                 #:user (passwd:uid (getpwnam "rummager"))
                 #:directory "/var/apps/rummager"
                 #:pid-file #$pid-file
                 #:pid-file-timeout 10
                 #:log-file "/var/log/rummager.log"
                 #:environment-variables environment-variables)))))))

(define (rummager-shepherd-services parameters)
  (let* ((service-startup-config
          (or (find service-startup-config? parameters)
              (error "Missing service-startup-config for rummager")))
         (run-pre-startup-scripts-program
          (if (null?
               (service-startup-config-pre-startup-scripts
                service-startup-config))
              #f
              (program-file
               (string-append "start-rummager-pre-startup-scripts")
               #~(exit #$(run-pre-startup-scripts-gexp
                          'rummager
                          (service-startup-config-pre-startup-scripts
                           service-startup-config))))))
         (run-root-pre-startup-scripts
          (run-pre-startup-scripts-gexp
           'rummager
           (service-startup-config-root-pre-startup-scripts
            service-startup-config)))
         (plek-config (find plek-config? parameters))
         (package (find package? parameters))
         (environment-variables (environment-variable-pairs->strings
                                 (rummager-environment
                                  service-startup-config
                                  plek-config
                                  package
                                  (filter database-connection-config?
                                          parameters)))))

    (cons*
     (shepherd-service (documentation "Rummager web service")
                       (provision '(rummager))
                       (requirement '(elasticsearch))
                       (respawn? #f)
                       (start (rummager-web-start-gexp
                               environment-variables
                               plek-config
                               package
                               run-pre-startup-scripts-program
                               run-root-pre-startup-scripts))
                       (stop #~(make-kill-destructor)))

     (shepherd-service (documentation "Rummager Sidekiq workers")
                       (provision '(rummager-sidekiq))
                       (requirement (cons* 'redis 'elasticsearch
                                           rummager-govuk-dependencies))
                       (respawn? #f)
                       (start (let* ((sidekiq-config (find sidekiq-config?
                                                           parameters))
                                     (config-file (sidekiq-config-file
                                                   sidekiq-config))
                                     (pid-file "/tmp/rummager-sidekiq.pid"))
                                #~(make-forkexec-constructor
                                   '("/var/apps/rummager/bin/bundle"
                                     "exec"
                                     "sidekiq"
                                     #$@(if config-file `("-C" ,config-file) '())
                                     "--pidfile" #$pid-file)
                                   #:user (passwd:uid (getpwnam "rummager"))
                                   #:directory "/var/apps/rummager"
                                   #:pid-file #$pid-file
                                   #:pid-file-timeout 5
                                   #:log-file "/var/log/rummager-sidekiq.log"
                                   #:environment-variables
                                   (list #$@environment-variables))))
                       (stop #~(make-kill-destructor)))

     (queue-listener-shepherd-services package
                                       environment-variables))))

(define (rummager-activation parameters)
  (define package (find package? parameters))

  (define environment-variables
    (rummager-environment (find service-startup-config?
                                parameters)
                          (find plek-config? parameters)
                          package
                          (filter database-connection-config?
                                  parameters)))

  #~(let* ((dir (string-append "/tmp/env.d/"))
           (file (string-append dir "rummager")))

      #$(setup-app-directory "rummager" package)

      (mkdir-p dir)
      (call-with-output-file file
        (lambda (port)
          (for-each
           (lambda (string) (display string port))
           (list #$@(map (lambda (pair)
                           #~(string-append #$(car pair)
                                            "=\""
                                            #$(cdr pair)
                                            "\"\n"))
                         environment-variables)))))))

(define rummager-accounts
  (const
   (list (user-account (name "rummager")
                       (group "nogroup")
                       (system? #t)
                       (home-directory "/var/empty")
                       (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define rummager-service-type
  (service-type (name 'rummager)
                (extensions
                 (modify-service-extensions-for-signon
                  name
                  (list
                   (service-extension shepherd-root-service-type
                                      rummager-shepherd-services)
                   (service-extension activation-service-type
                                      rummager-activation)
                   (service-extension account-service-type
                                      rummager-accounts))))))
