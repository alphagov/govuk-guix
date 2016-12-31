(define-module (gds services sidekiq)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gds services)
  #:export (<sidekiq-config>
            sidekiq-config
            sidekiq-config?
            sidekiq-config-file

            generic-sidekiq-start-script))

(define-record-type* <sidekiq-config>
  sidekiq-config make-sidekiq-config
  sidekiq-config?
  (file sidekiq-config-file
        (default #f)))

(define (generic-sidekiq-start-script
         name
         .
         rest)
  (let*
      ((package (find package? rest))
       (service-startup-config (find service-startup-config? rest))
       (sidekiq-config (find sidekiq-config? rest))
       (string-name (symbol->string name))
       (root-directory
        (string-append "/var/lib/" string-name))
       (config-file (sidekiq-config-file sidekiq-config))
       (environment-variables
        (service-startup-config-environment-variables
         service-startup-config)))
    (program-file
     (string-append "start-" string-name "-sidekiq")
     (with-imported-modules '((guix build utils)
                              (ice-9 popen))
       #~(let ((user (getpwnam #$string-name))
               (bundle (string-append #$root-directory "/bin/bundle")))
           (use-modules (guix build utils)
                        (ice-9 popen))

           ;; Start the service
           (setgid (passwd:gid user))
           (setuid (passwd:uid user))

           (for-each
            (lambda (env-var)
              (setenv (car env-var) (cdr env-var)))
            '#$environment-variables)

           (chdir #$root-directory)
           (and
            (zero? (apply
                    system*
                    (cons*
                     bundle "exec" "sidekiq"
                     (if #$config-file '("-C" #$config-file) '()))))))))))
