(define-module (gds services sidekiq)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services shepherd)
  #:use-module (gds services)
  #:export (<sidekiq-config>
            sidekiq-config
            sidekiq-config?
            sidekiq-config-file

            sidekiq-shepherd-service))

(define-record-type* <sidekiq-config>
  sidekiq-config make-sidekiq-config
  sidekiq-config?
  (file sidekiq-config-file
        (default #f)))

(define (sidekiq-shepherd-service
         name
         sidekiq-config
         requirements
         directory
         user
         environment)
  (let*
      ((config-file (sidekiq-config-file sidekiq-config))
       (pidfile (string-append "/tmp/" name ".pid"))
       (start-command
        `(,(string-append directory "/bin/bundle")
          "exec"
          "sidekiq"
          ,@(if config-file `("-C" ,config-file) '())
          "--pidfile" ,pidfile)))
    (shepherd-service
     (provision (list (string->symbol name)))
     (documentation
      (simple-format #f "~A service" name))
     (requirement requirements)
     (respawn? #f)
     (start #~(lambda args
                (display
                 #$(simple-format #f "starting ~A service: ~A\n"
                                  name (string-join start-command)))
                (apply
                 #$#~(make-forkexec-constructor
                      '#$start-command
                      #:user #$user
                      #:pid-file #$pidfile
                      #:pid-file-timeout 60
                      #:log-file #$(string-append "/var/log/" name".log")
                      #:directory #$directory
                      #:environment-variables
                      '#$(map
                          (match-lambda
                            ((key . value)
                             (string-append key "=" value)))
                          environment))
                 args)))
     (stop #~(make-kill-destructor)))))
