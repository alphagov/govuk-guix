(define-module (gds services delayed-job)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services shepherd)
  #:use-module (gds services)
  #:export (<delayed-job-config>
            delayed-job-config
            delayed-job-config?
            delayed-job-config-queues

            delayed-job-worker-shepherd-service))

(define-record-type* <delayed-job-config>
  delayed-job-config make-delayed-job-config
  delayed-job-config?
  (queues delayed-job-config-queues
          (default #f)))

(define (delayed-job-worker-shepherd-service
         name
         delayed-job-config
         requirements
         directory
         user
         environment)
  (shepherd-service
   (provision (list (string->symbol name)))
   (documentation
    (simple-format #f "~A service" name))
   (requirement requirements)
   (respawn? #f)
   (start
    #~(lambda args
        (display #$(simple-format #f "starting ~A service\n" name))
        (apply
         #$#~(make-forkexec-constructor
              '("rake" "jobs:work")
              #:user #$user
              #:log-file #$(string-append
                            "/var/log/" name ".log")
              #:directory #$directory
              #:environment-variables
              '#$(map
                  (match-lambda
                    ((key . value)
                     (string-append key "=" value)))
                  environment))
         args)))
   (stop #~(make-kill-destructor))))
