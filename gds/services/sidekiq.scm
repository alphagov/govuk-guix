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
