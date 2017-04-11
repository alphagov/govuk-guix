(define-module (gds services base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:export (pretend-loopback-service))

(define pretend-loopback-service
  (service
   (shepherd-service-type
    'dummy-loopback-service
    (const
     (shepherd-service
      (documentation "Pretend loopback service, just provides 'loopback")
      (provision '(loopback))
      (start #~(const #t))
      (stop #~(const #t)))))
   '()))
