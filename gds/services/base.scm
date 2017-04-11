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
  #:export (/usr/share/zoneinfo-service-type
            /usr/share/zoneinfo-service

            pretend-loopback-service))

(define (/usr/share/zoneinfo-service-type package)
  (service-type
   (name '/usr/share/zoneinfo)
   (extensions
    (list
     (service-extension activation-service-type
                        (const
                         #~(begin
                             (use-modules (guix build utils))

                             (if (not (file-exists? "/usr/share/zoneinfo"))
                                 (begin
                                   (display "Creating /usr/share/zoneinfo symlink\n")
                                   (mkdir-p "/usr/share")
                                   (symlink (string-append
                                             #$package
                                             "/share/zoneinfo")
                                            "/usr/share/zoneinfo"))))))))))

(define* (/usr/share/zoneinfo-service #:optional (package tzdata))
  (service (/usr/share/zoneinfo-service-type package) package))

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
