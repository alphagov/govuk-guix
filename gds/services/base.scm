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
  #:export (/usr/bin/env-service-type
            /usr/bin/env-service

            /usr/share/zoneinfo-service-type
            /usr/share/zoneinfo-service))

(define /usr/bin/env-service-type
  (shepherd-service-type
   '/usr/bin/env
   (lambda (package)
     (shepherd-service
      (provision (list '/usr/bin/env))
      (documentation "Ensure /usr/bin/env exists")
      (start
       #~(lambda _
           (use-modules (guix build utils))

           (if (not (file-exists? "/usr/bin/env"))
               (let ((path
                      (string-append
                       #$package
                       "/bin/env")))
                 (mkdir-p "/usr/bin")
                 (symlink path
                          "/usr/bin/env")))
           #t))
      (stop #~(lambda _
                #f))
      (respawn? #f)))))

(define* (/usr/bin/env-service #:optional (package coreutils))
  (service /usr/bin/env-service-type package))

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
