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

(define /usr/share/zoneinfo-service-type
  (shepherd-service-type
   '/usr/share/zoneinfo
   (lambda (package)
     (shepherd-service
      (provision (list '/usr/share/zoneinfo))
      (documentation "Ensure /usr/share/zoneinfo exists")
      (start
       #~(begin
           (use-modules (guix build utils))

           (if (not (file-exists? "/usr/share/zoneinfo"))
               (begin
                 (mkdir-p "/usr/share")
                 (symlink (string-append
                           #$package
                           "/share/zoneinfo")
                          "/usr/share/zoneinfo")))
           #t))
      (stop #~(lambda _
                #f))
      (respawn? #f)))))

(define* (/usr/share/zoneinfo-service #:optional (package tzdata))
  (service /usr/share/zoneinfo-service-type package))
