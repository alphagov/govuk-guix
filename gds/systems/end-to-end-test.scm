(define-module (gds systems end-to-end-test)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services databases)
  #:use-module (gds packages govuk)
  #:use-module (gds packages mongodb)
  #:use-module (gds services govuk)
  #:use-module (gds services base)
  #:use-module (gds services mongodb))

(define %base-services
  (list
   (syslog-service)
   (urandom-seed-service)
   (nscd-service)
   (guix-service)))

(define system-ports
  `((postgresql . 55432)
    (mongodb . 57017)
    (publishing-api . 53039)
    (content-store . 53000)
    (draft-content-store . 53001)))

(define end-to-end-test-os
  (operating-system
    (host-name "govuk-test")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (bootloader (grub-configuration (device "/dev/sdX")))
    (file-systems
     (cons (file-system
             (device "my-root")
             (title 'label)
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))
    (services
     (parameterize ((ports system-ports))
       (cons*
        (publishing-api-service)
        (content-store-service)
        (draft-content-store-service)
        (govuk-content-schemas-service)
        (postgresql-service #:port (number->string (port-for 'postgresql)))
        (mongodb-service #:port (port-for 'mongodb))
        (/usr/share/zoneinfo-service)
        %base-services)))))

end-to-end-test-os
