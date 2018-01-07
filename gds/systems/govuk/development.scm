(define-module (gds systems govuk development)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages screen)
  #:use-module (gnu services web)
  #:use-module (gnu packages wget)
  #:use-module (gnu services admin)
  #:use-module (gnu packages web)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages code)
  #:use-module (gnu services databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (gds packages guix)
  #:use-module (gds packages utils custom-sources)
  #:use-module (gds packages third-party mongodb)
  #:use-module (guix store)
  #:use-module (gds services base)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services rails)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases elasticsearch)
  #:use-module (gds services third-party elasticsearch)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk content-access-limits)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk tailon)
  #:use-module (gds services govuk nginx))

(define-public base-services
  (list
   (syslog-service)
   (nscd-service)
   (guix-service
    (guix-configuration
     (guix guix)))
   (service
    set-file-ownership-service-type
    '(("/var/lib/mysql" "mysql" "mysql" #:recursive)
      ("/var/lib/postgresql" "postgres" "postgres" #:recursive)
      ("/var/lib/mongodb" "mongodb" "mongodb" #:recursive)))
   (service special-files-service-type
            `(("/bin/sh" ,(file-append (canonical-package bash)
                                       "/bin/sh"))
              ("/bin/bash" ,(file-append (canonical-package bash)
                                         "/bin/bash"))))
   pretend-loopback-service))

(define services
  (append
   api-services
   publishing-application-services
   supporting-application-services
   frontend-services
   draft-frontend-services
   (list
    (service govuk-nginx-service-type)
    (service redis-service-type (redis-configuration))
    (service memcached-service-type)
    (postgresql-service)
    (service mongodb-service-type)
    (service elasticsearch-service-type)
    (service mysql-service-type (mysql-configuration))
    govuk-content-schemas-service
    (service govuk-tailon-service-type
             (tailon-configuration
              (config-file
               (tailon-configuration-file
                (bind "localhost:54001")
                (files '("/var/log/shepherd.log"
                         ("NGinx Logs" "/var/log/nginx/*.access.log"))))))))
   base-services))

(define routing-configuration-arguments
  '(#:use-high-ports? #t
    #:use-https? #f
    #:app-domain "dev.gov.uk"
    #:web-domain "www.dev.gov.uk"))

(define plek-config
  (apply plek-config-from-routing-configuration-arguments
         services
         routing-configuration-arguments))

(define-public setup-services
  (let
      ((service-setup-functions
        ;; Service setup functions, order alphabetically if possible,
        ;; and add comments to indicate any interdependencies in the
        ;; configuration
        (list
         add-signon-dev-user
         (cut map
           (cut update-rails-app-config-environment-for-service "development" <>)
           <>)
         (cut apply set-routing-configuration-for-services <>
              routing-configuration-arguments)
         (cut update-database-service-ports-for-services
           high-database-service-ports <>)
         set-jwt-auth-secret
         ;; TODO: ensure-database-user-exists-on-service-startup and
         ;; configure-rails-services-database setup must happen after
         ;; update-database-connection-config-ports, or the wrong
         ;; database connection configuration is used.
         (cut map ensure-database-user-exists-on-service-startup <>)
         (cut map run-db:setup-if-postgresql-or-mysql-is-used <>)
         update-services-with-random-signon-secrets
         (cut use-gds-sso-strategy <> "real"))))

    (apply compose (reverse service-setup-functions))))

(define (skeletons database-service-ports)
  `((".psqlrc" ,(local-file "skeletons/psqlrc"))
    (".bashrc" ,(local-file "skeletons/bashrc"))
    (".bash_aliases"
     ,(plain-file
       "aliases"
       (apply
        string-append
        (map (match-lambda
               ((name . value)
                (simple-format #f "alias ~A=\"~A\"\n" name value)))
             `(("redis" .
                ,(simple-format #f "redis-cli -p ~A"
                                (assoc-ref database-service-ports 'redis)))
               ("memcached-telnet" .
                ,(simple-format #f "telnet ~A"
                                (assoc-ref database-service-ports 'memcached))))))))
    (".environment"
     ,(plain-file
       "environment"
       (apply
        string-append
        (map (match-lambda
               ((name . value)
                (simple-format #f "export ~A=~A\n" name value)))
             `(("PGPORT" . ,(assoc-ref database-service-ports 'postgresql))
               ("PGUSER" . "postgres"))))))
    ,@(remove
       (match-lambda
         ((name rest ...)
          (member name '(".bashrc"
                         ".Xdefaults"
                         ".guile-wm"))))
       (default-skeletons))))

(define packages
  (cons*
   smokey
   govuk-setenv
   strace
   (specification->package+output "bind" "utils")
   glibc
   git
   postgresql
   mariadb
   mongodb
   redis
   mongo-tools
   htop
   iotop
   screen
   vim
   ncdu
   the-silver-searcher
   tree
   jq
   wget
   lsof
   curl
   %base-packages))

(define (hosts-file host-name plek-config)
  (plain-file "hosts"
              (string-join
               (list
                (local-host-aliases host-name)
                (plek-config->/etc/hosts-string plek-config)
                "\n"))))

(define-public development-os
  (operating-system
    (host-name "govuk-dev")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (bootloader (grub-configuration (device "/dev/sdX")))
    (packages packages)
    (skeletons (skeletons high-database-service-ports))
    (hosts-file (hosts-file host-name plek-config))
    (file-systems
     (cons (file-system
             (device "my-root")
             (title 'label)
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))
    (services (setup-services services))))

