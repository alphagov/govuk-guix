(define-module (gds systems govuk base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (gnu)
  #:use-module (gnu services admin)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (gds packages govuk)
  #:use-module (gds packages guix)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages code)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gds services base)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk content-access-limits)
  #:use-module (gds services govuk nginx)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk tailon)
  #:use-module (gds services rails)
  #:use-module (gds services utils databases elasticsearch)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils)
  #:use-module (gds systems govuk utils)
  #:export (base-services
            optional-services
            setup-services
            useful-packages))

(define base-services
  (list
   (syslog-service)
   (nscd-service)
   (guix-service (guix-configuration
                  (guix guix)))
   (service  set-file-ownership-service-type
             '(("/var/lib/mysql" "mysql" "mysql" #:recursive)
               ("/var/lib/postgresql" "postgres" "postgres" #:recursive)
               ("/var/lib/mongodb" "mongodb" "mongodb" #:recursive)))
   (service special-files-service-type
            `(("/bin/sh" ,(file-append (canonical-package bash)
                                       "/bin/sh"))
              ("/bin/bash" ,(file-append (canonical-package bash)
                                         "/bin/bash"))))
   (service govuk-tailon-service-type
            (tailon-configuration
             (config-file
              (tailon-configuration-file
               (bind "localhost:54001")
               (files '("/var/log/shepherd.log"
                        ("NGinx Logs" "/var/log/nginx/*.access.log")))))))
   pretend-loopback-service
   (service govuk-nginx-service-type)
   (service govuk-skeletons-service-type)))

(define optional-services
  (append
   govuk-services
   (list
    (service redis-service-type (redis-configuration))
    (service memcached-service-type)
    (postgresql-service)
    (service mongodb-service-type)
    (service elasticsearch-service-type)
    (service mysql-service-type (mysql-configuration))
    govuk-content-schemas-service)))

(define setup-services
  (let
      ((service-setup-functions
        ;; Service setup functions, order alphabetically if possible,
        ;; and add comments to indicate any interdependencies in the
        ;; configuration
        (list
         set-random-devise-secrets-for-the-signon-service
         set-jwt-auth-secret
         update-rails-app-config-with-random-secret-key-base-for-services
         (cut set-routing-configuration-for-services <>)
         ;; TODO: ensure-database-user-exists-on-service-startup and
         ;; configure-rails-services-database setup must happen after
         ;; update-database-connection-config-ports, or the wrong
         ;; database connection configuration is used.
         (cut map ensure-database-user-exists-on-service-startup <>)
         (cut map run-db:setup-if-postgresql-or-mysql-is-used <>)
         (cut use-gds-sso-strategy <> "real")
         update-services-with-random-signon-secrets)))

    (apply compose (reverse service-setup-functions))))

(define useful-packages
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
   openssl
   nss-certs
   %base-packages))
