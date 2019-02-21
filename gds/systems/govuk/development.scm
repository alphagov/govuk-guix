(define-module (gds systems govuk development)
  #:use-module (srfi srfi-26)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds services rails)
  #:use-module (gds services utils databases)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds systems govuk production)
  #:export (govuk-development-os))

(define (setup-other-services services)
  (modify-services services
    (postgresql-service-type
     config => (postgresql-configuration
                (inherit config)
                (config-file
                 (postgresql-config-file
                  (inherit (postgresql-configuration-file config))
                  (extra-config
                   '(("session_preload_libraries" "'auto_explain'")
                     ("auto_explain.log_min_duration" "'500ms'")))))))))

(define setup-services-for-development-os
  (let
      ((service-setup-functions
        ;; Service setup functions, order alphabetically if possible,
        ;; and add comments to indicate any interdependencies in the
        ;; configuration
        (list
         setup-other-services
         (cut map
           (cut update-rails-app-config-environment-for-service "development" <>)
           <>)
         (cut map
           (cut update-service-database-connection-config-for-environment "development" <>)
           <>)
         (cut set-routing-configuration-for-services <>
              #:use-high-ports? #t
              #:use-https? #f
              #:app-domain "dev.gov.uk"
              #:web-domain "dev.gov.uk")
         ;; TODO: ensure-database-user-exists-on-service-startup and
         ;; configure-rails-services-database setup must happen after
         ;; update-database-connection-config-ports, or the wrong
         ;; database connection configuration is used.
         (cut map ensure-database-user-exists-on-service-startup <>)
         (cut map run-db:setup-if-postgresql-or-mysql-is-used <>))))

    (apply compose (reverse service-setup-functions))))

(define govuk-development-os
  (operating-system
    (inherit govuk-production-os)
    (host-name "govuk-development")
    (services (setup-services-for-development-os
               (operating-system-user-services govuk-production-os)))))

govuk-development-os
