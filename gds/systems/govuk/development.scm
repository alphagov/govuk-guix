(define-module (gds systems govuk development)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu services memcached)
  #:use-module (gnu services admin)
  #:use-module (gnu packages web)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages code)
  #:use-module (gnu services databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gds packages guix)
  #:use-module (gds packages utils custom-sources)
  #:use-module (gds packages third-party mongodb)
  #:use-module (guix store)
  #:use-module (gds services base)
  #:use-module (gds services third-party mongodb)
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
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk nginx))

(define-public govuk-ports
  (let ((defaults '((publishing-api . 53039)
                    (content-store . 53000)
                    (draft-content-store . 53001)
                    (content-tagger . 53116)
                    (whitehall . 53020)
                    (specialist-publisher . 53064)
                    (need-api . 53052)
                    (maslow . 53053)
                    (specialist-frontend . 53065)
                    (draft-specialist-frontend . 53066)
                    (government-frontend . 53090)
                    (draft-government-frontend . 53091)
                    (contentapi . 53022)
                    (frontend . 53005)
                    (draft-frontend . 53007)
                    (rummager . 53009)
                    (search . 53009)
                    (publisher . 53006)
                    (signon . 53016)
                    (static . 53013)
                    (info-frontend . 53010)
                    (draft-static . 53014)
                    (router-api . 53056)
                    (draft-router-api . 53557))))
    (define (get-next-port ports)
      (define (get-next-free-port candidate-port)
        (if (memq candidate-port
                  (map cdr ports))
            (get-next-free-port (+ 1 candidate-port))
            candidate-port))

      (get-next-free-port 50000))

    (fold
     (lambda (service ports)
       (let ((name (service-type-name (service-kind service))))
         (if (assq-ref ports name)
             ports
             (cons
              (cons name (get-next-port ports))
              ports))))
     defaults
     (append
      api-services
      publishing-application-services
      supporting-application-services
      frontend-services
      draft-frontend-services))))

(define system-ports
  `((postgresql . 55432)
    (mongodb . 57017)
    (redis . 56379)
    (elasticsearch . 59200)
    (mysql . 53306)))

(define (port-for service)
  (or (assq-ref govuk-ports service)
      (assq-ref system-ports service)))

(define-public base-services
  (list
   (syslog-service)
   (urandom-seed-service)
   (nscd-service)
   (guix-service
    (guix-configuration
     (guix guix)))
   (service special-files-service-type
            `(("/bin/sh" ,(file-append (canonical-package bash)
                                       "/bin/sh"))))
   pretend-loopback-service))

(define-public live-router-config
  (router-config (public-port (port-for 'router))
                 (api-port 51002)
                 (debug? #t)))

(define-public draft-router-config
  (router-config (public-port (port-for 'draft-router))
                 (api-port 51004)
                 (debug? #t)))

(define services
  (append
   api-services
   publishing-application-services
   supporting-application-services
   frontend-services
   draft-frontend-services
   (list
    (govuk-nginx-service
     govuk-ports
     live-router-config
     draft-router-config
     '((rummager . ("search"))))
    (service
     redis-service-type
     (redis-configuration
      (port (assq-ref system-ports 'redis))))
    (service
     memcached-service-type
     (memcached-configuration))
    (postgresql-service #:port (assq-ref system-ports 'postgresql))
    (mongodb-service #:port (assq-ref system-ports 'mongodb))
    (service
     elasticsearch-service-type
     (elasticsearch-configuration
      (http-port (assq-ref system-ports 'elasticsearch))))
    (mysql-service #:config (mysql-configuration
                             (port (assq-ref system-ports 'mysql))))
    govuk-content-schemas-service
    ;; Position is significant for /usr/bin/env-service and
    ;; /usr/share/zoneinfo-service, as these need to be activated
    ;; before services which require them in their activation
    (/usr/bin/env-service)
    (/usr/share/zoneinfo-service))
   base-services))

(define (update-routing-services-configuration
         services)
  (let
      ((router-config->router-nodes-value
        (lambda (router-config)
          (simple-format
           #f
           "localhost:~A"
           (router-config-api-port router-config)))))

    (update-services-parameters
     services
     (list
      (cons router-service-type
            (list
             (cons router-config?
                   (const live-router-config))))
      (cons draft-router-service-type
            (list
             (cons router-config?
                   (const draft-router-config))))
      (cons router-api-service-type
            (list
             (cons service-startup-config?
                   (lambda (ssc)
                     (service-startup-config-with-additional-environment-variables
                      ssc
                      `(("ROUTER_NODES"
                         .
                         ,(router-config->router-nodes-value
                           live-router-config))))))))
      (cons draft-router-api-service-type
            (list
             (cons service-startup-config?
                   (lambda (ssc)
                     (service-startup-config-with-additional-environment-variables
                      ssc
                      `(("ROUTER_NODES"
                         .
                         ,(router-config->router-nodes-value
                           draft-router-config))))))))))))

(define (update-database-connection-config-ports services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        database-connection-config?
        (lambda (config)
          (update-database-connection-config-port
           (lambda (service)
             (or (assq-ref system-ports service)
                 (begin
                   (display "ports: ")
                   (display system-ports)
                   (display "\n")
                   (error "Missing port for " service))))
           config))))))
   services))

(define plek-config
  (make-custom-plek-config
   govuk-ports
   #:govuk-app-domain "guix-dev.gov.uk"
   #:use-https? #f
   #:port 50080
   #:aliases '((rummager . (search)))))

(define (set-services-plek-config services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        plek-config?
        (const plek-config)))))
   services))

(define* (set-jwt-auth-secret services
                              #:optional #:key
                              (secret (random-base16-string 30)))
  (define (add-environment-variable ssc)
    (service-startup-config-with-additional-environment-variables
     ssc
     `(("JWT_AUTH_SECRET" . ,secret))))

  (update-services-parameters
   services
   (list
    (cons
     authenticating-proxy-service-type
     (list
      (cons service-startup-config? add-environment-variable)))
    (cons
     publisher-service-type
     (list
      (cons service-startup-config? add-environment-variable)))
    (cons
     whitehall-service-type
     (list
      (cons service-startup-config? add-environment-variable))))))

(define (add-signon-dev-user services)
  (update-services-parameters
   services
   (list
    (cons
     signon-service-type
     (list
      (cons
       signon-config?
       (lambda (config)
         (signon-config
          (inherit config)
          (users
           (list
            (signon-user
             (name "Dev")
             (email "dev@example.com")
             (passphrase "wies1Oc8Gi0uGaim")
             (role "superadmin")
             (application-permissions
              (map
               (lambda (app)
                 (cons
                  (signon-application-name app)
                  (signon-application-supported-permissions app)))
               (filter-map
                (lambda (service)
                  (and (list? (service-parameters service))
                       (find signon-application? (service-parameters service))))
                services))))))))))))))

(define (update-services-with-random-signon-secrets services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        signon-application?
        (lambda (app)
          (update-signon-application-with-random-oauth app)))
       (cons
        signon-api-user?
        (lambda (api-user)
          (update-signon-api-user-with-random-authorisation-tokens api-user))))))
   services))

(define (services-in-rails-development-environment services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        rails-app-config?
        (lambda (config)
          (update-rails-app-config-environment
           "development"
           (update-rails-app-config-with-random-secret-key-base config)))))))
   services))

(define (set-authenticating-proxy-upstream-url services)
  (update-services-parameters
   services
   (list
    (cons
     authenticating-proxy-service-type
     (list
      (cons service-startup-config?
            (lambda (ssc)
              (service-startup-config-with-additional-environment-variables
               ssc
               `(("GOVUK_UPSTREAM_URI"
                  .
                  ,(service-uri-from-plek-config
                    plek-config 'draft-router)))))))))))

(define-public setup-services
  (let
      ((service-setup-functions
        ;; Service setup functions, order alphabetically if possible,
        ;; and add comments to indicate any interdependencies in the
        ;; configuration
        (list
         add-signon-dev-user
         correct-services-package-source-from-environment
         services-in-rails-development-environment
         set-authenticating-proxy-upstream-url
         set-jwt-auth-secret
         set-services-plek-config
         update-database-connection-config-ports
         update-routing-services-configuration
         update-services-with-random-signon-secrets
         (cut use-gds-sso-strategy <> "real"))))

    (apply compose (reverse service-setup-functions))))

(define development-os-services
  (setup-services services))

(define-public development-os
  (operating-system
    (host-name "govuk-test")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (bootloader (grub-configuration (device "/dev/sdX")))
    (hosts-file
     (plain-file "hosts"
                 (string-join
                  (list
                   (local-host-aliases host-name)
                   (plek-config->/etc/hosts-string plek-config))
                  "\n")))
    (packages
     (cons*
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
      vim
      ncdu
      the-silver-searcher
      tree
      jq
      %base-packages))
    (file-systems
     (cons (file-system
             (device "my-root")
             (title 'label)
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))
    (services
     development-os-services)))

development-os
