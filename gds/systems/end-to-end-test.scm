(define-module (gds systems end-to-end-test)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gds packages govuk)
  #:use-module (gnu packages linux)
  #:use-module (gds packages mongodb)
  #:use-module (gds services govuk)
  #:use-module (gds services base)
  #:use-module (gds services mongodb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix store))

(define github-url-regex
  (make-regexp
   "https:\\/\\/github\\.com\\/[^\\/]*\\/[^\\/]*\\/archive\\/([^\\/]*)\\.tar\\.gz"))

(define* (custom-github-archive-source-for-package
          app-package
          commit-ish)
  (let*
      ((old-url
        (origin-uri (package-source app-package)))
       (regexp-match
        (regexp-exec github-url-regex old-url)))
    (if (not (regexp-match? regexp-match))
        (error "No match"))
    (with-store store
      (download-to-store
       store
       (string-replace
        old-url
        commit-ish
        (match:start regexp-match 1)
        (match:end regexp-match 1))
       #:recursive? #t))))

(define environment-variable-commit-ish-regex
  (make-regexp
   "GDS_GUIX_([A-Z0-9_]*)_COMMIT_ISH=(.*)"))

(define environment-variable-path-regex
  (make-regexp
   "GDS_GUIX_([A-Z0-9_]*)_PATH=(.*)"))

(define (get-package-source-config-list-from-environment regex)
  (map
   (lambda (name-value-match)
     (cons
      (string-map
       (lambda (c)
         (if (eq? c #\_) #\- c))
       (string-downcase
        (match:substring name-value-match 1)))
      (match:substring name-value-match 2)))
   (filter
    regexp-match?
    (map
     (lambda (name-value)
       (regexp-exec regex name-value))
     (environ)))))

(define (update-database-connection-config-ports ports config)
  (define (port-for service)
    (or (assq-ref ports service)
        (begin
          (display "ports: ")
          (display ports)
          (display "\n")
          (error "Missing port for " service))))

  (cond
   ((postgresql-connection-config? config)
    (postgresql-connection-config
     (inherit config)
     (port (port-for 'postgresql))))
   ((mysql-connection-config? config)
    (mysql-connection-config
     (inherit config)
     (port (port-for 'mysql))))
   ((mongodb-connection-config? config)
    (mongodb-connection-config
     (inherit config)
     (port (port-for 'mongodb))))
   ((redis-connection-config? config)
    (redis-connection-config
     (inherit config)
     (port (port-for 'redis))))
   (else (error "unknown database connection config " config))))

(define (ensure-service-parameters s test-and-value-pairs)
  (service
   (service-kind s)
   (fold
    (lambda (test+value parameters)
      (match test+value
        ((test? . value)
         (if (list? parameters)
             (if (any test? parameters)
                 (map (lambda (x) (if (test? x) value x))
                      parameters)
                 (append parameters (list value)))
             (if (test? parameters)
                 value
                 (list parameters value))))))
    (service-parameters s)
    test-and-value-pairs)))

(define (update-service-parameters s test-and-function-pairs)
  (define (update-parameter parameter)
    (fold
     (lambda (test+function parameter)
       (match test+function
         ((test . function)
          (if (test parameter)
              (function parameter)
               parameter))))
     parameter
     test-and-function-pairs))

  (service
   (service-kind s)
   (let
       ((parameters (service-parameters s)))
     (if
      (list? parameters)
      (map update-parameter parameters)
      (update-parameter parameters)))))

(define (correct-source-of package-path-list package-commit-ish-list pkg)
  (let
      ((custom-path (assoc-ref package-path-list
                               (package-name pkg)))
       (custom-commit-ish (assoc-ref package-commit-ish-list
                                     (package-name pkg))))
    (cond
     ((and custom-commit-ish custom-path)
      (error "cannot specify custom-commit-ish and custom-path"))
     (custom-commit-ish
      (package
        (inherit pkg)
        (source
         (custom-github-archive-source-for-package
          pkg
          custom-commit-ish))))
     (custom-path
      (package
        (inherit pkg)
        (source custom-path)))
     (else
      pkg))))

(define (log-package-path-list package-path-list)
  (for-each
   (match-lambda
     ((package . path)
      (simple-format
       #t
       "Using path \"~A\" for the ~A package\n"
       path
       package)))
   package-path-list))

(define (log-package-commit-ish-list package-commit-ish-list)
  (for-each
   (match-lambda
     ((package . commit-ish)
      (simple-format
       #t
       "Using commit-ish \"~A\" for the ~A package\n"
       commit-ish
       package)))
   package-commit-ish-list))

(define %base-services
  (list
   (syslog-service)
   (urandom-seed-service)
   (nscd-service)
   (guix-service)
   pretend-loopback-service))

(define govuk-ports
  `((publishing-api . 53039)
    (content-store . 53000)
    (draft-content-store . 53001)
    (specialist-publisher . 53064)
    (need-api . 53052)
    (maslow . 53053)
    (specialist-frontend . 53065)
    (signon . 53016)
    (static . 53013)
    (router-api . 53056)
    (draft-router-api . 53556)))

(define system-ports
  `((postgresql . 55432)
    (mongodb . 57017)
    (redis . 56379)
    (mysql . 53306)))

(define (port-for service)
  (or (assq-ref govuk-ports service)
      (assq-ref system-ports service)))

(define (nginx service-and-ports
               router-config
               draft-router-config)
  (nginx-service
   #:upstream-list
   (cons*
    (nginx-upstream-configuration
     (name "www.guix-dev.gov.uk-proxy")
     (server (string-append
              "localhost:"
              (number->string
               (router-config-public-port router-config)))))
    (nginx-upstream-configuration
     (name "draft-origin.guix-dev.gov.uk-proxy")
     (server (string-append
              "localhost:"
              (number->string
               (router-config-public-port draft-router-config)))))
    (map
     (match-lambda
       ((service . port)
        (nginx-upstream-configuration
         (name (string-append (symbol->string service) ".guix-dev.gov.uk-proxy"))
         (server (string-append "localhost:" (number->string port))))))
     service-and-ports))
   #:vhost-list
   (let
       ((base
         (nginx-vhost-configuration
          (http-port 50080)
          (https-port 50443)
          (ssl-certificate #f)
          (ssl-certificate-key #f))))
     (cons*
      (nginx-vhost-configuration
       (inherit base)
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://www.guix-dev.gov.uk-proxy;")))))
       (server-name (list "www.guix-dev.gov.uk")))
      (nginx-vhost-configuration
       (inherit base)
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://draft-origin.guix-dev.gov.uk-proxy;")))))
       (server-name (list "draft-origin.guix-dev.gov.uk")))
      (map
       (match-lambda
         ((service . port)
          (nginx-vhost-configuration
           (inherit base)
           (locations
            (list
             (nginx-location-configuration
              (uri "/")
              (body '("try_files $uri/index.html $uri.html $uri @app;")))
             (nginx-location-configuration
              (name "@app")
              (body (list (simple-format
                           #f
                           "proxy_pass http://~A.guix-dev.gov.uk-proxy;"
                           (symbol->string service)))))))
           (server-name (list (string-append
                               (symbol->string service)
                               ".guix-dev.gov.uk")))
           (root (string-append "/var/lib/" (symbol->string service) "/public")))))
       service-and-ports)))))

(define (set-random-rails-secret-token service)
  (update-service-parameters
   service
   (list
    (cons rails-app-config?
          update-rails-app-config-with-random-secret-token))))

(define (set-plek-config services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        plek-config?
        (const (make-custom-plek-config
                govuk-ports
                #:govuk-app-domain "guix-dev.gov.uk"
                #:use-https? #f
                #:port 50080))))))
   services))

(define (correct-services-package-source package-path-list package-commit-ish-list services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        package?
        (lambda (pkg)
          (correct-source-of
           package-path-list
           package-commit-ish-list
           pkg))))))
   services))

(define (set-common-app-service-config services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        database-connection-config?
        (lambda (config)
          (update-database-connection-config-ports system-ports config)))
       (cons
        rails-app-config?
        (lambda (config)
          (update-rails-app-config-environment
           "development"
           (update-rails-app-config-with-random-secret-key-base config))))
       (cons
        gds-sso-config?
        (gds-sso-config)))))
   services))

(define services
  (let
      ((package-commit-ish-list
        (get-package-source-config-list-from-environment
         environment-variable-commit-ish-regex))
       (package-path-list
        (get-package-source-config-list-from-environment
         environment-variable-path-regex))

       (live-router-config
        (router-config
         (public-port 51001)
         (api-port 51002)
         (debug? #t)))
       (draft-router-config
        (router-config
         (public-port 51003)
         (api-port 51004)
         (debug? #t)))

       (redis (redis-service #:port (port-for 'redis)))
       (postgresql (postgresql-service #:port (port-for 'postgresql)))
       (mongodb (mongodb-service #:port (port-for 'mongodb)))
       ;; Note that the mysql service in Guix defaults to using MariaDB
       (mysql (mysql-service #:config
                             (mysql-configuration
                              (port (port-for 'mysql))))))
    (log-package-path-list package-path-list)
    (log-package-commit-ish-list package-commit-ish-list)
    (append
     (map
      (lambda (service)
        (update-service-parameters
         service
         (list
          (cons
           package?
           (lambda (pkg)
             (correct-source-of
              package-path-list
              package-commit-ish-list
              pkg)))
          (cons
           plek-config?
           (const (make-custom-plek-config
                   govuk-ports
                   #:govuk-app-domain "guix-dev.gov.uk"
                   #:use-https? #f
                   #:port 50080)))
          (cons
           database-connection-config?
           (lambda (config)
             (update-database-connection-config-ports system-ports config)))
          (cons
           rails-app-config?
           (lambda (config)
             (update-rails-app-config-environment
              "development"
              (update-rails-app-config-with-random-secret-key-base config)))))))
      (list
       publishing-api-service
       content-store-service
       draft-content-store-service
       (update-service-parameters
        router-service
        (list
         (cons router-config?
               (const live-router-config))))
       (update-service-parameters
        draft-router-service
        (list
         (cons router-config?
               (const draft-router-config))))
       (update-service-parameters
        router-api-service
        (list
         (cons router-api-config?
          (const
           (router-api-config
            (router-nodes
             (list
              (simple-format #f "localhost:~A"
                             (router-config-api-port live-router-config)))))))))
       (update-service-parameters
        draft-router-api-service
        (list
         (cons router-api-config?
               (const
                (router-api-config
                 (router-nodes
                  (list
                   (simple-format #f "localhost:~A"
                                  (router-config-api-port draft-router-config)))))))))
       (set-random-rails-secret-token
        specialist-publisher-service)
       maslow-service
       need-api-service
       specialist-frontend-service
       static-service
       govuk-content-schemas-service
       publishing-e2e-tests-service
       signon-service))
     (cons*
      (nginx
       govuk-ports
       live-router-config
       draft-router-config)
      redis
      postgresql
      mongodb
      mysql
      ;; Position is significant for /usr/bin/env-service and
      ;; /usr/share/zoneinfo-service, as these need to be activated
      ;; before services which require them in their activation
      (/usr/bin/env-service)
      (/usr/share/zoneinfo-service)
      %base-services))))

(define end-to-end-test-os
  (operating-system
    (host-name "govuk-test")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (bootloader (grub-configuration (device "/dev/sdX")))
    (packages
     (cons* strace glibc %base-packages))
    (file-systems
     (cons (file-system
             (device "my-root")
             (title 'label)
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))
    (services services)))

end-to-end-test-os
