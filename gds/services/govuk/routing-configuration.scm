(define-module (gds services govuk routing-configuration)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk nginx)
  #:use-module (gds services govuk router)
  #:export (standard-ports-for-services
            high-ports-for-services
            plek-config-from-routing-configuration-arguments

            %default-aliases
            set-routing-configuration-for-services
            high-database-service-ports
            default-database-service-ports
            update-database-service-ports-for-services))

(define (generate-port-range start-port services)
  (define (get-next-port ports)
    (define (get-next-free-port candidate-port)
      (if (memq candidate-port
                (map cdr ports))
          (get-next-free-port (+ 1 candidate-port))
          candidate-port))

    (get-next-free-port start-port))

  (fold
   (lambda (service ports)
     (let ((name (service-type-name (service-kind service))))
       (if (assq-ref ports name)
           ports
           (cons
            (cons name (get-next-port ports))
            ports))))
   `(;; Not to be confused with the router-api service, the router
     ;; has a port on which it can be sent commands, in addition to
     ;; the port which acts as a proxy.
     (router-api-port . ,(+ start-port 101))
     (draft-router-api-port . ,(+ start-port 102)))
   (stable-sort
    services
    (lambda (a b)
      (string<? (symbol->string (service-type-name (service-kind a)))
                (symbol->string (service-type-name (service-kind b))))))))

(define (standard-ports-for-services services)
  (generate-port-range 3000 services))

(define (high-ports-for-services services)
  (generate-port-range 33000 services))

(define (set-router-api-router-nodes port)
  (lambda (ssc)
    (service-startup-config-with-additional-environment-variables
     ssc
     `(("ROUTER_NODES"
        .
        ,(simple-format #f "localhost:~A" port))))))

(define (service-to-port-mapping services use-high-ports?)
  (define services-discoverable-through-plek
    (filter (lambda (service)
              (or
               (and (list? (service-value service))
                    (find plek-config?
                          (service-value service)))
               (memq (service-kind service)
                     (list router-service-type
                           draft-router-service-type))))
            services))

  (if use-high-ports?
      (high-ports-for-services services-discoverable-through-plek)
      (standard-ports-for-services services-discoverable-through-plek)))

(define* (plek-config-from-routing-configuration-arguments
          services
          #:key use-high-ports? use-https? app-domain web-domain
          (aliases %default-aliases)
          #:allow-other-keys)
  (make-custom-plek-config
   (service-to-port-mapping services use-high-ports?)
   #:app-domain app-domain
   #:web-domain web-domain
   #:use-https? use-https?
   #:port (if use-high-ports?
              (if use-https? 30443 30080)
              (if use-https? 443 80))
   #:aliases aliases))

(define %default-aliases
  '((rummager . (search))
    (whitehall . (whitehall-admin whitehall-frontend))))

(define* (set-routing-configuration-for-services
          services
          #:key
          (use-high-ports? #f)
          (http-ports-mode #f)
          (use-https? 'development)
          (authenticated-draft-origin? #t)
          (app-domain "publishing.service.gov.uk")
          (web-domain "www.gov.uk")
          (aliases %default-aliases))

  (define plek-config
    (plek-config-from-routing-configuration-arguments
     services
     #:use-high-ports? use-high-ports?
     #:use-https? use-https?
     #:app-domain app-domain
     #:web-domain web-domain
     #:aliases aliases))

  (define ports
    (service-to-port-mapping services use-high-ports?))

  (define http-ports
    (let ((mode (or http-ports-mode
                    (if use-high-ports?
                        'high
                        'standard))))
      (assq-ref
       '((standard
          . ((http . 80)
             (https . 443)))
         (alternative
          . ((http . 8080)
             (https . 8443)))
         (high
          . ((http . 30080)
             (https . 30443))))
       mode)))

  (define (port-for-service-name name)
    (assq-ref ports name))

  (define (port-for-service service)
    (port-for-service-name
     (service-type-name (service-kind service))))

  (define (update-services-plek-config services)
    (map
     (lambda (service)
       (update-service-parameters
        service
        (list
         (cons
          plek-config?
          (const plek-config)))))
     services))

  (define database-service-ports
    (if use-high-ports?
        high-database-service-ports
        default-database-service-ports))

  (define (add-the-certbot-activation-service services)
    (cons (simple-service 'govuk-certbot
                          activation-service-type
                          (certbot-activation
                           8080
                           web-domain
                           (plek-config->domains plek-config)))
          services))

  (define service-setup-functions
    `(,update-services-plek-config
      ,(cut update-database-service-ports-for-services
         database-service-ports <>)
      ,@(if (eq? use-https? 'certbot)
            (list add-the-certbot-activation-service)
            '())))

  (update-services-parameters
   ((apply compose (reverse service-setup-functions)) services)
   (list
    (cons
     govuk-nginx-service-type
     (list
      (cons govuk-nginx-configuration?
            (lambda (config)
              (govuk-nginx-configuration
               (http-port (assq-ref http-ports 'http))
               (https-port (and use-https?
                                (assq-ref http-ports 'https)))
               (include-port-in-host-header? use-high-ports?)
               (tls use-https?)
               (service-and-ports ports)
               (origin-service 'router)
               (draft-origin-service (if authenticated-draft-origin?
                                         'authenticating-proxy
                                         'draft-router))
               (server-aliases (map (match-lambda
                                      ((name . aliases)
                                       (cons name
                                             (map symbol->string aliases))))
                                    aliases))
               (web-domain web-domain)
               (app-domain app-domain))))))
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
                    plek-config 'draft-router))))))))
    (cons
     asset-manager-service-type
     (list
      (cons service-startup-config?
            (lambda (ssc)
              (service-startup-config-with-additional-environment-variables
               ssc
               `(("ALLOW_FAKE_S3_IN_PRODUCTION_FOR_PUBLISHING_E2E_TESTS" . "true")
                 ("FAKE_S3_HOST"
                  .
                  ,(service-uri-from-plek-config
                    plek-config 'asset-manager))))))))
    (cons router-service-type
          (list
           (cons router-config?
                 (lambda (config)
                   (router-config
                    (public-port (port-for-service-name 'router))
                    (api-port (port-for-service-name 'router-api-port)))))))
    (cons draft-router-service-type
          (list
           (cons router-config?
                 (lambda (config)
                   (router-config
                    (public-port (port-for-service-name 'draft-router))
                    (api-port (port-for-service-name 'draft-router-api-port)))))))
    (cons router-api-service-type
          (list
           (cons service-startup-config?
                 (set-router-api-router-nodes
                  (port-for-service-name 'router-api-port)))))
    (cons draft-router-api-service-type
          (list
           (cons service-startup-config?
                 (set-router-api-router-nodes
                  (port-for-service-name 'draft-router-api-port))))))))

(define high-database-service-ports
  '((postgresql . 35432)
    (mongodb . 37017)
    (redis . 36379)
    (elasticsearch . 39200)
    (mysql . 33306)
    (memcached . 31211)))

(define default-database-service-ports
  `((postgresql . ,((@@ (gnu services databases) postgresql-configuration-port)
                    (postgresql-configuration
                     (config-file #f)
                     (data-directory #f))))
    (mongodb . 27107)
    (redis . ,((@@ (gnu services databases) redis-configuration-port)
               (redis-configuration)))
    (elasticsearch . ,(elasticsearch-configuration-port
                       (elasticsearch-configuration)))
    (mysql . ,((@@ (gnu services databases) mysql-configuration-port)
               (mysql-configuration)))
    (memcached . ,(memcached-configuration-tcp-port
                   (memcached-configuration)))))

(define (mongodb-configuration-file port)
  (mixed-text-file
   "mongodb.yaml"
   "
processManagement:
  pidFilePath: /var/run/mongodb/pid
storage:
  dbPath: /var/lib/mongodb
net:
  port: " (number->string port) "
"))

(define (update-database-service-ports-for-services ports services)
  (define (update-database-connection-configs service)
    (update-service-parameters
     service
     (list
      (cons
       database-connection-config?
       (lambda (config)
         (update-database-connection-config-port
          (lambda (service)
            (or (assq-ref ports service)
                (begin
                  (display "ports: ")
                  (display ports)
                  (display "\n")
                  (error "Missing port for " service))))
          config))))))

  (modify-services
      (map update-database-connection-configs services)

    (redis-service-type parameter =>
                        (redis-configuration
                         (inherit parameter)
                         (port (assq-ref ports 'redis))))
    (memcached-service-type parameter =>
                            (memcached-configuration
                             (inherit parameter)
                             (tcp-port (assq-ref ports 'memcached))
                             (udp-port (assq-ref ports 'memcached))))
    (postgresql-service-type parameter =>
                             (postgresql-configuration
                              (inherit parameter)
                              (port (assq-ref ports 'postgresql))))
    (mongodb-service-type parameter =>
                          (mongodb-configuration
                           (inherit parameter)
                           (config-file
                            (mongodb-configuration-file
                             (assq-ref ports 'mongodb)))))
    (elasticsearch-service-type parameter =>
                                (elasticsearch-configuration
                                 (inherit parameter)
                                 (http-port (assq-ref ports 'elasticsearch))
                                 ;; TODO: The transport-port should
                                 ;; not be hardcoded.
                                 (transport-port 39300)))
    (mysql-service-type parameter =>
                        (mysql-configuration
                         (inherit parameter)
                         (port (assq-ref ports 'mysql))))))

(define (certbot-activation http-port
                            web-domain
                            domains)
  #~(begin
      (system* #$(file-append certbot "/bin/certbot")
               "certonly"
               "--cert-name" #$web-domain
               "-d" #$(string-join domains ",")
               "-n" ;; Run non-interactively
               ;; TODO: This is useful for ;; testing, but not
               ;; sensible in the long run.
               "--email" "mail@cbaines.net"
               "--agree-tos"
               "--standalone"
               "--http-01-port" #$(number->string http-port))))
