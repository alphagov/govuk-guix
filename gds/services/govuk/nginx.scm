(define-module (gds services govuk nginx)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu services web)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk tls)
  #:export (<govuk-nginx-configuration>
            govuk-nginx-configuration
            make-govuk-nginx-configuration
            govuk-nginx-configuration-http-port
            govuk-nginx-configuration-https-port
            govuk-nginx-configuration-service-and-ports
            govuk-nginx-configuration-router-config
            govuk-nginx-configuration-draft-router-config
            govuk-nginx-configuration-server-aliases
            govuk-nginx-configuration-authenticated-draft-origin?
            govuk-nginx-configuration-domain
            govuk-nginx-configuration-tls-certificate
            govuk-nginx-configuration-tls-private-key
            govuk-nginx-configuration-additional-server-blocks

            govuk-nginx-service-type))

(define (nginx-upstream-configurations service-and-ports
                                       router-config
                                       draft-router-config
                                       authenticated-draft-origin)
  (cons*
   (nginx-upstream-configuration
    (name "origin-proxy")
    (servers (list
              (string-append
               "localhost:"
               (number->string
                (router-config-public-port router-config))))))
   (nginx-upstream-configuration
    (name "draft-origin-proxy")
    (servers (list
              (string-append
               "localhost:"
               (number->string
                (if authenticated-draft-origin
                    (assq-ref service-and-ports 'authenticating-proxy)
                    (router-config-public-port draft-router-config)))))))
   (map
    (match-lambda
     ((service . port)
      (nginx-upstream-configuration
       (name (string-append (symbol->string service) "-proxy"))
       (servers (list
                 (string-append "localhost:" (number->string port)))))))
    service-and-ports)))

(define* (nginx-server-configurations base-nginx-server-configuration
                                      service-and-ports
                                      server-aliases
                                      domain
                                      #:key https?)
    (cons*
     (nginx-server-configuration
      (inherit base-nginx-server-configuration)
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body '("proxy_pass http://origin-proxy;")))
        (nginx-location-configuration
         (uri "/api/content")
         (body '("proxy_pass http://content-store-proxy;")))))
      (server-name (list (string-append "www." domain))))
     (nginx-server-configuration
      (inherit base-nginx-server-configuration)
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body '("proxy_pass http://draft-origin-proxy;
proxy_set_header Host $host:$server_port;")))
        (nginx-location-configuration
         (uri "/api/content")
         (body '("proxy_pass http://draft-content-store-proxy;")))))
      (server-name (list (string-append "draft-origin." domain))))
     (nginx-server-configuration
      (inherit base-nginx-server-configuration)
      (locations
       (cons*
        (nginx-location-configuration
         (uri "/media")
         (body '("add_header \"Access-Control-Allow-Origin\" \"*\";"
                 "add_header \"Access-Control-Allow-Methods\" \"GET, OPTIONS\";"
                 "add_header \"Access-Control-Allow-Headers\" \"origin, authorization\";"
                 "proxy_pass http://asset-manager-proxy;")))
        (map
         (match-lambda
          ((service . port)
           (nginx-location-configuration
            (uri (simple-format #f "/~A" service))
            (body (list
                   "add_header \"Access-Control-Allow-Origin\" \"*\";"
                   "add_header \"Access-Control-Allow-Methods\" \"GET, OPTIONS\";"
                   "add_header \"Access-Control-Allow-Headers\" \"origin, authorization\";"
                   (simple-format #f "proxy_pass http://~A-proxy;" service))))))
         service-and-ports)))
      (server-name (list (string-append "assets." domain))))
     (map
      (match-lambda
       ((service . port)
        (nginx-server-configuration
         (inherit base-nginx-server-configuration)
         (locations
          `(,(nginx-location-configuration
              (uri "/")
              (body (list "try_files $uri/index.html $uri.html $uri @app;")))
            ,(nginx-named-location-configuration
              (name "app")
              (body
               `(,(simple-format #f "access_log /var/log/nginx/~A.access.log;" service)
                 ,(simple-format #f "proxy_pass http://~A-proxy;" service)
                 ,@(if https?
                       '("# Set X-Forwarded-SSL for OmniAuth"
                         "proxy_set_header X-Forwarded-SSL 'on';")
                       '())
                 "proxy_set_header Host $host:$server_port;")))
            ,@(if (eq? service 'whitehall)
                  (list
                   (nginx-location-configuration
                    (uri "/government/uploads")
                    (body (list (simple-format
                                 #f
                                 "proxy_pass http://whitehall-proxy;
proxy_set_header Host whitehall-admin.~A:$server_port;"
                                 domain)))))
                  '())))
         (server-name (map
                       (lambda (name)
                         (simple-format #f "~A.~A" name domain))
                       (cons service
                             (or (assq-ref server-aliases service)
                                 '()))))
         (root (string-append "/var/apps/" (symbol->string service) "/public")))))
      service-and-ports)))

(define-record-type* <govuk-nginx-configuration>
  govuk-nginx-configuration make-govuk-nginx-configuration
  govuk-nginx-configuration?
  (http-port                      govuk-nginx-configuration-http-port
                                  (default 80))
  (https-port                     govuk-nginx-configuration-https-port
                                  (default 443))
  (service-and-ports              govuk-nginx-configuration-service-and-ports)
  (router-config                  govuk-nginx-configuration-router-config)
  (draft-router-config            govuk-nginx-configuration-draft-router-config)
  (server-aliases                 govuk-nginx-configuration-server-aliases)
  (authenticated-draft-origin?    govuk-nginx-configuration-authenticated-draft-origin?
                                  (default #t))
  (domain                         govuk-nginx-configuration-domain
                                  (default "gov.uk"))
  (tls-certificate                govuk-nginx-configuration-tls-certificate
                                  (default #f))
  (tls-private-key                govuk-nginx-configuration-tls-private-key
                                  (default #f))
  (additional-nginx-server-blocks govuk-nginx-configuration-additional-server-blocks
                                  (default '())))

(define (apply-base-nginx-server-configuration
         govuk-nginx-config nginx-server-config)
  (nginx-server-configuration
   (inherit nginx-server-config)
   (http-port (govuk-nginx-configuration-http-port govuk-nginx-config))
   (https-port (govuk-nginx-configuration-https-port govuk-nginx-config))
   (ssl-certificate (govuk-nginx-configuration-tls-certificate govuk-nginx-config))
   (ssl-certificate-key (govuk-nginx-configuration-tls-private-key govuk-nginx-config))))

(define (base-nginx-server-configuration govuk-nginx-config)
  (apply-base-nginx-server-configuration govuk-nginx-config
                                         (nginx-server-configuration)))

(define (govuk-nginx-configuration->nginx-configuration config)
  (match config
    (($ <govuk-nginx-configuration> http-port
                                    https-port
                                    service-and-ports
                                    router-config
                                    draft-router-config
                                    server-aliases
                                    authenticated-draft-origin?
                                    domain
                                    tls-certificate
                                    tls-private-key
                                    additional-nginx-server-blocks)
    (nginx-configuration
     (server-blocks
      (nginx-server-configurations (base-nginx-server-configuration config)
                                   service-and-ports
                                   server-aliases
                                   domain
                                   #:https? (number? https-port)))
     (upstream-blocks
      (nginx-upstream-configurations service-and-ports
                                     router-config
                                     draft-router-config
                                     authenticated-draft-origin?))
     (server-names-hash-bucket-size 128)))))

(define (maybe-convert-to-nginx-configuration config)
  (if (govuk-nginx-configuration? config)
      (govuk-nginx-configuration->nginx-configuration config)
      config))

(define govuk-nginx-service-type
  (service-type
   (inherit nginx-service-type)
   (extensions
    (map (lambda (se)
           (let ((target (service-extension-target se))
                 (compute (service-extension-compute se)))
             (cond ((eq? target shepherd-root-service-type)
                    (service-extension
                     target
                     (compose (@@ (gnu services web) nginx-shepherd-service)
                              maybe-convert-to-nginx-configuration)))
                   ((eq? target activation-service-type)
                    (service-extension
                     target
                     (compose (@@ (gnu services web) nginx-activation)
                              maybe-convert-to-nginx-configuration)))
                   (else (service-extension target compute)))))
         (cons*
          (service-extension
           special-files-service-type
           (lambda (nginx-config)
             `(("/etc/nginx/dev.gov.uk.cert"
                ,(development-os-tls-certificate
                  (append-map nginx-server-configuration-server-name
                              (nginx-configuration-server-blocks nginx-config))))
               ("/etc/nginx/dev.gov.uk.key" ,development-os-tls-private-key))))
          (service-extension
           profile-service-type
           (lambda (nginx-config)
             (list
              (development-os-certificates-package-for-domains
               (append-map nginx-server-configuration-server-name
                           (nginx-configuration-server-blocks nginx-config))))))
          (service-type-extensions nginx-service-type))))
   (extend (lambda (config servers)
             ((service-type-extend nginx-service-type)
              (govuk-nginx-configuration->nginx-configuration config)
              (map (lambda (server)
                     (apply-base-nginx-server-configuration config server))
                   servers))))))
