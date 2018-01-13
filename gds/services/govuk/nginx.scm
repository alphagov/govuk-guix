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
            govuk-nginx-configuration?
            make-govuk-nginx-configuration
            govuk-nginx-configuration-http-port
            govuk-nginx-configuration-https-port
            govuk-nginx-configuration-service-and-ports
            govuk-nginx-configuration-origin-service
            govuk-nginx-configuration-draft-origin-service
            govuk-nginx-configuration-server-aliases
            govuk-nginx-configuration-web-domain
            govuk-nginx-configuration-app-domain
            govuk-nginx-configuration-tls-certificate
            govuk-nginx-configuration-tls-private-key
            govuk-nginx-configuration-additional-server-blocks

            govuk-nginx-service-type))

(define (nginx-upstream-configurations service-and-ports)
  (map
   (match-lambda
     ((service . port)
      (nginx-upstream-configuration
       (name (string-append (symbol->string service) "-proxy"))
       (servers (list
                 (string-append "localhost:" (number->string port)))))))
   service-and-ports))

(define* (nginx-server-configurations base-nginx-server-configuration
                                      service-and-ports
                                      server-aliases
                                      origin-service
                                      draft-origin-service
                                      web-domain
                                      app-domain
                                      #:key https?
                                      include-port-in-host-header?)
  (define proxy_set_header
    (if include-port-in-host-header?
        "proxy_set_header Host $host:$server_port;"
        "proxy_set_header Host $host;"))

    (cons*
     (nginx-server-configuration
      (inherit base-nginx-server-configuration)
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body (list (simple-format
                      #f "proxy_pass http://~A-proxy;" origin-service))))
        (nginx-location-configuration
         (uri "/api/content")
         (body '("proxy_pass http://content-store-proxy;")))))
      (server-name (list (string-append web-domain))))
     (nginx-server-configuration
      (inherit base-nginx-server-configuration)
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body `(,(simple-format
                   #f "proxy_pass http://~A-proxy;\n" draft-origin-service)
                 ,proxy_set_header
                 ,@(if https?
                       '("# Set X-Forwarded-SSL for OmniAuth"
                         "proxy_set_header X-Forwarded-SSL 'on';")
                       '()))))
        (nginx-location-configuration
         (uri "/api/content")
         (body '("proxy_pass http://draft-content-store-proxy;")))))
      (server-name (list (string-append "draft-origin." app-domain))))
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
        (nginx-location-configuration
         (uri "~ /cloud-storage-proxy/(.*)")
         (body '("internal;"
                 "set $download_url $1$is_args$args;"
                 "proxy_pass $download_url;")))
        (nginx-location-configuration
         (uri "~ /fake-s3/(.*)")
         (body `(,proxy_set_header
                 "proxy_pass http://asset-manager-proxy;")))
        (map
         (match-lambda
          ((service . port)
           (nginx-location-configuration
            (uri (simple-format #f "/~A" service))
            (body `("add_header \"Access-Control-Allow-Origin\" \"*\";"
                    "add_header \"Access-Control-Allow-Methods\" \"GET, OPTIONS\";"
                    "add_header \"Access-Control-Allow-Headers\" \"origin, authorization\";"
                    ,@(if https?
                          '("# Set X-Forwarded-SSL for OmniAuth"
                            "proxy_set_header X-Forwarded-SSL 'on';")
                          '())
                    ,(simple-format #f "proxy_pass http://~A-proxy;" service))))))
         service-and-ports)))
      (server-name (list (string-append "assets." app-domain))))
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
                 ,proxy_set_header)))
            ,@(if (eq? service 'whitehall)
                  (list
                   (nginx-location-configuration
                    (uri "/government/uploads")
                    (body (list (simple-format
                                 #f
                                 "proxy_pass http://whitehall-proxy;
proxy_set_header Host whitehall-admin.~A~A:$server_port;"
                                 app-domain
                                 (if include-port-in-host-header?
                                     ":$server_port"
                                     ""))))))
                  '())))
         (server-name (map
                       (lambda (name)
                         (simple-format #f "~A.~A" name app-domain))
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
  ;; This is useful when the intended port to connect to nginx on,
  ;; isn't the port it's listening on, e.g. if it's running on 8080,
  ;; but traffic comes in on port 80, and is redirected by iptables.
  ;;
  ;; This is a substitute for more complex network handling.
  (include-port-in-host-header?   govuk-nginx-configuration-include-port-in-host-header?
                                  (default #f))
  (service-and-ports              govuk-nginx-configuration-service-and-ports
                                  (default '()))
  (origin-service                 govuk-nginx-configuration-origin-service
                                  (default 'router))
  (draft-origin-service           govuk-nginx-configuration-draft-origin-service
                                  (default 'authenticating-proxy))
  (server-aliases                 govuk-nginx-configuration-server-aliases
                                  (default '()))
  (web-domain                     govuk-nginx-configuration-web-domain
                                  (default "www.gov.uk"))
  (app-domain                     govuk-nginx-configuration-app-domain
                                  (default "publishing.service.gov.uk"))
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
   (listen
    `(,(number->string (govuk-nginx-configuration-http-port govuk-nginx-config))
      ,@(or (and=> (govuk-nginx-configuration-https-port govuk-nginx-config)
                   (lambda (https-port)
                     (list (string-append (number->string https-port) " ssl"))))
            '())))
   (ssl-certificate (govuk-nginx-configuration-tls-certificate govuk-nginx-config))
   (ssl-certificate-key (govuk-nginx-configuration-tls-private-key govuk-nginx-config))))

(define (base-nginx-server-configuration govuk-nginx-config)
  (apply-base-nginx-server-configuration govuk-nginx-config
                                         (nginx-server-configuration)))

(define (govuk-nginx-configuration->nginx-configuration config)
  (match config
    (($ <govuk-nginx-configuration> http-port
                                    https-port
                                    include-port-in-host-header?
                                    service-and-ports
                                    origin-service
                                    draft-origin-service
                                    server-aliases
                                    web-domain
                                    app-domain
                                    tls-certificate
                                    tls-private-key
                                    additional-nginx-server-blocks)
     (nginx-configuration
      (server-blocks
       (nginx-server-configurations (base-nginx-server-configuration config)
                                    service-and-ports
                                    server-aliases
                                    origin-service
                                    draft-origin-service
                                    web-domain
                                    app-domain
                                    #:https? (number? https-port)
                                    #:include-port-in-host-header?
                                    include-port-in-host-header?))
      (upstream-blocks
       (nginx-upstream-configurations service-and-ports))
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
             `(("/etc/nginx/cert"
                ,(development-os-tls-certificate
                  (append-map nginx-server-configuration-server-name
                              (nginx-configuration-server-blocks nginx-config))))
               ("/etc/nginx/key" ,development-os-tls-private-key))))
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
                   servers))))
   (default-value (govuk-nginx-configuration))))
