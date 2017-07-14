(define-module (gds services govuk nginx)
  #:use-module (ice-9 match)
  #:use-module (gnu services web)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk router)
  #:export (<govuk-nginx-configuration>
            govuk-nginx-configuration
            make-govuk-nginx-configuration
            govuk-nginx-configuration-service-and-ports
            govuk-nginx-configuration-router-config
            govuk-nginx-configuration-draft-router-config
            govuk-nginx-configuration-server-aliases
            govuk-nginx-configuration-authenticated-draft-origin?
            govuk-nginx-configuration-domain

            govuk-nginx-service-type
            govuk-nginx-server-configuration-base))

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

(define (govuk-nginx-server-configuration-base)
  (nginx-server-configuration
   (http-port 50080)
   (https-port 50443)
   (ssl-certificate #f)
   (ssl-certificate-key #f)))

(define (nginx-server-configurations service-and-ports
                                     server-aliases
                                     domain)
  (let ((base (govuk-nginx-server-configuration-base)))
    (cons*
     (nginx-server-configuration
      (inherit base)
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
      (inherit base)
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
      (inherit base)
      (locations
       (cons*
        (nginx-location-configuration
         (uri "/media")
         (body '("proxy_pass http://asset-manager-proxy;")))
        (map
         (match-lambda
          ((service . port)
           (nginx-location-configuration
            (uri (simple-format #f "/~A" service))
            (body (list (simple-format #f "proxy_pass http://~A-proxy;" service))))))
         service-and-ports)))
      (server-name (list (string-append "assets." domain))))
     (map
      (match-lambda
       ((service . port)
        (nginx-server-configuration
         (inherit base)
         (locations
          (list
           (nginx-location-configuration
            (uri "/")
            (body '("try_files $uri/index.html $uri.html $uri @app;")))
           (nginx-named-location-configuration
            (name "app")
            (body (list (simple-format
                         #f
                         "proxy_pass http://~A-proxy;
proxy_set_header Host $host:$server_port;"
                         (symbol->string service)))))))
         (server-name (map
                       (lambda (name)
                         (simple-format #f "~A.~A" name domain))
                       (cons service
                             (or (assq-ref server-aliases service)
                                 '()))))
         (root (string-append "/var/apps/" (symbol->string service) "/public")))))
      service-and-ports))))

(define-record-type* <govuk-nginx-configuration>
  govuk-nginx-configuration make-govuk-nginx-configuration
  govuk-nginx-configuration?
  (service-and-ports              govuk-nginx-configuration-service-and-ports)
  (router-config                  govuk-nginx-configuration-router-config)
  (draft-router-config            govuk-nginx-configuration-draft-router-config)
  (server-aliases                 govuk-nginx-configuration-server-aliases)
  (authenticated-draft-origin?    govuk-nginx-configuration-authenticated-draft-origin?
                                  (default #t))
  (domain                         govuk-nginx-configuration-domain
                                  (default "gov.uk"))
  (additional-nginx-server-blocks govuk-nginx-configuration-additional-server-blocks
                                  (default '())))


(define govuk-nginx-configuration->nginx-configuration
  (match-lambda
   (($ <govuk-nginx-configuration> service-and-ports
                                   router-config
                                   draft-router-config
                                   server-aliases
                                   authenticated-draft-origin?
                                   domain)
    (nginx-configuration
     (server-blocks
      (nginx-server-configurations service-and-ports
                                   server-aliases
                                   domain))
     (upstream-blocks
      (nginx-upstream-configurations service-and-ports
                                     router-config
                                     draft-router-config
                                     authenticated-draft-origin?))))))

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
         (service-type-extensions nginx-service-type)))
   (extend (lambda (config servers)
             ((service-type-extend nginx-service-type)
              (govuk-nginx-configuration->nginx-configuration config)
              servers)))))
