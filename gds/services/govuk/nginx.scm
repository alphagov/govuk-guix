(define-module (gds services govuk nginx)
  #:use-module (ice-9 match)
  #:use-module (gnu services web)
  #:use-module (gds services govuk))

(define-public govuk-nginx-service
  (lambda* (service-and-ports
            router-config
            draft-router-config
            server-aliases
            #:optional #:key
            (authenticated-draft-origin #t))
    (nginx-service
     #:upstream-list
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
       service-and-ports))
     #:server-list
     (let
         ((base
           (nginx-server-configuration
            (http-port 50080)
            (https-port 50443)
            (ssl-certificate #f)
            (ssl-certificate-key #f))))
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
         (server-name (list "www.guix-dev.gov.uk")))
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
         (server-name (list "draft-origin.guix-dev.gov.uk")))
        (nginx-server-configuration
         (inherit base)
         (locations
          (list
           (nginx-location-configuration
            (uri "/frontend")
            (body '("proxy_pass http://frontend-proxy;")))
           (nginx-location-configuration
            (uri "/info-frontend")
            (body '("proxy_pass http://info-frontend-proxy;")))
           (nginx-location-configuration
            (uri "/specialist-frontend")
            (body '("proxy_pass http://specialist-frontend-proxy;")))
           (nginx-location-configuration
            (uri "/static")
            (body '("proxy_pass http://static-proxy;")))))
         (server-name (list "assets.guix-dev.gov.uk")))
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
             (server-name (cons (string-append
                                 (symbol->string service)
                                 ".guix-dev.gov.uk")
                                (or (assq-ref server-aliases service)
                                    '())))
             (root (string-append "/var/apps/" (symbol->string service) "/public")))))
         service-and-ports))))))
