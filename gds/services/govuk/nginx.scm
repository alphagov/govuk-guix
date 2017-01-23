(define-module (gds services govuk nginx)
  #:use-module (ice-9 match)
  #:use-module (gnu services web)
  #:use-module (gds services govuk))

(define-public (nginx
                service-and-ports
                router-config
                draft-router-config)
  (nginx-service
   #:upstream-list
   (cons*
    (nginx-upstream-configuration
     (name "origin-proxy")
     (server (string-append
              "localhost:"
              (number->string
               (router-config-public-port router-config)))))
    (nginx-upstream-configuration
     (name "draft-origin-proxy")
     (server (string-append
              "localhost:"
              (number->string
               (router-config-public-port draft-router-config)))))
    (map
     (match-lambda
       ((service . port)
        (nginx-upstream-configuration
         (name (string-append (symbol->string service) "-proxy"))
         (server (string-append "localhost:" (number->string port))))))
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
          (body '("proxy_pass http://origin-proxy;")))))
       (server-name (list "www.guix-dev.gov.uk")))
      (nginx-server-configuration
       (inherit base)
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://draft-origin-proxy;")))))
       (server-name (list "draft-origin.guix-dev.gov.uk")))
      (nginx-server-configuration
       (inherit base)
       (locations
        (list
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
             (nginx-location-configuration
              (name "@app")
              (body (list (simple-format
                           #f
                           "proxy_pass http://~A-proxy;"
                           (symbol->string service)))))))
           (server-name (list (string-append
                               (symbol->string service)
                               ".guix-dev.gov.uk")))
           (root (string-append "/var/lib/" (symbol->string service) "/public")))))
       service-and-ports)))))
