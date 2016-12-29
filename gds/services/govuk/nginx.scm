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
