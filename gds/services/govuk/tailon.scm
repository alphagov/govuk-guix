(define-module (gds services govuk tailon)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services web)
  #:use-module (gds services govuk nginx)
  #:use-module (gds services govuk signon))

(define-public govuk-tailon-service-type
  (service-type
   (inherit tailon-service-type)
   (extensions
    (cons*
     (service-extension govuk-nginx-service-type
                        (const
                         (list
                          (nginx-server-configuration
                           (locations
                            (list
                             (nginx-location-configuration
                              (uri "/ws")
                              (body '("
                   proxy_pass http://localhost:54001/ws;
                   proxy_http_version 1.1;
                   proxy_set_header Upgrade $http_upgrade;
                   proxy_set_header Connection \"upgrade\";
                  ")))
                             (nginx-location-configuration
                              (uri "/")
                              (body '("proxy_pass http://localhost:54001;")))))
                           (server-name (list
                                         "tailon.dev.gov.uk"
                                         "logs.dev.gov.uk"))))))
     (service-type-extensions tailon-service-type)))))
