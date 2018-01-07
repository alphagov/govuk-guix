(define-module (gds services govuk content-access-limits)
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:use-module (gds services govuk)
  #:export (set-jwt-auth-secret))

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
