(define-module (gds services govuk plek)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gds services)
  #:use-module (gds services rails)
  #:export (<plek-config>
            plek-config
            plek-config?
            plek-config-govuk-app-domain
            plek-config-govuk-asset-root
            plek-config-govuk-website-root
            plek-config-govuk-asset-host
            plek-config-dev-domain
            plek-config-service-ports
            plek-config-service-uri-function

            plek-config->environment-variables
            make-custom-plek-config
            filter-plek-config-service-ports
            update-service-extension-parameters-for-plek-config
            extend-service-type-with-plek))

(define-record-type* <plek-config>
  plek-config make-plek-config
  plek-config?
  (govuk-app-domain plek-config-govuk-app-domain
                    (default "publishing.service.gov.uk"))
  (govuk-asset-root plek-config-govuk-asset-root
                    (default "https://static.publishing.service.gov.uk"))
  (govuk-website-root plek-config-govuk-website-root
                      (default "https://www.gov.uk"))
  (govuk-asset-host plek-config-govuk-asset-host
                    (default "https://static.publishing.service.gov.uk"))
  (dev-domain plek-config-dev-domain
              (default #f))
  (service-ports plek-config-service-ports
                 (default '()))
  (service-uri-function plek-config-service-uri-function
                        (default #f)))

(define (service-port-from-plek-config plek-config service)
  (or
   (assq-ref
    (plek-config-service-ports plek-config)
    service)
   (begin
     (display "plek-config-service-ports: ")
     (display (plek-config-service-ports plek-config))
     (display "\n")
     (error "Port missing from plek-config for" service))))

(define (service-symbol->environment-variable-string service)
  (string-map
   (lambda (c)
     (if (eq? c #\-) #\_ c))
   (string-upcase
    (symbol->string service))))

(define* (make-custom-plek-config
          service-ports
          #:optional #:key
          (govuk-app-domain (plek-config-govuk-app-domain (plek-config)))
          (use-https? #t)
          (port (if use-https? 443 80)))
  (let
      ((scheme
        (if use-https? "https" "http"))
       (string-port
        (cond
         ((and use-https? (= port 443)) "")
         ((and (not use-https?) (= port 80)) "")
         (else (string-append
                ":" (number->string port))))))
    (plek-config
     (govuk-app-domain govuk-app-domain)
     (dev-domain
      (if use-https?
         ;; Plek uses DEV_DOMAIN to decide whether to use http or https
         govuk-app-domain
         #f))
     (govuk-asset-root
      (string-append scheme "://static." govuk-app-domain string-port))
     (govuk-website-root
      (string-append scheme "://www." govuk-app-domain string-port))
     (govuk-asset-host
      (string-append scheme "://static." govuk-app-domain string-port))
     (service-ports service-ports)
     (service-uri-function
      (lambda (service port)
        ;; For the default implementation, deliberately don't use
        ;; port, and just use string-port to route connections through
        ;; NGinx
        (string-append
         scheme
         "://"
         (symbol->string service)
         "."
         govuk-app-domain
         string-port))))))

(define* (plek-config->environment-variables plek-config)
  (cons*
   (cons "GOVUK_APP_DOMAIN" (plek-config-govuk-app-domain plek-config))
   (cons "GOVUK_ASSET_ROOT" (plek-config-govuk-asset-root plek-config))
   (cons "GOVUK_WEBSITE_ROOT" (plek-config-govuk-website-root plek-config))
   (cons "GOVUK_ASSET_HOST" (plek-config-govuk-asset-host plek-config))
   (map
    (match-lambda
      ((service . port)
       (cons
        (string-append
         "PLEK_SERVICE_"
         (service-symbol->environment-variable-string service)
         "_URI")
        ((plek-config-service-uri-function plek-config) service port))))
    (plek-config-service-ports plek-config))))

(define (filter-plek-config-service-ports pc services)
  (plek-config
   (inherit pc)
   (service-ports
    (lset-difference
     eq?
     (plek-config-service-ports pc)
     services))))

(define (update-service-extension-parameters-for-plek-config
         service-name
         parameters)
  (let
      ((plek-config (find plek-config? parameters))
       (shepherd-service (find
                          shepherd-service?
                          parameters)))
    (map
     (lambda (parameter)
       (cond
        ((service-startup-config? parameter)
         (service-startup-config-with-additional-environment-variables
          parameter
          (plek-config->environment-variables
           (filter-plek-config-service-ports
            plek-config
            (shepherd-service-requirement shepherd-service)))))
        ((rails-app-config? parameter)
         (rails-app-config
          (inherit parameter)
          (port (service-port-from-plek-config
                 plek-config
                 service-name))))
        (else
         parameter)))
     parameters)))

(define (extend-service-type-with-plek type)
  (service-type
   (inherit type)
   (extensions
    (map
     (lambda (se)
       (service-extension
        (service-extension-target se)
        (lambda (parameters)
          (apply
           (service-extension-compute se)
           (list
            (update-service-extension-parameters-for-plek-config
             (service-type-name type)
             parameters))))))
     (service-type-extensions type)))))
