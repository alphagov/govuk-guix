(define-module (gds systems govuk publishing-e2e-tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services rails)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk nginx)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk publishing-e2e-tests)
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds systems utils)
  #:use-module (gds systems govuk development))

(define (services-in-rails-production-environment services)
  (map
   (lambda (service)
     (update-rails-app-config-environment-for-service
      "production"
      service))
   services))

(define services
  (modify-services
      (set-routing-configuration-for-services
       (services-in-rails-production-environment
        (append
         (setup-services (list publishing-e2e-tests-service))
         (operating-system-user-services development-os)))
       #:use-high-ports? #t
       #:use-https? #t
       #:app-domain "dev.gov.uk"
       #:web-domain "www.dev.gov.uk")
    (asset-manager-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
            (service-startup-config-with-additional-environment-variables
             parameter
             '(("PROXY_PERCENTAGE_OF_ASSET_REQUESTS_TO_S3_VIA_NGINX" . "100")
               ("PROXY_PERCENTAGE_OF_WHITEHALL_ASSET_REQUESTS_TO_S3_VIA_NGINX" . "100")
               ("FAKE_S3_HOST" . "http://asset-manager-proxy")
               ("ALLOW_FAKE_S3_IN_PRODUCTION_FOR_PUBLISHING_E2E_TESTS" . "true")))
            parameter))
      parameters))
    (travel-advice-publisher-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
         (service-startup-config-add-pre-startup-scripts
          parameter
          `((db-seed . ,#~(lambda ()
                            (run-command "rake" "db:seed")))))
         parameter))
      parameters))
    (publisher-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
            (service-startup-config-with-additional-environment-variables
             parameter
             '(("DISABLE_EMAIL" . "true")))
            parameter))
      parameters))
    (specialist-publisher-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
            (service-startup-config-add-pre-startup-scripts
             parameter
             `((db-seed
                . ,#~(lambda ()
                       (run-command "rake" "db:seed")))
               (publish-finders
                . ,#~(lambda ()
                       (run-command "rake" "publishing_api:publish_finders")))))
            parameter))
      parameters))
    (rummager-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
            (service-startup-config-add-pre-startup-scripts
             parameter
             `((create-all-indices
                . ,#~(lambda ()
                       (setenv "RUMMAGER_INDEX" "all")
                       (run-command "bundle" "exec" "rake"
                                    "rummager:create_all_indices")))))
            parameter))
      parameters))
    (router-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (router-config? parameter)
            (router-config
             (inherit parameter)
             ;; Performance for the initial requests to frontend apps seems
             ;; to be poor, so until this is improved, extend the timeout for
             ;; the router
             (backend-header-timeout "60s"))
            parameter))
      parameters))
    (signon-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (signon-config? parameter)
            (signon-config-with-random-secrets parameter)
            parameter))
      parameters))
    (govuk-nginx-service-type
     parameter =>
     (govuk-nginx-configuration
      (inherit parameter)
      (https-port 50443)
      (tls-certificate "/etc/nginx/dev.gov.uk.cert")
      (tls-private-key "/etc/nginx/dev.gov.uk.key")
      (additional-nginx-server-blocks
       (list
        (nginx-server-configuration
         (server-name '("publishing-e2e-tests.dev.gov.uk"))
         (root "/var/apps/publishing-e2e-tests")
         (locations
          (list
           (nginx-location-configuration
            (uri "/")
            (body '("autoindex on;"))))))))))))

(define-public publishing-e2e-tests-os
  (system-without-unnecessary-services
   (cons* (find (lambda (s) (eq? (service-kind s)
                                 publishing-e2e-tests-service-type))
                services)
          ;; TODO: Currently nothing depends on authenticating-proxy,
          ;; so its removed
          (find (lambda (s) (eq? (service-type-name (service-kind s))
                                 'authenticating-proxy))
                services)
          base-services)
   (operating-system
    (inherit development-os)
    (packages (cons*
               openssl
               nss-certs
               (operating-system-packages development-os)))
    (services services))))

publishing-e2e-tests-os
