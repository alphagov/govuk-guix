(define-module (gds systems govuk test)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
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
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds systems utils)
  #:use-module (gds systems govuk production)
  #:export (govuk-test-os))

(define (setup-services-for-test-os services)
  (define apply-general-configuration
    (let
        ((service-setup-functions
          ;; Service setup functions, order alphabetically if possible,
          ;; and add comments to indicate any interdependencies in the
          ;; configuration
          (list
           (cut set-routing-configuration-for-services <>
                #:use-high-ports? #t
                #:use-https? 'development
                #:app-domain "test.gov.uk"
                #:web-domain "test.gov.uk")
           ;; TODO: ensure-database-user-exists-on-service-startup and
           ;; configure-rails-services-database setup must happen after
           ;; update-database-connection-config-ports, or the wrong
           ;; database connection configuration is used.
           (cut map ensure-database-user-exists-on-service-startup <>)
           (cut map run-db:setup-if-postgresql-or-mysql-is-used <>))))

      (apply compose (reverse service-setup-functions))))

  (modify-services (apply-general-configuration services)
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
    (specialist-publisher-service-type
     parameters =>
     (map
      (lambda (parameter)
        (if (service-startup-config? parameter)
            (service-startup-config-add-pre-startup-scripts
             parameter
             `((publish-finders
                . ,#~(lambda ()
                       (run-command "rake" "publishing_api:publish_finders")))))
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
      parameters))))

(define govuk-test-os
  (operating-system
    (inherit govuk-production-os)
    (host-name "govuk-test")
    (services (setup-services-for-test-os
               (operating-system-user-services govuk-production-os)))))
