(define-module (gds systems govuk publishing-e2e-tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu system)
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
  #:use-module (gds systems utils)
  #:use-module (gds systems govuk development))

(define (services-in-rails-production-environment services)
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        rails-app-config?
        (lambda (config)
          (update-rails-app-config-environment
           "production"
           config))))))
   services))

(define setup-functions
  (list
   services-in-rails-production-environment
   ;; This is not a real value that the gds-sso gem uses, as it
   ;; just checks if the value is "real" or not.
   (cut use-gds-sso-strategy <> "mock")))

(define services
  (modify-services
      ((apply compose (reverse setup-functions))
       (append
        (setup-services (list publishing-e2e-tests-service))
        (operating-system-user-services development-os)))
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
    (govuk-nginx-service-type
     parameter =>
     (govuk-nginx-configuration
      (inherit parameter)
      (authenticated-draft-origin? #f)
      (additional-nginx-server-blocks
       (list
        (nginx-server-configuration
         (inherit (govuk-nginx-server-configuration-base))
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
          (or (find (lambda (service)
                      (eq? 'tailon
                           (service-type-name
                            (service-kind service))))
                    services)
              (error "Could not find tailon"))
          base-services)
   (operating-system
    (inherit development-os)
    (services services))))

publishing-e2e-tests-os
