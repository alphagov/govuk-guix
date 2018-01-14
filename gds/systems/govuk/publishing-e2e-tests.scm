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
  #:use-module (gds systems govuk base)
  #:use-module (gds systems govuk test)
  #:export (publishing-e2e-tests-os))

(define services
  (modify-services
      (operating-system-user-services govuk-test-os)
    (govuk-nginx-service-type
     parameter =>
     (govuk-nginx-configuration
      (inherit parameter)
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

(define plek-config
  (any (lambda (service)
         (and (list? (service-value service))
              (find plek-config? (service-value service))))
       (operating-system-user-services govuk-test-os)))

(define publishing-e2e-tests-os
  (system-without-unnecessary-services
   (cons* (find (lambda (s) (eq? (service-kind s)
                                 publishing-e2e-tests-service-type))
                services)
          ;; TODO: Currently nothing depends on authenticating-proxy,
          ;; so its removed
          (find (lambda (s) (eq? (service-type-name (service-kind s))
                                 'authenticating-proxy))
                services)
          (filter (lambda (s)
                    (memq (service-kind s)
                          (map service-kind base-services)))
                  services))
   (operating-system
     (inherit govuk-test-os)
     (services services)
     (hosts-file
      (plek-config->hosts-file plek-config)))))

publishing-e2e-tests-os
