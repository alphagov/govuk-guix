(define-module (gds services govuk admin-environment-style)
  #:use-module (gnu services)
  #:use-module (gds services)
  #:export (use-govuk-admin-template-environment-label))

(define (use-govuk-admin-template-environment-label services label)
  (map
   (lambda (s)
     (service
      (service-kind s)
      (if
       (list? (service-parameters s))
       (map
        (lambda (parameter)
          (if
           (service-startup-config? parameter)
           (service-startup-config-with-additional-environment-variables
            parameter
            `(("GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_LABEL" . ,label)))
           parameter))
        (service-parameters s))
       (service-parameters s))))
   services))
