(define-module (gds systems govuk publishing-e2e-tests-isolated)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gds systems govuk publishing-e2e-tests))

(define publishing-e2e-tests-isolated-os
  (operating-system
    (inherit publishing-e2e-tests-os)
    (services
     (cons
      (service static-networking-service-type
               (list (static-networking (interface "lo")
                                        (ip "127.0.0.1")
                                        (provision '(loopback)))))
      (filter
       (lambda (s)
         (not (eq?
               'dummy-loopback-service
               (service-type-name
                (service-kind s)))))
       (operating-system-user-services
        publishing-e2e-tests-os))))))

publishing-e2e-tests-isolated-os
