(define-module (gds services)
  #:use-module (guix records)
  #:export (<service-startup-config>
            service-startup-config
            service-startup-config?
            service-startup-config-environment-variables
            service-startup-config-pre-startup-script))

(define-record-type* <service-startup-config>
  service-startup-config make-service-startup-config
  service-startup-config?
  (environment-variables service-startup-config-environment-variables
                         (default '()))
  (pre-startup-scripts service-startup-config-pre-startup-scripts
                       (default '())))
