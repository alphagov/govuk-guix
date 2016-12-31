(define-module (gds services)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:export (<service-startup-config>
            service-startup-config
            service-startup-config?
            service-startup-config-environment-variables
            service-startup-config-pre-startup-scripts

            service-startup-config-with-additional-environment-variables
            service-startup-config-add-pre-startup-scripts))

(define-record-type* <service-startup-config>
  service-startup-config make-service-startup-config
  service-startup-config?
  (environment-variables service-startup-config-environment-variables
                         (default '()))
  (pre-startup-scripts service-startup-config-pre-startup-scripts
                       (default '())))

(define (service-startup-config-with-additional-environment-variables
         ssc
         environment-variables)
  (service-startup-config
   (inherit ssc)
   (environment-variables
    (append
     environment-variables
     (service-startup-config-environment-variables ssc)))))

(define (service-startup-config-add-pre-startup-scripts
         ssc
         scripts)
  (service-startup-config
   (inherit ssc)
   (pre-startup-scripts
    (append
     (let
         ((new-keys
           (map car scripts)))
       (filter
        (match-lambda
          ((key . value)
           (not (memq key new-keys))))
        (service-startup-config-pre-startup-scripts ssc)))
     scripts))))
