(define-module (gds services)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:export (<service-startup-config>
            service-startup-config
            service-startup-config?
            service-startup-config-environment-variables
            service-startup-config-pre-startup-scripts
            service-startup-config-root-pre-startup-scripts

            service-startup-config-with-additional-environment-variables
            service-startup-config-add-pre-startup-scripts
            service-type-extensions-modify-parameters))

(define-record-type* <service-startup-config>
  service-startup-config make-service-startup-config
  service-startup-config?
  (environment-variables service-startup-config-environment-variables
                         (default '()))
  (pre-startup-scripts service-startup-config-pre-startup-scripts
                       (default '()))
  (root-pre-startup-scripts service-startup-config-root-pre-startup-scripts
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

(define* (service-startup-config-add-pre-startup-scripts
          ssc
          scripts
          #:optional #:key (run-as-root #f))
  (define (filter-out-replaced-scripts old-scripts)
    (let
        ((new-keys
          (map car scripts)))
      (filter
       (match-lambda
         ((key . value)
          (not (memq key new-keys))))
       old-scripts)))

  (if run-as-root
      (service-startup-config
       (inherit ssc)
       (root-pre-startup-scripts
        (append
         (filter-out-replaced-scripts
          (service-startup-config-root-pre-startup-scripts ssc))
         scripts)))
      (service-startup-config
       (inherit ssc)
       (pre-startup-scripts
        (append
         (filter-out-replaced-scripts
          (service-startup-config-pre-startup-scripts ssc))
         scripts)))))

(define (service-type-extensions-modify-parameters type f)
  (service-type
   (inherit type)
   (extensions
    (map
     (lambda (se)
       (service-extension
        (service-extension-target se)
        (lambda (parameters)
          ((service-extension-compute se)
           (f parameters)))))
     (service-type-extensions type)))))
