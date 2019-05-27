(define-module (gds services)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:export (<service-startup-config>
            service-startup-config
            service-startup-config?
            service-startup-config-environment-variables
            service-startup-config-pre-startup-scripts
            service-startup-config-root-pre-startup-scripts

            service-startup-config-with-additional-environment-variables
            service-startup-config-add-pre-startup-scripts
            service-extensions-modify-parameters
            service-type-extensions-modify-parameters
            run-pre-startup-scripts-gexp

            set-aws-xray-context-missing))

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
     (filter
      (match-lambda
        ((key . value)
         (not (assoc-ref environment-variables key))))
      (service-startup-config-environment-variables ssc))))))

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

(define (service-extensions-modify-parameters service-extensions f)
  (map
   (lambda (se)
     (service-extension
      (service-extension-target se)
      (lambda (parameters)
        ((service-extension-compute se)
         (f parameters)))))
   service-extensions))

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

(define* (run-pre-startup-scripts-gexp name pre-startup-scripts
                                       #:key home)
  (let
      ((script-gexps
        (map
         (match-lambda
           ((key . script)
            #~(lambda ()
                (simple-format #t "Running pre-startup-script ~A\n" '#$key)

                (let* ((start-time (get-internal-run-time))
                       (result
                        (catch
                          #t
                          #$script
                          (lambda (key . args) (cons key args))))
                       (seconds-taken
                        (/ (- (get-internal-run-time) start-time)
                           internal-time-units-per-second)))
                  (if (eq? result #t)
                      (begin
                        (format
                         #t "pre-startup-script ~a succeeded (~1,2f seconds)\n"
                         '#$key seconds-taken)
                        #t)
                      (begin
                        (format
                         #t "pre-startup-script ~a failed (~1,2f seconds)\n"
                         '#$key seconds-taken)
                        (format #t "result: ~A\n" result)
                        #f))))))
         pre-startup-scripts)))
    (if (null? script-gexps)
        #~#t
        (with-imported-modules '((gds build utils))
          #~(let ((old-HOME (getenv "HOME")))
              (use-modules (gds build utils)
                           (ice-9 format))
              (when #$home
                (setenv "HOME" #$home))
              (simple-format
               #t
               "Running ~A startup scripts for ~A\n"
               #$(length script-gexps)
               '#$name)
              (for-each
               (lambda (key) (simple-format #t "  - ~A\n" key))
               '#$(map car pre-startup-scripts))
              (let ((overall-result
                     (let run ((scripts (list #$@script-gexps)))
                       (if (null? scripts)
                           #t
                           (let ((result ((car scripts))))
                             (if (eq? result #t)
                                 (run (cdr scripts))
                                 #f))))))
                (when #$home
                  (setenv "HOME" old-HOME))
                overall-result))))))

(define (set-aws-xray-context-missing services value)
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
            `(("AWS_XRAY_CONTEXT_MISSING" . ,value)))
           parameter))
        (service-parameters s))
       (service-parameters s))))
   services))
