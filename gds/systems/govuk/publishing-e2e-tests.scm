(define-module (gds systems govuk publishing-e2e-tests)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services rails)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services govuk)
  #:use-module (gds systems govuk development))

(define services
  (setup-services
   (cons
    publishing-e2e-tests-service
    (operating-system-user-services development-os))))

(operating-system
  (inherit development-os)
  (services
   (modify-services
       (map
        (lambda (s)
          (if (and
               (list? (service-parameters s))
               (find rails-app-config? (service-parameters s))
               (any
                (lambda (p)
                  (or (postgresql-connection-config? p)
                      (mysql-connection-config? p)))
                (service-parameters s)))
              (rails-run-db:setup s)
              s))
        (map
         setup-blank-databases-on-service-startup
         services))
     (specialist-publisher-service-type
      parameters =>
      (map
       (lambda (parameter)
         (if
          (service-startup-config?
           parameter)
          (service-startup-config-add-pre-startup-scripts
           parameter
           `((db-seed
              . ,(run-command "rake" "db:seed"))
             (publish-finders
              . ,(run-command "rake" "publishing_api:publish_finders"))
             ;; (grant-permissions
             ;; . ,(run-command "rake" "permissions:grant[David Heath]"))
             ))
          parameter))
       parameters)))))
