(define-module (gds systems utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gds services utils)
  #:export (system-without-unnecessary-services
            update-system-services-package-source-from-environment))

(define-public (shepherd-services-from-service service)
  (let ((shepherd-root-service-type-extension
         (find (lambda (service-extension)
                 (eq? (service-extension-target
                       service-extension)
                      shepherd-root-service-type))
               (service-type-extensions (service-kind service)))))
    (if shepherd-root-service-type-extension
         ((service-extension-compute shepherd-root-service-type-extension)
          (service-parameters service))
         '())))

(define (get-requirement->service-alist services)
  (concatenate
   (map
    (lambda (service)
      (concatenate
       (map
        (lambda (shepherd-service)
          (map (lambda (provisioned-name)
                 (cons provisioned-name service))
               (shepherd-service-provision shepherd-service)))
        (shepherd-services-from-service service))))
    services)))

(define (service->name service)
  (service-type-name (service-kind service)))

(define (service-names services)
  (map service->name services))

(define-public (find-missing-requirements
                services requirement->service-alist)
  (define (services-for-requirements service)
    (filter-map
     (lambda (requirement)
       (assq-ref requirement->service-alist requirement))
     (append-map
      shepherd-service-requirement
      (shepherd-services-from-service service))))

  (fold
   (lambda (service missing-requirements)
     (append
      (fold
       (lambda (service new-services)
         (if (or (member service services)
                 (member service missing-requirements)
                 (member service new-services))
             new-services
             (cons
              service
              new-services)))
       '()
       (services-for-requirements service))
      missing-requirements))
   '()
   services))

(define-public (find-missing-extension-targets services all-services)
  (let* ((service-types (map service-kind services))
         (extensions
          (append-map service-type-extensions service-types))
         (missing-service-types
          (lset-difference
           eq?
           (delete-duplicates
            (map service-extension-target extensions))
           service-types)))
    (filter
     (lambda (service)
       (member (service-kind service)
               missing-service-types))
     all-services)))

(define-public (add-missing-requirements
                services all-services requirement->service)
  (unless (eq? (length services)
               (length (delete-duplicates services)))
    (simple-format #t "\n~A\n\n" (service-names services))
    (simple-format #t "\n~A\n\n" (service-names (delete-duplicates services)))
    (error "duplicates"))
  (let
      ((missing-requirements
        (lset-union
         equal?
         (find-missing-requirements services requirement->service)
         (find-missing-extension-targets services all-services))))
    (if (null? missing-requirements)
        services
        (add-missing-requirements
         (append services
                 missing-requirements)
         all-services
         requirement->service))))

(define (system-without-unnecessary-services
         services-to-keep
         system)
  (for-each
   (lambda (service)
     (unless (service? service)
       (simple-format #t "error: system-without-unnecessary-services: not a service ~A\n" service)))
   services-to-keep)
  (for-each (lambda (service)
              (unless (member service
                              (operating-system-user-services system))
                (error "Could not find ~A in system services"
                       service)))
            services-to-keep)
  (operating-system
   (inherit system)
   (services
    (add-missing-requirements
     services-to-keep
     (operating-system-user-services system)
     (get-requirement->service-alist
      (operating-system-user-services system))))))

(define (update-system-services-package-source-from-environment system)
  (operating-system
   (inherit system)
   (services
    (correct-services-package-source-from-environment
     (operating-system-user-services system)))))
