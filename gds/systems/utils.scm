(define-module (gds systems utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:export (system-without-unnecessary-services))

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
                services all-services requirement->service)
  (fold
   (lambda (service missing-requirements)
     (append
      (fold
       (lambda (service new-services)
         (if (or (member service services)
                 (member service missing-requirements)
                 (member service new-services))
             new-services
             (begin
               ;; (simple-format #t "(memq service services): ~A ~A\n" (member service services) (service-names services))
               ;; (simple-format #t "adding: ~A\n" (service->name service))
               (cons
              service
              new-services))))
       '()
       (filter-map
        (lambda (requirement)
          (or (assq-ref requirement->service requirement)
              #f ;; TODO(error "Could not find service ~A\n" requirement)
              ))
        (concatenate
         (map
          shepherd-service-requirement
          (shepherd-services-from-service service)))))
      missing-requirements))
   '()
   services))

(define-public (add-missing-requirements
                services all-services requirement->service)
  (unless (eq? (length services)
               (length (delete-duplicates services)))
    (simple-format #t "\n~A\n\n" (service-names services))
    (simple-format #t "\n~A\n\n" (service-names (delete-duplicates services)))
    (error "duplicates"))
  (let
      ((missing-requirements
        (find-missing-requirements
         services all-services requirement->service)))
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
