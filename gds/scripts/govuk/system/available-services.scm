(define-module (gds scripts govuk system available-services)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gds services govuk)
  #:use-module (gds systems govuk development)
  #:use-module (gds scripts utils)
  #:export (available-services))

(define (shepherd-services service)
  (let* ((shepherd-service-extension
          (find (lambda (service-extension)
                  (eq? (service-extension-target service-extension)
                       shepherd-root-service-type))
                (service-type-extensions
                 (service-kind service))))
         (compute
          (service-extension-compute shepherd-service-extension)))
    (compute
     (service-value (matching-service-from-development-os
                     service)))))

(define (package service)
  (let ((value (service-value service)))
    (and (list? value)
         (find package? value))))

(define (group service)
  (any (lambda (group)
         (if (memq (service-kind service)
                   (map service-kind
                        (service-group-services group)))
             group
             #f))
       service-groups))

(define (matching-service-from-development-os service)
  (find (lambda (development-os-service)
          (eq? (service-kind development-os-service)
               (service-kind service)))
        (operating-system-user-services govuk-development-os)))

(define (display-available-services-in-json)
  (define (origin-sexp origin)
    (cond
     ((eq? (origin-method origin) url-fetch)
      `((uri . ,(origin-uri origin))))
     ((eq? (origin-method origin) git-fetch)
      `((uri . ,(let ((git-reference (origin-uri origin)))
                  `((url .    ,(git-reference-url git-reference))
                    (commit . ,(git-reference-commit git-reference)))))))
     (else
      (error "Unhandled origin method"))))

  (define (package-sexp package)
    `((name . ,(package-name package))
      (version . ,(package-version package))
      (homepage . ,(package-home-page package))
      (source . ,(origin-sexp (package-source package)))))

  (define (shepherd-services-sexp shepherd-services)
    (map (lambda (shepherd-service)
           `(,(shepherd-service-canonical-name
               shepherd-service)
             .
             ((provision . ,(shepherd-service-provision
                             shepherd-service))
              (requirement . ,(shepherd-service-requirement
                               shepherd-service)))))
         shepherd-services))

  (define (service-sexp service)
    `((name . ,(service-type-name (service-kind service)))

      ,@(or (and=> (service-type-description (service-kind service))
                   (lambda (description)
                     `((description . ,description))))
            '())

      (package . ,(package-sexp (package service)))
      (shepherd-services . ,(shepherd-services-sexp
                             (shepherd-services service)))))

  (scm->json
   (map (match-lambda
          (($ <service-group> name description services)
          `((name . ,name)
            (description . ,description)
            (services . ,(map service-sexp
                              services)))))
        service-groups)
   #:pretty #t))

(define (display-available-services)
  (for-each (lambda (service)
              (simple-format #t "~A:" (service-type-name
                                       (service-kind service)))
              (newline)

              (and=> (package service)
                     (lambda (package)
                       (simple-format #t "  package: ~A@~A\n"
                                      (package-name package)
                                      (package-version package))
                       (newline)))

              (and=> (group service)
                     (lambda (group)
                       (simple-format #t "  group: ~A\n"
                                      (service-group-name group))
                       (newline)))

              (display "  shepherd services:\n")
              (for-each
               (lambda (shepherd-service)
                 (simple-format #t "   - name:        ~A\n"
                                (shepherd-service-canonical-name
                                 shepherd-service))
                 (simple-format #t "     provision:   ~A\n"
                                (string-join (map symbol->string
                                                  (shepherd-service-provision
                                                   shepherd-service))
                                             ", "))
                 (simple-format #t "     requirement: ~A\n"
                                (string-join (map symbol->string
                                                  (shepherd-service-requirement
                                                   shepherd-service))
                                             ", ")))
               (shepherd-services service))
              (newline))
            govuk-services))

(define (available-services opts)
  (if (option-value opts 'json-output? #:default #f)
      (display-available-services-in-json)
      (display-available-services)))
