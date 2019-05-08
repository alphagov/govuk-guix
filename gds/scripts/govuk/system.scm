(define-module (gds scripts govuk system)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix scripts build)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services web)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system linux-container)
  #:use-module (gds utils)
  #:use-module (gds services rails)
  #:use-module (gds services govuk)
  #:use-module (gds services govuk admin-environment-style)
  #:use-module (gds services govuk data-snapshot)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk nginx)
  #:use-module (gds services govuk routing-configuration)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds systems utils)
  #:use-module (gds systems govuk base)
  #:use-module (gds systems govuk aws)
  #:use-module (gds scripts utils)
  #:export (vm-start-script
            container-start-script

            alter-services-for-vm

            display-system-information

            opts->operating-system))

(define %default-services
  (apply append
         (map service-group-services
              (list publishing-application-services
                    backend-services
                    publishing-platform-services
                    supporting-application-services
                    transition-services
                    frontend-services
                    draft-frontend-services))))

(define (parse-services services)
  (define (no-service-called name)
    (let
        ((similarly-named-services
          (find-similar-strings name
                                (map (lambda (service)
                                       (symbol->string
                                        (service-type-name
                                         (service-kind service))))
                                     govuk-services))))
      (if (null? similarly-named-services)
          (error "No service called" name)
          (error (simple-format
                  #f "No service called ~A, did you mean ~A?"
                  name (first similarly-named-services))))))

  (define (service-name->type name)
    (or (find (lambda (service-type)
                (eq?
                 (string->symbol name)
                 (service-type-name service-type)))
              (map service-kind govuk-services))
        (no-service-called name)))

  (map
   (lambda (service)
     (match (string-split service #\@)
       ((name revision)
        (cons (service-name->type name) revision))
       ((name) (service-name->type name))))
   services))

(define (default-signon-user-permissions services)
  (map
   (lambda (app)
     (cons
      (signon-application-name app)
      (or (assoc-ref '(("Whitehall" . ("signin" "GDS Admin"
                                       "Managing Editor"))
                       ("Asset Manager" . ())
                       ("HMRC Manuals API" . ())
                       ("Publishing API" . ()))
                     (signon-application-name app))
          (signon-application-supported-permissions app))))
   (filter-map
    (lambda (service)
      (and (list? (service-parameters service))
           (find signon-application? (service-parameters service))))
    services)))

(define* (operating-system-for-services
          base-os
          host-name
          services
          rails-environment
          app-domain
          web-domain
          admin-environment-label
          signon-instance-name
          use-high-ports
          use-https
          signon-users
          origin-basic-auth-usernames-and-passwords
          read-bundle-install-input-as-tar-archive
          http-ports-mode
          use-error-pages?
          data-snapshot)

  (define parsed-services (parse-services services))

  (define service-types
    (map (match-lambda
           ((type . revision) type)
           (type type))
         parsed-services))

  (define service-types-to-keep
    (append
     (if (null? service-types)
         (map service-kind %default-services)
         service-types)
     `(;; These service types are used by the aws system
       ,aws-pubkey-service-type
       ,openssh-service-type
       ;; Nothing requires NGinx, but it's needed
       ,govuk-nginx-service-type
       ;; Only include the authenticating proxy if needed
       ,@(if (any (lambda (draft-frontend-service-type)
                    (memq draft-frontend-service-type service-types))
                  (map service-kind (service-group-services
                                     draft-frontend-services)))
             (list authenticating-proxy-service-type)
             '())
       ;; Include the router if any frontend service is present
       ,@(if (any (lambda (frontend-service-type)
                    (memq frontend-service-type service-types))
                  (map service-kind (service-group-services
                                     frontend-services)))
             (list router-service-type)
             '())
       ,@(map service-kind base-services))))

  (define minimal-base-os
    (update-system-services-package-source
     (filter pair? parsed-services)
     (system-without-unnecessary-services
      (filter
       (lambda (service)
         (member (service-kind service) service-types-to-keep))
       (operating-system-user-services base-os))
      base-os)))

  (define routing-configuration-arguments
    `(#:use-high-ports? ,use-high-ports
      #:use-https? ,use-https
      #:http-ports-mode ,http-ports-mode
      #:app-domain ,(or app-domain
                        host-name)
      #:web-domain ,(or web-domain
                        (string-append "www." host-name))))

  (define plek-config
    (apply plek-config-from-routing-configuration-arguments
           (operating-system-user-services minimal-base-os)
           routing-configuration-arguments))

  (define database-service-ports
    (if use-high-ports
        high-database-service-ports
        default-database-service-ports))

  (define (add-database-startup-scripts services)
    (map ensure-database-user-exists-on-service-startup
         (map run-db:setup-if-postgresql-or-mysql-is-used services)))

  (define (configure-signon-users services)
    (cond
     ((memq 'none signon-users)
      (unless (eq? (length signon-users) 1)
        (display "error: if --signon-users=none is specified, addition users cannot be\n")
        (exit 1))
      services)
     ((null? signon-users)
      (update-signon-service-add-users
       (list
        (signon-user
         (name "Dev")
         (email "dev@dev.gov.uk")
         (passphrase (signon-dev-user-passphrase))
         (role "superadmin")
         (application-permissions
          (default-signon-user-permissions services))))
       services))
     (else
      (let ((users
             (map (lambda (user)
                    (if (eq? 'unset
                             (signon-user-application-permissions user))
                        (signon-user
                         (inherit user)
                         (application-permissions
                          (default-signon-user-permissions services)))
                        user))
                  signon-users)))

        (update-signon-service-add-users users services)))))

  (define (set-routing-configuration services)
    (apply
     set-routing-configuration-for-services
     services
     routing-configuration-arguments))

  (define (set-database-configuration services)
    (map
     (cut update-service-database-connection-config-for-environment
       rails-environment
       <>)
     services))

  (define (set-rails-environment services)
    (map
     (cut update-rails-app-config-environment-for-service
       rails-environment <>)
     services))

  (define (set-admin-environment-label services)
    (if admin-environment-label
        (use-govuk-admin-template-environment-label
         services
         admin-environment-label)
        services))

  (define (set-origin-basic-auth services)
    (if (null? origin-basic-auth-usernames-and-passwords)
        services
        (modify-services services
          (govuk-nginx-service-type config =>
                                    (govuk-nginx-configuration
                                     (inherit config)
                                     (origin-password-file
                                      (password-file
                                       (usernames-and-passwords
                                        origin-basic-auth-usernames-and-passwords))))))))

  (define (set-nginx-intercept-errors services)
    (modify-services services
      (govuk-nginx-service-type config =>
                                (govuk-nginx-configuration
                                 (inherit config)
                                 (intercept-errors use-error-pages?)))))

  (define (set-signon-instance-name services)
    (update-services-parameters
     services
     (list
      (cons
       signon-service-type
       (list
        (cons
         signon-config?
         (lambda (config)
           (signon-config
            (inherit config)
            (instance-name
             (if (and (string? signon-instance-name)
                      (string=? signon-instance-name ""))
                 #f
                 signon-instance-name))))))))))

  (define (set-read-bundle-install-input-as-tar-archive services)
    (update-rails-app-set-read-bundle-install-input-as-tar-archive
     read-bundle-install-input-as-tar-archive
     services))

  (define (add-load-data-snapshot-service-if-necessary services)
    (if data-snapshot
        (append
         services
         (list
          (service load-data-snapshot-service-type
                   data-snapshot)))
        services))

  (define set-services-configuration
    (let
        ((service-setup-functions
          (list
           ;; TODO The ordering is important here, without this coming
           ;; before add-database-startup-scripts, the wrong ports can
           ;; be used
           set-routing-configuration
           set-signon-instance-name
           set-database-configuration
           set-read-bundle-install-input-as-tar-archive
           add-database-startup-scripts
           set-admin-environment-label
           set-rails-environment
           configure-signon-users
           set-origin-basic-auth
           set-nginx-intercept-errors
           add-load-data-snapshot-service-if-necessary)))

      (apply compose (reverse service-setup-functions))))

  (operating-system
   (inherit minimal-base-os)
   (host-name host-name)
   (hosts-file (plek-config->hosts-file plek-config))
   (packages (useful-packages))
   (services (set-services-configuration
              (operating-system-user-services minimal-base-os)))))

(define* (opts->operating-system
          opts
          #:key
          default-read-bundle-install-input-as-tar-archive?)
  (operating-system-for-services
   (assq-ref opts 'base-os)
   (assq-ref opts 'host-name)
   (option-values opts 'argument)
   (assq-ref opts 'rails-environment)
   (assq-ref opts 'app-domain)
   (assq-ref opts 'web-domain)
   (assq-ref opts 'admin-environment-label)
   (assq-ref opts 'signon-instance-name)
   (assq-ref opts 'use-high-ports?)
   (assq-ref opts 'use-https?)
   (option-values opts 'signon-users)
   (option-values opts 'origin-basic-auth)
   (option-value opts 'read-bundle-install-input-as-tar-archive?
                 #:default default-read-bundle-install-input-as-tar-archive?)
   (assq-ref opts 'http-ports-mode)
   (option-value opts 'use-error-pages?
                 #:default #f)
   (option-value opts 'data-snapshot
                 #:default #f)))

(define (container-start-script os opts)
  (with-store store
    (set-build-options-from-command-line store opts)

    (run-with-store store
      (mbegin %store-monad
        (set-grafting #f)
        (mlet* %store-monad
            ((sys (container-script
                   os
                   #:mappings (option-values opts 'file-system-mapping)
                   #:container-shared-network? #t)))
          (mbegin %store-monad
            (built-derivations (list sys))
            (return (derivation->output-path sys))))))))

(define (alter-services-for-vm os)
  (operating-system
    (inherit os)
    (services
     (cons* (login-service)

            (service virtual-terminal-service-type)
            (service console-font-service-type
                     (map (lambda (tty)
                            (cons tty %default-console-font))
                          '("tty1" "tty2" "tty3")))

            (agetty-service (agetty-configuration
                             (extra-options '("-L")) ; no carrier detect
                             (term "vt100")
                             (tty #f))) ; automatic

            (mingetty-service (mingetty-configuration
                               (tty "tty1")))
            (mingetty-service (mingetty-configuration
                               (tty "tty2")))
            (mingetty-service (mingetty-configuration
                               (tty "tty3")))

            (udev-service #:rules (list lvm2 fuse alsa-utils crda))

            (service static-networking-service-type
                     (list (static-networking (interface "lo")
                                              (ip "127.0.0.1")
                                              (requirement '())
                                              (provision '(loopback)))))

            (service dhcp-client-service-type)

            (filter
             (lambda (s)
               (not (eq?
                     'dummy-loopback-service
                     (service-type-name
                      (service-kind s)))))
             (operating-system-user-services os))))))

(define (display-system-information os)

  (define (display-signon-service value)
    (display "Service: Signon\n")
    (display "  Users:\n")
    (match (find signon-config? value)
      (($ <signon-config> applications users)
       (for-each (match-lambda
                   (($ <signon-user>
                       name email passphrase role application-permissions)
                    (format #t "   - Name: ~a~%" name)
                    (format #t "     Email: ~a~%" email)
                    (format #t "     Role: ~a~%" role)
                    (format #t "     Passphrase: ~a~%"
                            passphrase)
                    (display "     Application permissions:\n")
                    (for-each (match-lambda
                                ((app . permissions)
                                 (simple-format #t "      - ~A (~A)\n"
                                                app (string-join
                                                     permissions
                                                     ", "))))
                              application-permissions)
                    (newline)))
                 users))))

  (define display-functions
    (list
     (cons signon-service-type display-signon-service)))

  (define (display-services-and-gather-others services)
    (fold
     (lambda (service other-services)
       (let ((type (service-kind service))
             (value (service-value service)))

         (let ((display-function
                (assq-ref display-functions type)))
           (if display-function
               (begin
                 (display-function value)
                 other-services)
               (cons service
                     other-services)))))
     '()
     services))

  (newline)
  (let* ((rest (display-services-and-gather-others
                (operating-system-user-services os))))
    (display "Other services:\n")
    (for-each (lambda (service)
                (format #t " - ~a~%" service))
              (stable-sort
               (map (lambda (service)
                      (symbol->string
                       (service-type-name (service-kind service))))
                    rest)
                string<?))))

(define (vm-start-script os opts)
  (with-store store
    (set-build-options-from-command-line store opts)

    (run-with-store store
      (mbegin %store-monad
        (set-grafting #f)
        (mlet* %store-monad
            ((item
              (system-qemu-image/shared-store-script
               (alter-services-for-vm os))))
          (mbegin %store-monad
            (built-derivations (list item))
            (return (derivation->output-path item))))))))
