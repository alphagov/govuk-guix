(define-module (gds services govuk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases elasticsearch)
  #:use-module (gds services utils databases rabbitmq)
  #:use-module (gds services govuk plek)
  #:use-module (gds services sidekiq)
  #:use-module (gds services delayed-job)
  #:use-module (gds services govuk signon)
  #:use-module (gds services govuk search-api)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk publishing-e2e-tests)
  #:use-module (gds services rails)
  #:export (<router-api-config>
            router-api-config
            router-api-config?
            router-api-config-nodes

            <service-group>
            service-group
            service-group?
            service-group-name
            service-group-description
            service-group-services))

;;;
;;; GOV.UK Content Schemas
;;;

(define-public govuk-content-schemas-service-type
  (shepherd-service-type
   'govuk-content-schemas
   (lambda (package)
     (shepherd-service
      (provision (list 'govuk-content-schemas))
      (documentation "Ensure /var/apps/govuk-content-schemas exists")
      (start
       #~(lambda _
           (use-modules (guix build utils))

           (if (not (file-exists? "/var/apps/govuk-content-schemas"))
               (begin
                 (mkdir-p "/var/apps")
                 (symlink #$package
                          "/var/apps/govuk-content-schemas")))
           #t))
   (stop #~(lambda _
             #f))
   (respawn? #f)))))

(define-public govuk-content-schemas-service
  (service govuk-content-schemas-service-type govuk-content-schemas))

;;;
;;; Asset Manager
;;;

(define asset-manager-mount-uploads-gexp
  (with-imported-modules (source-module-closure
                          '((guix build utils)
                            (gnu build file-systems)))
    #~(lambda ()
        (use-modules (gds build utils)
                     (gnu build file-systems))

        (for-each
         (lambda (directory)
           ;; TODO: This shouldn't be necessary
           (define bind-mount
             (@ (gnu build file-systems) bind-mount))

           (let ((app-dir
                  (string-append "/var/apps/asset-manager/" directory))
                 (storage-dir
                  (string-append "/var/lib/asset-manager/" directory)))
             (simple-format #t "asset-manager: setting up directory: ~A\n"
                            directory)
             (mkdir-p storage-dir)
             (bind-mount storage-dir app-dir)
             (chmod app-dir #o777)))
         '("uploads" "fake-s3"))
        #t)))

(define-public asset-manager-service-type
  (service-type
   (name 'asset-manager)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(asset-manager))
            (requirement '(signon redis)))
           (plek-config)
           (rails-app-config
            (precompiled-assets-are-environment-specific? #f))
           asset-manager
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (signon-application
            (name "Asset Manager")
            (supported-permissions '("signin")))
           (service-startup-config
            (root-pre-startup-scripts
             `((mount-uploads . ,asset-manager-mount-uploads-gexp))))
           (redis-connection-config)
           (mongodb-connection-config
            (database "asset_manager_production"))))))

;;;
;;; Authenticating Proxy
;;;

(define-public authenticating-proxy-service-type
  (service-type
   (name 'authenticating-proxy)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(authenticating-proxy))
            (requirement '(signon draft-router)))
           (plek-config)
           (rails-app-config
            (assets? #f))
           authenticating-proxy
           (service-startup-config)
           (signon-application
            (name "Content Preview")
            (supported-permissions '("signin")))
           (mongodb-connection-config
            (database "authenticating_proxy_production"))))))

;;;
;;; Calculators
;;;

(define-public calculators-service-type
  (service-type
   (name 'calculators)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(calculators))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           calculators
           (service-startup-config)))))

;;;
;;; Calendars
;;;

(define-public calendars-service-type
  (service-type
   (name 'calendars)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(calendars))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           calendars
           (service-startup-config)))))

;;;
;;; Collections
;;;

(define-public collections-service-type
  (service-type
   (name 'collections)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(collections))
            (requirement '(content-store static search-api)))
           (plek-config)
           (rails-app-config)
           collections
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "collections"))))))))

(define-public draft-collections-service-type
  (service-type
   (name 'draft-collections)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-collections))
            (requirement '(draft-content-store draft-static search-api)))
           (plek-config)
           (rails-app-config)
           collections
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "draft-collections"))))))))

;;;
;;; Collections Publisher
;;;

(define-public collections-publisher-service-type
  (service-type
   (name 'collections-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(collections-publisher))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           collections-publisher
           (signon-application
            (name "Collections Publisher")
            (supported-permissions '("signin" "GDS Editor")))
           (signon-api-user
            (name "Collections Publisher")
            (email "collections-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (service-startup-config)
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (redis-connection-config)
           (memcached-connection-config)
           (mysql-connection-config
            (user "collections_pub")
            (password (random-base16-string 30))
            (database "collections_publisher_production"))))))

;;;
;;; Contacts Admin
;;;

(define-public contacts-admin-service-type
  (service-type
   (name 'contacts-admin)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(contacts-admin))
            (requirement '(publishing-api whitehall signon)))
           (plek-config)
           (rails-app-config)
           contacts-admin
           (signon-application
            (name "Contacts Admin")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Contacts Admin")
            (email "contacts-admin@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (redis-connection-config)
           (mysql-connection-config
            (user "contacts")
            (password (random-base16-string 30))
            (database "contacts_production"))))))

;;;
;;; Content Data API
;;;

(define-public content-data-api-service-type
  (service-type
   (name 'content-data-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-data-api))
            (requirement '(signon postgres)))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (plek-config)
           (rails-app-config)
           content-data-api
           (signon-application
            (name "Content Data API")
            (supported-permissions '("signin" "inventory_management")))
           (signon-api-user
            (name "Content Data API")
            (email "content-data-api@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Support API"))
               '("signin")))))
           (service-startup-config)
           (redis-connection-config)
           (postgresql-connection-config
            (user "content_performance_manager")
            (database "content_performance_manager_production"))))))

;;;
;;; Content Audit Tool
;;;

(define-public content-audit-tool-service-type
  (service-type
   (name 'content-audit-tool)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-audit-tool))
            (requirement '(publishing-api whitehall signon)))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (plek-config) (rails-app-config) content-audit-tool
           (signon-application
            (name "Content Audit Tool")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Content Audit Tool")
            (email "content-audit-tool@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (redis-connection-config)
           (postgresql-connection-config
            (user "content_audit_tool")
            (database "content_audit_tool_production"))))))

;;;
;;; Content Data Admin
;;;

(define-public content-data-admin-service-type
  (service-type
   (name 'content-data-admin)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-data-admin))
            (requirement '(content-data-api
                           signon
                           postgres)))
           (plek-config)
           (rails-app-config)
           content-data-admin
           (signon-application
            (name "Content Data Admin")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Content Data Admin")
            (email "content-data-admin@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Content Data API"))
               '("signin")))))
           (service-startup-config)
           (postgresql-connection-config
            (user "content_data_admin")
            (database "content_data_admin_production"))))))

;;;
;;; Content Publisher
;;;

(define-public content-publisher-service-type
  (service-type
   (name 'content-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-publisher))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           content-publisher
           (signon-application
            (name "Content Publisher")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Content Publisher")
            (email "content-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config
            (environment-variables
             '(("ACTIVE_STORAGE_SERVICE" . "local"))))
           (postgresql-connection-config
            (user "content_publisher")
            (database "content_publisher_production"))))))

;;;
;;; Email Alert API
;;;

(define-public email-alert-api-service-type
  (service-type
   (name 'email-alert-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(email-alert-api))
            (requirement '(postgres
                           signon
                           publishing-api
                           content-store)))
           (plek-config)
           (rails-app-config)
           email-alert-api
           (service-startup-config
            (environment-variables
             `(("EMAIL_ALERT_AUTH_TOKEN" . ,(random-base16-string 30)))))
           (redis-connection-config)
           (signon-application
            (name "Email Alert API")
            (supported-permissions '("signin" "internal_app")))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (postgresql-connection-config
            (user "email-alert-api")
            (database "email_alert_api-production")
            ;; Required for creating the uuid-ossp extension
            (superuser? #t))))))

;;;
;;; Email Alert Frontend
;;;

(define-public email-alert-frontend-service-type
  (service-type
   (name 'email-alert-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(email-alert-frontend))
            (requirement '(content-store
                           email-alert-api
                           static
                           publishing-api)))
           (plek-config)
           (rails-app-config)
           email-alert-frontend
           (signon-api-user
            (name "Email Alert Frontend")
            (email "email-alert-frontend@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Email Alert API"))
               '("signin" "internal_app")))))
           (service-startup-config)))))

(define-public draft-email-alert-frontend-service-type
  (service-type
   (name 'draft-email-alert-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-email-alert-frontend))
            (requirement '(draft-content-store
                           email-alert-api
                           draft-static
                           publishing-api)))
           (signon-api-user
            (name "Draft Email Alert Frontend")
            (email "draft-email-alert-frontend@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Email Alert API"))
               '("signin" "internal_app")))))
           (plek-config)
           (rails-app-config)
           email-alert-frontend
           (service-startup-config)))))

;;;
;;; Email Alert Service
;;;

;; TODO: This is not actually a Rails app...
(define-public email-alert-service-type
  (service-type
   (name 'email-alert-service)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(email-alert-service))
            (requirement '(rabbitmq email-alert-api)))
           (plek-config)
           (rails-app-config
            (run-with "bin/email_alert_service"))
           (rabbitmq-connection-config (user "email_alert_service")
                                       (password "email_alert_service"))
           email-alert-service
           (service-startup-config)
           (redis-connection-config)))))

;;;
;;; Feedback
;;;

(define-public feedback-service-type
  (service-type
   (name 'feedback)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(feedback))
            (requirement '(publishing-api support-api signon)))
           (plek-config) (rails-app-config) feedback
           (signon-api-user
            (name "Feedback")
            (email "feedback@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Support API"))
               '("signin")))))
           (service-startup-config)))))

;;;
;;; Finder Frontend
;;;

(define-public finder-frontend-service-type
  (service-type
   (name 'finder-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(finder-frontend))
            (requirement '(content-store
                           search-api
                           whitehall ;; For the Worldwide API
                           static
                           memcached)))
           (plek-config)
           (rails-app-config)
           finder-frontend
           (memcached-connection-config)
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "finder-frontend"))))))))

(define-public draft-finder-frontend-service-type
  (service-type
   (name 'draft-finder-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-finder-frontend))
            (requirement '(draft-content-store
                           search-api
                           whitehall ;; For the Worldwide API
                           draft-static
                           memcached)))
           (plek-config)
           (rails-app-config)
           finder-frontend
           (memcached-connection-config)
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "draft-finder-frontend"))))))))

;;;
;;; HMRC Manuals API
;;;

(define-public hmrc-manuals-api-service-type
  (service-type
   (name 'hmrc-manuals-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(hmrc-manuals-api))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           hmrc-manuals-api
           (signon-application
            (name "HMRC Manuals API")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "HMRC Manuals API")
            (email "hmrc-manuals-api@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (redis-connection-config)))))

;;;
;;; Licence Finder
;;;

(define-public licence-finder-service-type
  (service-type
   (name 'licencefinder)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(licence-finder))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           licence-finder
           (signon-api-user
            (name "Licence Finder")
            (email "licence-finder@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (mongodb-connection-config
            (database "licence_finder_production"))))))

;;;
;;; Link Checker API
;;;

(define-public link-checker-api-service-type
  (service-type
   (name 'link-checker-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(link-checker-api))
            (requirement '(signon)))
           (plek-config)
           (rails-app-config
            (assets? #f))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           link-checker-api
           (signon-application
            (name "Link Checker API")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Link Checker API")
            (email "link-checker-api@guix-dev.gov.uk")
            (authorisation-permissions (list)))
           (service-startup-config)
           (redis-connection-config)
           (postgresql-connection-config
            (user "link_checker_api")
            (database "link_checker_api_production"))))))

;;;
;;; Local Links Manager
;;;

(define-public local-links-manager-service-type
  (service-type
   (name 'local-links-manager)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(local-links-manager))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) local-links-manager
           (signon-api-user
            (name "Local Links Manager")
            (email "local-links-manager@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (signon-application
            (name "Local Links Manager")
            (supported-permissions '("signin")))
           (service-startup-config)
           (redis-connection-config)
           (postgresql-connection-config
            (user "local_links_manager")
            (database "local-links-manager_production"))))))

;;;
;;; Imminence
;;;

(define-public imminence-service-type
  (service-type
   (name 'imminence)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(imminence))
            (requirement '(publishing-api signon redis)))
           (plek-config)
           (rails-app-config)
           imminence
           (signon-application
            (name "Imminence")
            (supported-permissions '("signin")))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (service-startup-config)
           (redis-connection-config)
           (mongodb-connection-config
            (database "imminence_production"))))))

;;;
;;; Manuals Frontend
;;;

(define-public manuals-frontend-service-type
  (service-type
   (name 'manuals-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(manuals-frontend))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) manuals-frontend
           (service-startup-config)
           (mongodb-connection-config
            (database "manuals_frontend"))))))

(define-public draft-manuals-frontend-service-type
  (service-type
   (name 'draft-manuals-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-manuals-frontend))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) manuals-frontend
           (service-startup-config)
           (mongodb-connection-config
            (database "manuals_frontend"))))))

;;;
;;; Manuals Publisher
;;;

(define-public manuals-publisher-service-type
  (service-type
   (name 'manuals-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(manuals-publisher))
            (requirement '(publishing-api
                           signon
                           whitehall))) ;; Whitehall required for the Organisation API
           (plek-config) (rails-app-config) manuals-publisher
           (signon-application
            (name "Manuals Publisher")
            (supported-permissions '("signin" "editor" "gds_editor")))
           (signon-api-user
            (name "Manuals Publisher")
            (email "manuals-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (service-startup-config)
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (redis-connection-config)
           (mongodb-connection-config
            (database "govuk_content_production"))))))

;;;
;;; Mini Environment Admin
;;;

(define-public mini-environment-admin-service-type
  (service-type
   (name 'mini-environment-admin)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(mini-environment-admin))
            (requirement '(signon postgres)))
           (plek-config)
           (rails-app-config
            (run-with 'rails))
           mini-environment-admin
           (signon-application
            (name "Mini Environment Admin")
            (supported-permissions '("signin")))
           (service-startup-config)
           (postgresql-connection-config
            (user "govuk_mini_environment_admin")
            (database "govuk_mini_environment_admin")
            ;; TODO: Due to the plpgsql extension
            (superuser? #t))))))

;;;
;;; Release
;;;

(define-public release-service-type
  (service-type
   (name 'release)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(release))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           release
           (signon-application
            (name "Release")
            (supported-permissions '("signin" "deploy")))
           (service-startup-config)
           (mysql-connection-config
            (user "release")
            (password (random-base16-string 30))
            (database "release_production"))))))

;;;
;;; Search Admin
;;;

(define-public search-admin-service-type
  (service-type
   (name 'search-admin)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(search-admin))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           search-admin
           (signon-application
            (name "Search Admin")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Search Admin")
            (email "search-admin@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Search API"))
               '("signin")))))
           (service-startup-config)
           (mysql-connection-config
            (user "search_admin")
            (database "search_admin_production")
            (password (random-base16-string 30)))))))

;;;
;;; Service Manual Publisher
;;;

(define-public service-manual-publisher-service-type
  (service-type
   (name 'service-manual-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(service-manual-publisher))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) service-manual-publisher
           (signon-application
            (name "Service Manual Publisher")
            (supported-permissions '("signin" "gds_editor")))
           (signon-api-user
            (name "Service Manual Publisher")
            (email "service-manual-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (postgresql-connection-config
            (user "service_manual_publisher")
            (database "service-manual-publisher_production"))))))

;;;
;;; Service Manual Frontend
;;;

(define-public service-manual-frontend-service-type
  (service-type
   (name 'service-manual-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(service-manual-frontend))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           service-manual-frontend
           (service-startup-config)))))

(define-public draft-service-manual-frontend-service-type
  (service-type
   (name 'draft-service-manual-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-service-manual-frontend))
            (requirement '(publishing-api signon)))
           (plek-config)
           (rails-app-config)
           service-manual-frontend
           (service-startup-config)))))

;;;
;;; Short Url Manager
;;;

(define-public short-url-manager-service-type
  (service-type
   (name 'short-url-manager)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(short-url-manager))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) short-url-manager
           (signon-application
            (name "Short URL Manager")
            (supported-permissions '("signin" "manage_short_urls" "request_short_urls")))
           (signon-api-user
            (name "Short URL Manager")
            (email "short-url-manager@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config)
           (mongodb-connection-config
            (database "short_url_manager_production"))))))

;;;
;;; Smart Answers
;;;

(define-public smart-answers-service-type
  (service-type
   (name 'smartanswers)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(smart-answers))
            (requirement '(publishing-api
                           signon
                           content-store
                           imminence
                           static
                           whitehall)))
           (signon-api-user
            (name "Smart Answers")
            (email "smart-answers@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config)
           (rails-app-config)
           smart-answers
           (service-startup-config)))))

;;;
;;; Smokey
;;;


(define (smokey-start-script environment-variables package)
  (program-file
   (string-append "start-smokey")
   (with-imported-modules '((guix build utils)
                            (gnu services herd))
    #~(let ((bundle (string-append #$package "/bin/bundle")))
        (use-modules (guix build utils)
                     (gnu services herd)
                     (srfi srfi-26)
                     (ice-9 popen)
                     (ice-9 rw)
                     (ice-9 rdelim))

        (mkdir-p "/var/apps/smokey")

        (for-each
         (lambda (env-var)
           (setenv (car env-var) (cdr env-var)))
         '#$environment-variables)
        (chdir #$package)
        (let
            ((result
              (zero? (system*
                      bundle
                      "exec"
                      "rake"))))

          result)))))

(define (smokey-activation environment-variables package)
  (with-imported-modules (source-module-closure
                          '((guix build syscalls)
                            (gnu build file-systems)))
    #~(begin
        (use-modules (guix build utils)
                     (gnu build file-systems)
                     (guix build syscalls)
                     (ice-9 match)
                     (ice-9 ftw)
                     (srfi srfi-26))
        (let* ((root-directory "/var/apps/smokey"))
          (if (file-exists? root-directory)
              (begin
                (mkdir-p (string-append root-directory "/bin"))
                (mount "tmpfs" (string-append root-directory "/bin") "tmpfs")
                (copy-recursively
                 (string-append #$package "/bin")
                 (string-append root-directory "/bin")
                 #:log (%make-void-port "w")
                 #:follow-symlinks? #f)
                (substitute* (find-files (string-append root-directory "/bin")
                                         (lambda (name stat)
                                           (access? name X_OK)))
                  (((string-append #$package "/bin"))
                   "${BASH_SOURCE%/*}"))
                (substitute* (find-files (string-append root-directory "/bin")
                                         (lambda (name stat)
                                           (access? name X_OK)))
                  (("File\\.expand_path\\([\"']\\.\\./spring[\"'], __FILE__\\)")
                   "File.expand_path('../.spring-real', __FILE__)"))
                (for-each
                 (lambda (path)
                   (mkdir-p (string-append root-directory path))
                   (chmod (string-append root-directory path) #o777))
                 '("/tmp" "/log")))
              (begin
                (mkdir-p root-directory)
                (bind-mount #$package root-directory)

                (for-each
                 (lambda (file)
                   (if (file-exists? file)
                       (mount "tmpfs" file "tmpfs")))
                 (map
                  (lambda (dir)
                    (string-append root-directory "/" dir))
                  '("log" "public")))))

          (let* ((dir (string-append "/tmp/env.d/"))
                 (file (string-append dir "smokey")))
            (mkdir-p dir)
            (call-with-output-file file
              (lambda (port)
                (for-each
                 (lambda (env-var)
                   (simple-format port "export ~A=\"~A\"\n" (car env-var) (cdr env-var)))
                 '#$environment-variables))))))))

(define-public smokey-service-type
  (service-type
   (name 'smokey)
   (extensions
    (list
     (service-extension
      activation-service-type
      (match-lambda
        ((plek-config package)
         (smokey-activation
          (plek-config->environment-variables plek-config)
          package))))
     (service-extension
      shepherd-root-service-type
      (match-lambda
        ((plek-config package)
         (let* ((start-script
                 (smokey-start-script
                  (plek-config->environment-variables plek-config)
                  package)))
           (list
            (shepherd-service
             (provision (list 'smokey))
             (documentation "Smokey")
             (requirement '(smart-answers))
             (respawn? #f)
             (start #~(make-forkexec-constructor #$start-script))
             (stop #~(make-kill-destructor))))))))))
   (default-value
     (list (plek-config)
           smokey))))

;;;
;;; Support
;;;

(define-public support-service-type
  (service-type
   (name 'support)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(support))
            (requirement '(support-api signon)))
           (signon-application
            (name "Support")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Support")
            (email "support@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Support API"))
               '("signin")))))
           (plek-config)
           (rails-app-config)
           support
           (redis-connection-config)
           (service-startup-config)))))

;;;
;;; Support API
;;;

(define-public support-api-service-type
  (service-type
   (name 'support-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(support-api))
            (requirement '(publishing-api signon)))
           (plek-config) (rails-app-config) support-api
           (service-startup-config)
           (signon-application
            (name "Support API")
            (supported-permissions '("signin")))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (redis-connection-config)
           (postgresql-connection-config
            (user "support_contacts")
            (database "support_contacts_production"))))))

;;;
;;; Travel Advice Publisher
;;;

(define-public travel-advice-publisher-service-type
  (service-type
   (name 'travel-advice-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(travel-advice-publisher))
            (requirement '(publishing-api signon static asset-manager)))
           (plek-config) (rails-app-config) travel-advice-publisher
           (signon-application
            (name "Travel Advice Publisher")
            (supported-permissions '("signin" "gds_editor")))
           (signon-api-user
            (name "Travel Advice Publisher")
            (email "travel-advice-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Email Alert API"))
               '("signin" "internal_app"))
              (cons
               (signon-authorisation
                (application-name "Asset Manager"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (service-startup-config)
           (redis-connection-config)
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (mongodb-connection-config
            (database "travel_advice_publisher_production"))))))

;;;
;;; Publishing API Service
;;;

(define-public publishing-api-service-type
  (service-type
   (name 'publishing-api)
   (description "Manages content and publishing on GOV.UK")
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(publishing-api))
            (requirement '(content-store draft-content-store signon
                                         govuk-content-schemas redis loopback postgres
                                         rabbitmq memcached)))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config
             (environment-variables
              '(("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/apps/govuk-content-schemas"))))
            `((setup-exchange
               . ,#~(lambda ()
                      (run-command "rake" "setup_exchange")))))
           (plek-config)
           (rails-app-config
            (assets? #f))
           publishing-api
           (signon-application
            (name "Publishing API")
            (supported-permissions '("signin" "view_all")))
           (signon-api-user
            (name "Publishing API")
            (email "publishing-api@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Draft Content Store"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Content Store"))
               '("signin")))))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (memcached-connection-config)
           (postgresql-connection-config
            (user "publishing_api")
            (database "publishing_api_production"))
           (rabbitmq-connection-config (user "publishing_api")
                                       (password "publishing_api"))
           (redis-connection-config)))))

;;;
;;; Content store
;;;

(define-public content-store-service-type
  (service-type
   (name 'content-store)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-store))
            (requirement '(router-api signon nginx mongodb)))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config)
            `((register-backends
               . ,#~(lambda ()
                      (run-command "rake" "register_backends")))))
           (signon-application
            (name "Content Store")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Content Store")
            (email "content-store@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Router API"))
               '("signin")))))
           (plek-config)
           (rails-app-config
            (assets? #f))
           content-store
           (mongodb-connection-config
            (database "content_store_production"))))))

(define-public draft-content-store-service-type
  (service-type
   (name 'draft-content-store)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-content-store))
            (requirement '(draft-router-api signon nginx mongodb)))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config)
            `((register-backends
               . ,#~(lambda ()
                      (run-command "rake" "register_backends")))))
           (signon-application
            (name "Draft Content Store")
            (supported-permissions '("signin")))
           (signon-api-user
            (name "Draft Content Store")
            (email "draft-content-store@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Draft Router API")
                (environment-variable "ROUTER_API_BEARER_TOKEN"))
               '("signin")))))
           (plek-config)
           (rails-app-config
            (assets? #f))
           content-store
           (mongodb-connection-config
            (database "draft_content_store_production"))))))

;;;
;;; Specialist Publisher
;;;

(define-public specialist-publisher-service-type
  (service-type
   (name 'specialist-publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(specialist-publisher))
            (requirement '(publishing-api
                           asset-manager
                           email-alert-api
                           signon mongodb nginx)))
           (plek-config) (rails-app-config) specialist-publisher
           (signon-application
            (name "Specialist Publisher")
            (supported-permissions '("signin" "editor" "gds_editor")))
           (signon-api-user
            (name "Specialist Publisher")
            (email "specialist-publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Email Alert API"))
               '("signin" "internal_app"))
              (cons
               (signon-authorisation
                (application-name "Asset Manager"))
               '("signin")))))
           (service-startup-config)
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (mongodb-connection-config
            (database "govuk_content_production"))
           (redis-connection-config)))))

;;;
;;; Government Frontend
;;;

(define-public government-frontend-service-type
  (service-type
   (name 'government-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(government-frontend))
            (requirement '(content-store static)))
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "government-frontend"))))
           (memcached-connection-config)
           (plek-config)
           (rails-app-config)
           government-frontend))))

(define-public draft-government-frontend-service-type
  (service-type
   (name 'draft-government-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-government-frontend))
            (requirement '(draft-content-store draft-static)))
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "draft-government-frontend"))))
           (memcached-connection-config)
           (plek-config)
           (rails-app-config)
           government-frontend))))

;;;
;;; Frontend
;;;

(define-public frontend-service-type
  (service-type
   (name 'frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(frontend))
            (requirement '(static
                           search-api
                           content-store
                           ;; For publishing special routes
                           publishing-api)))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config
             (environment-variables
              '(("GOVUK_APP_NAME" . "frontend"))))
            `((setup-exchange
               . ,#~(lambda ()
                      (run-command "rake" "publishing_api:publish_special_routes")))))
           (signon-api-user
            (name "Frontend")
            (email "frontend@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config)
           (rails-app-config)
           frontend))))

(define-public draft-frontend-service-type
  (service-type
   (name 'draft-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-frontend))
            (requirement '(draft-static search-api draft-content-store)))
           (service-startup-config
            (environment-variables
             '(("GOVUK_APP_NAME" . "draft-frontend"))))
           (signon-api-user
            (name "Draft Frontend")
            (email "draft-frontend@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config)
           (rails-app-config)
           frontend))))

;;;
;;; Publisher
;;;

(define-public publisher-service-type
  (service-type
   (name 'publisher)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(publisher))
            (requirement '(publishing-api frontend draft-frontend
                                          asset-manager calendars signon)))
           (service-startup-config)
           (plek-config) (rails-app-config) publisher
           (redis-connection-config)
           (signon-application
            (name "Publisher")
            (supported-permissions '("signin" "skip_review")))
           (signon-api-user
            (name "Publisher")
            (email "publisher@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Asset Manager")
                (environment-variable "PUBLISHER_ASSET_MANAGER_CLIENT_BEARER_TOKEN"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (mongodb-connection-config
            (database "govuk_content_production"))))))

;;;
;;; Router
;;;

(define default-router-database-connection-configs
  (list
   (mongodb-connection-config
    (database "router"))))

(define-public router-service-type
  (make-router-service-type
   'router
   (cons* (router-config)
          router
          default-router-database-connection-configs)))

(define default-draft-router-database-connection-configs
  (list
   (mongodb-connection-config
    (database "draft_router"))))

(define-public draft-router-service-type
  (make-router-service-type
   'draft-router
   (cons* (router-config)
          router
          default-draft-router-database-connection-configs)))

;;;
;;; Router API
;;;

(define-record-type* <router-api-config>
  router-api-config make-router-api-config
  router-api-config?
  (router-nodes router-api-config-router-nodes
                (default '())))

(define-public router-api-service-type
  (service-type
   (name 'router-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (cons* (shepherd-service
             (inherit default-shepherd-service)
             (provision '(router-api))
             (requirement '(router signon)))
            (service-startup-config)
            (signon-application
             (name "Router API")
             (supported-permissions '("signin")))
            (plek-config)
            (rails-app-config
             (assets? #f))
            router-api
            (router-api-config)
            default-router-database-connection-configs))))

(define-public draft-router-api-service-type
  (service-type
   (name 'draft-router-api)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (cons* (shepherd-service
             (inherit default-shepherd-service)
             (provision '(draft-router-api))
             (requirement '(draft-router signon)))
            (service-startup-config)
            (signon-application
             (name "Draft Router API")
             (supported-permissions '("signin")))
            (plek-config)
            (rails-app-config
             (assets? #f))
            router-api
            (router-api-config)
            default-draft-router-database-connection-configs))))

;;;
;;; Content Tagger
;;;

(define-public content-tagger-service-type
  (service-type
   (name 'content-tagger)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(content-tagger))
            (requirement '(publishing-api
                           signon
                           search-api
                           email-alert-api)))
           (service-startup-config)
           (signon-application
            (name "Content Tagger")
            (supported-permissions '("signin" "GDS Editor"
                                     "Tagathon participant")))
           (signon-api-user
            (name "Content Tagger")
            (email "content-tagger@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config) (rails-app-config) content-tagger
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (redis-connection-config)
           (postgresql-connection-config
            (user "content_tagger")
            (database "content_tagger_production"))))))

;;;
;;; Maslow
;;;

(define-public maslow-service-type
  (service-type
   (name 'maslow)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(maslow))
            (requirement '(publishing-api signon)))
           (service-startup-config)
           (signon-application
            (name "Maslow")
            (supported-permissions '("signin" "admin" "editor")))
           (signon-api-user
            (name "Maslow")
            (email "maslow@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (plek-config) (rails-app-config) maslow
           (mongodb-connection-config
            (database "maslow_production"))))))

;;;
;;; Info Frontend
;;;

(define-public info-frontend-service-type
  (service-type
   (name 'info-frontend)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(info-frontend))
            (requirement '(content-store publishing-api static)))
           (signon-api-user
            (name "Info Frontend")
            (email "info-frontend@dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin")))))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config)
            `((publish-special-routes
               . ,#~(lambda ()
                      (run-command
                       "rake" "publishing_api:publish_special_routes")))))
           (plek-config) (rails-app-config)
           info-frontend))))

;;;
;;; Static service
;;;

(define-public static-service-type
  (service-type
   (name 'static)
   (extensions
    (modify-service-extensions-for-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(static))
            (requirement '(publishing-api)))
           (service-startup-config) (plek-config) (rails-app-config)
           static))))

(define-public draft-static-service-type
  (service-type
   (name 'draft-static)
   (extensions
    (modify-service-extensions-for-plek
     name
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(draft-static))
            (requirement '(publishing-api)))
           (service-startup-config
            (environment-variables
             '(("DRAFT_ENVIRONMENT" . "true"))))
           (plek-config)
           (rails-app-config)
           static))))

;;;
;;; Whitehall
;;;

(define-public whitehall-service-type
  (service-type
   (name 'whitehall)
   (extensions
    (modify-service-extensions-for-signon-and-plek
     'whitehall-admin
     (standard-rails-service-type-extensions name)))
   (default-value
     (list (shepherd-service
            (inherit default-shepherd-service)
            (provision '(whitehall))
            (requirement '(publishing-api
                           ;; The frontend component of Whitehall uses
                           ;; the content store directly
                           content-store
                           search-api
                           email-alert-api
                           signon
                           asset-manager
                           link-checker-api
                           static
                           memcached)))
           (service-startup-config-add-pre-startup-scripts
            (service-startup-config)
            `((create-directories
               . ,(with-imported-modules '((guix build utils))
                    #~(lambda ()
                        (let ((user (getpwnam "whitehall")))
                          (for-each
                           (lambda (name)
                             (let ((mount-target
                                    (string-append "/var/apps/whitehall/" name))
                                   (mount-source
                                    (string-append "/var/lib/whitehall/" name)))
                               (mkdir-p mount-source)
                               (unless (file-exists? mount-target)
                                 (mkdir-p mount-target))
                               (chown mount-source
                                      (passwd:uid user)
                                      (passwd:gid user))
                               (bind-mount mount-source mount-target)))
                           '("incoming-uploads"
                             "clean-uploads"
                             "infected-uploads"
                             "asset-manager-tmp"
                             "carrierwave-tmp"
                             "attachment-cache"
                             "bulk-upload-zip-file-tmp")))
                        #t))))
            #:run-as-root #t)
           (plek-config) (rails-app-config) whitehall
           (signon-application
            (name "Whitehall")
            (supported-permissions '("signin" "Editor" "GDS Editor" "GDS Admin"
                                     "Import CSVs" "Managing Editor"
                                     "Upload Executable File Attachments"
                                     "World Editor" "World Writer")))
           (signon-api-user
            (name "Whitehall")
            (email "whitehall@guix-dev.gov.uk")
            (authorisation-permissions
             (list
              (cons
               (signon-authorisation
                (application-name "Publishing API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Email Alert API"))
               '("signin" "internal_app"))
              (cons
               (signon-authorisation
                (application-name "Asset Manager"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Search API"))
               '("signin"))
              (cons
               (signon-authorisation
                (application-name "Link Checker API"))
               '("signin")))))
           (sidekiq-config
            (file "config/sidekiq.yml"))
           (memcached-connection-config)
           (redis-connection-config)
           (mysql-connection-config
            (user "whitehall")
            (database "whitehall_production")
            (password (random-base16-string 30)))))))

;;;
;;; Service Groups
;;;

(define-record-type <service-group>
  (service-group name description services)
  service-group?
  (name service-group-name)
  (description service-group-description)
  (services service-group-services))

(define-public publishing-application-services
  (service-group
   "Publishing Applications"
   "Applications which publish to GOV.UK"
   (list (service collections-publisher-service-type)
         (service contacts-admin-service-type)
         (service content-publisher-service-type)
         (service content-tagger-service-type)
         (service local-links-manager-service-type)
         (service manuals-publisher-service-type)
         (service maslow-service-type)
         (service publisher-service-type)
         (service service-manual-publisher-service-type)
         (service short-url-manager-service-type)
         (service specialist-publisher-service-type)
         (service travel-advice-publisher-service-type)
         (service whitehall-service-type))))

(define-public backend-services
  (service-group
   "Backend Services"
   "Services which are used by other services"
   (list (service email-alert-api-service-type)
         (service email-alert-service-type)
         (service imminence-service-type)
         (service search-api-service-type)
         (service asset-manager-service-type)
         (service support-api-service-type)
         (service hmrc-manuals-api-service-type)
         ;; mapit-service
         )))

(define-public publishing-platform-services
  (service-group
   "Publishing Platform"
   "Services key to publishing on GOV.UK"
   (list (service publishing-api-service-type)
         (service content-store-service-type)
         (service draft-content-store-service-type)
         (service router-api-service-type)
         (service draft-router-api-service-type)
         (service router-service-type)
         (service draft-router-service-type)
         (service authenticating-proxy-service-type))))

(define-public supporting-application-services
  (service-group
   "Supporting Applications"
   "Applications to support GOV.UK"
   (list (service content-audit-tool-service-type)
         (service content-data-admin-service-type)
         (service content-data-api-service-type)
         (service link-checker-api-service-type)
         (service search-admin-service-type)
         (service signon-service-type)
         (service support-service-type)
         (service release-service-type))))

(define-public transition-services
  (service-group
   "Transition Services"
   "Services to support the transition sites to GOV.UK"
   (list ;; bouncer-service
         ;; transition-service
    )))

(define-public monitoring-services
  (service-group
   "Transition Services"
   "Services to support the transition sites to GOV.UK"
   (list (service smokey-service-type))))

(define-public development-services
  (service-group
   "Development Services"
   "Services to support the development of GOV.UK"
   (list (service mini-environment-admin-service-type)
         (service publishing-e2e-tests-service-type))))

(define-public frontend-services
  (service-group
   "Frontend Services"
   "Services responsible for rendering GOV.UK"
   (list (service calculators-service-type)
         (service calendars-service-type)
         (service collections-service-type)
         (service email-alert-frontend-service-type)
         (service feedback-service-type)
         (service finder-frontend-service-type)
         (service frontend-service-type)
         (service government-frontend-service-type)
         (service info-frontend-service-type)
         (service licence-finder-service-type)
         (service manuals-frontend-service-type)
         (service service-manual-frontend-service-type)
         (service smart-answers-service-type)
         (service static-service-type))))

(define-public draft-frontend-services
  (service-group
   "Frontend Services"
   "Services responsible for rendering the draft GOV.UK"
   (list (service draft-collections-service-type)
         (service draft-email-alert-frontend-service-type)
         (service draft-frontend-service-type)
         (service draft-government-frontend-service-type)
         (service draft-finder-frontend-service-type)
         (service draft-manuals-frontend-service-type)
         (service draft-service-manual-frontend-service-type)
         (service draft-static-service-type))))

(define-public service-groups
  (list publishing-application-services
        backend-services
        publishing-platform-services
        supporting-application-services
        transition-services
        monitoring-services
        development-services
        frontend-services
        draft-frontend-services))

(define-public govuk-services
  (apply append
         (map service-group-services service-groups)))
