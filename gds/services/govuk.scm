(define-module (gds services govuk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (guix records)
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
  #:use-module (gds services govuk plek)
  #:use-module (gds services sidekiq)
  #:use-module (gds services delayed-job)
  #:use-module (gds services govuk signon)
  #:use-module (gds services rails)
  #:export (<signon-config>
            signon-config
            signon-config?
            signon-config-applications
            signon-config-users

            <router-config>
            router-config
            router-config?
            router-config-public-port
            router-config-api-port

            <router-api-config>
            router-api-config
            router-api-config?
            router-api-config-nodes))

;;;
;;; Utilities
;;;


(define (make-rails-app-using-plek-and-signon-service-type name)
  (let ((base-service-type
         (make-rails-app-using-plek-service-type name)))

    (define (update-service-startup-config-for-signon-application parameters)
      (let ((signon-application (find signon-application? parameters)))
        (if signon-application
            (map
             (lambda (parameter)
               (if (service-startup-config? parameter)
                   (service-startup-config-with-additional-environment-variables
                    parameter
                    `(("OAUTH_ID" . ,(signon-application-oauth-id
                                      signon-application))
                      ("OAUTH_SECRET" . ,(signon-application-oauth-secret
                                          signon-application))))
                   parameter))
             parameters)
            parameters)))

    (define (update-service-startup-config-for-signon-api-user parameters)
      (map
       (lambda (parameter)
         (if (service-startup-config? parameter)
             (service-startup-config-with-additional-environment-variables
              parameter
              (map
               (match-lambda
                 (($ <signon-authorisation> application-name token)
                  (let ((name
                         (string-append
                          (string-map
                           (lambda (c)
                             (if (eq? c #\space) #\_ c))
                           (string-upcase application-name))
                          "_BEARER_TOKEN")))
                    (cons name token))))
               (concatenate
                (map
                 (match-lambda
                   (($ <signon-api-user> name email authorisation-permissions)
                    (map car authorisation-permissions)))
                 (filter signon-api-user? parameters)))))
             parameter))
       parameters))

    (define (update-signon-application parameters)
      (let ((plek-config (find plek-config? parameters)))
        (if plek-config
            (map
             (lambda (parameter)
               (if (signon-application? parameter)
                   (let ((service-uri
                          (if (eq? name 'authenticating-proxy)
                              (plek-config-draft-origin plek-config)
                              (service-uri-from-plek-config plek-config name))))
                     (signon-application
                      (inherit parameter)
                      (home-uri service-uri)
                      (redirect-uri
                       (string-append service-uri "/auth/gds/callback"))))
                   parameter))
             parameters)
            parameters)))

    (service-type-extensions-modify-parameters
     (service-type
      (inherit base-service-type)
      (extensions
       (cons
        (service-extension signon-service-type
                           (lambda (parameters)
                             (filter
                              (lambda (parameter)
                                (or (signon-application? parameter)
                                    (signon-api-user? parameter)))
                              parameters)))
        (service-type-extensions base-service-type))))
     (lambda (parameters)
       (update-service-startup-config-for-signon-application
        (update-service-startup-config-for-signon-api-user
         (update-signon-application parameters)))))))

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
;;; Signon
;;;

(define-record-type* <signon-config>
  signon-config make-signon-config
  signon-config?
  (applications signon-config-applications
                (default '()))
  (users        signon-config-users
                (default '()))
  (api-users    signon-config-api-users
                (default '())))

(define-public signon-service-type
  (service-type
   (inherit
    (service-type-extensions-modify-parameters
     (make-rails-app-using-plek-service-type 'signon)
     (lambda (parameters)
       (let ((config (find signon-config? parameters)))
         (map
          (lambda (parameter)
            (if (service-startup-config? parameter)
                (service-startup-config-add-pre-startup-scripts
                 parameter
                 `((signon-setup-applications
                    .
                    ,#~(lambda ()
                         (run-command
                          "rails" "runner"
                          #$(signon-setup-applications-script
                             (signon-config-applications config)))))
                    (signon-setup-users
                    .
                    ,#~(lambda ()
                         (run-command
                          "rails" "runner"
                          #$(signon-setup-users-script
                             (map
                              (cut filter-signon-user-application-permissions
                                <> (signon-config-applications config))
                              (signon-config-users config))))))
                   (signon-setup-api-users
                    .
                    ,#~(lambda ()
                         (run-command
                          "rails" "runner"
                          #$(signon-setup-api-users-script
                             (signon-config-api-users config)))))))
                parameter))
          parameters)))))
   (compose concatenate)
   (extend (lambda (parameters extension-parameters)
             (map
              (lambda (parameter)
                (if (signon-config? parameter)
                    (signon-config
                     (inherit parameter)
                     (applications (append
                                    (signon-config-applications parameter)
                                    (filter signon-application?
                                            extension-parameters)))
                     (api-users (append
                                 (signon-config-api-users parameter)
                                 (filter signon-api-user?
                                         extension-parameters))))
                    parameter))
              parameters)))))

(define-public signon-service
  (service
   signon-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(signon))
           (requirement '(mysql loopback)))
          (service-startup-config)
          (plek-config) (rails-app-config) (@ (gds packages govuk) signon)
          (signon-config)
          (sidekiq-config
           (file "config/sidekiq.yml"))
          (mysql-connection-config
           (host "localhost")
           (user "halberd")
           (port "-")
           (database "signon_production")
           (password ""))
          (redis-connection-config))))

;;;
;;; Asset Manager
;;;

(define-public asset-manager-service-type
  (make-rails-app-using-plek-and-signon-service-type 'asset-manager))

(define-public asset-manager-service
  (service
   asset-manager-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(asset-manager))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) asset-manager
          (delayed-job-config)
          (signon-application
           (name "Asset Manager")
           (supported-permissions '("signin")))
          (service-startup-config
           (root-pre-startup-scripts
            `((mount-uploads
               .
               ,(with-imported-modules '((guix build utils)
                                         (guix build bournish)
                                         (gnu build file-systems))
                  #~(lambda ()
                      (use-modules (gds build utils)
                                   (gnu build file-systems))
                      (let ((app-dir "/var/apps/asset-manager/uploads")
                            (storage-dir "/var/lib/asset-manager-uploads"))
                        (mkdir-p storage-dir)
                        (bind-mount storage-dir app-dir)
                        (chmod app-dir #o777)
                        #t)))))))
          (mongodb-connection-config
           (database "asset_manager")))))

;;;
;;; Authenticating Proxy
;;;

(define-public authenticating-proxy-service-type
  (make-rails-app-using-plek-and-signon-service-type 'authenticating-proxy))

(define-public authenticating-proxy-service
  (service
   authenticating-proxy-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(authenticating-proxy))
           (requirement '(signon)))
          (plek-config) (rails-app-config) authenticating-proxy
          (service-startup-config)
          (signon-application
           (name "Content Preview")
           (supported-permissions '("signin")))
          (mongodb-connection-config
           (database "authenticating_proxy")))))

;;;
;;; Calculators
;;;

(define-public calculators-service-type
  (make-rails-app-using-plek-and-signon-service-type 'calculators))

(define-public calculators-service
  (service
   calculators-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(calculators))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) calculators
          (service-startup-config))))

;;;
;;; Calendars
;;;

(define-public calendars-service-type
  (make-rails-app-using-plek-and-signon-service-type 'calendars))

(define-public calendars-service
  (service
   calendars-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(calendars))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) calendars
          (service-startup-config))))

;;;
;;; Collections
;;;

(define-public collections-service-type
  (make-rails-app-using-plek-and-signon-service-type 'collections))

(define-public collections-service
  (service
   collections-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(collections))
           (requirement '(publishing-api signon)))
         (plek-config) (rails-app-config) collections
         (service-startup-config)
         (mongodb-connection-config
          (database "authenticating_proxy")))))

(define-public draft-collections-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-collections))

(define-public draft-collections-service
  (service
   draft-collections-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(draft-collections))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) collections
          (service-startup-config))))

;;;
;;; Collections Publisher
;;;

(define-public collections-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'collections-publisher))

(define-public collections-publisher-service
  (service
   collections-publisher-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(collections-publisher))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) collections-publisher
          (signon-application
           (name "Collections Publisher")
           (supported-permissions '("signin" "GDS Editor" "Edit Taxonomy" "Sidekiq Monitoring")))
          (signon-api-user
           (name "Collections Publisher")
           (email "collections-publisher@guix-dev.gov.uk")
           (authorisation-permissions
            (list
             (cons
              (signon-authorisation
               (application-name "Publishing API"))
              '("signin")))))
          (service-startup-config)
          (mysql-connection-config
           (user "collections-pub")
           (password (random-base16-string 30))
           (database "collections_publisher_production")))))

;;;
;;; Contacts Admin
;;;

(define-public contacts-admin-service-type
  (make-rails-app-using-plek-and-signon-service-type 'contacts-admin))

(define-public contacts-admin-service
  (service
   contacts-admin-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(contacts-admin))
           (requirement '(publishing-api whitehall-admin signon)))
          (plek-config) (rails-app-config) contacts-admin
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
           (user "contacts-admin")
           (password (random-base16-string 30))
           (database "contacts_production")))))

;;;
;;; Content Performance Manager
;;;

(define-public content-performance-manager-service-type
  (make-rails-app-using-plek-and-signon-service-type 'content-performance-manager))

(define-public content-performance-manager-service
  (service
   content-performance-manager-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(content-performance-manager))
           (requirement '(publishing-api whitehall-admin signon)))
          (sidekiq-config
           (file "config/sidekiq.yml"))
          (plek-config) (rails-app-config) content-performance-manager
          (signon-application
           (name "Content Performance Manager")
           (supported-permissions '("signin")))
          (signon-api-user
           (name "Content Performance Manager")
           (email "content-performance-manager@guix-dev.gov.uk")
           (authorisation-permissions
            (list
             (cons
              (signon-authorisation
               (application-name "Publishing API"))
              '("signin")))))
          (service-startup-config)
          (redis-connection-config)
          (postgresql-connection-config
           (user "content_performance_manager")
           (database "content_performance_manager_production")))))

;;;
;;; Design Principles
;;;

(define-public design-principles-service-type
  (make-rails-app-using-plek-and-signon-service-type 'design-principles))

(define-public design-principles-service
  (service
   design-principles-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(design-principles))
           (requirement '()))
          (plek-config) (rails-app-config) design-principles
          (service-startup-config))))

;;;
;;; Email Alert API
;;;

(define-public email-alert-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'email-alert-api))

(define-public email-alert-api-service
  (service
   email-alert-api-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(email-alert-api))
           (requirement '(postgres)))
          (plek-config) (rails-app-config) email-alert-api
          (service-startup-config)
          (redis-connection-config)
          (sidekiq-config
           (file "config/sidekiq.yml"))
          (postgresql-connection-config
           (user "email-alert-api")
           (database "email_alert_api")))))

;;;
;;; Email Alert Frontend
;;;

(define-public email-alert-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'email-alert-frontend))

(define-public email-alert-frontend-service
  (service
   email-alert-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(email-alert-frontend))
           (requirement '()))
          (plek-config) (rails-app-config) email-alert-frontend
          (service-startup-config))))

(define-public draft-email-alert-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-email-alert-frontend))

(define-public draft-email-alert-frontend-service
  (service
   draft-email-alert-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(draft-email-alert-frontend))
           (requirement '()))
          (plek-config) (rails-app-config) email-alert-frontend
          (service-startup-config))))

;;;
;;; Email Alert Service
;;;

;; TODO: This is not actually a Rails app...
(define-public email-alert-service-type
  (make-rails-app-using-plek-and-signon-service-type 'email-alert-service))

(define-public email-alert-service-service
  (service
   email-alert-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(email-alert-service))
           (requirement '()))
         (plek-config)
         (rails-app-config
          (run-with "bin/email_alert_service"))
         email-alert-service
         (service-startup-config)
         (redis-connection-config))))

;;;
;;; Feedback
;;;

(define-public feedback-service-type
  (make-rails-app-using-plek-and-signon-service-type 'feedback))

(define-public feedback-service
  (service
   feedback-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(feedback))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) feedback
          (service-startup-config))))

;;;
;;; Finder Frontend
;;;

(define-public finder-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'finder-frontend))

(define-public finder-frontend-service
  (service
   finder-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(finder-frontend))
           (requirement '()))
          (plek-config) (rails-app-config) finder-frontend
          (service-startup-config))))

;;;
;;; HMRC Manuals API
;;;

(define-public hmrc-manuals-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'hmrc-manuals-api))

(define-public hmrc-manuals-api-service
  (service
   hmrc-manuals-api-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(hmrc-manuals-api))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) hmrc-manuals-api
          (signon-application
           (name "HMRC Manuals Api")
           (supported-permissions '("signin")))
          (signon-api-user
           (name "HMRC Manuals Api")
           (email "hmrc-manuals-api@guix-dev.gov.uk")
           (authorisation-permissions
            (list
             (cons
              (signon-authorisation
               (application-name "Publishing API"))
              '("signin")))))
          (service-startup-config)
          (redis-connection-config))))

;;;
;;; Licence Finder
;;;

(define-public licence-finder-service-type
  (make-rails-app-using-plek-and-signon-service-type 'licence-finder))

(define-public licence-finder-service
  (service
   licence-finder-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(licence-finder))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) licence-finder
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
           (database "licence_finder")))))

;;;
;;; Local Links Manager
;;;

(define-public local-links-manager-service-type
  (make-rails-app-using-plek-and-signon-service-type 'local-links-manager))

(define-public local-links-manager-service
  (service
   local-links-manager-service-type
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
              '("signin")))))
          (signon-application
           (name "Local Links Manager")
           (supported-permissions '("signin")))
          (service-startup-config)
          (redis-connection-config)
          (postgresql-connection-config
           (user "local_links_manager")
           (database "local-links-manager_production")))))

;;;
;;; Imminence
;;;

(define-public imminence-service-type
  (make-rails-app-using-plek-and-signon-service-type 'imminence))

(define-public imminence-service
  (service
   imminence-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(imminence))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) imminence
          (signon-application
           (name "Imminence")
           (supported-permissions '("signin")))
          (service-startup-config)
          (mongodb-connection-config
           (database "imminence")))))

;;;
;;; Manuals Frontend
;;;

(define-public manuals-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'manuals-frontend))

(define-public manuals-frontend-service
  (service
   manuals-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(manuals-frontend))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) manuals-frontend
          (service-startup-config)
          (mongodb-connection-config
           (database "manuals_frontend")))))

(define-public draft-manuals-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-manuals-frontend))

(define-public draft-manuals-frontend-service
  (service
   draft-manuals-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(draft-manuals-frontend))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) manuals-frontend
          (service-startup-config)
          (mongodb-connection-config
           (database "manuals_frontend")))))

;;;
;;; Manuals Publisher
;;;

(define-public manuals-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'manuals-publisher))

(define-public manuals-publisher-service
  (service
   manuals-publisher-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(manuals-publisher))
           (requirement '(publishing-api
                          signon
                          whitehall-admin))) ;; Whitehall required for the Organisation API
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
              '("signin")))))
          (service-startup-config)
          (mongodb-connection-config
           (database "manuals_publisher")))))

;;;
;;; Policy Publisher
;;;

(define-public policy-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'policy-publisher))

(define-public policy-publisher-service
  (service
   policy-publisher-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(policy-publisher))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) policy-publisher
          (signon-application
           (name "Policy Publisher")
           (supported-permissions '("signin")))
          (signon-api-user
           (name "Policy Publisher")
           (email "policy-publisher@guix-dev.gov.uk")
           (authorisation-permissions
            (list
             (cons
              (signon-authorisation
               (application-name "Publishing API"))
              '("signin")))))
          (service-startup-config)
          (postgresql-connection-config
           (user "policy_publisher")
           (database "policy-publisher_production")))))

;;;
;;; Release
;;;

(define-public release-service-type
  (make-rails-app-using-plek-and-signon-service-type 'release))

(define-public release-service
  (service
   release-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(release))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) release
          (signon-application
           (name "Release")
           (supported-permissions '("signin" "deploy")))
          (service-startup-config)
          (mysql-connection-config
           (host "localhost")
           (user "release")
           (port "-")
           (database "release_production")
           (password "")))))

;;;
;;; Search Admin
;;;

(define-public search-admin-service-type
  (make-rails-app-using-plek-and-signon-service-type 'search-admin-publisher))

(define-public search-admin-service
  (service
   search-admin-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(search-admin))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) search-admin
          (signon-application
           (name "Search Admin")
           (supported-permissions '("signin")))
          (service-startup-config)
          (mysql-connection-config
           (host "localhost")
           (user "search_admin")
           (port "-")
           (database "search_admin_production")
           (password "")))))

;;;
;;; Service Manual Publisher
;;;

(define-public service-manual-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'service-manual-publisher))

(define-public service-manual-publisher-service
  (service
   service-manual-publisher-service-type
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
           (database "service-manual-publisher_production")))))

;;;
;;; Service Manual Frontend
;;;

(define-public service-manual-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'service-manual-frontend))

(define-public service-manual-frontend-service
  (service
   service-manual-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(service-manual-frontend))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) service-manual-frontend
          (service-startup-config))))

(define-public draft-service-manual-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-service-manual-frontend))

(define-public draft-service-manual-frontend-service
  (service
   draft-service-manual-frontend-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(draft-service-manual-frontend))
           (requirement '(publishing-api signon)))
          (plek-config) (rails-app-config) service-manual-frontend
          (service-startup-config))))

;;;
;;; Short Url Manager
;;;

(define-public short-url-manager-service-type
  (make-rails-app-using-plek-and-signon-service-type 'short-url-manager))

(define-public short-url-manager-service
  (service
   short-url-manager-service-type
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
           (database "short_url_manager")))))

;;;
;;; Smart Answers
;;;

(define-public smart-answers-service-type
  (make-rails-app-using-plek-and-signon-service-type 'smartanswers))

(define-public smart-answers-service
  (service
   smart-answers-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(smart-answers))
           (requirement '(publishing-api
                          signon
                          content-store
                          static
                          whitehall-admin)))
          (plek-config) (rails-app-config) smart-answers
          (service-startup-config))))

;;;
;;; Support
;;;

(define-public support-service-type
  (make-rails-app-using-plek-and-signon-service-type 'support))

(define-public support-service
  (service
   support-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(support))
          (requirement '(publishing-api signon)))
         (signon-application
          (name "Support")
          (supported-permissions '("signin")))
         (plek-config) (rails-app-config) support
         (redis-connection-config)
         (service-startup-config))))

;;;
;;; Support API
;;;

(define-public support-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'support-api))

(define-public support-api-service
  (service
   support-api-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(support-api))
          (requirement '(publishing-api signon)))
         (plek-config) (rails-app-config) support-api
         (service-startup-config)
         (postgresql-connection-config
          (user "support-api")
          (database "support_api")))))

;;;
;;; Travel Advice Publisher
;;;

(define-public travel-advice-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'travel-advice-publisher))

(define-public travel-advice-publisher-service
  (service
   travel-advice-publisher-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(travel-advice-publisher))
           (requirement '(publishing-api signon static rummager asset-manager)))
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
               (application-name "Asset Manager"))
              '("signin")))))
          (service-startup-config)
          (redis-connection-config)
          (sidekiq-config
           (file "config/sidekiq.yml"))
          (mongodb-connection-config
           (database "travel_advice_publisher")))))

;;
;; Publishing E2E Tests
;;

(define (make-publishing-e2e-tests-start-script environment-variables package)
  (let*
      ((environment-variables
        (append
         environment-variables
         `(("SECRET_KEY_BASE" . "t0a")
           ("CAPYBARA_SAVE_PATH" . "/var/apps/publishing-e2e-tests/")
           ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/apps/govuk-content-schemas")))))
    (program-file
     (string-append "start-publishing-e2e-tests")
     (with-imported-modules '((guix build utils)
                              (gnu services herd)
                              (srfi srfi-26)
                              (ice-9 popen)
                              (ice-9 rw)
                              (ice-9 rdelim))
       #~(let ((bundle (string-append #$package "/bin/bundle"))
               (test-results.html
                "/var/apps/publishing-e2e-tests/test-results.html"))
           (use-modules (guix build utils)
                        (gnu services herd)
                        (srfi srfi-26)
                        (ice-9 popen)
                        (ice-9 rw)
                        (ice-9 rdelim))

           (mkdir-p "/var/apps/publishing-e2e-tests")

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
                         "rspec"
                         "--format" "documentation"
                         "--format" "html"
                         "--out" test-results.html))))

             ;; Links to pages and screenshots are absolute, so turn
             ;; them in to relative links so that they work outside of
             ;; the container
             (let ((substring "file:///var/apps/publishing-e2e-tests/"))
               (with-atomic-file-replacement test-results.html
                 (lambda (in out)
                   (write-string/partial
                     (let filter ((str (read-string in)))
                       (let ((index (string-contains str substring)))
                         (if index
                             (filter
                              (string-replace
                               str
                               ""
                               index
                               (+ index (string-length substring))))
                             str)))
                     out))))

             ;; Change file permissions to be writable by all
             (for-each (lambda (f) (chmod f #o666))
                       (find-files "/var/apps/publishing-e2e-tests"))

             (if result
                 (call-with-output-file
                     "/var/apps/publishing-e2e-tests/all-tests-succeeded"
                   (lambda (port)
                     (simple-format port ""))))

             (stop-service 'root)
             result))))))

(define-public publishing-e2e-tests-service-type
  (service-type
   (name 'publishing-e2e-tests-service)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           (match-lambda
            ((plek-config package)
             (let* ((start-script
                     (make-publishing-e2e-tests-start-script
                      (plek-config->environment-variables plek-config)
                      package)))
               (list
                (shepherd-service
                 (provision (list 'publishing-e2e-tests))
                 (documentation "publishing-e2e-tests")
                 (requirement '(specialist-publisher
                                government-frontend
                                draft-government-frontend
                                travel-advice-publisher
                                nginx
                                asset-manager))
                 (respawn? #f)
                 (start #~(make-forkexec-constructor #$start-script))
                 (stop #~(make-kill-destructor))))))))))))

(define-public publishing-e2e-tests-service
  (service
   publishing-e2e-tests-service-type
   (list (plek-config) publishing-e2e-tests)))

;;;
;;; Publishing API Service
;;;

(define-public publishing-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'publishing-api))

(define-public publishing-api-service
  (service
   publishing-api-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(publishing-api))
           (requirement '(content-store draft-content-store signon
                          govuk-content-schemas redis loopback postgres)))
          (service-startup-config
           (environment-variables
            '(("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/apps/govuk-content-schemas"))))
          (plek-config) (rails-app-config) publishing-api
          (signon-application
           (name "Publishing API")
           (supported-permissions '("signon" "view_all")))
          (sidekiq-config
           (file "config/sidekiq.yml"))
          (postgresql-connection-config
           (user "publishing-api")
           (port "5432")
           (database "publishing_api_production"))
          (redis-connection-config))))

;;;
;;; Content store
;;;

(define-public content-store-service-type
  (make-rails-app-using-plek-and-signon-service-type 'content-store))

(define-public content-store-service
  (service
   content-store-service-type
   (list (shepherd-service
           (inherit default-shepherd-service)
           (provision '(content-store))
           (requirement '(router-api mongodb)))
          (service-startup-config)
          (plek-config) (rails-app-config) content-store
          (mongodb-connection-config
           (database "content-store")))))

(define-public draft-content-store-service-type
  (make-rails-app-using-plek-service-type 'draft-content-store))

(define-public draft-content-store-service
  (service
   draft-content-store-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(draft-content-store))
          (requirement '(draft-router-api mongodb)))
         (service-startup-config)
         (plek-config) (rails-app-config) content-store
         (mongodb-connection-config
          (database "draft-content-store")))))

;;;
;;; Specialist Publisher
;;;

(define-public specialist-publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'specialist-publisher))

(define-public specialist-publisher-service
  (service
   specialist-publisher-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(specialist-publisher))
          (requirement '(publishing-api asset-manager signon mongodb nginx)))
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
              (application-name "Asset Manager"))
             '("signin")))))
         (service-startup-config)
         (mongodb-connection-config
          (database "specialist_publisher"))
         (redis-connection-config))))

;;;
;;; Government Frontend
;;;

(define-public government-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'government-frontend))

(define-public government-frontend-service
  (service
   government-frontend-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(government-frontend))
          (requirement '(content-store static)))
         (service-startup-config
          (environment-variables
           '(("GOVUK_APP_NAME" . "government-frontend"))))
         (plek-config) (rails-app-config) government-frontend)))

(define-public draft-government-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-government-frontend))

(define-public draft-government-frontend-service
  (service
   draft-government-frontend-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(draft-government-frontend))
          (requirement '(draft-content-store draft-static)))
         (service-startup-config
          (environment-variables
           '(("GOVUK_APP_NAME" . "draft-government-frontend"))))
         (plek-config) (rails-app-config) government-frontend)))

;;;
;;; Frontend
;;;

(define-public frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'frontend))

(define-public frontend-service
  (service
   frontend-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(frontend))
          (requirement '(static rummager content-store)))
         (service-startup-config)
         (plek-config) (rails-app-config) frontend)))

(define-public draft-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-frontend))

(define-public draft-frontend-service
  (service
   draft-frontend-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(draft-frontend))
          (requirement '(draft-static rummager draft-content-store)))
         (service-startup-config)
         (plek-config) (rails-app-config) frontend)))

;;;
;;; Publisher
;;;

(define-public publisher-service-type
  (make-rails-app-using-plek-and-signon-service-type 'publisher))

(define-public publisher-service
  (service
   publisher-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(publisher))
          (requirement '(publishing-api frontend draft-frontend
                         rummager signon)))
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
             '("signin")))))
         (sidekiq-config
          (file "config/sidekiq.yml"))
         (mongodb-connection-config
          (database "govuk_content_production")))))

;;;
;;; Router
;;;

(define-record-type* <router-config>
  router-config make-router-config
  router-config?
  (public-port router-config-public-port
               (default 8080))
  (api-port router-config-api-port
            (default 8081))
  (debug? router-config-debug
          (default #f)))

(define router-config->environment-variables
  (match-lambda
    (($ <router-config> public-port api-port debug?)
     (append
      (list
       (cons "ROUTER_PUBADDR" (simple-format #f ":~A" public-port))
       (cons "ROUTER_APIADDR" (simple-format #f ":~A" api-port)))
      (if debug?
          (list (cons "DEBUG" "true"))
          '())))))

(define (make-router-shepherd-service name)
  (match-lambda
    ((router-config package rest ...)
     (let ((environment-variables
            (map
             (match-lambda
              ((key . value)
               (string-append key "=" value)))
             (map
              (match-lambda
               ((name . value)
                (cond
                 ((equal? name "MONGO_DB")
                  (cons "ROUTER_MONGO_DB" value))
                 ((equal? name "MONGODB_URI")
                  (cons "ROUTER_MONGO_URL" value))
                 (else
                  (cons name value)))))
              (append
               (router-config->environment-variables router-config)
               (concatenate
                (map database-connection-config->environment-variables
                     (filter database-connection-config? rest)))))))
           (user (getpwnam "nobody"))
           (string-service-name
            (symbol->string name)))
       (list
        (shepherd-service
         (provision (list name))
         (documentation string-service-name)
         (requirement '(mongodb))
         (respawn? #f)
         (start #~(make-forkexec-constructor
                   (string-append #$package "/bin/router")
                   #:user (passwd:uid #$user)
                   #:environment-variables '#$environment-variables
                   #:log-file (string-append "/var/log/" #$string-service-name)))
         (stop #~(make-kill-destructor))))))))

(define (make-router-service-type name)
  (service-type
   (name name)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (make-router-shepherd-service name))))))

(define default-router-database-connection-configs
  (list
   (mongodb-connection-config
    (database "router"))))

(define-public router-service-type
  (make-router-service-type 'router))

(define-public router-service
  (service
   router-service-type
   (cons* (router-config) router
          default-router-database-connection-configs)))

(define default-draft-router-database-connection-configs
  (list
   (mongodb-connection-config
    (database "draft-router"))))

(define-public draft-router-service-type
  (make-router-service-type 'draft-router))

(define-public draft-router-service
  (service
   draft-router-service-type
   (cons* (router-config) router
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
  (make-rails-app-using-plek-and-signon-service-type 'router-api))

(define-public router-api-service
  (service
   router-api-service-type
   (cons* (shepherd-service
           (inherit default-shepherd-service)
           (provision '(router-api))
           (requirement '(router)))
          (service-startup-config)
          (plek-config) (rails-app-config) router-api
          (router-api-config)
          default-router-database-connection-configs)))

(define-public draft-router-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'draft-router-api))

(define-public draft-router-api-service
  (service
   draft-router-api-service-type
   (cons* (shepherd-service
           (inherit default-shepherd-service)
           (provision '(draft-router-api))
           (requirement '(draft-router)))
          (service-startup-config)
          (plek-config) (rails-app-config) router-api
          (router-api-config)
          default-draft-router-database-connection-configs)))

;;;
;;; Content Tagger
;;;

(define-public content-tagger-service-type
  (make-rails-app-using-plek-and-signon-service-type 'content-tagger))

(define-public content-tagger-service
  (service
   content-tagger-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(content-tagger))
          (requirement '(publishing-api signon)))
         (service-startup-config)
         (signon-application
          (name "Content Tagger")
          (supported-permissions '("signin")))
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
         (postgresql-connection-config
          (user "content-tagger")
          (port "5432")
          (database "content_tagger")))))

;;;
;;; Maslow
;;;

(define-public maslow-service-type
  (make-rails-app-using-plek-and-signon-service-type 'maslow))

(define-public maslow-service
  (service
   maslow-service-type
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
          (database "maslow")))))

;;;
;;; Need API
;;;

(define-public need-api-service-type
  (make-rails-app-using-plek-and-signon-service-type 'need-api))

(define-public need-api-service
  (service
   need-api-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(need-api))
          (requirement '(publishing-api signon)))
         (service-startup-config)
         (signon-application
          (name "Need API")
          (supported-permissions '("signin" "write")))
         (plek-config) (rails-app-config) need-api
         (mongodb-connection-config
          (database "govuk_needs_development")))))

;;;
;;; Rummager
;;;

(define-public rummager-service-type
  (make-rails-app-using-plek-service-type 'rummager))

(define-public rummager-service
  (service
   rummager-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(rummager))
          (requirement '(content-store publishing-api static)))
         (service-startup-config)
         (redis-connection-config)
         (plek-config) (rails-app-config)
         rummager)))

;;;
;;; Info Frontend
;;;

(define-public info-frontend-service-type
  (make-rails-app-using-plek-and-signon-service-type 'info-frontend))

(define-public info-frontend-service
  (service
   info-frontend-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(info-frontend))
          (requirement '(content-store publishing-api static)))
         (signon-api-user
          (name "Info Frontend")
          (email "info-frontend@guix-dev.gov.uk")
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
                    (run-command "rake" "publishing_api:publish_special_routes")))))
         (plek-config) (rails-app-config)
         info-frontend)))

;;;
;;; Static service
;;;

(define-public static-service-type
  (make-rails-app-using-plek-service-type 'static))

(define-public static-service
  (service
   static-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(static))
          (requirement '(publishing-api)))
         (service-startup-config) (plek-config) (rails-app-config)
         static)))

(define-public draft-static-service-type
  (make-rails-app-using-plek-service-type 'draft-static))

(define-public draft-static-service
  (service
   draft-static-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(draft-static))
          (requirement '(publishing-api)))
         (service-startup-config
          (environment-variables
           '(("DRAFT_ENVIRONMENT" . "true"))))
         (plek-config) (rails-app-config) static)))

;;;
;;; Whitehall
;;;

(define-public whitehall-service-type
  (make-rails-app-using-plek-and-signon-service-type 'whitehall-admin))

(define-public whitehall-service
  (service
   whitehall-service-type
   (list (shepherd-service
          (inherit default-shepherd-service)
          (provision '(whitehall-admin))
          (requirement '(publishing-api signon static need-api)))
         (service-startup-config)
         (plek-config) (rails-app-config) whitehall
         (signon-application
          (name "Whitehall")
          (supported-permissions '("signin" "Editor" "GDS Editor" "GDS Admin"
                                   "Import CSVs" "Managing Editor" "Upload Executable File Attachments"
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
              (application-name "Need API"))
             '("signin")))))
         (sidekiq-config
          (file "config/sidekiq.yml"))
         (redis-connection-config)
         (mysql-connection-config
          (host "localhost")
          (user "whitehall")
          (port "-")
          (database "whitehall_production")
          (password "")))))

;;;
;;; Service Lists
;;;

(define-public publishing-application-services
  (list
   collections-publisher-service
   contacts-admin-service
   content-tagger-service
   local-links-manager-service
   manuals-publisher-service
   maslow-service
   policy-publisher-service
   publisher-service
   service-manual-publisher-service
   short-url-manager-service
   specialist-publisher-service
   travel-advice-publisher-service
   whitehall-service))

(define-public api-services
  (list
   content-store-service
   draft-content-store-service
   ;; email-alert-api-service Can't connect to Redis for some reason
   ;; email-alert-service-service Missing dependency on RabbitMQ
   need-api-service
   imminence-service
   publishing-api-service
   rummager-service
   asset-manager-service
   router-api-service
   draft-router-api-service
   support-api-service
   hmrc-manuals-api-service
   ;; mapit-service
   ))

(define-public supporting-application-services
  (list
   ;; bouncer-service
   authenticating-proxy-service
   content-performance-manager-service
   search-admin-service
   signon-service
   support-service
   ;; transition-service
   release-service
   router-service
   draft-router-service))

(define-public frontend-services
  (list
   calculators-service
   calendars-service
   collections-service
   design-principles-service
   email-alert-frontend-service
   feedback-service
   finder-frontend-service
   frontend-service
   government-frontend-service
   info-frontend-service
   licence-finder-service
   manuals-frontend-service
   service-manual-frontend-service
   smart-answers-service
   static-service))

(define-public draft-frontend-services
  (list
   draft-collections-service
   draft-email-alert-frontend-service
   draft-frontend-service
   draft-government-frontend-service
   draft-manuals-frontend-service
   draft-service-manual-frontend-service
   draft-static-service))

(define-public govuk-services
  (append
   publishing-application-services
   api-services
   supporting-application-services
   frontend-services
   draft-frontend-services))
