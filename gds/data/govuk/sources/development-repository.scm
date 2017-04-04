(define-module (gds data govuk sources development-repository)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (guix gexp)
  #:use-module (gds services govuk)
  #:use-module (gds data data-source)
  #:use-module (gds data data-extract)
  #:use-module (gds data tar-extract)
  #:export (development-repository-data-source))

(define postgresql-extracts
  `(("postgresql-primary-1.backend.integration"
     ("support_contacts_production" . (,support-api-service-type))
     ("email-alert-api_production" . (,email-alert-api-service-type))
     ("publishing_api_production"  .(,publishing-api-service-type))
     ("policy_publisher_production" . (,policy-publisher-service-type))
     ("local-links-manager_production" . (,local-links-manager-service-type))
     ("content_tagger_production" . (,content-tagger-service-type))
     ("service-manual-publisher_production" . (,service-manual-publisher-service-type)))))

(define mysql-extracts
  `(("mysql-backup-1.backend.integration"
     ("collections_publisher_production" . (,collections-publisher-service-type))
     ("contacts_production" . (,contacts-admin-service-type))
     ("release_production" . (,release-service-type))
     ("search_admin_production" . (,search-admin-service-type))
     ("signon_production" . (,signon-service-type)))
    ("whitehall-mysql-backup-1.backend.integration"
     ("whitehall_production" . (,whitehall-service-type)))))

(define mongodb-extracts
  `(("mongo-1.backend.integration"
     ("govuk_assets_production" . (,asset-manager-service-type))
     ("publisher_production" . (,publisher-service-type))
     ("govuk_content_production" .
      (,content-api-service-type ,specialist-publisher-service-type))
     ("govuk_needs_production" . (,need-api-service-type))
     ("short_url_manager_production" . (,short-url-manager-service-type))
     ("imminence_production" . (,imminence-service-type))
     ("licence_finder_production" . (,licence-finder-service-type))
     ("manuals_publisher_production" . (,manuals-publisher-service-type))
     ("maslow_production" . (,maslow-service-type)))
    ("api-mongo-1.api.integration"
     ("draft_content_store_production" . (,draft-content-store-service-type))
     ("content_store_production" . (,content-store-service-type)))
    ("router-backend-1.router.integration"
     ("authenticating_proxy_production" . (,authenticating-proxy-service-type)))
     ("router" . (,router-service-type ,router-api-service-type))
     ("draft_router" .
      (,draft-router-service-type ,draft-router-api-service-type))))

(define (find-extracts backup-directory)
  (define (process-date-dir date stat . children)
    (concatenate
     (map (cut apply process-database-dir date <>) children)))

  (define (process-database-dir date database stat . children)
    (concatenate
     (map (cut apply process-hostname-dir date database <>) children)))

  (define (process-hostname-dir date database hostname stat . children)
    (let*
        ((extracts-by-hostname
          (or
           (assoc-ref
            `(("postgresql" . ,postgresql-extracts)
              ("mysql" . ,mysql-extracts)
              ("mongo" . ,mongodb-extracts)
              ("elasticsearch" #f))
            database)
           (begin
             (simple-format #t "Warning, unknown database ~A\n" database)
             #f)))
         (extract-name->services
          (if extracts-by-hostname
              (or (assoc-ref
                   extracts-by-hostname
                   hostname)
                  (begin
                    (simple-format #t "Warning, unknown hostname ~A (~A)\n"
                                   hostname
                                   (string-join
                                    (list date database hostname)
                                    "/"))
                    '()))
              '())))
      (map
       (lambda (extract-name)
         (data-extract
          (file (cond
                 ((equal? database "postgresql")
                  (tar-extract
                   (name extract-name)
                   (archive
                    (local-file
                     (string-join
                      (list backup-directory date database hostname "latest.tbz2")
                      "/")))
                   (member
                    (string-append "latest/" extract-name))))
                 ((equal? database "mysql")
                  (tar-extract
                   (name extract-name)
                   (archive
                    (local-file
                     (string-join
                      (list backup-directory date database hostname "latest.tbz2")
                      "/")))
                   (member
                    (string-append "latest/daily_" extract-name "*"))))
                 ((equal? database "mongo")
                  (let
                      ((filename
                        (first
                         (scandir
                          (string-join (list backup-directory date database hostname) "/")
                          (lambda (name) (string-suffix? ".tgz" name))))))
                    (tar-extract
                     (name extract-name)
                     (archive
                      (local-file
                       (string-join
                        (list backup-directory date database hostname filename)
                        "/")))
                     (member
                      (string-append (string-drop-right filename 4) "/" extract-name)))))
                 ((equal? database "elasticsearch")
                  '())
                 (else
                  (error "Unknown database ~A\n" database))))
          (datetime date)
          (database database)
          (services (assoc-ref extract-name->services extract-name))))
       (map car extract-name->services))))

  (let ((tree (file-system-tree backup-directory)))
    (concatenate
     (map (cut apply process-date-dir <>) (cddr tree)))))

(define list-extracts
  (lambda ()
    (let* ((development-repository-directory
            (or (getenv "GDS_GUIX_DEVELOPMENT_REPOSITORY")
                "../../../development"))
           (backup-directory
            (string-append development-repository-directory
                           "/replication/backups")))
      (if (file-exists? backup-directory)
          (find-extracts backup-directory)
          (error "Directory does not exist ~A\n" backup-directory)))))

(define-public development-repository-data-source
  (data-source
   (list-extracts list-extracts)))
