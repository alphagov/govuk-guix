(define-module (gds data govuk sources govuk-puppet)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (guix gexp)
  #:use-module (gds services govuk)
  #:use-module (gds data data-source)
  #:use-module (gds data data-extract)
  #:use-module (gds data tar-extract)
  #:export (govuk-puppet-data-source))

(define postgresql-extracts
  `(("postgresql-primary-1.backend.integration"
     ("content_performance_manager_production" .
      (,content-performance-manager-service-type))
     ("content_tagger_production" . (,content-tagger-service-type))
     ("email-alert-api_production" . (,email-alert-api-service-type))
     ("local-links-manager_production" . (,local-links-manager-service-type))
     ("policy-publisher_production" . (,policy-publisher-service-type))
     ("publishing_api_production"  .(,publishing-api-service-type))
     ("service-manual-publisher_production" . (,service-manual-publisher-service-type))
     ("support_contacts_production" . (,support-api-service-type)))))

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
     ("govuk_content_production" .
      (,publisher-service-type
       ,specialist-publisher-service-type
       ,manuals-publisher-service-type))
     ("govuk_needs_production" . (,need-api-service-type))
     ("imminence_production" . (,imminence-service-type))
     ("licence_finder_production" . (,licence-finder-service-type))
     ;;("manuals_publisher_production" . (,manuals-publisher-service-type))
     ("maslow_production" . (,maslow-service-type))
     ("short_url_manager_production" . (,short-url-manager-service-type))
     ("travel_advice_publisher_production" .
      (,travel-advice-publisher-service-type)))
    ("api-mongo-1.api.integration"
     ("content_store_production" . (,content-store-service-type))
     ("draft_content_store_production" . (,draft-content-store-service-type)))
    ("router-backend-1.router.integration"
     ("authenticating_proxy_production" . (,authenticating-proxy-service-type))
     ("draft_router" .
      (,draft-router-service-type ,draft-router-api-service-type))
     ("router" . (,router-service-type ,router-api-service-type)))))

(define (find-extracts backup-directory)
  (define (process-date-dir date stat . children)
    (concatenate
     (map (cut apply process-database-dir date <>) children)))

  (define (process-database-dir date database stat . children)
    (concatenate
     (map (cut apply process-hostname-dir date database <>) children)))

  (define (process-hostname-dir date database hostname stat . children)
    (let* ((get-extract-local-file
            (assoc-ref
             `(("postgresql" . ,postgresql-extract-local-file)
               ("mysql" . ,mysql-extract-local-file)
               ("mongo" . ,mongo-extract-local-file)
               ("elasticsearch" . #f))
             database))
           (local-file
            (if get-extract-local-file
                (get-extract-local-file date database hostname)
                #f)))
      (if local-file
          (process-local-file date database hostname local-file)
          '())))

  (define (generic-latest.tbz2-local-file date database hostname)
    (let ((path
           (string-join
            (list backup-directory date database hostname "latest.tbz2")
            "/")))
      (if (file-exists? path)
          (local-file path)
          #f)))

  (define postgresql-extract-local-file
    generic-latest.tbz2-local-file)

  (define mysql-extract-local-file
    generic-latest.tbz2-local-file)

  (define (mongo-extract-local-file date database hostname)
    (let
        ((files
          (scandir
           (string-join (list backup-directory date database hostname) "/")
           (lambda (name) (string-suffix? ".tgz" name)))))
      (if (null? files)
          #f
          (local-file
           (string-join
            (list backup-directory date database hostname (first files))
            "/")))))

  (define (process-local-file date database hostname local-file)
    (let*
        ((extracts-by-hostname
          (or
           (assoc-ref `(("postgresql" . ,postgresql-extracts)
                        ("mysql" . ,mysql-extracts)
                        ("mongo" . ,mongodb-extracts)
                        ("elasticsearch" #f))
                      database)
           (begin
             (simple-format #t "Warning, unknown database ~A\n" database)
             #f)))
         (extract-name->services
          (if extracts-by-hostname
              (or (assoc-ref extracts-by-hostname hostname)
                  (begin
                    (simple-format #t "Warning, unknown hostname ~A (~A)\n"
                                   hostname
                                   (string-join
                                    (list date database hostname)
                                    "/"))
                    '()))
              '())))
      (filter-map
       (lambda (extract-name)
         (let ((get-extract-file
                (assoc-ref `(("postgresql" . ,postgresql-extract-file)
                             ("mysql" . ,mysql-extract-file)
                             ("mongo" . ,mongo-extract-file))
                           database)))
           (data-extract
            (file (get-extract-file local-file extract-name))
            (datetime (string->date date "~Y-~m-~d"))
            (database database)
            (services (assoc-ref extract-name->services extract-name)))))
       (map car extract-name->services))))

  (define (postgresql-extract-file local-file extract-name)
    (tar-extract
     (name (string-append extract-name ".sql.gz"))
     (archive local-file)
     (member
      (string-append "latest/" extract-name "_*.sql.gz"))
     (strip-components 1)))

  (define (mysql-extract-file local-file extract-name)
    (tar-extract
     (name (string-append extract-name ".sql.bz2"))
     (archive local-file)
     (member
      (string-append "latest/daily_" extract-name "*"))
     (strip-components 1)))

  (define (mongo-extract-file local-file extract-name)
    (tar-extract
     (name extract-name)
     (archive local-file)
     (member (string-append "*/" extract-name))
     (strip-components 1)))

  (let ((tree (file-system-tree backup-directory)))
    (concatenate
     (map (cut apply process-date-dir <>) (cddr tree)))))

(define list-extracts
  (lambda ()
    (let* ((govuk-puppet-directory
            (or (getenv "GDS_GUIX_GOVUK_PUPPET_REPOSITORY")
                (let ((govuk-guix-root
                       (or (and=>
                            (current-filename)
                            (lambda (x)
                              (string-drop-right
                               x
                               (string-length "gds/data/govuk/sources/govuk-puppet.scm"))))
                           (getenv "GOVUK_GUIX_ROOT")
                           (error "Unable to locate the govuk-guix root"))))
                  (string-append
                   (dirname govuk-guix-root)
                   "/govuk-puppet"))))
           (backup-directory
            (string-append govuk-puppet-directory
                           "/development-vm/replication/backups")))
      (if (file-exists? backup-directory)
          (find-extracts backup-directory)
          (error "Directory does not exist ~A\n" backup-directory)))))

(define-public govuk-puppet-data-source
  (data-source
   (list-extracts list-extracts)))
