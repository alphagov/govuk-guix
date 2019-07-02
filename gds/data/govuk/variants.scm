(define-module (gds data govuk variants)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gds systems govuk base)
  #:use-module (gds services govuk)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds data data-extract)
  #:use-module (gds data transformations)
  #:use-module (gds data transformations postgresql)
  #:use-module (gds data transformations mongodb)
  #:export (mongodb-extract-plus-variants
            postgresql-extract-plus-variants))

;;;
;;; MongoDB
;;;

(define (mongodb-extract-plus-variants base-extract database-name)
  (list
   ;; The base-extract is a archive file, with a priority value of 1
   base-extract
   ;; Generate a variant which uses a directory, rather than a single
   ;; file. This is useful for compatibility with older versions of MongoDB.
   (data-extract (inherit base-extract)
                 (file
                  (mongodb-convert-archive-to-directory
                   (data-extract-file base-extract)
                   database-name))
                 (variant-name "directory")
                 (variant-label "Dump directory")
                 (variant-properties
                  '((format . directory)
                    (priority . 0)))
                 (directory? #t))))

;;;
;;; PostgreSQL
;;;

(define (postgresql-extract-plus-variants base-extract)
  (define custom-postgresql-extract-variants
    `(("publishing_api_production" . ,publishing-api-variants)
      ("content_performance_manager_production" . ,content-data-api-variants)
      ;; This is mostly for testing, as content-tagger has a small database
      ("content_tagger_production" . , content-tagger-variants)))

  (define variant-details
    ;; The variant-details are defined as a alist, where the keys are the
    ;; priority values, and the values are a list with 3 elements: name, label
    ;; and the SQL to generate the variant
    (cons
     ;; Given the base extract is a plain SQL file, generate a variant with no
     ;; changes to the data, but instead using the pg_dump directory format
     ;; (which is the default way the PostgreSQL variants are dumped).
     '(1 . ("directory"
            "Directory format dump."
            ()))
     ;; Add any custom variants for this extract
     (or (assoc-ref custom-postgresql-extract-variants
                    (data-extract-name base-extract))
         '())))

  (define postgresql-service
    (find (lambda (service)
            (eq? (service-kind service)
                 postgresql-service-type))
          optional-services))

  (define format
    (assq-ref (data-extract-variant-properties base-extract)
              'format))

  (define transformation
    (postgresql-multi-output-data-transformation
     postgresql-service
     base-extract
     (database-connection-config-for-data-extract base-extract)
     (map cdr variant-details)
     #:post-restore-superuser-sql
     `(;; This avoids errors when restoring the dump
       ;; with a user that doesn't have permission to
       ;; comment on the default plpgsql
       ;; schema.
       "COMMENT ON EXTENSION plpgsql IS null"
       ,@(if (string=? format "custom")
             '(;; Remove the use of the aws_db_admin role, so that
               ;; it's not required when restoring
               "REVOKE ALL ON SCHEMA public FROM aws_db_admin"
               "DROP ROLE aws_db_admin")
             '()))
     #:pre-restore-superuser-sql
     `(,@(if (string=? format "custom")
             '(;; Otherwise restoring data with dumps from AWS fails
               ;; with:
               ;;
               ;;  could not execute query: ERROR:  role "rdsadmin" does not exist
               ;;  Command was: REVOKE ALL ON SCHEMA public FROM rdsadmin;
               ;;
               ;;  could not execute query: ERROR:  role "aws_db_admin" does not exist
               ;;  GRANT ALL ON SCHEMA public TO aws_db_admin
               ;;
               "CREATE ROLE rdsadmin"
               "CREATE ROLE aws_db_admin")
             '()))))

  (cons
   ;; The base extract is plain compressed SQL, with a priority value of 0.
   base-extract
   ;; The other variants correspond to different outputs of the transformation
   ;; defined above.
   (map (match-lambda
          ((priority name label sql)
           (data-extract
            (inherit base-extract)
            (file (gexp-output-alias (gexp transformation)
                                     (output name)))
            (variant-name name)
            (variant-label label)
            (variant-properties `((format . "directory")
                                  (priority . ,priority)))
            (directory? #t))))
        variant-details)))

(define (database-connection-config-for-data-extract extract)
  (define (configure-service service)
    (first
     (map
      (cut update-service-database-connection-config-for-environment
        "production" <>)
      (list service))))

  (find postgresql-connection-config?
        (service-parameters
         (configure-service
          (find (lambda (service)
                  (eq? (service-kind service)
                       (first
                        (data-extract-services extract))))
                govuk-services)))))

;; The variant-details are defined as a alist, where the keys are the priority
;; values, and the values are a list with 3 elements: name, label and the SQL
;; to generate the variant. These custom variants are in addition to the
;; default plain and directory variants, which have a priority or 0 and 1
;; respectively.

(define content-tagger-variants
  '((2 . ("no-taxonomy-health-warnings"
          "No taxonomy health warnings."
          ("TRUNCATE taxonomy_health_warnings")))))

(define publishing-api-variants
  (let ((truncate-expanded-links
         '(;; This table is used as a cache, so just truncate it
           "TRUNCATE expanded_links"))

        (delete-all-but-one-month-of-actions
         '(;; actions are not very useful, so remove most of them
           "CREATE TEMP TABLE tmp_actions AS SELECT * FROM actions WHERE created_at > (CURRENT_DATE - INTERVAL '1 months')"
           "CREATE TEMP TABLE tmp_link_changes AS SELECT * FROM link_changes WHERE action_id IN (SELECT id FROM tmp_actions)"
           "TRUNCATE actions CASCADE"
           "INSERT INTO actions SELECT * FROM tmp_actions"
           "DROP TABLE tmp_actions"
           "INSERT INTO link_changes SELECT * FROM tmp_link_changes"
           "DROP TABLE tmp_link_changes"))

        (delete-all-but-one-month-of-events
         '("CREATE TEMP TABLE tmp_events AS SELECT * FROM events WHERE created_at > (CURRENT_DATE - INTERVAL '1 months')"
           "TRUNCATE events"
           "INSERT INTO events SELECT * FROM tmp_events"))

        (set-details-to-NULL-for-old-superseded-editions
         '("UPDATE editions SET details = NULL WHERE id IN (SELECT id FROM editions WHERE state = 'superseded' AND updated_at < (CURRENT_DATE - INTERVAL '1 months'))")))

    `((2 . ("no-expanded-links"
            "Expanded_links table truncated."
            ,truncate-expanded-links))
      (3 . ("small"
            "Expanded links table truncated, only 1 month of actions, events, link_changes and set details to NULL for superseded editions older than 1 month."
            ,(append delete-all-but-one-month-of-actions
                     delete-all-but-one-month-of-events
                     set-details-to-NULL-for-old-superseded-editions))))))

(define content-data-api-variants
  (let* ((select-top-n-for-each-document-type-from-facts-metrics
          (lambda (n)
            (string-append
             "SELECT * FROM facts_metrics WHERE dimensions_edition_id IN ("
             "SELECT selected.dimensions_edition_id FROM "
             "(SELECT DISTINCT document_type FROM dimensions_editions) AS document_types, "
             "LATERAL ("
             "SELECT aggregations_search_last_thirty_days.dimensions_edition_id "
             "FROM aggregations_search_last_thirty_days "
             "WHERE document_type = document_types.document_type "
             "ORDER BY upviews DESC LIMIT " (number->string n)
             ") AS selected"
             ")")))

         (trim-facts-metrics
          `(,(string-append
              "CREATE TEMP TABLE tmp_facts_metrics AS "
              (select-top-n-for-each-document-type-from-facts-metrics 1000))
            "TRUNCATE facts_metrics"
            "INSERT INTO facts_metrics SELECT * FROM tmp_facts_metrics"))

         (slim-publishing-api-events
          `(,(string-append
              "UPDATE publishing_api_events SET payload = '{}'::jsonb WHERE id NOT IN ("
              "SELECT id FROM publishing_api_events ORDER BY id DESC LIMIT 100"
              ")")))

         (refresh-materialized-views
          (map (lambda (view)
                 (string-append
                  "REFRESH MATERIALIZED VIEW " view))
               '("aggregations_search_last_months"
                 "aggregations_search_last_six_months"
                 "aggregations_search_last_thirty_days"
                 "aggregations_search_last_three_months"
                 "aggregations_search_last_twelve_months"))))

    `((2 . ("small"
            "Only the 10000 items for each document_type with the most unique page views"
            ,(append trim-facts-metrics
                     slim-publishing-api-events
                     refresh-materialized-views))))))
