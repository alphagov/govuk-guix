(define-module (gds data transformations postgresql)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services databases)
  #:export (with-postgresql))

(define* (with-postgresql
          postgresql-service
          gexp-to-run
          #:key (base-directory #~(string-append
                                   (getcwd)
                                   "/postgresql")))
  (let ((shepherd-service
         (first
          ((service-extension-compute
            (find (lambda (service-extension)
                    (eq? shepherd-root-service-type
                         (service-extension-target service-extension)))
                  (service-type-extensions (service-kind postgresql-service))))
           (service-parameters postgresql-service))))
        (postgresql
         ((@@ (gnu services databases)
              postgresql-configuration-postgresql)
          (service-parameters postgresql-service))))
    (with-imported-modules '((guix build utils)
                             (gds data transformations build postgresql))
      #~(begin
          (add-to-load-path #$(file-append shepherd
                                           "/share/guile/site/"
                                           (effective-version)))

          (use-modules (shepherd service)
                       (guix build utils)
                       (gds data transformations build postgresql))

          (define base-directory #$base-directory)

          (setenv "PATH"
                  (list->search-path-as-string
                   (cons*
                    (string-append
                     #$((@@ (gnu services databases)
                            postgresql-configuration-postgresql)
                        (service-parameters postgresql-service))
                     "/bin")
                    (string-append #$pv "/bin")
                    (string-append #$gzip "/bin")
                    (string-append #$glibc "/bin")
                    (string-append #$xz "/bin")
                    (search-path-as-string->list (getenv "PATH")))
                   ":"))
          (setenv "GUIX_LOCPATH" #$(file-append glibc-locales "/lib/locale"))
          (setenv "LC_ALL" "en_GB.UTF-8")
          (setenv "PGUSER" "postgres")

          (define data-directory
            (string-append base-directory "/data"))

          (define (activate)
            (simple-format #t "activating PostgreSQL\n")
            (mkdir-p data-directory)
            (chmod data-directory #o700)
            (invoke "initdb"
                    "--locale=en_GB.UTF-8"
                    "-U" "postgres"
                    "-D"
                    data-directory))

          (define (pg_ctl . args)
            (let ((command
                   `(,(string-append #$postgresql "/bin/pg_ctl")
                     "-D" ,data-directory
                     ,@args)))
              (simple-format #t "running: ~A\n" (string-join command " "))
              (apply invoke command)))

          (activate)
          (pg_ctl "start")
          (let ((result
                 (#$gexp-to-run '())))

            (pg_ctl "stop")

            result)))))
