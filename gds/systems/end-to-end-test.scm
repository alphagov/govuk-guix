(define-module (gds systems end-to-end-test)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services databases)
  #:use-module (gds packages govuk)
  #:use-module (gds packages mongodb)
  #:use-module (gds services govuk)
  #:use-module (gds services base)
  #:use-module (gds services mongodb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix store))

(define %base-services
  (list
   (syslog-service)
   (urandom-seed-service)
   (nscd-service)
   (guix-service)))

(define system-ports
  `((postgresql . 55432)
    (mongodb . 57017)
    (mysql . 53306)
    (publishing-api . 53039)
    (content-store . 53000)
    (draft-content-store . 53001)
    (specialist-publisher . 53064)))

(define github-url-regex
  (make-regexp
   "https:\\/\\/github\\.com\\/[^\\/]*\\/[^\\/]*\\/archive\\/([^\\/]*)\\.tar\\.gz"))

(define* (custom-github-archive-source-for-package
          app-package
          commit-ish)
  (let*
      ((old-url
        (origin-uri (package-source app-package)))
       (regexp-match
        (regexp-exec github-url-regex old-url)))
    (if (not (regexp-match? regexp-match))
        (error "No match"))
    (with-store store
      (download-to-store
       store
       (string-replace
        old-url
        commit-ish
        (match:start regexp-match 1)
        (match:end regexp-match 1))
       #:recursive? #t))))

(define environment-variable-regex
  (make-regexp
   "GDS_GUIX_([A-Z0-9_]*)_COMMIT_ISH=(.*)"))

(define (get-package-commit-ish-list-from-environment)
  (map
   (lambda (name-value-match)
     (cons
      (string-map
       (lambda (c)
         (if (eq? c #\_) #\- c))
       (string-downcase
        (match:substring name-value-match 1)))
      (match:substring name-value-match 2)))
   (filter
    regexp-match?
    (map
     (lambda (name-value)
       (regexp-exec environment-variable-regex
                    name-value))
     (environ)))))

(define end-to-end-test-os
  (operating-system
    (host-name "govuk-test")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (bootloader (grub-configuration (device "/dev/sdX")))
    (file-systems
     (cons (file-system
             (device "my-root")
             (title 'label)
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))
    (services
     (let*
         ((package-commit-ish-list
           (get-package-commit-ish-list-from-environment))
          (correct-source-of
           (lambda (app-package)
             (let
                 ((custom-commit-ish (assoc-ref package-commit-ish-list
                                                (package-name app-package))))
               (if custom-commit-ish
                   (package
                     (inherit app-package)
                     (source
                      (custom-github-archive-source-for-package
                       app-package
                       custom-commit-ish)))
                   app-package)))))
       (for-each
        (match-lambda
          ((package . commit-ish)
           (simple-format
            #t
            "Using commit-ish \"~A\" for the ~A package\n"
            commit-ish
            package)))
        package-commit-ish-list)
       (parameterize ((ports system-ports))
         (cons*
          (publishing-api-service
           #:package (correct-source-of publishing-api))
          (content-store-service
           #:package (correct-source-of content-store))
          (draft-content-store-service
           #:package (correct-source-of content-store))
          (specialist-publisher-service
           #:package (correct-source-of specialist-publisher))
          (govuk-content-schemas-service
           #:package (correct-source-of govuk-content-schemas))
          (publishing-e2e-tests-service
           #:package (correct-source-of publishing-e2e-tests))
          (postgresql-service #:port (number->string (port-for 'postgresql)))
          (mongodb-service #:port (port-for 'mongodb))
          (mysql-service #:config (mysql-configuration
                                   (port (port-for 'mysql))))
          (/usr/share/zoneinfo-service)
          (/usr/bin/env-service)
          %base-services))))))

end-to-end-test-os
