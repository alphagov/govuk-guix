(define-module (gds data s3)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (gnu packages python-web)
  #:export (s3-fetch-for-profile))

(define* (s3-fetch-for-profile profile)
  (define environment-variables
    (let* ((json-data
            (read-line
             (open-pipe* OPEN_READ
                         "govuk" "aws" "--profile" profile
                         "--export-json"))))
      (hash-map->list
       (lambda (key value)
         (if (member key '("access_key_id" "secret_access_key" "session_token"))
             (cons (string-append "AWS_" (string-upcase key))
                   value)
             (error "Unknown key " key)))
       (json-string->scm json-data))))

  (lambda* (uri
            hash-algo hash
            #:optional name
            #:key (system (%current-system)) (guile (default-guile)))

    (define build
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (invoke #$(file-append awscli "/bin/aws")
                    "s3" "cp" (getenv "s3 uri") #$output))))

    (mlet %store-monad ((guile (package->derivation guile system)))
      (gexp->derivation (or name "s3-fetch")
                        build
                        ;; Use environment variables and a fixed script name so
                        ;; there's only one script in store for all the
                        ;; downloads.
                        #:script-name "s3-fetch"
                        #:env-vars
                        `(("s3 uri" . ,uri)
                          ,@environment-variables)
                        #:system system
                        #:local-build? #t
                        #:hash-algo hash-algo
                        #:hash hash
                        #:recursive? #t
                        #:guile-for-build guile))))
