(define-module (gds packages utils bundler-build)
  #:use-module (ice-9 match)
  #:use-module (guix build utils)
  #:use-module (gnu build install)
  #:export (run-bundle-package))

(define (run-bundle-package source
                            output-path
                            working-directory
                            input-store-paths
                            ca-certificates-path
                            nss-certs-path
                            search-paths
                            bundle-without
                            ruby-version)
  (define (run . args)
    (simple-format #t "running ~A\n" (string-join args))
    (force-output)
    (zero? (apply system* args)))

  (set-path-environment-variable "PATH" '("bin" "sbin")
                                 input-store-paths)

  (for-each (match-lambda
              ((env-var (files ...) separator type pattern)
               (set-path-environment-variable
                env-var files
                (cons*
                 ca-certificates-path
                 input-store-paths)
                #:separator separator
                #:type type
                #:pattern pattern)))
            search-paths)

  (if (null? bundle-without)
      (unsetenv "BUNDLE_WITHOUT")
      (setenv "BUNDLE_WITHOUT" (string-join bundle-without ":")))

  (let* ((home (string-append working-directory "/HOME"))
         (vendor/cache (string-append output-path "/vendor/cache")))

    (mkdir-p vendor/cache)

    (mkdir-p home)
    (setenv "HOME" home)
    (setenv "GEM_HOME" home)

    (setenv "SSL_CERT_DIR" (string-append
                            nss-certs-path "/etc/ssl/certs"))
    (setenv "SSL_CERT_FILE" (string-append
                             ca-certificates-path
                             "/etc/ssl/certs/ca-certificates.crt"))

    (if (directory-exists? source)
        (for-each
         (lambda (file)
           (let ((from (string-append source "/" file))
                 (to   (string-append output-path "/" file)))
           (simple-format #t "Copying ~A from ~A to ~A\n" file from to)
           (copy-file from to)))
         '("Gemfile" "Gemfile.lock"))
        (run "tar"
             "--extract"
             "--anchored"
             "--wildcards"
             "--no-wildcards-match-slash"
             "--strip-components=1"
             "-C" output-path
             "-x"
             "-f" source
             "*/Gemfile*"))
    (simple-format #t "Using Gemfile ~A\n" (string-append
                                            output-path
                                            "/Gemfile\n"))
    (simple-format #t "Using Gemfile.lock ~A\n" (string-append
                                                 output-path
                                                 "/Gemfile.lock\n"))

    (simple-format #f "Setting .ruby-version to ~A\n" ruby-version)
    (call-with-output-file (string-append output-path "/.ruby-version")
      (lambda (port)
        (simple-format port "~A\n" ruby-version)))

    (chmod (string-append output-path "/Gemfile.lock") #o644)

    (with-directory-excursion output-path
      (run "bundle"
           "config"
           "build.nokogiri"
           "--use-system-libraries")
      (let loop ((retry 0))
        (unless (run "bundle"
                     "package"
                     "--all"
                     "--no-install")
          (if (> retry 3)
              (exit 1)
              (loop (+ retry 1))))))

    (let ((files (find-files output-path".*\\.gemspec")))
      (if (null? files)
          (simple-format #t "No gemspecs to substitute dates for\n")
          (begin
            (simple-format #t "Substituting dates in ~A\n"
                           (string-join files ", "))
            (substitute* files
              ((".*s\\.date = \".*\"")
               "  # date removed by govuk-guix")
              ((".*s\\.rubygems\\_version = .*$")
               "  # rubygems_version removed by govuk-guix\n")))))
    (reset-timestamps output-path)))
