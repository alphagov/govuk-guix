(define-module (gds packages utils bundler)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages ruby) #:prefix guix:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages certs)
  #:use-module (gds packages utils)
  #:export (bundler

            <bundle-package>
            bundle-package
            bundle-package?
            bundle-package-source
            bundle-package-name
            bundle-package-hash
            bundle-package-ruby
            bundle-package-without

            package-with-bundler))

(define (patch-ruby ruby)
  (package
    (inherit ruby)
    (arguments
     (substitute-keyword-arguments
         (default-keyword-arguments
           (package-arguments guix:ruby)
           `(#:tests? #t))
       ((#:tests? _) #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after
            'configure 'patch-for-reproducible-gemspecs
            (lambda _
              (substitute* "lib/rubygems/specification.rb"
                (("current_value = self\\.send\\(attr_name\\)")
                 "current_value = self.send(attr_name)\n      current_value = current_value.sort if %i(files test_files).include? attr_name  # patched through govuk-guix"))))))))))

(define bundler
  (package
    (inherit guix:bundler)
    (version "1.13.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "0fxr7aq7qhlga423mygy7q96cwxmvqlcy676v2x5swlw8rlha2in"))))
    (arguments
     (substitute-keyword-arguments
         (default-keyword-arguments
           (package-arguments guix:bundler)
           `(#:ruby ,guix:ruby))
       ((#:ruby ruby)
        (patch-ruby ruby))))))

(define-record-type* <bundle-package>
  bundle-package make-bundle-package
  bundle-package?
  (source bundle-package-source
          (default #f))
  (name bundle-package-name
        (default "bundle-package"))
  (hash bundle-package-hash)
  (ruby bundle-package-ruby
        (default guix:ruby))
  (without bundle-package-without
           (default '())))

(define-gexp-compiler (bundle-package-for-source-compiler
                       (bundle-package <bundle-package>) system target)
  ;; "Compile" FILE by adding it to the store.
  (match bundle-package
    (($ <bundle-package> source name hash ruby without)
     (run-bundle-package source name hash (patch-ruby ruby) without
                         #:system system))))

(define (gemrc ruby)
  (mixed-text-file "gemrc"
                   "custom_shebang: " ruby "/bin/ruby\n"))

(define* (run-bundle-package source name hash ruby without
                    #:key (system (%current-system)) (guile (default-guile)))
  "Return a fixed-output derivation that fetches REF, a <git-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (mlet %store-monad ((ca-certificates
                       (ca-certificate-bundle
                        (packages->manifest (list nss-certs))))
                      (guile (package->derivation guile system)))
    (define inputs
      (list ruby tar gzip bundler git))

    (define search-paths
      (map
       search-path-specification->sexp
       (delete-duplicates
        (append-map
         package-native-search-paths
         inputs))))

    (define ruby-version (package-version ruby))

    (define build
      (with-imported-modules `((guix build utils)
                               (guix build store-copy)
                               (gnu build install))
        #~(begin
            (use-modules (guix build utils)
                         (srfi srfi-26)
                         (gnu build install)
                         (ice-9 match))
            (define (run . args)
              (simple-format #t "running ~A\n" (string-join args))
              (force-output)
              (zero? (apply system* args)))

            (set-path-environment-variable "PATH" '("bin" "sbin") '#+inputs)

            (for-each (match-lambda
                        ((env-var (files ...) separator type pattern)
                         (set-path-environment-variable
                          env-var files
                          '#+inputs
                          #:separator separator
                          #:type type
                          #:pattern pattern)))
                      '#$search-paths)

            (if (not (null? '#$without))
                (setenv "BUNDLE_WITHOUT"
                        (string-join
                         '#$without
                         ":")))

            (let* ((working-directory (getcwd))
                   (home (string-append working-directory "/HOME"))
                   (vendor/cache (string-append #$output "/vendor/cache"))
                   (bundle (string-append #$bundler "/bin/bundle")))

              (mkdir-p vendor/cache)

              (mkdir-p home)
              (setenv "HOME" home)
              (setenv "GEM_HOME" home)

              (setenv "GIT_SSL_CAINFO"
                      (string-append
                       #$ca-certificates
                       "/etc/ssl/certs/ca-certificates.crt"))
              (setenv "SSL_CERT_DIR"
                      (string-append #$nss-certs "/etc/ssl/certs"))
              (setenv "SSL_CERT_FILE"
                      (string-append
                       #$ca-certificates
                       "/etc/ssl/certs/ca-certificates.crt"))

              (if (directory-exists? #$source)
                  (for-each
                   (lambda (file)
                     (copy-file (string-append #$source "/" file)
                                (string-append #$output "/" file)))
                   '("Gemfile" "Gemfile.lock"))
                  (run "tar"
                       "--extract"
                       "--anchored"
                       "--wildcards"
                       "--no-wildcards-match-slash"
                       "--strip-components=1"
                       "-C" #$output
                       "-x"
                       "-f" #$source
                       "*/Gemfile*"))
              (simple-format #t "Using Gemfile ~A\n" (string-append
                                                    #$output
                                                    "/Gemfile\n"))
              (simple-format #t "Using Gemfile.lock ~A\n" (string-append
                                                         #$output
                                                         "/Gemfile.lock\n"))

              (simple-format #f "Setting .ruby-version to ~A\n" #$ruby-version)
              (call-with-output-file (string-append #$output "/.ruby-version")
                (lambda (port)
                  (simple-format port "~A\n" #$ruby-version)))

              (chdir #$output)
              (run bundle
                   "config"
                   "build.nokogiri"
                   "--use-system-libraries")
              (let loop ((retry 0))
                (unless (run bundle
                             "package"
                             "--all"
                             "--no-install")
                  (if (> retry 3)
                      (exit 1)
                      (loop (+ retry 1)))))

              (let
                  ((files
                    (find-files #$output
                                ".*\\.gemspec")))
                (if (null? files)
                    (simple-format #t "No gemspecs to substitute dates for\n")
                    (begin
                      (simple-format
                       #t "Substituting dates in ~A\n"
                       (string-join files ", "))
                      (substitute* files
                        ((".*s\\.date = \".*\"")
                         "  # date removed by govuk-guix")))))
               (reset-timestamps #$output)))))

    (gexp->derivation name build
                      #:system system
                      #:local-build? #t
                      #:hash-algo 'sha256
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile
                      #:local-build? #t)))

(define (bundle-install-package
         bundle-pkg
         pkg)
  (let
      ((ruby
        (let
            ((ruby-input
              (find
               (match-lambda ((name pkg rest ...)
                              (equal? name "ruby")))
               (package-inputs pkg))))
          (match
              ruby-input
            ((#f)
             #f)
            ((name pkg rest ...)
             pkg)))))
    (package
     (name (string-append (package-name pkg) "-bundle-install"))
     (version "0")
     (source (bundle-package
              (inherit bundle-pkg)
              (source (package-source pkg))
              (name (string-append (package-name pkg) "-bundle-package"))
              (ruby ruby)))
     (build-system gnu-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'validate-runpath)
          (delete 'configure)
          (delete 'check)
          (delete 'install)
          (add-after 'patch-generated-file-shebangs 'set-bundle-without
                     (lambda _ (setenv "BUNDLE_WITHOUT"
                                       ,(string-join
                                         (bundle-package-without bundle-pkg)
                                         ":"))))
          (add-after 'set-bundle-without 'path-remove-source
                     (lambda _
                       (setenv
                        "PATH"
                        (list->search-path-as-string
                         (filter
                          (lambda (path)
                            (not (string-prefix?
                                  (assoc-ref %build-inputs "source")
                                  path)))
                          (search-path-as-string->list (getenv "PATH")))
                         ":"))))
          (add-after 'path-remove-source 'set-ld-library-path
                     (lambda* (#:key inputs #:allow-other-keys)
                       (display inputs)
                       (set-path-environment-variable
                        "LD_LIBRARY_PATH"
                        '("lib")
                        (map cdr inputs))))
          (add-after 'set-ld-library-path 'replace-ruby-version
                     ,(replace-ruby-version (package-version ruby)))
          (replace 'build
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((cwd (getcwd))
                            (out (assoc-ref outputs "out")))
                       (setenv "GEMRC"  (assoc-ref %build-inputs "gemrc"))
                       (setenv "CC" "gcc")
                       ;; (if (catch
                       ;;       'system-error
                       ;;       (lambda ()
                       ;;         (lstat (string-append cwd "/vendor/cache"))
                       ;;         #t)
                       ;;       (lambda (key . args)
                       ;;         #f))
                       ;;     (delete-file (string-append cwd "/vendor/cache")))
                       ;; (if (catch
                       ;;       'system-error
                       ;;       (lambda ()
                       ;;         (lstat (string-append cwd "/vendor/bundle"))
                       ;;         #t)
                       ;;       (lambda (key . args)
                       ;;         #f))
                       ;;     (delete-file (string-append cwd "/vendor/bundle")))
                       (if (file-exists? (string-append cwd "/.bundle/config"))
                           (chmod (string-append cwd "/.bundle/config") #o644))
                       (and
                        (zero? (system*
                                "bundle"
                                "config"
                                "--local"
                                "build.nokogiri"
                                "--use-system-libraries"))
                        (zero? (system*
                                "bundle"
                                "install"
                                "--local"
                                "--deployment"
                                "--path" (string-append
                                          (assoc-ref outputs "out")
                                          "/vendor/bundle")
                                "--jobs=4"))))))
          (add-after 'build 'install-bundle-package-cache
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out")))
                         (mkdir (string-append out "/vendor/bundle/bundler"))
                         (symlink
                          (string-append
                           (assoc-ref inputs "gems")
                           "/vendor/cache")
                          (string-append out "/vendor/bundle/bundler/gems")))))
          (add-after 'build 'patch-tzinfo-data-source
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* (find-files "vendor/bundle/ruby"
                                                "zoneinfo_data_source.rb")
                                    (("DEFAULT_SEARCH_PATH = .*$")
                                     (string-append
                                      "DEFAULT_SEARCH_PATH = ['"
                                      (assoc-ref inputs "tzdata") "/share/zoneinfo"
                                      "'].freeze"))))))))
    (inputs
     (cons*
      (list "bundler" bundler)
      (list "gems" (bundle-package
                    (inherit bundle-pkg)
                    (source (package-source pkg))
                    (name (string-append (package-name pkg) "-bundle-package"))
                    (ruby ruby)))
      (list "gemrc" (gemrc ruby))
      (package-inputs pkg)))
    (native-inputs
     (package-native-inputs pkg))
    (synopsis "")
    (license "")
    (description "")
    (home-page ""))))

(define (package-with-bundler
         bundle-pkg
         pkg)
  (let
      ((ruby
        (let
            ((ruby-input
              (find
               (match-lambda ((name pkg rest ...)
                              (equal? name "ruby")))
               (package-inputs pkg))))
          (match
              ruby-input
            ((#f)
             #f)
            ((name pkg rest ...)
             pkg)))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'set-bundle-without)
             (delete 'path-remove-source)
             (delete 'bundle-install)
             (delete 'patch-tzinfo-data-source)
             (delete 'wrap-bin-files-for-bundler)
             (add-after 'patch-generated-file-shebangs 'set-bundle-without
                        (lambda _ (setenv "BUNDLE_WITHOUT"
                                          ,(string-join
                                            (bundle-package-without bundle-pkg)
                                            ":"))))
             (add-after 'set-bundle-without 'path-remove-source
                        (lambda _
                          (setenv
                           "PATH"
                           (list->search-path-as-string
                            (filter
                             (lambda (path)
                               (not (string-prefix?
                                     (assoc-ref %build-inputs "source")
                                     path)))
                             (search-path-as-string->list (getenv "PATH")))
                            ":"))))
             (add-after 'path-remove-source 'ensure-/bin/bundle-exists
                        (lambda* (#:key inputs outputs #:allow-other-keys)
                          (let*
                              ((out (assoc-ref outputs "out"))
                               (gemfile (string-append out "/Gemfile"))
                               (ruby
                                (string-append (assoc-ref inputs "ruby")
                                               "/bin/ruby"))
                               (bundle
                                (string-append (getcwd) "/bin/bundle")))
                            (if (not (file-exists? bundle))
                                (begin
                                  (mkdir-p (string-append (getcwd) "/bin"))
                                  (call-with-output-file bundle
                                    (lambda (port)
                                      (format port "#!~A
ENV[\"BUNDLE_GEMFILE\"] ||= \"~A\"

load Gem.bin_path(\"bundler\", \"bundler\")" ruby gemfile)))
                                       (chmod bundle #o544)))
                            #t)))
             (add-after 'ensure-/bin/bundle-exists 'set-ld-library-path
                        (lambda* (#:key inputs #:allow-other-keys)
                          (display inputs)
                          (set-path-environment-variable
                           "LD_LIBRARY_PATH"
                           '("lib")
                           (map cdr inputs))))
             (add-after 'set-ld-library-path 'replace-ruby-version
                        ,(replace-ruby-version (package-version ruby)))
             (add-after 'replace-ruby-version 'add-bundle-install-bin-to-path
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let*
                               ((prefix
                                 (string-append
                                  (assoc-ref %build-inputs "bundle-install")
                                  "/vendor/bundle/ruby/"))
                                (ruby-version
                                 (first
                                  (scandir prefix
                                           (negate
                                            (lambda (f)
                                              (member f '("." "..")))))))
                                (bin
                                 (string-append
                                  prefix
                                  ruby-version
                                  "/bin")))
                             (setenv
                              "PATH"
                              (list->search-path-as-string
                               (cons*
                                bin
                                (search-path-as-string->list (getenv "PATH")))
                               ":")))))
             (add-after 'add-bundle-install-bin-to-path
                        'add-bundle-install-gems-to-path
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let*
                               ((prefix
                                 (string-append
                                  (assoc-ref %build-inputs "bundle-install")
                                  "/vendor/bundle/ruby/"))
                                (ruby-version
                                 (first
                                  (scandir prefix
                                           (negate
                                            (lambda (f)
                                              (member f '("." ".."))))))))
                             (setenv
                              "GEM_PATH"
                              (list->search-path-as-string
                               (cons*
                                (string-append
                                 prefix
                                 ruby-version)
                                (search-path-as-string->list (getenv "GEM_PATH")))
                               ":")))))
           (add-after 'install 'wrap-bin-files-for-bundler
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (gem_home
                       (let ((prefix
                              (string-append
                               (assoc-ref %build-inputs "bundle-install")
                               "/vendor/bundle/ruby/")))
                         (string-append
                          prefix
                          (first (scandir prefix (negate (lambda (f)
                                                           (member f '("." ".."))))))
                          "/gems"))))
               (for-each
                  (lambda (script)
                    (chmod script #o777)
                    (wrap-program
                        script
                      `("PATH" = (,(getenv "PATH")))
                      `("SSL_CERT_DIR" = (,(string-append (assoc-ref %build-inputs "nss-certs")
                                                          "/etc/ssl/certs")))
                      `("LIBRARY_PATH" ":" prefix (,(getenv "LIBRARY_PATH")))
                      `("GEM_PATH" ":" prefix (,(getenv "GEM_PATH")))
                      `("BUNDLE_PATH" ":" prefix (,(string-append
                                                    (assoc-ref %build-inputs
                                                               "bundle-install")
                                                    "/vendor/bundle")))
                      `("BUNDLE_WITHOUT" ":" prefix (,(getenv "BUNDLE_WITHOUT")))
                      `("LD_LIBRARY_PATH" ":" prefix (,(getenv "LD_LIBRARY_PATH")))))
                  (find-files
                   (string-append out "/bin")
                   (lambda (name stat)
                     (or
                      (access? name X_OK)
                      (begin
                        (simple-format #t "Skipping wrapping ~A as its not executable\n" name)
                        #f)))))
               #t)))))))
    (inputs
     (cons*
      (list "bundler" bundler)
      (list "bundle-install" (bundle-install-package bundle-pkg pkg))
      (list "gemrc" (gemrc ruby))
      (filter
       (match-lambda
         ((name rest ...)
          (not (any
                (cut string= name <>)
                '("bundler" "gemrc" "bundle-install")))))
       (package-inputs pkg))))
    (native-inputs
     (cons
      (list "gems" (bundle-package
                    (inherit bundle-pkg)
                    (source (package-source pkg))
                    (name (string-append (package-name pkg) "-bundle-package"))
                    (ruby ruby)))
      (filter
       (match-lambda
         ((name rest ...)
          (not (string= name "gems"))))
       (package-native-inputs pkg)))))))
