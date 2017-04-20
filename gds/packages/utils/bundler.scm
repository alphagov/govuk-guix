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
  #:use-module (guix derivations)
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
  #:use-module (gds packages utils bundler-build)
  #:export (bundler

            <bundle-package>
            bundle-package
            bundle-package?
            bundle-package-source
            bundle-package-name
            bundle-package-hash
            bundle-package-ruby
            bundle-package-without

            bundle-package-to-store
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
     (mlet %store-monad ((ca-certificates
                          (ca-certificate-bundle
                           (packages->manifest (list nss-certs)))))
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
                                  (gnu build install)
                                  (gds packages utils bundler-build))
           #~(begin
               (use-modules (gds packages utils bundler-build))
               (run-bundle-package #$source
                                   #$output
                                   (getcwd)
                                   '#$inputs
                                   #$ca-certificates
                                   #$nss-certs
                                   '#$search-paths
                                   '#$without
                                   #$ruby-version))))

       (gexp->derivation name build
                         #:system system
                         #:local-build? #t
                         #:hash-algo 'sha256
                         #:hash hash
                         #:recursive? #t
                         #:local-build? #t)))))

(define bundle-package-to-store
  (match-lambda
    (($ <bundle-package> source name hash ruby without)
     (with-store store
       (let* ((inputs (list ruby bundler gzip tar git))
              (input-derivations (map (cut package-derivation store <>)
                                      inputs))
              (input-store-outputs (map derivation->output-path
                                        input-derivations))
              (source-derivation (and (not (string? source))
                                      ((store-lower lower-object)
                                       store source %current-system)))
              (source-store-path (if (string? source)
                                     source
                                     (derivation->output-path source-derivation)))
              (nss-certs-derivation (package-derivation store nss-certs))
              (nss-certs-store-output (derivation->output-path
                                       nss-certs-derivation))
              (ca-certificates-derivation
               ((ca-certificate-bundle
                 (packages->manifest (list nss-certs)))
                store))
              (ca-certificates-store-output (derivation->output-path
                                             ca-certificates-derivation))
              (search-paths
               (map
                search-path-specification->sexp
                (delete-duplicates
                 (append-map package-native-search-paths
                             inputs))))
              (working-directory (tmpnam))
              (output (string-append working-directory "/output")))

         (run-bundle-package source-store-path
                             output
                             working-directory
                             input-store-outputs
                             ca-certificates-store-output
                             nss-certs-store-output
                             search-paths
                             without
                             (package-version ruby))

         (add-to-store store name #t "sha256" output))))))

(define (gemrc ruby)
  (mixed-text-file "gemrc"
                   "custom_shebang: " ruby "/bin/ruby\n"))

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
              (source (or (bundle-package-source bundle-pkg)
                          (package-source pkg)))
              (name (string-append (package-name pkg) "-bundle-package"))
              (ruby ruby)))
     (build-system gnu-build-system)
     (arguments
      `(#:modules ((srfi srfi-1)
                   (ice-9 ftw)
                   ,@%gnu-build-system-modules)
        #:phases
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
                           (assoc-ref inputs "source")
                           "/vendor/cache")
                          (string-append out "/vendor/bundle/bundler/gems")))))
          (add-after 'build 'patch-gem-bin-files
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let*
                           ((prefix
                             (string-append
                              (assoc-ref outputs "out")
                              "/vendor/bundle/ruby"))
                            (ruby-version
                             (first
                              (scandir prefix
                                       (negate
                                        (lambda (f)
                                          (member f '("." "..")))))))
                            (gems
                             (string-append prefix "/" ruby-version "/gems")))
                         (for-each
                          (lambda (gem)
                            (let ((bin (string-append gems "/" gem "/bin")))
                              (if (directory-exists? bin)
                                  (for-each patch-shebang (find-files bin)))))
                          (scandir gems
                                   (negate
                                    (lambda (f)
                                      (member f '("." "..")))))))))
          (add-after 'build 'patch-tzinfo-data-source
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (substitute* (find-files
                                     (string-append
                                      (assoc-ref outputs "out")
                                      "/vendor/bundle/ruby")
                                     "zoneinfo_data_source.rb")
                                    (("DEFAULT_SEARCH_PATH = .*$")
                                     (string-append
                                      "DEFAULT_SEARCH_PATH = ['"
                                      (assoc-ref inputs "tzdata") "/share/zoneinfo"
                                      "'].freeze"))))))))
    (inputs
     (cons*
      (list "bundler" bundler)
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
             (add-before 'configure 'set-bundle-without
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
                          (set-path-environment-variable
                           "LD_LIBRARY_PATH"
                           '("lib")
                           (map cdr inputs))))
             (add-after 'set-ld-library-path 'replace-ruby-version
                        ,(replace-ruby-version (package-version ruby)))
             (add-before 'configure 'add-bundle-install-bin-to-path
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
                             (setenv "BUNDLE_PATH"
                                     (string-append
                                      (assoc-ref %build-inputs "bundle-install")
                                      "/vendor/bundle"))
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
                      `("BUNDLE_PATH" = (,(getenv "BUNDLE_PATH")))
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
       (package-inputs pkg)))))))
