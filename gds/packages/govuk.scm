(define-module (gds packages govuk)
  #:use-module (ice-9 regex)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix store)
  #:use-module (guix git-download)
  #:use-module (gds packages third-party))

(define (make-govuk-package
         name
         source)
  (package
    (name name)
    (version "0")
    (source source)
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)))
    (propagated-inputs
     `(("ruby" ,ruby)
       ("bundler" ,bundler)
       ("gnu-make" ,gnu-make)
       ("gcc-toolchain" ,gcc-toolchain-5)
       ("linux-libre-headers" ,linux-libre-headers)
       ("nss-certs" ,nss-certs)
       ("postgresql" ,postgresql)
       ("libxml2" ,libxml2)
       ("tzdata" ,tzdata)
       ("coreutils" ,coreutils) ;; just for ls
       ("libxslt" ,libxslt)
       ("node" ,node)
       ("phantomjs" ,phantomjs)
       ))
    (arguments
     `(#:modules ((srfi srfi-1)
                  (ice-9 ftw)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path
                     (list->search-path-as-string
                       (filter
                        (lambda (path)
                          (not (string-prefix? (assoc-ref %build-inputs "source")
                                              path)))
                        (search-path-as-string->list (getenv "PATH")));)
                      ":")))
               (copy-recursively "." out)
               ;; Guix has QT 5.6, which does not work with capybara-webkit
               (substitute* (string-append out "/Gemfile")
                 (("gem [\"']capybara-webkit[\"']")
                  "#gem 'capybara-webkit'"))
               (substitute* (find-files (string-append out "/bin"))
                 (("/usr/bin/env") (which "env")))
               (for-each
                (lambda (script)
                  (chmod script #o777)
                  (wrap-program
                      script
                    `("PATH" = (,path))
                    `("SSL_CERT_DIR" = (,(string-append (assoc-ref %build-inputs "nss-certs")
                                                        "/etc/ssl/certs")))
                    `("C_INCLUDE_PATH" ":" prefix (,(getenv "C_INCLUDE_PATH")))
                    `("CPLUS_INCLUDE_PATH" ":" prefix (,(getenv "CPLUS_INCLUDE_PATH")))
                    `("LIBRARY_PATH" ":" prefix (,(getenv "LIBRARY_PATH")))
                    `("GEM_PATH" ":" prefix (,(getenv "GEM_PATH")))))
                (find-files (string-append out "/bin")))
               #t))))))
    (synopsis name)
    (description name)
    (license #f)
    (home-page #f)))

(define* (github-archive
          #:optional #:key
          repository
          (commit-ish (default-commit-ish))
          (user-or-org "alphagov")
          (url (if repository
                (string-append
                 "https://github.com/"
                 user-or-org "/"
                 repository "/archive/"
                 commit-ish ".tar.gz")
                #f))
          (hash-algo 'sha256)
          (hash #f))
  (if (not url)
      (error "Either repository, or the full url must be specified"))
  (origin
    (method url-fetch)
    (uri url)
    (sha256 hash)))

(define-public publishing-api
  (make-govuk-package
   "publishing-api" ;; TODO: Pass archive details to make-govuk-package
   (github-archive
    #:repository "publishing-api"
    #:commit-ish "release_607"
    #:hash (base32 "1gz4njkbfdmv1qxb0d28wlzkmp421sxid5m7dh42vknjkp0prdqy"))))

(define-public content-store
  (make-govuk-package
   "content-store"
   (github-archive
    #:repository "content-store"
    #:commit-ish "release_585"
    #:hash (base32 "0i42za14jn0wrsninahymg9w3sddfnll8fbxmirrvcqcbxx55ydc"))))

(define-public specialist-publisher
  (make-govuk-package
   "specialist-publisher"
   (github-archive
    #:repository "specialist-publisher"
    #:commit-ish "release_725"
    #:hash (base32 "0a6n4a1qhfmak089xjn3payyz6b3vyfkbljdifxzrw4b9m232yn9"))))

(define-public signonotron2
  (make-govuk-package
   "signonotron2"
   (github-archive
    #:repository "signonotron2"
    #:commit-ish "release_688"
    #:hash (base32 "1xqqcs4bncb8lkk4x0h90a4v1sfvmzr6x6b4sa24v8ap8707bbsa"))))

(define-public static
  (make-govuk-package
   "static"
   (github-archive
    #:repository "static"
    #:commit-ish "release_2431"
    #:hash (base32 "14l7523zhb07vxql8i87v9wmzcw6g2328k8zaiqp2s8x8i2iwrs2"))))

(define-public publishing-e2e-tests
  (let
      ((govuk-package
        (make-govuk-package
         "publishing-e2e-tests"
         (github-archive
          #:user-or-org "kevindew"
          #:repository "publishing-e2e-tests"
          #:commit-ish "da2c5e759fd4ff4bae62b7a41a80cee40cf136cb"
          #:hash (base32 "1arrlch7681yjs6v4qyn9wnwsxzpar1lsca0qfm8zdizl6nfi0l1")))))
    (package
      (inherit govuk-package)
      (arguments
       (substitute-keyword-arguments (package-arguments govuk-package)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'install 'add-bundle
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let*
                     ((out (assoc-ref outputs "out"))
                      (gemfile (string-append out "/Gemfile"))
                      (ruby
                       (string-append (assoc-ref inputs "ruby")
                                      "/bin/ruby")))
                   (define* (bundle ruby-path #:optional (port #f))
                     (format port "#!~A
ENV[\"BUNDLE_GEMFILE\"] ||= \"~A\"

load Gem.bin_path(\"bundler\", \"bundler\")" ruby-path gemfile))

                   (mkdir-p (string-append out "/bin"))
                   (call-with-output-file (string-append out "/bin/bundle")
                     (lambda (port)
                       (bundle ruby port)))
                   (chmod (string-append out "/bin/bundle") #o544)
                   #t))))))))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (github-archive
      #:repository "govuk-content-schemas"
      #:commit-ish "release_416"
      #:hash (base32 "03is44yfnq5xs08d80cw4rhw4psm6v09ki4cascx3k3rllcs8a6p")))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (copy-recursively "." out)
               #t))))))
    (synopsis "govuk-content-schemas")
    (description "govuk-content-schemas")
    (license #f)
    (home-page #f)))
