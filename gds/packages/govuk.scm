(define-module (gds packages govuk)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix records)
  #:use-module (guix git-download)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rsync)
  #:use-module (gds packages utils)
  #:use-module (gds packages utils bundler)
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
     `(("bash" ,bash)
       ("ruby" ,ruby)
       ("tzdata" ,tzdata)
       ("gnu-make" ,gnu-make)
       ("gcc-toolchain" ,gcc-toolchain-5)
       ("linux-libre-headers" ,linux-libre-headers)
       ("nss-certs" ,nss-certs)
       ("postgresql" ,postgresql)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("zlib" ,zlib)
       ("libxml2" ,libxml2)
       ("tzdata" ,tzdata)
       ("coreutils" ,coreutils) ;; just for ls
       ("libxslt" ,libxslt)
       ("libffi" ,libffi)
       ("pkg-config" ,pkg-config)
       ("node" ,node)
       ("phantomjs" ,phantomjs)))
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
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (copy-recursively
                "."
                out
                #:log (%make-void-port "w")))))
         (add-after 'install 'patch-bin-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute*
                   (find-files
                    (string-append out "/bin")
                    (lambda (name stat)
                      (or
                       (access? name X_OK)
                       (begin
                         (simple-format #t "Skipping patching ~A as its not executable\n" name)
                         #f))))
                 (("/usr/bin/env") (which "env")))))))))
    (synopsis name)
    (description name)
    (license #f)
    (home-page #f)))

(define* (package-rails-app name source
                            #:optional #:key
                            (precompile-assets #t))
  (let ((pkg (make-govuk-package name source))
        (phase-modifications
         `(,@(if
              precompile-assets
              '(add-before
                'install 'asset-precompile
                (lambda*
                 (#:key inputs #:allow-other-keys)
                 (zero?
                  (system* "bundle" "exec" "rake" "assets:precompile"))))
              '()))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments
        (package-arguments pkg)
        ((#:phases phases)
         (if (null? phase-modifications)
             phases
             `(modify-phases
               ,phases
               ,phase-modifications))))))))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0jn3cp980rgskncspy1gy0cd3zshpc01mb1a4awrfvk2cvymgds1")))
   (make-govuk-package
    "publishing-api"
    (github-archive
     #:repository "publishing-api"
     #:commit-ish "release_654"
     #:hash (base32 "1ja4pdpa9jz044zlcc8r4hqynn1vdyzz2kb1n9iix8sb4ciw9w2q")))))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "01flp6kgb17386p9a6bykvh9133irwz70qabkz52mxra2lsmxa5x")))
   (make-govuk-package
    "content-store"
    (github-archive
     #:repository "content-store"
     #:commit-ish "release_599"
     #:hash (base32 "1b0hkvl6nr54pap80q6y7zph03ryqqh4v1frmf9r7nycw8ga6nzl")))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "1qpx3n6gp3v280k0a689cxn8iy413csmf1grrnj2qzs5g54l23bn"))
    (without '("test")))
   (make-govuk-package
    "content-tagger"
    (github-archive
     #:repository "content-tagger"
     #:commit-ish "release_341"
     #:hash (base32 "17xgyq9ir72v63f3xfkvjjlsq8w2s0psx9frxg32jv167m0fy801")))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j9i72vzwymb5zlz8xl62663w888l7z3ynfhvngmix175jfa1byk"))
    (without '("development" "test")))
   (make-govuk-package
    "specialist-publisher"
    (github-archive
     #:repository "specialist-publisher"
     #:commit-ish "release_751"
     #:hash (base32 "1l2ncdw3p1a4kygp5gc2gjzbdh2prdygaqrhvp78q5fr1pk315lv")))))

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0f3kjqq52bhpl2a5sy2j422kkg2gawjd09kykwr6ipiqadlj4z8r")))
   (package-rails-app
    "specialist-frontend"
    (github-archive
     #:repository "specialist-frontend"
     #:commit-ish "release_174"
     #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44"))
    #:precompile-assets #t)))

(define-public signon
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "1x6ycb7b1l9pgf1vlp7ys9jnzls00hh2sfwcxb5gvk6d8l0vx478")))
          (make-govuk-package
           "signon"
           (github-archive
            #:repository "signon"
            #:commit-ish "release_896"
            #:hash (base32 "0bsg7pk5g4h2if6x1253rv48hssnm1xsqfjiyyg46w5ag5m13dl1"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
               ,(use-blank-database.yml)))))))))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "0sifa4xwbrx7h0ncnbl8kxmi44zva3qv3b7pjnqzz3jxibjqab99")))
   (package-rails-app
    "static"
    (github-archive
     #:repository "static"
     #:commit-ish "release_2441"
     #:hash (base32 "05rbf7x2n2kzz0n1bww0190l7ly9cvvl10j3mdy9gb891sm74n2h"))
    #:precompile-assets #t)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1idjf7d44jjgprnv651ghsi4md450p3kyqlykmhd6hw14fcrc30c")))
   (package-rails-app
    "info-frontend"
    (github-archive
     #:repository "info-frontend"
     #:commit-ish "release_60"
     #:hash (base32 "1sh7wgfhaazbl5sdmj69s73xh1ppv3j2dpyzxxkkdsmi76v7brz7"))
    #:precompile-assets #t)))

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b15pz35acpzwykvl34b7ygcsizxjwzg67p1n4lbz0dcc9vjhrky")))
   (make-govuk-package
    "router-api"
    (github-archive
     #:repository "router-api"
     #:commit-ish "release_106"
     #:hash (base32 "1f2gycb51mi7cfvm83lldczi56l9j6ra9c5db4b6hmm2wigwh53d")))))

(define-public router
  (let
      ((release "release_144"))
    (package
      (name "router")
      (version release)
      (source
       (github-archive
        #:repository "router"
        #:commit-ish release
        #:hash (base32 "0lq5zhvsahs436aagsf89bzs9b7ydhysng4kj88is7p69i6f1h2i")))
      (build-system gnu-build-system)
      (native-inputs
       `(("go" ,go)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'install)
           (delete 'check)
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (cwd (getcwd)))
                 (copy-recursively cwd "../router-copy")
                 (mkdir-p "__build/src/github.com/alphagov")
                 (mkdir-p "__build/bin")
                 (setenv "GOPATH" (string-append cwd "/__build"))
                 (setenv "BINARY" (string-append cwd "/router"))
                 (rename-file "../router-copy"
                              "__build/src/github.com/alphagov/router")
                 (and
                  (with-directory-excursion
                      "__build/src/github.com/alphagov/router"
                    (and
                     (zero? (system* "make" "build" (string-append "RELEASE_VERSION=" ,release)))
                     (mkdir-p (string-append out "/bin"))))
                  (begin
                    (copy-file "router"
                               (string-append out "/bin/router"))
                    #t))))))))
      (synopsis "")
      (description "")
      (license "")
      (home-page "https://github.com/alphagov/router"))))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "0sar9zb44w1rcd987dkvw9p96bjwrqjfjarsasm488iv9qqmv9vn")))
   (make-govuk-package
    "publishing-e2e-tests"
    (github-archive
     #:repository "publishing-e2e-tests"
     #:commit-ish "44058ef1421b81a9c4c11f5a7dba40d0404de29a"
     #:hash (base32 "1xlgs4a7k49h54nv19ax3cd4a17jv74g13zn05hbrv041css3my9")))))

(define-public maslow
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "18ljpg3s12fwqj7pmjp44sddj4ich3ailvgjw6a0k88fwv3nlrbb")))
         (make-govuk-package
          "maslow"
          (github-archive
           #:repository "maslow"
           #:commit-ish "release_187"
           #:hash (base32 "0x8cvmaw7687zrskkfni3024xrywcbihi3qck9xlh3f9wcajn51r"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:mongoid-version "3")))))))))

(define-public need-api
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "1hg85n9qf4710a93fkqfcpa8avj3q972ppk5w0is9sb0zi7kih5m")))
         (make-govuk-package
          "need-api"
          (github-archive
           #:repository "govuk_need_api"
           #:commit-ish "release_141"
           #:hash (base32 "1ds6mp42fflmqm7jx5aw2jfgwr33hc6r8p1krfj0jczsll7r70f9"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:mongoid-version "3")))))))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (github-archive
      #:repository "govuk-content-schemas"
      #:commit-ish "release_475"
      #:hash (base32 "1an8qzbpsx10bk4wgwzrr2bnh2d769ig44fb5c0jynmj9121a9hn")))
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

(define-public govuk-setenv
  (package
   (name "govuk-setenv")
   (version "1")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let
                      ((bash (string-append
                              (assoc-ref %build-inputs "bash")
                              "/bin/bash"))
                       (sudo (string-append
                              (assoc-ref %build-inputs "sudo")
                              "/bin/sudo")))
                    (mkdir-p (string-append %output "/bin"))
                    (call-with-output-file (string-append
                                            %output
                                            "/bin/govuk-setenv")
                      (lambda (port)
                        (simple-format port "#!~A
set -exu
APP=\"$1\"
shift
BINDIR=\"/var/apps/$APP/bin\"
. \"$BINDIR/env.sh\"
cd \"/var/apps/$APP\"
~A --preserve-env -u \"$APP\" \"$@\"
" bash sudo)))
                    (chmod (string-append %output "/bin/govuk-setenv") #o555)
                    #t))))
   (native-inputs
    `(("bash" ,bash)
      ("sudo" ,sudo)))
   (synopsis "govuk-setenv script for running commands in the service environment")
   (description "This script runs the specified command in an
environment similar to that which the service is running. For example,
running govuk-setenv @code{publishing-api rails console} runs the
@code{rails console} command as the user associated with the
Publishing API service, and with the environment variables for this
service setup.")
   (license #f)
   (home-page #f)))
