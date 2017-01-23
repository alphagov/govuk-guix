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
  (package-with-bundler
   (bundle-package
    (hash (base32 "0chz3xwgn9nbs1q8919r8mbcn73ca96jxk4aaf72nk4a28lc3qv4")))
   (make-govuk-package
    "publishing-api" ;; TODO: Pass archive details to make-govuk-package
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
    (hash (base32 "1f32hnh59wq9h6sf6aswsar566axqk269sp2czg3lh8wgvxjp4zl")))
   (make-govuk-package
    "specialist-frontend"
    (github-archive
     #:repository "specialist-frontend"
     #:commit-ish "release_174"
     #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44")))))

(define-public signonotron2
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f14g354y6xiy86vl5dwjz7yfq92cg15pwviw21c4awch04b0hm9")))
   (make-govuk-package
    "signonotron2"
    (github-archive
     #:repository "signonotron2"
     #:commit-ish "release_689"
     #:hash (base32 "0idacqj1232hcsfzlf3yyx78s2qsvrr67ii2hf907ghjrjw7f9dz")))))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "0sifa4xwbrx7h0ncnbl8kxmi44zva3qv3b7pjnqzz3jxibjqab99")))
   (make-govuk-package
    "static"
    (github-archive
     #:repository "static"
     #:commit-ish "release_2441"
     #:hash (base32 "05rbf7x2n2kzz0n1bww0190l7ly9cvvl10j3mdy9gb891sm74n2h")))))

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0a77i21cmjwvk2dqkx80fv8vkm7s2spsvaqh21nrz14c7gzrr6hz")))
   (make-govuk-package
    "router-api"
    (github-archive
     #:repository "router-api"
     #:commit-ish "release_106"
     #:hash (base32 "1f2gycb51mi7cfvm83lldczi56l9j6ra9c5db4b6hmm2wigwh53d")))))

(define-public router
  (let
      ((release "release_140"))
    (package
      (name "router")
      (version "0")
      (source
       (github-archive
        #:repository "router"
        #:commit-ish release
        #:hash (base32 "0v4r3vglqyrl6nrh24n7dxvh80vysgximgccqgyfxvmwlqrx26m7")))
      (build-system gnu-build-system)
      (native-inputs
       `(("go" ,go)
         ("bash" ,bash)
         ("rsync" ,rsync)))
      (arguments
       `(#:make-flags (list ,(string-append "RELEASE_VERSION=" release))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'install)
           (delete 'check)
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (cwd (getcwd))
                      (bash (string-append
                             (assoc-ref inputs "bash")
                             "/bin/bash")))
                 (mkdir-p "__build/src/github.com/alphagov")
                 (mkdir-p "__build/bin")
                 (setenv "GOPATH" (string-append cwd "/__build"))
                 (setenv "BINARY" (string-append cwd "/router"))
                 (system*
                  "rsync"
                  "-a"
                  "./"
                  "__build/src/github.com/alphagov/router" "--exclude=__build")
                 (and
                  (with-directory-excursion
                      "__build/src/github.com/alphagov/router"
                    (and
                     (zero? (system* "make" "build"))
                     (mkdir-p (string-append out "/bin"))))
                  (begin
                    (copy-file "router"
                               (string-append out "/bin/router"))
                    #t))))))))
      (synopsis name)
      (description name)
      (license #f)
      (home-page #f))))

(define-public publishing-e2e-tests
  (let
      ((govuk-package
        (make-govuk-package
         "publishing-e2e-tests"
         (github-archive
          #:repository "publishing-e2e-tests"
          #:commit-ish "make-capybara-save-path-configurable"
          #:hash (base32 "07zfd47b3aqh0hhcdi4h7bazrzb6q9b2ar6bis3apnxx2bbakzs3")))))
    (package-with-bundler
     (bundle-package
      (hash
       (base32 "0sar9zb44w1rcd987dkvw9p96bjwrqjfjarsasm488iv9qqmv9vn")))
     (package
       (inherit govuk-package)
       (arguments
        (substitute-keyword-arguments (package-arguments govuk-package)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-before 'install 'create-bin-bundle
                ,(create-bin-bundle))))))))))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "0g1y50vds1rc2qgis6b8wbydmfmja9wgr5s6hw8hra1z7nm3m3hs")))
   (make-govuk-package
    "maslow"
    (github-archive
     #:repository "maslow"
     #:commit-ish "release_180"
     #:hash (base32 "0zxcxznhp95sss85lrx4gdipwb75jhd3y9wsd01nrd1nl12hr0jx")))))

(define-public need-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1hg85n9qf4710a93fkqfcpa8avj3q972ppk5w0is9sb0zi7kih5m")))
   (make-govuk-package
    "need-api"
    (github-archive
     #:repository "govuk_need_api"
     #:commit-ish "release_133"
     #:hash (base32 "0yrwyky6af8i3l2gl0z0qvcv6f9cgp9kjmyva1bjnkpdszx67qmc")))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (github-archive
      #:repository "govuk-content-schemas"
      #:commit-ish "release_455"
      #:hash (base32 "1k4ck56lg0qglkr9zmla8cxmz88fdm08nvanqk9fai8wcj5hmjqm")))
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
BINDIR=\"/var/lib/$APP/bin\"
. \"$BINDIR/env.sh\"
cd \"/var/lib/$APP\"
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
