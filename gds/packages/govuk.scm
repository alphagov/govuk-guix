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
  #:use-module (gnu packages curl)
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
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rsync)
  #:use-module (gds packages utils)
  #:use-module (gds packages utils bundler)
  #:use-module (gds packages third-party phantomjs))

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
    (hash (base32 "0hppfwyar1987jphr9kwzilr2mmr89xssa82kmzfb27hvzh682pq")))
   (make-govuk-package
    "publishing-api"
    (github-archive
     #:repository "publishing-api"
     #:commit-ish "release_758"
     #:hash (base32 "0261kwc4qfix9xfhlnc8q4xmya7k10z9yh4xy8l1ll5clcpjfqnf")))))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "11l8r4mdip55xhgr0486dxg9qlvb6bpscw25lrns5a86cqxnrig5")))
   (make-govuk-package
    "content-store"
    (github-archive
     #:repository "content-store"
     #:commit-ish "release_599"
     #:hash (base32 "1b0hkvl6nr54pap80q6y7zph03ryqqh4v1frmf9r7nycw8ga6nzl")))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "1q1h9bpshrijrzzrzgfgsrqs017rfmzn4zxrdvqbz0cnj3igj1af")))
   (make-govuk-package
    "content-tagger"
    (github-archive
     #:repository "content-tagger"
     #:commit-ish "release_341"
     #:hash (base32 "17xgyq9ir72v63f3xfkvjjlsq8w2s0psx9frxg32jv167m0fy801")))))

(define-public specialist-publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0iwgfd6ppv40iwps1m3li1al6glh2ckinfby8vs7ckay5yr1bg7v"))
          (without '("development" "test")))
         (make-govuk-package
          "specialist-publisher"
          (github-archive
           #:repository "specialist-publisher"
           #:commit-ish "release_751"
           #:hash (base32 "1l2ncdw3p1a4kygp5gc2gjzbdh2prdygaqrhvp78q5fr1pk315lv"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after
              'install 'alter-secrets.yml
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* (string-append
                              (assoc-ref outputs "out")
                              "/config/secrets.yml")
                  (("SECRET_TOKEN")
                   "SECRET_KEY_BASE")))))))))))

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j0sms0d5dwgdlj6y9cfq7ay2r7szk5ih5cs04hrhlqplilk7q1d")))
   (package-rails-app
    "specialist-frontend"
    (github-archive
     #:repository "specialist-frontend"
     #:commit-ish "release_174"
     #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44"))
    #:precompile-assets #t)))

(define-public content-api
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0csjlacjx7r9iv8j82vl2fbki824bq76hvsjzvbxmabpwwbyn0zk")))
         (package-rails-app
          "content-api"
          (github-archive
           #:repository "govuk_content_api"
           #:commit-ish "release_397"
           #:hash (base32 "0smvrqf81c5xhcrw6bf9b28hj848d4yqibh5chlkm8hflj2d14bv"))
          #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:path "/mongoid.yml")))))))))

(define-public publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "1ria1slkcbgvzfp072i3p47l2h8bwv4izxxg4gi75g5pfxgd91ci")))
         (package-rails-app
          "publisher"
          (github-archive
           #:repository "publisher"
           #:commit-ish "release_1696"
           #:hash (base32 "1pnzbgfsg29icgi5li70895mlr2xj2vdhnnw9k6zg3xnq4w4smfn"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml))
             (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
               ,(replace-gds-sso-initializer)))))))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j15vj0rv7ydj62w3f8rwl72b6521alk3aq5s1nnv0rzb6zmwbn4")))
   (package-rails-app
    "frontend"
    (github-archive
     #:repository "frontend"
     #:commit-ish "release_2024"
     #:hash (base32 "0qh97i164akfmfjl7zqavl5iiknybr4cynsh6klcqjqr9lknd03m")))))

(define-public signon
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "0vidm9aikp74yvdnh2infmf45xyx9sfiya27h4g8s6lj06dx6p2f"))
           (without '("development" "test")))
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
    (hash (base32 "179brwqjg8hxkqc4r0ga194nc5alfajyp9ms21hbqx7ykh9vwhan")))
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
    (hash (base32 "18xi02azlxjqll2h9fqhy3rgqm41zhkkh464psli0d6h3hgw3phk")))
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

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ik8q0zs05r6s5kgrll9pvl3d16n1jjsnim1jpb3gl7rsl2r51vj")))
   (package-rails-app
    "rummager"
    (github-archive
     #:repository "rummager"
     #:commit-ish "release_1315"
     #:hash (base32 "09sryv4cd1if5qgwpcm6gsnd0x8kclrq3wqqk4x4kk8hfnqxcbmy"))
    #:precompile-assets #f)))

(define-public metadata-api
  (let
      ((release "release_70"))
    (package
      (name "metadata-api")
      (version release)
      (source
       (github-archive
        #:repository "metadata-api"
        #:commit-ish release
        #:hash (base32 "0jdwvx546f4fd7gvablvpg38srja04klw9m9hv9ix2zp94wdrlns")))
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
                      (cwd (getcwd))
                      (gopath (string-append cwd "/__build/"))
                      (repo "alphagov/metadata-api")
                      (build-path (string-append
                                   gopath
                                   "src/github.com/"
                                   repo)))
                 (copy-recursively cwd "../metadata-api-copy"
                                   #:log (%make-void-port "w"))
                 (mkdir-p "__build/src/github.com/alphagov")
                 (mkdir-p "__build/bin")
                 (setenv "GOPATH" gopath)
                 (rename-file "../metadata-api-copy"
                              "__build/src/github.com/alphagov/metadata-api")
                 (and
                  (with-directory-excursion
                   "__build/src/github.com/alphagov/metadata-api"
                   (zero? (system* "make" "build")))
                  (begin
                    (mkdir-p (string-append out "/bin"))
                    (copy-file (string-append build-path "/metadata-api")
                               (string-append out "/bin/metadata-api"))
                    #t))))))))
      (synopsis "")
      (description "")
      (license "")
      (home-page "https://github.com/alphagov/metadata-api"))))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "1bcs0j60pz620jn19n94sbkvp6drzanw60mmd361vqhgnzj8pmff")))
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
          (hash (base32 "0911wcbx3ixbw3l2yqsmcjqxx8lsfqja3skbj17ah8dr22pszs5d")))
         (make-govuk-package
          "maslow"
          (github-archive
           #:repository "maslow"
           #:commit-ish "release_194"
           #:hash (base32 "0npk0nj9mfw7fgs5rarhd8fig3359c6vnj6614jvmpwspqraqdxv"))))))
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
          (hash (base32 "0kfxvx2cw42zkc07vayymra5a6sssbqlppsnh73q7hvmr20ykyq8")))
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

(define-public whitehall
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "024nibhwc61g1awj3v7mqgq80wmf7jmpa5055ryqkv9xfliijcl2")))
         (make-govuk-package
          "whitehall"
          (github-archive
           #:repository "whitehall"
           #:commit-ish "release_12554"
           #:hash (base32 "15c894aaz73657c79nlws6d9243lqz5p9cngj8vqr98m1nqxbxhx"))))))
    (package
      (inherit pkg)
      (inputs
       `(("curl" ,curl)
         ("imagemagick" ,imagemagick)
         ,@(package-inputs pkg)))
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
               ,(use-blank-database.yml)))))))))

(define-public government-frontend
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "1nwb9qdz4iybf640rbdfg6v155gh0358c4ly77zzkds9sg79a3c4")))
          (make-govuk-package
           "government-frontend"
           (github-archive
            #:repository "government-frontend"
            #:commit-ish "release_244"
            #:hash (base32 "0zlxr1340ax3xpvnry48v3jr9sv6vlr268h647svg2y86a2avm0i"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'bundle-install 'replace-ruby-version
                         ,(replace-ruby-version (package-version ruby))))))))))
