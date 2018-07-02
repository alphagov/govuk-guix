(define-module (gds packages govuk)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix records)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages guile)
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
  #:use-module (gds build-system rails)
  #:use-module (gds packages guix)
  #:use-module (gds packages utils)
  #:use-module (gds packages utils bundler)
  #:use-module (gds packages third-party phantomjs))

(define govuk-admin-template-initialiser
  '(lambda _
     (with-output-to-file
         "config/initializers/govuk_admin_template_environment_indicators.rb"
       (lambda ()
         (display "GovukAdminTemplate.environment_style = ENV.fetch('GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_STYLE', 'development')
GovukAdminTemplate.environment_label = ENV.fetch('GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_LABEL', 'Development')
")))))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "18vzgzzdh3ygl1cv0v3w1dhkdwlm4fcrpg1zwgqxgx6d9lvqjf9h")))
   (package
     (name "asset-manager")
     (version "release_297")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "104qdwm9pw894l11775nfwcxv0v179h643fbaydsy41xr26mld3m")))
     (build-system rails-build-system)
     (inputs
      `(("govuk_clamscan"
         ,
         (package
           (name "fake-govuk-clamscan")
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
                                      "/bin/bash")))
                            (mkdir-p (string-append %output "/bin"))
                            (call-with-output-file (string-append
                                                    %output
                                                    "/bin/govuk_clamscan")
                              (lambda (port)
                                (simple-format port "#!~A\nexit 0\n" bash)))
                            (chmod (string-append %output "/bin/govuk_clamscan") #o555)
                            #t))))
           (native-inputs
            `(("bash" ,bash)))
           (synopsis "")
           (description "")
           (license #f)
           (home-page #f)))))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'create-uploads-and-fake-s3-directories
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (mkdir-p (string-append out "/uploads"))
                         (mkdir-p (string-append out "/fake-s3")))
                       #t)))))
     (synopsis "Manages uploaded assets (e.g. PDFs, images, ...)")
     (description "The Asset Manager is used to manage assets for the GOV.UK Publishing Platform")
     (license license:expat)
     (home-page "https://github.com/alphagov/asset-manager"))
   #:extra-inputs (list libffi)))

(define-public authenticating-proxy
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z1yws17csificafc2yrdq81c75pg0mssxlkjzmyx7dq3208sa5g")))
   (package
     (name "authenticating-proxy")
     (version "release_90")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ilmwpvqgsz6zyf87c52madccxfq7b3npg7ky5i7y42qjk9khdl1")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
            ,(replace-mongoid.yml)))))
     (synopsis "Proxy to add authentication via Signon")
     (description "The Authenticating Proxy is a Rack based proxy,
written in Ruby that performs authentication using gds-sso, and then
proxies requests to some upstream")
     (license #f)
     (home-page "https://github.com/alphagov/authenticating-proxy"))))

(define-public bouncer
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zrpg7y1h52lvah4y5ghw1szca4cmqvkba13l4ra48qpin61wkh2")))
   (package
     (name "bouncer")
     (version "release_227")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06jb2w45ww13mv7spb41yxkz1gvc5zvijhicw3i8z5jp8iqizdl2")))
     (build-system rails-build-system)
     (arguments
      '(#:precompile-rails-assets? #f))
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))
   #:extra-inputs (list libffi postgresql)))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "1bll9ps17nrmn85812asv72bshvd8ssxqy7fi5r6xdxnmgarzfyb")))
   (package
     (name "calculators")
     (version "release_331")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i5rprdnr77xcy28pn9hms5ybqzw8pj7wqpyz2bmxvswc6ifp1aa")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "00lj0fpk6zljkg55z2sgf6h55vi3h2d2ywdimaa42i5hjz272m12")))
   (package
     (name "calendars")
     (version "release_553")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k99jmi267bfndgiasz8g4yjy765b0mnk2a8gkmis9mpb2d9cq0p")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "12nspj5vsfx66r4gcdbkrv9463manaakm1sh0kxch70y8hgz56wj")))
   (package
     (name "collections")
     (version "release_633")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gxfxc9rcpdbg47a5r3k35gajr21fc2d3yjqy3y396k3iy71ydgv")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "17z1c0vicyxsrz4blh20drvq9294c7s3brhag4bs6scr1p9ap7w2")))
   (package
     (name "collections-publisher")
     (version "release_410")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jq9bas89dailcy9lhbvvkq512rlnl2a7g7lqr37hihdclizqki0")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "Used to create browse and topic pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections-publisher"))
   #:extra-inputs (list mariadb
                        libffi)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hk25d1xwmgagz2knzafp60gbc8f4ifmg05vl11yx9m8hvj0ps75")))
   (package
     (name "contacts-admin")
     (version "release_478")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ynqrf3vdlh1vn59hzr4v7drir7pd3x1yvqp4cpkzv011hipq7x5")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(;; The mock_organisations_api, from the spec directory is used
        ;; in development
        #:exclude-files ("tmp")
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
                      ,govuk-admin-template-initialiser))))
     (synopsis "Used to publish organisation contact information to GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/contacts-admin"))
   #:extra-inputs (list libffi
                        mariadb)))

(define-public content-audit-tool
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zpvs7hvpfxcf85y9wkh5zdm020f199g0brpflqwyiys6n5wvnka")))
   (package
     (name "content-audit-tool")
     (version "release_458")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n38a28mqr7zyynziqypfks3h25403la4rjj6kn3pwjagcjpxnry")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-audit-tool"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1whc7hxfqlq6739m0yavy1lz0by327kr5ag8hp33a8fhc0vczhda")))
   (package
     (name "content-performance-manager")
     (version "release_631")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05pf5864cqrby0cj62jkfswjz972sz1fif3pdix2zxvvgq1g5z7m")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-performance-manager"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j16zbfi579n0vp7mhdr5v1fb21qzzazdqrcqgxvka18dndfibmy")))
   (package
     (name "content-store")
     (version "release_776")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q7n2qlsvxr7qi53mm63c64jwv03m03y4r7969j2sb6bgj5q9ng7")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))
   #:extra-inputs (list libffi)))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "15fakpsi8hj2hn2miki2rz63vrm2x93inanr8i1naryapdqwraib")))
   (package
     (name "content-tagger")
     (version "release_821")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dd2qh1a27jz564snlw6k9zn688ywrcdr94avgc0d22vrhpq43kx")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))

     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql
                        libffi)))

(define-public email-alert-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1s8aq53852asiwr67d2jxjhr8vjk4k6wk1yffw4szk805dj4kldh")))
   (package
     (name "email-alert-api")
     (version "release_616")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ld3vn3zznh1is32nsgrs5ypn0ldpf96f39i5c6ipcml6xs0w9v7")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-api"))
   #:extra-inputs (list libffi postgresql)))

(define-public email-alert-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1alcgraa0jgszvqd6780sphmxmgdna5kzhp78q24wk22wwh0nksj")))
   (package
     (name "email-alert-frontend")
     (version "release_206")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cihw7jm96np5550cy8jmjigw1xf63h6x64j2b0ashars3yhcxxr")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pcwv9q46sh76a669pq97ip34q647gnhfz08by90k2h7ybrp6phd")))
   (package
     (name "email-alert-service")
     (version "release_162")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12jcny8z646pskn9ingrwz143795lyyl7ddp4y12d971gjdrad4n")))
     (build-system gnu-build-system)
     (inputs
      `(("ruby" ,ruby)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'configure (lambda args #t))
          (replace 'build (lambda args #t))
          (replace 'check (lambda args #t))
          (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out")))
                       (copy-recursively
                        "."
                        out
                        #:log (%make-void-port "w")))))
          (add-after 'patch-bin-files 'wrap-with-relative-path
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out")))
                         (substitute* (find-files
                                       (string-append out "/bin"))
                           (((string-append out "/bin"))
                            "${BASH_SOURCE%/*}"))))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-service/"))
   #:extra-inputs (list libffi)))

(define-public feedback
  (package-with-bundler
   (bundle-package
    (hash (base32 "1kriqn11zrnwvazvkc9dsv7nwnv35wkg5zah6lc3ni33gra6y2fl")))
   (package
     (name "feedback")
     (version "release_466")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15vn39n8qwa7ppfpdrx7gx0y2bdaa80h1p5i28r6qsgdmjpd6cjw")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1qz7c1iw8x6hf2q6gg0mbvxxy3kz89shsh0kir7ka65kzjgnfsqw")))
   (package
     (name "finder-frontend")
     (version "release_494")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jgxnmas74hfm7ra3df3cw68zkvmfc9micz1d12l99i9zngbyln9")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vbb41yv9xswm68w6b11vrqk04j9kyclb40qwgn43ni6ny0m30kc")))
   (package
     (name "frontend")
     (version "release_2933")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kw1x3wfl9pnpsqlijkhwr2zbplxbzgk96sq3pssicl7kk9qihcm")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "18jx332qsz868ixpq7m58r1wjlfxff3gzfqaiiw061y0yv0whxvd")))
   (package
     (name "government-frontend")
     (version "release_794")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "074515pf7klyvy49g6rvs27h3gnsx964n229z6gda2aazkn605q0")))
     (build-system rails-build-system)
     (arguments
      '(;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))
   #:extra-inputs (list libffi)))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_768")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0ky5nh0ck4f909y6gb8v8m85vb0v25khndc2p9756vilslm7y6nm")))
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
source \"/tmp/env.d/$APP\"
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

(define-public current-govuk-guix
  (let* ((repository-root (canonicalize-path
                           (string-append (current-source-directory)
                                          "/../..")))
         (select? (delay (git-predicate repository-root))))
    (lambda ()
      (package
        (name "govuk-guix")
        (version "0")
        (source (local-file repository-root "govuk-guix-current"
                            #:recursive? #t
                            #:select? (force select?)))
        (build-system gnu-build-system)
        (inputs
         `(("coreutils" ,coreutils)
           ("bash" ,bash)
           ("guix" ,guix)
           ("guile" ,guile-2.2)))
        (arguments
         '(#:phases
           (modify-phases %standard-phases
             (replace 'configure (lambda args #t))
             (replace 'build (lambda args #t))
             (replace 'check (lambda args #t))
             (replace 'install
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (use-modules (ice-9 rdelim)
                              (ice-9 popen))
                 (let* ((out (assoc-ref outputs "out"))
                        (effective (read-line
                                    (open-pipe* OPEN_READ
                                                "guile" "-c"
                                                "(display (effective-version))")))
                        (module-dir (string-append out "/share/guile/site/"
                                                   effective))
                        (object-dir (string-append out "/lib/guile/" effective
                                                   "/site-ccache"))
                        (prefix     (string-length module-dir)))
                   (install-file "bin/govuk" (string-append out "/bin"))
                   (for-each (lambda (file)
                               (install-file
                                file
                                (string-append  out "/share/govuk-guix/bin")))
                             (find-files "bin"))
                   (copy-recursively
                    "gds"
                    (string-append module-dir "/gds")
                    #:log (%make-void-port "w"))
                   (setenv "GUILE_AUTO_COMPILE" "0")
                   (for-each (lambda (file)
                               (let* ((base (string-drop (string-drop-right file 4)
                                                         prefix))
                                      (go   (string-append object-dir base ".go")))
                                 (invoke "guild" "compile"
                                          "--warn=unused-variable"
                                          "--warn=unused-toplevel"
                                          "--warn=unbound-variable"
                                          "--warn=arity-mismatch"
                                          "--warn=duplicate-case-datum"
                                          "--warn=bad-case-datum"
                                          "--warn=format"
                                          "-L" module-dir
                                          file "-o" go)))
                             (find-files module-dir "\\.scm$"))
                   (setenv "GUIX_PACKAGE_PATH" module-dir)
                   (setenv "GUILE_LOAD_PATH" (string-append
                                              (getenv "GUILE_LOAD_PATH")
                                              ":"
                                              module-dir))
                   (setenv "GUILE_LOAD_COMPILED_PATH"
                           (string-append
                            (getenv "GUILE_LOAD_COMPILED_PATH")
                            ":"
                            object-dir))
                   #t)))
             (add-after 'install 'wrap-bin-files
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (wrap-program (string-append out "/bin/govuk")
                     `("PATH" prefix (,(string-append
                                        (assoc-ref inputs "coreutils")
                                        "/bin")
                                      ,(string-append
                                        (assoc-ref inputs "guile")
                                        "/bin")
                                      ,(string-append
                                        (assoc-ref inputs "bash") "/bin")))
                     `("GUILE_LOAD_COMPILED_PATH" =
                       (,(getenv "GUILE_LOAD_COMPILED_PATH")))
                     `("GUILE_LOAD_PATH" = (,(getenv "GUILE_LOAD_PATH")))
                     `("GOVUK_EXEC_PATH" suffix
                       (,(string-append out "/share/govuk-guix/bin")))
                     `("GUIX_PACKAGE_PATH" = (,(getenv "GUIX_PACKAGE_PATH")))
                     `("GUIX_UNINSTALLED" = ("true")))))))))
        (home-page #f)
        (synopsis "Package, service and system definitions for GOV.UK")
        (description "")
        (license #f)))))

(define-public hmrc-manuals-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "186n07nbfbk0nqhvmxrhk007m3d1bh4l56asqydab68mgy56ix0g")))
   (package
     (name "hmrc-manuals-api")
     (version "release_268")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08b004gf817csxrd3faq34gd7bsaziwmds5aq3ad8kjdcirmw3zn")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/hmrc-manuals-api"))
   #:extra-inputs (list libffi)))

(define-public imminence
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mclz2ggac2vvqbsg384992lqj77mhxydsj4pcfqlcs1plrsld2m")))
   (package
     (name "imminence")
     (version "release_410")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lqb3jkh1nvlfvh6a3sg6lfi0qq8fny6cz0f1rqz2w8fchhidjv7")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
                       (add-after 'install 'replace-mongoid.yml
                                  ,(replace-mongoid.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "02rj0s2wrx79z8n7cr46m0dbk5bqzhh56caqwjb05qq28lp15k5a")))
   (package
     (name "info-frontend")
     (version "release_188")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wf3z7m3vnmarxab7mb60g0hng6dsb40jshzwm0bnzddmcmvrgfb")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1c900s6c8ych9ndfmq1wv0yrvg1qnc2f4ys9c36c8xa7w4rgkx2f")))
   (package
     (name "licence-finder")
     (version "release_423")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p0k2r0lmsn3f3myixd6b2gjcapa7akk9amaywkv5xki5irlkxcc")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0s5dq7jzy5lm2wv6vkn8i26xkxdqdz6jfyb2fkmbjc7k18lw8lhm")))
   (package
     (name "link-checker-api")
     (version "release_138")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k9h1b53z5m47040rc2gy0k8427wklwd2mfi24c82hkxfmkf1ydr")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/link-checker-api"))
   #:extra-inputs (list postgresql libffi
                        ;; TODO: Remove sqlite once it's been removed
                        ;; from the package
                        sqlite)))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "19wvqjwbkfab9wnixywckn6lxp5fgphb4b9qy4ppv76qak4mk1p3")))
   (package
     (name "local-links-manager")
     (version "release_236")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ys1828fm2jk7zgim4qnvmy8n38hb2hsmk79j87l8dvk6bxq40bg")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
            ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/local-links-manager"))
   #:extra-inputs (list postgresql
                        libffi)))

(define-public manuals-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zyc0h2bzf1v6ywgdpz17fwzxbx11kjrpc2i2bf63j3dblilv4ry")))
   (package
     (name "manuals-frontend")
     (version "release_346")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lpqczc7j4dwz44frv2zfs1x0shzm8x0r0pz7plggg68yqsmnh9p")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1g9n9x3ilhxq2bkrhci7pnpskm29vnv137649mnh2idv809jlgqj")))
   (package
     (name "manuals-publisher")
     (version "release_1109")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v1awqkahvq9y7acpi4sib5wxa4gkm0gys0j7ly002x97qm0dbjn")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after
              'install 'alter-secrets.yml
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (string-append
                            (assoc-ref outputs "out")
                            "/config/secrets.yml")
                (("SECRET_TOKEN")
                "SECRET_KEY_BASE")))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-publisher"))
   #:extra-inputs (list libffi)))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "1vgz9kp17s4fv4cfyvcbl0v733w0mnn7i5v1wcf7wxwknijbhr3h")))
   (package
     (name "maslow")
     (version "release_306")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1injjz4sp1dixk5ps6kmxg4m4kgc6bzn5lhjhsfq4n2x4kwzkj6z")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml))
          (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
                     ,(replace-gds-sso-initializer)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/maslow"))
   #:extra-inputs (list libffi)))

(define-public organisations-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "13mwdahp7cixc2cjm14kbgk0jx42d7c389ifllygn4p3pdn8hpg6")))
   (package
     (name "organisations-publisher")
     (version "release_6")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y13mvm8jw4k3c50bazfhngm537r50msq7b31lglz50763c2qib2")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/organisations-publisher"))
   #:extra-inputs (list libffi postgresql
                        ;; TODO Remove sqlite if it's unused, it's still in the Gemfile
                        sqlite)))

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "09hm2mmcfiqmlr6gdap8y5nxgmniaqvgbmbawzvxs4swf6h8wybq")))
   (package
     (name "policy-publisher")
     (version "release_282")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0s9m3g23xh987vimzf2fxz5msqnsn6r59qxs5gqv93hhzkzjbm60")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/policy-publisher"))
   #:extra-inputs (list libffi
                        postgresql)))

(define-public publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0agj5skkqbj2qwy5jsxh1hvyjwpgh30j73vv8a350pzd9ah48qqx")))
   (package
     (name "publisher")
     (version "release_1992")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bzcpv0kwysmbbwafdbab57vga2xpn50xw13v629037rvq2z6v4r")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml))
          (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
                     ,(replace-gds-sso-initializer)))))
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/publisher"))
   #:extra-inputs (list libffi)))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wxwrqsf8a3h8lqghfhsm7j357bpaswk3ppmw1gg7bg72f99aml3")))
   (package
     (name "publishing-api")
     (version "release_1213")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "02hv798xnsn9nxyb3pcgw0s8i8fqvcwf22jbz8c4h41fq72b3vk5")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "Service for storing and providing workflow for GOV.UK content")
     (description
      "The Publishing API is a service that provides a HTTP API for
managing content for GOV.UK.  Publishing applications can use the
Publishing API to manage their content, and the Publishing API will
populate the appropriate Content Stores (live or draft) with that
content, as well as broadcasting changes to a message queue.")
     (license license:expat)
     (home-page "https://github.com/alphagov/publishing-api"))
   #:extra-inputs (list
                   libffi
                   ;; Required by the pg gem
                   postgresql)))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "1ygjwxvsvyns0ygn74bqacjipdyysf6xhdw3b434nqzaa93jchqs")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "c57f87fbf5615705e95fe13031b62ad501f9d5fe"
       #:hash (base32 "016rc11df3spfhpfnyzrrppwwihxlny0xvc2d98bsdc43b78kjb2")))
     (build-system gnu-build-system)
     (inputs
      `(("ruby" ,ruby)
        ("phantomjs" ,phantomjs)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'configure (lambda args #t))
          (replace 'build (lambda args #t))
          (replace 'check (lambda args #t))
          (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out")))
                       (copy-recursively
                        "."
                        out
                        #:log (%make-void-port "w"))
                       (mkdir-p (string-append out "/tmp/results"))))))))
     (synopsis "Suite of end-to-end tests for GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/publishing-e2e-tests"))
   #:extra-inputs (list
                   libffi
                   ;; For nokogiri
                   pkg-config
                   libxml2
                   libxslt)))

(define-public release
  (package-with-bundler
   (bundle-package
    (hash (base32 "15aqjh800rzxs4qwisbsj5nhz1j0k4hr379dkyh5c0bx14rh1vw5")))
   (package
     (name "release")
     (version "release_331")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v1q5c69g48xzr67qkl462a64d7xry3v3zsnw7p0lm597f691frh")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/release"))
   #:extra-inputs (list mariadb
                        libffi)))

(define-public router
  (package
    (name "router")
    (version "release_186")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0wjgkwbqpa0wvl4bh0d9mzbn7aa58jslmcl34k8xz2vbfrwcs010")))
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
                          (zero? (system*
                                  "make" "build"
                                          (string-append "RELEASE_VERSION="
                                                         ,version)))
                          (mkdir-p (string-append out "/bin"))))
                       (begin
                         (copy-file "router"
                                    (string-append out "/bin/router"))
                         #t))))))))
    (synopsis "")
    (description "")
    (license "")
    (home-page "https://github.com/alphagov/router")))

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0a6w95m55fq6m0m8r47fdc2j4091mdyb3s3smz3k89gc14zkr1kc")))
   (package
     (name "router-api")
     (version "release_172")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rvwr433zy407vc5w7pzw8qdmjp58pw606f756vq0v6jdbpzcmc9")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))
   #:extra-inputs (list libffi)))

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1p5wh3lykcl7w9l076a1y7qxw45n5v2gc8m08vlvy86siliz8pfj")))
   (package
     (name "rummager")
     (version "release_1772")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yy330c0w20gpjik744fzgygfakhq3y2mwfxhhrch6hw3pzcx6i7")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/rummager"))
   #:extra-inputs (list libffi)))

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "0i084g7y8wwd58nv90zn61zcgm7gf2nqgx2sajglyin182sdjkzw")))
   (package
     (name "search-admin")
     (version "release_184")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13fzqhdlng2ds7ajhjmcb6frbnlv2m9lyzmk6fcqkzzx14b0v6mw")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/search-admin"))
   #:extra-inputs (list libffi
                        mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jdhwwy6y12c778m7n8b6m7iyckfv9qypsc2zqzal5b830xqq0r2")))
   (package
     (name "service-manual-frontend")
     (version "release_182")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "198ngg8cxq3gkbmsqjjp2yqm7pvzg1v3znby249zsc7wmqzbvgjf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0siwxqcp1k4by6hwzdi155knvf4c67bnc6d569ff57q2klghd26q")))
   (package
     (name "service-manual-publisher")
     (version "release_382")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rl700s48jc33lqdxg3gyfg4vp07hf8f1r9dm1xzqxdpl70ln7rf")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (inputs
      `(;; Loading the database structure uses psql
        ("postgresql" ,postgresql)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-publisher"))
   #:extra-inputs (list libffi
                        postgresql)))

(define-public short-url-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0rm6ganqj08h4v8xanv5i49xykbfanlkjr4qr4qfs3d6s8gpl389")))
   (package
     (name "short-url-manager")
     (version "release_216")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n8dy197ys40yviz5zlfg1rzzvr7k6145gasavv4fgwd5ywrjddm")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'precompile-rails-assets 'set-production-rails-environment
            (lambda _
              ;; Short URL Manager attempts to create a 'Test User' when
              ;; running in development, which causes asset
              ;; precompilation to break
              (setenv "RAILS_ENV" "test")))
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))
   #:extra-inputs (list libffi)))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pc5klpvyrb3rd1ljy3827pk5hkx1sh05as0a427yg3wqy0pvlxz"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1051")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mrwqy38g98z0sxx9l3m7fphslwcgky5n64q701fzbbfbdlc2a1x")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'precompile-rails-assets 'set-dummy-devise-environment
            (lambda _
              (setenv "DEVISE_PEPPER" "dummy-govuk-guix-value")
              (setenv "DEVISE_SECRET_KEY" "dummy-govuk-guix-value")))
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          ;; Ideally this would be configurable, but as it's not, lets
          ;; just disable it
          (add-before 'install 'disable-google-analytics
            (lambda _
              (substitute* "config/initializers/govuk_admin_template.rb"
                (("false") "true"))))
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/signon"))
   #:extra-inputs (list libffi
                        mariadb
                        postgresql
                        openssl)))

(define-public smart-answers
  (package-with-bundler
   (bundle-package
    (hash (base32 "1kl57091gbm1a1k5gqpxr0vjaa77ris2k03s4qg879zcqx8h5hvy")))
   (package
     (name "smart-answers")
     (version "release_4018")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h8z3yfrmi9a99yh14ncc1awfqyfymfawq4lbc7l97inj5j3ayp3")))
     (build-system rails-build-system)
     ;; Asset precompilation fails due to the preload_working_days
     ;; initialiser
     (arguments
      '(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'delete-test
            (lambda _
              ;; This directory is large, ~50,000 files, so remove it
              ;; from the package to save space
              (delete-file-recursively "test"))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))
   #:extra-inputs (list libffi)))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0q1vqqf8i0f5dq9va1g7k4f2hpgr850q8a4mmzybcpcvxz674iyx"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_991")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xicn5kp6p6z542r6n3jca670qdx7aybyzyfi0xvahdpcmcpdns0")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after
           'install 'alter-secrets.yml
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (string-append
                           (assoc-ref outputs "out")
                           "/config/secrets.yml")
               (("SECRET_TOKEN")
                "SECRET_KEY_BASE")))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/specialist-publisher"))
   #:extra-inputs (list libffi)))

(define-public smokey
  (package-with-bundler
   (bundle-package
    (hash (base32 "19rzqm6731swpgyz0477vbk7kxysmjgaa8nh26jmwvps7701jl12")))
   (package
     (name "smokey")
     (version "0")
     (source
      (github-archive
       #:repository name
       #:commit-ish "61cd5a70ca48eb9a6e5ca2522d608db75dbb6582"
       #:hash (base32 "1n1ah83nps1bkqgpq8rd1v6c988w9mvkacrphwg7zz1d6k8fqska")))
     (build-system gnu-build-system)
     (inputs
      `(("ruby" ,ruby)
        ("phantomjs" ,phantomjs)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'configure (lambda args #t))
          (replace 'build (lambda args #t))
          (replace 'check (lambda args #t))
          (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out")))
                       (copy-recursively
                        "."
                        out
                        #:log (%make-void-port "w")))))
          (add-after 'patch-bin-files 'wrap-with-relative-path
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out")))
                         (substitute* (find-files
                                       (string-append out "/bin"))
                           (((string-append out "/bin"))
                            "${BASH_SOURCE%/*}"))))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smokey/"))
   #:extra-inputs (list
                   ;; For nokogiri
                   pkg-config
                   libxml2
                   libxslt)))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "06sap48c9sai8lh7zg6g70lna6sgzmarcvbj9h1pwqwqsrcp7xqv")))
   (package
     (name "static")
     (version "release_2917")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1408np46lj8ybsmcfya697wja5maghawlbiv93bcx07c20n7dd6r")))
     (build-system rails-build-system)
     (arguments
      '(;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'remove-redundant-page-caching
            (lambda* (#:key outputs #:allow-other-keys)
              ;; TODO: This caching causes problems, as the public
              ;; directory is not writable, and it also looks
              ;; redundant, as I can't see how the files are being
              ;; served from this directory.
              (substitute*
                  (string-append
                   (assoc-ref outputs "out")
                   "/app/controllers/root_controller.rb")
                (("  caches_page.*$")
                 "")))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))
   #:extra-inputs (list
                   libffi)))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "1yj8vl51inw7fma508hv4hk1ly4aaw2vbriigim6v7f361j4w1g3")))
   (package
     (name "support")
     (version "release_698")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b2awmpjvjxxs2w5yim1cn7vanp115kmv8r3q8xzj8bi994wz3jr")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails,
                                      ;; as it tries to connect to
                                      ;; redis
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after
           'install 'replace-redis.yml
           ,(replace-redis.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/support"))
   #:extra-inputs (list libffi)))

(define-public support-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "05wjlxaxn86l6hs9bjsfd71h299xybblgmlznvg5gd3kqyklpblx")))
   (package
     (name "support-api")
     (version "release_202")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kq8391lkri786pifrxrvfhmgspxs97r0x0w9lbqsjb3kc7mrs03")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)
        ;; Loading the database structure uses psql
        ("postgresql" ,postgresql)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/support-api"))
   #:extra-inputs (list postgresql libffi)))

(define-public transition
  (package-with-bundler
   (bundle-package
    (hash (base32 "09fvrlaljq069wc0zl492dfidwv0hsvcqjpbls5pp3j4vrh8zrsf")))
   (package
     (name "transition")
     (version "release_847")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g6d3wa7gwi8k1gzpihl1fi6y71rcy6yvk8s17k3r8mvrhg4lrfd")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))
   #:extra-inputs (list libffi
                        postgresql)))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mf9sxmj2q8r1ynlgpmjbl65dzrgp8zjf2id0j3g8kpdsfamzw24")))
   (package
     (name "travel-advice-publisher")
     (version "release_401")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fdqmjb44fr5vj8vmz3y1541np2vy1idk7s5yzrrw08a319xiv4l")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-mongoid.yml
            ,(replace-mongoid.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/travel-advice-publisher"))
   #:extra-inputs (list libffi)))

(define-public whitehall
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ib0hf77kg2z4qrxv7bcmw0gp55j8915rcd5zkl3cy7llq9fc2pf")))
   (package
     (name "whitehall")
     (version "release_13582")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g7ipmg3ifvix4jg5lxbg7hggqx4ns87mps18l9b4mnd8lp7y1q1")))
     (build-system rails-build-system)
     (inputs
      `(("node" ,node)
        ;; TODO Adding curl here is unusual as ideally the gem
        ;; requiring it would link against the exact location of the
        ;; library at compile time.
        ("curl" ,curl)
        ;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml))
          (add-after 'install 'create-data-directories
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (name)
                            (mkdir-p (string-append out "/" name)))
                          '("incoming-uploads"
                            "clean-uploads"
                            "infected-uploads"
                            "asset-manager-tmp"
                            "carrierwave-tmp"
                            "attachment-cache"
                            "bulk-upload-zip-file-tmp")))
              #t)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/whitehall"))
   #:extra-inputs (list mariadb
                        libffi
                        curl
                        imagemagick)))
