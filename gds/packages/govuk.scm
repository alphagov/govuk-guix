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
    (hash (base32 "0pwf1n2cwrzxlmv82xakai2xxskv1587sm9amijiik426cprb0ji")))
   (package
     (name "asset-manager")
     (version "release_293")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y962d0g01zi6s3116996gzi8nc6dc0jqf4gjm5pscwjp6cd6q36")))
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
    (hash (base32 "0w7jzizrn46zil5ys19ahd6s8mg8b94saaf7rn2dg4vdv8nnsi9i")))
   (package
     (name "authenticating-proxy")
     (version "release_87")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12bmqrhgi7m4xslsyvqgwripg9fmfsp6zaasdk58119bf6i2gh77")))
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
    (hash (base32 "1p2kn32mp2qgis9g5f94byclx88gm4jnlyrhmzlxbjhfa7q6sbiy")))
   (package
     (name "calculators")
     (version "release_315")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15pbslmz2198qh93k4l1ibdyldmmhm95m2cmh783q97k3xd8xydn")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1dpss1i3i7zi47xshs7csjlw57ighvgs9294syg6vi00l64r3w49")))
   (package
     (name "calendars")
     (version "release_537")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0skfb0dhm3w4wqviacpd037qghvflkxbjrv7rp1vgqdr0sq2p91s")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "03nzsyqd03mdzhzdgaakx1l8bkhrw3azc08s1xp54hnfa9g7nzdn")))
   (package
     (name "collections")
     (version "release_545")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hr58g01iycix08i9p4562alnhhijdnk45k5z1cy00w8m0m8mga2")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "01hr3jxglml8vlbpmxg9kaws891lspj5k4bb9i7k3apf0zsw0gwb")))
   (package
     (name "collections-publisher")
     (version "release_396")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "109fkp6lrgadmhw0iay155fmck8yfxmf2pmagfljzycpf8a0a9m6")))
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
    (hash (base32 "053831zg1xvb05pi7p8yh11krkdmbqgk83mfhlqvwrfb2qxm0qcp")))
   (package
     (name "contacts-admin")
     (version "release_468")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dxlmbd17di9xkdp232grmb5qz99hj0zlpz3nw3laxzjd3ywkrns")))
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
    (hash (base32 "02hvhcpniskd9fhhw25kbf9gj61f081ms88fggkn7k14qjwxiyr2")))
   (package
     (name "content-audit-tool")
     (version "release_442")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1szhspbm8yykjwhxa9abwpwdlc20mzvgvp1jmbz1c3q3pyh63hyy")))
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
    (hash (base32 "0a6smdrrzcyj5bvd2pwr2i5f4i77sqz1gg9z91cycw0i9hv0n4bz")))
   (package
     (name "content-performance-manager")
     (version "release_593")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11vxiy837wd76kqsmw0bjg0nd1zwpvcj09b0i4yjsg5qk27hsd9c")))
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
    (hash (base32 "056nka2lag2npqdd1r6jf2qwvkmz3s1kzm7bkjfhmpyajbxh3z45")))
   (package
     (name "content-store")
     (version "release_768")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yxrdc2dw0lnwm71sck8cz3h0ipn34nq23pklym1cwi32i9z1v8y")))
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
    (hash (base32 "1qjhrq0jk2l5q4dws871p5xf2zx6m3f8wfy5laczl1iaandcahbv")))
   (package
     (name "content-tagger")
     (version "release_800")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n8qasq051kaq307c14wd4siq0x76w1v6m5zbrmh4y410kyymhm9")))
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
    (hash (base32 "0b7z24l60l4gqlqn592j1fr4ajvspp59bjsawryr1iibd9rqdnrr")))
   (package
     (name "email-alert-api")
     (version "release_595")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04pr2dawi75vxr56zjsgjaz1lv97xcqciml4y1afkca446aaldmn")))
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
    (hash (base32 "0cgi3ylmw80alk8xqwlcx7z12kwh7wrn68i3k7lr4h9kvcijgyi1")))
   (package
     (name "email-alert-frontend")
     (version "release_187")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01z5b24lxk8m75p49qh22xh7c14378a4hfb2l74wdb6vc2p15pli")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "08h1yld1fh3yxkmqjs6x8ipycvsgs8p6prz40mibh669zc6xqman")))
   (package
     (name "email-alert-service")
     (version "release_156")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04qjbliy8kam3ia8ls1xkv8d2xz5nqhdmm9hlx1fgn87rzv10rfl")))
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
    (hash (base32 "02gfg0bd6b35xg9c5z6fq8448pszxndv9x4jmbkhd08hyx6295qk")))
   (package
     (name "feedback")
     (version "release_451")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fh9xgmygjk00dgqz5w57d08vdgcrlgfzkjrvbdk12f12r9maixi")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ynq88jlsys5j0ax3q0v4cais4vi49jr6ik35j7w5j8lf5gr4l25")))
   (package
     (name "finder-frontend")
     (version "release_468")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y9pxv0pfjmgz7cn8iqshcsqwi5yhvbi61bxs7piwwqv172r4583")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0r0j8fqpw3gg2zyv8j8djbjh6jw8icdmhg285g24hqpvwsiry8jv")))
   (package
     (name "frontend")
     (version "release_2910")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xg7kp8mdlx4lpyg7pgqni223j8cl0v9j23c25dri8vknq55q5jp")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "005z41hjbkxh6lrh8xgbrwvg1pk04aabif55cwll9323xkd4qxly")))
   (package
     (name "government-frontend")
     (version "release_768")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04bry3igyf7mr3n8rwvsvgvdz17fi9m1k6kv2kg1fqxl84cj3h0w")))
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
    (version "release_739")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1v93qkgq2bsfv2mhxh9mk8blclz0lmxas2cbl55fhkmzwbz28idk")))
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
    (hash (base32 "0fsf6yc3m58i8hvvyvsx9nskwhngv5xrspwci4m3qb8cgv8pncr5")))
   (package
     (name "hmrc-manuals-api")
     (version "release_264")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qc7gpaniqgddwil930x9m5lxm86a08g8xnkq1njl2sgsxpd89qd")))
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
    (hash (base32 "0w08igyvz1ah2fab7p8qaa5w2d1b12y0crq6c6b1qm6f8ik3khbk")))
   (package
     (name "imminence")
     (version "release_401")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zypph70nfki7fr812vqkd7vra7nrjcsbkvak6qvnjv2isf4kdbk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0lnfnxda3zqdvy04940k3swl1l985nam3ddcy3jqbz1jwvdrymsj")))
   (package
     (name "info-frontend")
     (version "release_173")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fm545iilpiwbrxkyb7zndbvlw2xqlmzxvgw4kw4v72arrwdb5p8")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "129cl3xqgh93n60snpxg6qdybf65n9wl03vlcsr4idjsln40iyn2")))
   (package
     (name "licence-finder")
     (version "release_404")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n1355gn3h90ab0nn8c553fsjzd1nfyrc46jd4qkskf7h7k5n8lr")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "18rshpm5pyfxsssw1nw62bmxk20m3s6f3wzyl3zxkw1rk39sd6ld")))
   (package
     (name "link-checker-api")
     (version "release_132")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hjah24y1cp0ykkv1ap7h17ajkc6dlal2a6ivim21lylcjmwzs7w")))
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
    (hash (base32 "0nyf30jypczij70ih22l4qyxf2q1sqxdgin65mcc44m0cxzihagv")))
   (package
     (name "local-links-manager")
     (version "release_227")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06gz3viril2kbf807jxk159agwx7apslbm1f357f9dyn4cf7rci2")))
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
    (hash (base32 "0dpi9qhp56qh3nfvp3h0p729598fsy6m644s1zlfn282nzdp1hn5")))
   (package
     (name "manuals-frontend")
     (version "release_329")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jl4q0vj79igrla67xlnd5jq9dyf94i0s8nwplnp8hxxambwxlgr")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "06i011lxbyz1qjlyhdaswj8n500sw5h4hnmb3b0rzk1l80r4qzg4")))
   (package
     (name "manuals-publisher")
     (version "release_1101")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lgiy15a2crcr7bk606ndlqygikhy826207hd68z3h0kgia2mpm4")))
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
    (hash (base32 "1dcip0b0cq0w1vk6cvam212pb2v7bsb9ajsdwr54ri68chyyzvfd")))
   (package
     (name "maslow")
     (version "release_297")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03z1kwx9cqgivlw5d099g6wf984lfnfcv5cqmv8ng64zd1n5n86n")))
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
     (version "release_4")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "086fa00pv852ilmi90s5ciabk0x197wrqw6y0x5ib625ya5yqrra")))
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
    (hash (base32 "0482mprhzq1b71f2f8csm9sxy0z8zgz9k3fg5ki3gn55yazf60im")))
   (package
     (name "policy-publisher")
     (version "release_275")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "143cm9si52aw7fdxp5466paqvzb39kc17bgcw42h7xm16jm35p1h")))
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
    (hash (base32 "1ic6cyhzsdcshwx7xrixxqzgcf8qcyz8li4hab0z716lhwhc9f8p")))
   (package
     (name "publisher")
     (version "release_1983")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1b9iskji4qdnnnkavppd63j79ff2q20hkxqsn9mrv4rgvblz7ri3")))
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
    (hash (base32 "1ldcsxk2jq7z3zybc08ycvl8pg1yxxj4bgrs3pjsh9bdy6jr06p3")))
   (package
     (name "publishing-api")
     (version "release_1190")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0kdbiq3nw55ih4kxlrhxkrgmhxjrh5i9fy4l1nvc4fz3ka9lz0fn")))
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
    (hash (base32 "152gzhizfsn80qmy3jbkdy6817fl7jmvhjrcmhw3r5w7hknmi6ac")))
   (package
     (name "release")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1i82a73pdp9bn7fz35isrrvgi19dzwmhhxp9qn4hdg5p2k7mfsi3")))
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
    (hash (base32 "04k0bhkq9qhy6dq6zhv3d8sz8vrv6rkmyxh6m34dlp14dp7f4vv9")))
   (package
     (name "router-api")
     (version "release_169")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bi7iv3cbq437n1pvpw632y8ch8ldb8pzg6fb2qbf48455879537")))
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
    (hash (base32 "0lf1lv9bfvl03cybvs77m0qfhhprcaw8ykqls5k1c22v7z4qwl63")))
   (package
     (name "rummager")
     (version "release_1755")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ba49525yvhxcpf3ris9c6lzwhxbgpm9gv9qkca3841pll0pj0kd")))
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
    (hash (base32 "0ykhya1lpm7vgbvz4lrw5v2r13vdkp3g7l9xdg7jd9licyavqcjr")))
   (package
     (name "search-admin")
     (version "release_178")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pb56jd6328dbjpvd5cwd93541nlm7jmqn1wa4pbx2ndy1c6lbv8")))
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
    (hash (base32 "0r874njy3fplwl1m7pn2sz1qcvvi8jbwgrfwhcsiy50mi7m124c6")))
   (package
     (name "service-manual-frontend")
     (version "release_166")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08nfg5pciikhwp48sc1hzwg87fx8wrrkcn347ag5gqksf40r65sh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "15vvkv0shv1rn1yavd3sqy300z49b8h9q7iay7d5sy2ypg49w7rl")))
   (package
     (name "service-manual-publisher")
     (version "release_370")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07525ylklic8y9wiw900w2pk6nf031gizclvr3019rm8a007ywg5")))
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
    (hash (base32 "1y4z7s6p6gra7abgl2cinbn7hiv48nalsjiyji4k4k2n3l3briyb")))
   (package
     (name "short-url-manager")
     (version "release_208")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1byjnpn1mdq5ypwl1gcr5kl32bjvifd0qzfb49cgqx9sbv9wn8kw")))
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
    (hash (base32 "1fxkn3baljwkv1ykcsgl7p6wvnvw1z2db1l7l4g39s62a822hbh1"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1041")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y4fxrhgw2c8nx5raifnj0hxwa541jjss00bxbdbnay43pv1ncnl")))
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
    (hash (base32 "0cm6nfrj04xg35br9gla8il6933l8wjb636fa4l99igx7ibndddj")))
   (package
     (name "smart-answers")
     (version "release_3990")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1spnqy67pbmlkrw7rmqd000h0vg84n779qgk2dbr9z6njklndqx3")))
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
    (hash (base32 "1c89j53d0b7zca2zcd5h8r7zc63hhswkj3lqqn23d2kx9bvpbq7n"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_961")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fy6hj3sng9s3baf1fjfgxllwnqn00f7xyjsyffd2sj04w4jwzwr")))
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
    (hash (base32 "0bd3sp73yy5zk8j61i08xp1kdjdkb70jamz0lkq6xcs633vwh2sw")))
   (package
     (name "static")
     (version "release_2896")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g4zgnp9fppxar1l736vyf7snsz0ryp0xbxk5zgi5v42hrsxhvsc")))
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
    (hash (base32 "0ymg7w6ym7p0kbbgfw7hjarg9200ysxchs05za9vlsf0ggn994w6")))
   (package
     (name "support")
     (version "release_690")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1cn5jvnv988dnrvpg20nmzhklkb0kzmd1x096cd2jhx7g0b3gc7x")))
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
    (hash (base32 "06csgh5di2vksklxjmsbqw3h8nv4bb58r581jpqi3a0bfq31v2ng")))
   (package
     (name "support-api")
     (version "release_197")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07g6y5hywchbziq1zfmx2c4vpdki2gfm88i29cgaak726cjkb97l")))
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
    (hash (base32 "0p9iprifnw52rb53vlb97wd38rfycpsk0c1adm17d3xy08b3g4in")))
   (package
     (name "travel-advice-publisher")
     (version "release_384")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kxilx59za0xz8xs4d8pwc344xiq6dbrl0bqm06hapj6584fq0vp")))
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
    (hash (base32 "1zylwn02l1n2aa2gn6967jy3cvy5c8v7pa5fp7q2015rdgxxi1hx")))
   (package
     (name "whitehall")
     (version "release_13513")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z7lf46hvknsa7wapf48ci62h9ny5lscbjhsf5qvk8bsa59xcacm")))
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
