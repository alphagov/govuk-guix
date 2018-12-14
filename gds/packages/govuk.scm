(define-module (gds packages govuk)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
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
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python-web)
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
  #:use-module (gds packages third-party chromium))

(define govuk-admin-template-initialiser
  '(lambda _
     (with-output-to-file
         "config/initializers/govuk_admin_template_environment_indicators.rb"
       (lambda ()
         (display "GovukAdminTemplate.environment_style = ENV.fetch('GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_STYLE', 'development')
GovukAdminTemplate.environment_label = ENV.fetch('GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_LABEL', 'Development')
")))
     #t))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0fidah1hazdzwz4awl1v2ky1mg503b6sgw00v03hd0ncznxy8bgn")))
   (package
     (name "asset-manager")
     (version "release_339")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00v2bih8f5gi8qhck4hma3q51sszrn6wzc09rfa6knbqm49rjmzv")))
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
    (hash (base32 "1r8lc2mx5ib8qkg88z63zn6wva6d3q7g9fpv0zwk2yr2a338bfn2")))
   (package
     (name "authenticating-proxy")
     (version "release_107")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mcyvpdkks3psmwf8vb7p9x95my2l8ggg98z9rys9zbwx26lymby")))
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
    (hash (base32 "1s6bd5gq7hmz88df35kvkbf987lpw88gsab67v6q2rr64lw5kfal")))
   (package
     (name "bouncer")
     (version "release_238")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "196b2vxjj0hq5djiqjpbb8rzvy1b0qi3g8ns8jh0hy9gb93yjajp")))
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
    (hash (base32 "01vr9i1arsvmx7d3yn326p7l9f7mxj6l4qy2fpjc2m79a141fggc")))
   (package
     (name "calculators")
     (version "release_426")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10jjir1jg365ikhb4hmv08w2k0zijpq74icjywacr9m0in1asbji")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jpd89ilv90bqf46swin124v1p2ww1m3j0vqx0wpj768zla89dwy")))
   (package
     (name "calendars")
     (version "release_655")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ib11f58ln8yg7j7b41aj8ddcb0c538dkppsdrp1riw1168g8alc")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mfjaiyh8kcrliidy47nsv02rka5rp771i3a08xph9hmqn969xg2")))
   (package
     (name "collections")
     (version "release_788")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0s3n7alyd453g89f2v7r2h82v6cs0ifh3rj15h52il8dc13wgw0g")))
     (build-system rails-build-system)
     (arguments
      '(;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")))
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xmsa2jmr2axmx7mcqzgkgw7k6ki1xblxcmjiw346sj6qlz7q689")))
   (package
     (name "collections-publisher")
     (version "release_510")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1a9v9frywsa4hmlc6760b4yhna9pdff1qjqv918i6w49zc1s6dfj")))
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
                        openssl
                        libffi)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "03mryfs5rbsm4qq5d0v1waj252cdqj4g1j550158q9r3pp9jb181")))
   (package
     (name "contacts-admin")
     (version "release_535")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mwdldzjn035jxmzyaznzdyq5vxmjw3rb2mwsqzg0bvdcma3pbz7")))
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
                        mariadb
                        openssl)))

(define-public content-audit-tool
  (package-with-bundler
   (bundle-package
    (hash (base32 "1kxba59vrix4fhhl90698qcxcp378ya57n2br67bns4h9vjfxnm1")))
   (package
     (name "content-audit-tool")
     (version "release_535")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bk8nkap1ihwwbjic45nzapwk7vhna7jk64mwn9w4rip0lnfbf7i")))
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

(define-public content-data-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "0x57yi8m81l33hgg2sg3g14q782li65qyrkb3afklhwx92kf32ph")))
   (package
     (name "content-data-admin")
     (version "release_251")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pqhmfr9kkwfr09cpp02qg7sxk2pgbjwps47ivj98lsz6vh8qrl7")))
     (build-system rails-build-system)
     (synopsis "A front end for the data warehouse")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-data-admin"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1n3xsmm5xgkvh0jnm05zngm19ias207dvxgkx98xgrad0224kfa4")))
   (package
     (name "content-performance-manager")
     (version "release_888")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rzc67m226rrra8l392gh3fhs1vzc90na4wwszwccji0xwk5i8rn")))
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

(define-public content-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "137wsgnzvki88rcvxvd8728f6cg6glv56ixxsfn1cbjvv04dw7s3")))
   (package
     (name "content-publisher")
     (version "release_535")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "125g90z2k06k28f81i1rraj9na6sd1qjh5bn6w8virci3az8007d")))
     (build-system rails-build-system)
     (arguments
      `(;; TODO: Asset precompilation is now failing, due to npm not
        ;; being used to download some JavaScript.
        #:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
         (add-before 'precompile-rails-assets 'set-fake-SECRET_KEY_BASE
          (lambda _
            ;; TODO: Active Storage seems to require the
            ;; SECRET_KEY_BASE Not sure why, so set a fake one to make
            ;; asset precompilation work
            (setenv "SECRET_KEY_BASE" "fake")
            ;; assets:precompile seems to fail without the
            ;; JWT_AUTH_SECRET being set
            (setenv "JWT_AUTH_SECRET" "fake")))
         (add-after 'install 'replace-database.yml
          ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-publisher"))
   #:extra-inputs (list libffi
                        postgresql)))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "0nizx84jnxfd8in09p4ddqhxlfbqb9ixhl0grf5gzr112dfygsz4")))
   (package
     (name "content-store")
     (version "release_816")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ah77lab426pcb66i78imavldwx35avx0x5adc87cmq9k1rk683p")))
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
    (hash (base32 "0xq0zjpv136zijga7dl0jdxvjs670fqv23hgsg312v8qziiwsvjx")))
   (package
     (name "content-tagger")
     (version "release_884")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "060w7m3dmsd8gyfnkrcby0k473qd3h4gpaqhi5n1g984bdlmhban")))
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
    (hash (base32 "1b2fladgqmdspvymhqrbhi1r1q9mfzl7nja0vvd16zzvp21pwmif")))
   (package
     (name "email-alert-api")
     (version "release_716")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06sdyf8dcj2nh0209y4s6sgiwkxg8qf7ridfp1n9pszypd359fc3")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
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
    (hash (base32 "1r7xlvic43qcmlsyzcgp2jg873x3bnwgx8czx7893y8nsc61waap")))
   (package
     (name "email-alert-frontend")
     (version "release_293")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1acdx617lbgz83824bkj0h9qigfinl7g4p3a5pdc2yka49kssji5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wsk74a6la267856jsvp3hlp06sp9b569zw2mh9ffy10rlb4w76q")))
   (package
     (name "email-alert-service")
     (version "release_198")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vgkcw3ghikr7lihra0p0754cvxnzjbgmwrb3ccm9ag192538j47")))
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
    (hash (base32 "1h1fxkv2rnxa8i578s3k3kqwm8bz9hqsi379gha76ql816q64a74")))
   (package
     (name "feedback")
     (version "release_576")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lbh8x9ny0jhd7nsd7nbx16r3g5lfyy0rrpdi3764hj4yy41psnk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "15hbfmsc316ifmxhn7ih9fvf22553nci857n78c88hrkyr6nmy49")))
   (package
     (name "finder-frontend")
     (version "release_666")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "047b6mrj3ibk8jdiq318hla51j7d0rk9w51dh8jnsvgan5rg6jvx")))
     (build-system rails-build-system)
     (arguments
      '(;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "11xdfikg0lmmb51qslp2xrl12gg8ca0jndph1v7x92ij3a60pplp")))
   (package
     (name "frontend")
     (version "release_3049")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sj8rswkj4nx4skj02avzc93ajwjpci17rn9dnr252fmsxpg8ws8")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wgds67ryvmswgj0mqssndi9bzfyfj43460d5676v85s74s57p3a")))
   (package
     (name "government-frontend")
     (version "release_976")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k18pbbq1dkk8y38va1rqk5zidja9ws79da65zmk8wzrldgcxx91")))
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
    (version "release_838")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0zahf8pblb30v31r5v377z2c1c2w4k5pda9gv0f401vzxl83pf2a")))
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

(define-public govuk-guix
  (package
    (name "govuk-guix")
    (version "release_2")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0632b4ycs7biifv3nnlpr9f6kwgvg6vh6clzwf3xlrvsbacbmidv")))
    (build-system guile-build-system)
    (inputs
     `(("coreutils" ,coreutils)
       ("bash" ,bash)
       ("guix" ,guix)
       ("guile" ,guile-2.2)
       ("guile-gcrypt" ,guile-gcrypt)
       ("awscli" ,awscli)
       ("gawk" ,gawk)
       ("ruby" ,ruby)
       ("pv" ,pv)
       ("pigz" ,pigz)
       ("xz" ,xz)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/govuk" (string-append out "/bin"))
               (for-each (lambda (file)
                           (install-file
                            file
                            (string-append  out "/share/govuk-guix/bin")))
                         (find-files "bin")))
             #t))
         (add-after 'install 'wrap-bin-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (ice-9 rdelim)
                          (ice-9 popen))
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (module-dir (string-append out "/share/guile/site/"
                                               effective)))

               (copy-recursively
                "gds/systems/govuk/skeletons"
                (string-append module-dir
                               "/gds/systems/govuk/skeletons"))

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
                 `("GUILE_LOAD_PATH" = (,(string-append
                                          (getenv "GUILE_LOAD_PATH")
                                          ":"
                                          module-dir)))
                 `("GOVUK_EXEC_PATH" suffix
                   (,(string-append out "/share/govuk-guix/bin")))
                 `("GUIX_PACKAGE_PATH" = (,module-dir))
                 `("GUIX_UNINSTALLED" = ("true")))

               (wrap-program (string-append
                              out "/share/govuk-guix/bin/govuk-aws")
                 `("PATH" =
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         (list "coreutils" "awscli" "ruby" "gawk"))))

               (wrap-program
                   (string-append out
                                  "/share/govuk-guix/bin/govuk-download-backups")
                 `("PATH" prefix
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         (list "awscli" "ruby" "pv" "pigz" "xz")))))

             #t)))))
    (home-page "https://github.com/alphagov/govuk-guix")
    (synopsis "Package, service and system definitions for GOV.UK")
    (description "")
    (license #f)))

(define-public (current-govuk-guix)
  (let* ((repository-root (canonicalize-path
                           (string-append (current-source-directory)
                                          "/../..")))
         (select? (delay (git-predicate repository-root))))
    (package
      (inherit govuk-guix)
      (version (string-append (package-version govuk-guix)
                              "-snapshot"))
      (source (local-file repository-root "govuk-guix-current"
                          #:recursive? #t
                          #:select? (force select?))))))

(define-public hmrc-manuals-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "01h197pj46d50jvf1gx2crddfj9klazm5mn86l9305q0am0rz6n7")))
   (package
     (name "hmrc-manuals-api")
     (version "release_300")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hmz0glr6zf58mi99mbnisl8hxi7hgl54j1gl5qbasls0b35an5l")))
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
    (hash (base32 "0ws0am24vlwbr4zlxly01bwhgyj566g57p9xy2rngbr57p2bv7db")))
   (package
     (name "imminence")
     (version "release_472")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "141r936wjkki9f0i2rh8pw786ha5rmvi2wg2w6jyf0rb2gsvyqji")))
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
    (hash (base32 "1s8yjvsxabjw2n3mjgpfy4g8vhr4b5sjh9y6y0hx09j1mnpy87hz")))
   (package
     (name "info-frontend")
     (version "release_282")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pxd0rj0drl7x3aahp5lmb244fgz05jdxgx7s2gzczp1kmcipypf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1yys29qw9mdv1ysiwrmn74489h6r5b0mn9j2xwmfgpiv5v2bggp1")))
   (package
     (name "licence-finder")
     (version "release_511")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bybg3rf476l6rqxdgphibf8vdbrp47zghj1816g0zbldz324vyf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1n8159wjk010kbddc1jv29fkwlck4amndf04clrw51c0g4g6crlq")))
   (package
     (name "link-checker-api")
     (version "release_169")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ngxzz0arz9znqvv7hdq9h4zn3zrl8mabgqkgxhh0vjx95bxgqda")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f))
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
    (hash (base32 "0aivhlwa6349mihh7575ia4h79i236xssnlpzr5lfivdcpmi0rwx")))
   (package
     (name "local-links-manager")
     (version "release_301")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ymidwhbsdn3d0rgxniiga2bz23bz1m6vg349vf1fkl976n1xr7c")))
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
    (hash (base32 "0n6m3knybyi06cqb0aglbcvgsva19ylg6h90qq08s17pfazmd54p")))
   (package
     (name "manuals-frontend")
     (version "release_436")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ayz4fksfxqm7dlw1xd8y2jvvmpndnl5lpiblfg3g7yvkjnpgw4h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1lca59g0hi6qsr6idgdfrr2c1gn656pxvzwzaa441b1ssh76lnys")))
   (package
     (name "manuals-publisher")
     (version "release_1153")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bbxk2hyl3gh8i305ybwrikxfz82hmfrn3rrg78hzk1qbgi0hzy5")))
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
                 "SECRET_KEY_BASE")))))
        ;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-publisher"))
   #:extra-inputs (list libffi)))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "0m14j7il0x65fk1q271jl7y5hr9rr4lzjy6h26dqh15gswjiq1yn")))
   (package
     (name "maslow")
     (version "release_358")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1443wh6zrkyflkbpp5wjk5469yam0cimlvvl0967gw3hi9vgcrdf")))
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
    (hash (base32 "0x4jgvdikiwyw7apir2l0ax82x705m48b4xl6alq8f0wnjpa0fqv")))
   (package
     (name "organisations-publisher")
     (version "release_8")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11wyclj4l89176svwxblky10lzbqymwm7sy8dkc0d945j30kb7sy")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/organisations-publisher"))
   #:extra-inputs (list libffi postgresql
                        ;; TODO Remove sqlite if it's unused, it's still in the Gemfile
                        sqlite)))

(define-public publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "06w3jn6sclw0xghmzkf3rgim9ijkv33ij3n709p5xg4yrvywwvj4")))
   (package
     (name "publisher")
     (version "release_2072")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14kzwrb9xlcq7bz2z2iqshrpwrq075cffikypiaavdz8gybxg88p")))
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
    (hash (base32 "1fi4s996y0i0wxwzd5zdfrphz5qckdx5hvsarpsb5fjn2lhn69h0")))
   (package
     (name "publishing-api")
     (version "release_1332")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1n4bdlqlx3prhjaijk75glv3sxashgac7gk6876c6bl77d5p96kw")))
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
     (base32 "0v0ywf7685mfdj7axz43wb8n3r943d5ys0jrzjzbc5dh97cdpaka")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "58ee6e4190d9b0d0316b2635f1c7944ce6118fc1"
       #:hash (base32 "14nn0czyrxhhbciv85y7nfajh6dhp670p29qw6zwmcsz9dng68xr")))
     (build-system gnu-build-system)
     (inputs
      `(("ruby" ,ruby)
        ("chromium" ,chromium)))
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
    (hash (base32 "0jnlrpq8b7jjcwmps6c2awdfxaf2r9hmbjjq8fgp5013zhf6i9qm")))
   (package
     (name "release")
     (version "release_394")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c9dbxl0jyf725jm3wvd9cvfvkis06xxfwwzdh4bhl43wj10rh34")))
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
                        openssl
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
    (hash (base32 "18b7jsxw9fm34k3wp6klq3bv77yfflf2ggm9q0jadp7vzgsv96l0")))
   (package
     (name "router-api")
     (version "release_195")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1phbk2k9s7dai7lm5bpgfjpw6flfqcxvsqwkfqm99ksa4nj5xaby")))
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
    (hash (base32 "03lxvhl8iq0rv8zawx8yxapdms905m0dgl64ai26p39r8rpaahsh")))
   (package
     (name "rummager")
     (version "release_1885")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i4z2zq0kjiq2byvww8znshc551p1fi2rhdwmb3zzndbcif5jvad")))
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
    (hash (base32 "19lx20gf1qx33bjsx9058q9d3rjmsgwx08iwvrm33mxgdcck8rz3")))
   (package
     (name "search-admin")
     (version "release_217")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fqz18y4j5cixb7g4b02qn5cfh0njg4laz15pqvdpcc771ggw89f")))
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
                        mariadb
                        openssl)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1v4yah183av61hfzs4vdjh9x6d4i9xl51jnaw4ikpqp48y1rn1in")))
   (package
     (name "service-manual-frontend")
     (version "release_268")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08nfz70823b71acz0d85ikbphs7m63532j0lnhlr56i2y18a69ry")))
     (build-system rails-build-system)
     (arguments
      '(;; jasmine-rails seems to get annoyed if it's configuration
        ;; doesn't exist in the spec directory
        #:exclude-files ("tmp")))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xl1z6110py5x6593favqlz38nb55qrvl0v8qzbcynxcrycwjz0f")))
   (package
     (name "service-manual-publisher")
     (version "release_437")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h1q7kd6bm1dnyaihz7zx52hlhj07j5v23m5cxgcn79xwyigpbhj")))
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
    (hash (base32 "0l7apdw0zq6lvf33dnp50lhhkrlm8pj2lqyg0mdri1qpb2c86cai")))
   (package
     (name "short-url-manager")
     (version "release_264")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04pmq473j54wqc33l0axak6zk97bz1b8hyvg0jdlcccclgpq6b68")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-gds_sso-initializer
            (lambda _
              ;; TODO: Disable the creation of the development user,
              ;; as this breaks asset precompilation
              (substitute* "config/initializers/gds_sso.rb"
                (("if Rails\\.env\\.development\\?")
                 "if Rails.env.development? and false"))
              #t))
          (add-before 'precompile-rails-assets 'set-production-rails-environment
            (lambda _
              ;; Short URL Manager attempts to create a 'Test User' when
              ;; running in development, which causes asset
              ;; precompilation to break
              (setenv "RAILS_ENV" "test")
              #t))
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
    (hash (base32 "1078y64zb18nsblydn00c60g1sp277vb5ybfb1mglx6xvswcwzy0"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1158")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1j0kwm7jmxiipz8yzagq0zk0f6rs8vxgsdkwl362sfrp0zq7dsc6")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'precompile-rails-assets 'set-dummy-devise-environment
            (lambda _
              (setenv "DEVISE_PEPPER" "dummy-govuk-guix-value")
              (setenv "DEVISE_SECRET_KEY" "dummy-govuk-guix-value")
              #t))
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
    (hash (base32 "0ys417md1y68csralrijz3ilzbxqqms16ifm70bz0ajgji9wcnqc")))
   (package
     (name "smart-answers")
     (version "release_4184")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kqlmjyzg8i3z0wwm9h1ry1y2l6rydcdk8923dziyamx05qgggw8")))
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
    (hash (base32 "08wmvnb7a4kam82slsyqmfv0mpaa38w8x1fch8qxixxi4w199yp4"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1074")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wy21pkg2gh0md8bma2lsrixrnjbmcgia6svrr59y1m7v4a62zwd")))
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
        ("chromium" ,chromium)))
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
    (hash (base32 "18cprx9xxvp4psx51yx0aav2q0g96jagdvp8alf9rbyszb8a69xc")))
   (package
     (name "static")
     (version "release_3024")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b7lxnyf9hv05vvb7zl9vq32kf660zqlisjh2h3prwdy16h4kxx6")))
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
    (hash (base32 "02yc0smz2vrbxrpq91cqbpm0dsdldvk0r9vbwr1d60jgfxyxxd6j")))
   (package
     (name "support")
     (version "release_760")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c7dzg8lsdj0rxjcmgigjs9cx6vd226q1802vjhaqbrj5gassb5w")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-gds_sso-initializer
            (lambda _
              ;; TODO: Disable the creation of the development user,
              ;; as this breaks asset precompilation
              (substitute* "config/initializers/gds-sso.rb"
                (("if Rails.env == \"development\"")
                 "if Rails.env.development? and false"))
              #t))
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/support"))
   #:extra-inputs (list libffi)))

(define-public support-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "04j64zxp0k52s0l0kxs90vfc3wjlkq0qvb4rqn2a887i2kw6l83n")))
   (package
     (name "support-api")
     (version "release_239")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lmm7mxv2q6cyqabfrfbvzs8245ncydnf6f83rsaxz57cb347rci")))
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
    (hash (base32 "05hrn8pfcw6sr4dgb3dh7gz7irizh51wkda4i2glzadvymkvjzp8")))
   (package
     (name "transition")
     (version "release_878")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05pzwmln5k3kx8z9813bd5jdashwx5l1z9p2wgrfgcd868nyrfpi")))
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
    (hash (base32 "0qp5vr0jg8cg47z9fah8ggsbpp6jw0shxkn14mxyywgs609v2888")))
   (package
     (name "travel-advice-publisher")
     (version "release_503")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hgfx8zxa3l7p6avjimfbm367cypca8l12hps4k45jvb012239qb")))
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
    (hash (base32 "16ssbqqjspmybsz3sg7dnrg0r0kpiyw1g96xdvqxmxl4zb0x4157")))
   (package
     (name "whitehall")
     (version "release_13894")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ar4q2rkp7q762gqncfwj6sanz4q5qa25m2n8yd0kf0axly5lz2i")))
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
          (add-before 'precompile-rails-assets 'shared-mustache-compile
            (lambda _
              (chmod "app/assets/javascripts/templates.js" #o666)
              (invoke "bundle" "exec" "rake" "shared_mustache:compile")))
          (delete 'reset-gzip-timestamps)
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
                        openssl
                        curl
                        imagemagick)))
