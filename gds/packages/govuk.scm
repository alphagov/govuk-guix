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
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages terraform)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sqlite)
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
  #:use-module (gds packages govuk ruby))

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
    (hash (base32 "1hq28jdq5v86i1yi7i00yqyvxhyv88a7bfahz3d4cz4j2xbhamvv")))
   (package
     (name "asset-manager")
     (version "release_484")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1m08786cnmcmf1bba7slcycknbcfsnbvaiqp8ss1whfwwh38vk9j")))
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
    (hash (base32 "129n07qzchj6dpjrxgm61399141cy76a5dzhr2ykkk7n5j61fkh5")))
   (package
     (name "authenticating-proxy")
     (version "release_197")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1cskg2v3k5m60jqvbzy93lp9d3h6xh4g6m1x6fxyxwj0xhmac2hn")))
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
    (hash (base32 "0x98xr0dq8wgjxs8rlshyyxk3s21ppwc6nmxb2rpixy73i4nr0bg")))
   (package
     (name "bouncer")
     (version "release_307")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kbkb6xmk361lwfzsg4ymsagycg6m1knyxk5gc60cipf7i9764ac")))
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
    (hash (base32 "1w35n333hglfzdbw8grrkxi40xkvlwvh5spckg7cy1wy1w643a4v")))
   (package
     (name "calculators")
     (version "release_735")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r5k3vn23k2msqgxnnblvc81zvr532nqra5x4k2ad2a34820x6fz")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "18l85q047x0dph3i7qndrgvyks4knw04d0nz9lpdg5ygfb041gyy")))
   (package
     (name "collections")
     (version "release_1506")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xfqyrv8p72scvyriyx6j9rl9bl3wdwrvh52hw16wiwli3zjapna")))
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
    (hash (base32 "1lbi41zq8cxqy3wb8az2gsyn86pcghi8g37hyz9aidhwhv23d780")))
   (package
     (name "collections-publisher")
     (version "release_941")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04gnffdbiir2d2mz1h44gnhb7hhh0ghw3nz4idg04b6y2yzgf3n7")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f   ; TODO yarn
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-before 'check 'set-GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER
            (lambda _
              (setenv "GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER" "true")
              #t)))))
     (synopsis "Used to create browse and topic pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections-publisher"))
   #:extra-inputs (list mysql
                        openssl
                        libffi)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "18krparayaj8bhic4sawgf0g2pih79k7wjxi4i5m58s5y5p9k785")))
   (package
     (name "contacts-admin")
     (version "release_739")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ra27702hd6k8f8cahjvc6h70gh5vbycld6163i469m2l2yah4lj")))
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
                        mysql
                        openssl)))

(define-public content-data-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "11f18sxd3x7w0a30bva936d834vhypgsl8jspf0z8fpgv6qw7wxm")))
   (package
     (name "content-data-admin")
     (version "release_688")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16w6h8zd962di9ljpnfpi1y81na4j84sgj1d4p0zjr2dc69gh32s")))
     (build-system rails-build-system)
     (native-inputs
      `(("chromium" ,ungoogled-chromium)))
     (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'set-GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER
            (lambda _
              (setenv "GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER" "true")
              #t)))))
     (synopsis "A front end for the data warehouse")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-data-admin"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-data-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1g4qr33bldv0ykfprnqrb96jpr55gcd7cd9ggvc1ynh0wldkbl9a")))
   (package
     (name "content-data-api")
     (version "release_1273")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vdj36qwz6zifg553007lmcc21y7nf01rp5h6qiz87c1gpc14wcl")))
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
     (home-page "https://github.com/alphagov/content-data-api"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1bdg09q7993vhgjiajnfkyc1jrlnm9ckfdfyda39yypslliqj112")))
   (package
     (name "content-publisher")
     (version "release_1858")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c75nvm56ki1jn8hp0z2l6p8syw2m662x8pf2s48s22mdwjqj96g")))
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
    (hash (base32 "1ahlxj1bgsgbiq7ki999bc7c9xl88nj1nkj5hlqd6mcwkbaxb1wr")))
   (package
     (name "content-store")
     (version "release_1027")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17ssfnmxri2jjkg0p117agqq2la9c2dzvcv0hlfglgs7d7mh203z")))
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
    (hash (base32 "1aqf7wcxz1n7fl818agml7hzcz7svgq3wcgf4lizyj40fagj2s0z")))
   (package
     (name "content-tagger")
     (version "release_1119")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r37arzds1aphyfja8fz3q8j8vcrz830nzbqzpxy9jnm34g2as49")))
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
    (hash (base32 "1wg0ivpagjqg0qrd4xjw1ia80knsv3vp1mvggzbw3y2p0m1949bl")))
   (package
     (name "email-alert-api")
     (version "release_1196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jkiwzk92dy0l0ijpa2dyasr4hikllsdcfcn37kj3pv42893wps3")))
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
    (hash (base32 "08d7n1znx3sflqnnc0lx125yiavba36b856wxml1gh8cyhxqqddc")))
   (package
     (name "email-alert-frontend")
     (version "release_618")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hay710gr3qssxijm46nvh8n55y060vjsbxnf7liy8xaljx0k2nl")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zczf4bmra6iqil0fi9f17jgcmirw23wibhll89y2xy64micbcaw")))
   (package
     (name "email-alert-service")
     (version "release_355")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hybcpqjq28wqzkmqwd1phvalqvmnjarjm7dpb8w38pblwsd0hfw")))
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
    (hash (base32 "081bvglm2kwrpkx5kw0lhahxy6hivhpr3gbbh4waksf20cjrsamv")))
   (package
     (name "feedback")
     (version "release_878")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c7vj7532j5mbkrvn43i55a1bq7p8c1gb0m0d63asy5i4f9wrzaq")))
     (build-system rails-build-system)
     (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'set-GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER
            (lambda _
              (setenv "GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER" "true")
              #t)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "19lzhfan0mhyrp9gdd5p3d1zj3kvvfdr13rbh29npnyg182v5l4c")))
   (package
     (name "finder-frontend")
     (version "release_1826")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m8hz98aqn370mi3k378z5gpzgah66jqxchk86zp10x672y60knz")))
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
    (hash (base32 "0hi6him4k5a3zzr7a5f07g156impyb0x0ib08wacy8xlgmjmgn9g")))
   (package
     (name "frontend")
     (version "release_3595")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r8qhdjniw9rvvlkl5s69jhga5bwa3vkbm949wqjlk72s69w6b32")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "03zcvb4r5xh93hswrk9z5rcz4f3m8zck5pcv43k99arn2rx9z9f4")))
   (package
     (name "government-frontend")
     (version "release_1479")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07w4ad8ilphw51wb5p21n7ajw8cb7njw402rp1njm988j5jlksj2")))
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
    (version "release_963")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "17khrf8krlj0bk4nn0wqkidq61z3a75pz983fqpqz2qa3jck09iz")))
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
    (version "release_3")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1g714x476g1b73rjfkxk8av8jcb4r7ql39c1d5pa7zjn45ndc14a")))
    (build-system guile-build-system)
    (inputs
     `(("coreutils" ,coreutils)
       ("bash" ,bash)
       ("sed" ,sed)
       ("guix" ,guix)
       ("guile" ,guile-3.0)
       ("guile-gcrypt" ,guile-gcrypt)
       ("awscli" ,awscli)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("ruby" ,ruby)
       ("pv" ,pv)
       ("pigz" ,pigz)
       ("xz" ,xz)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-GUILE_WARN_DEPRECATED
           (lambda _
             (setenv "GUILE_WARN_DEPRECATED" "detailed")
             #t))
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
                 `("GUIX_UNINSTALLED" = ("true")))

               (wrap-program (string-append
                              out "/share/govuk-guix/bin/govuk-aws")
                 `("PATH" =
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         (list "coreutils" "awscli" "ruby" "gawk" "grep" "sed"))))

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
    (hash (base32 "1xcnhbwk6bd08q8amxnzh05gzaz6a14zkxkckhfg3q9phy5828f8")))
   (package
     (name "hmrc-manuals-api")
     (version "release_458")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fpdi3zs4ayscmwnglap72lb6xqkhjakxkwj2359g91kl8pzk51x")))
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
    (hash (base32 "0i15is71v3r7y6l46mpx2whjjvmfaanymbycazbjqy8srsypw6p0")))
   (package
     (name "imminence")
     (version "release_651")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "088z8h7nywyqxz1yxwwdr2qza6zcfzwrjh87hg1sw0hnsm5v2b24")))
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
    (hash (base32 "03aqih1v0cwj002iqabmns9dm8q61rdb6zm0475dx2qncpxyrsid")))
   (package
     (name "info-frontend")
     (version "release_536")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "195f88v98q1m05pwdvm566qclnnhxaml4ghzlzfipk4vkpgbgxn5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "12ivx1wxjzyl0bppwzwmw2yzb7yjwzikfqziiy6wp78rwglcj2iw")))
   (package
     (name "licence-finder")
     (version "release_786")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ms3fmws0n884vfdjh9aav4zdk524iafkfdlc2jbhgpd11lvyjbp")))
     (build-system rails-build-system)
     (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'set-GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER
            (lambda _
              (setenv "GOVUK_TEST_USE_SYSTEM_CHROMEDRIVER" "true")
              #t)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1i04biksvblps39m0nms2y7ghsb68xgfhbi0myv2bgqa52mmvzw9")))
   (package
     (name "link-checker-api")
     (version "release_325")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l4352jwpcrynv24bqd4gqnabp4wgf3lzakcn0gnykms1zc69r8a")))
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
    (hash (base32 "1vmr31z1gq5j00gldxrz785r22c9p5163iz99lmys84wy4zdnfnk")))
   (package
     (name "local-links-manager")
     (version "release_568")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xh2p7d7ybpax9vp1rkdq19fz6wlw2xvbrcrv9865g71yfnp8gs9")))
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
    (hash (base32 "04b9i2hpjk6vh4bi6cq3aaijjm49kmq4i729adqi3fk2c81rnhc6")))
   (package
     (name "manuals-frontend")
     (version "release_735")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wkh0h42jvb482iy8plv1mpqykjdzqy337gzxw071ijsbv5cbpxw")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1km9m6n6p6jwhdk8j3fhfwszps56z904rj62rapb7l2pipddsnr8")))
   (package
     (name "manuals-publisher")
     (version "release_1337")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08kz0jxsn3j3qlcyrmklr1zwfvwldmn0yrj3avxsmdbfzip10m34")))
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
    (hash (base32 "0p632jjdk1vkjy0chm8jrp9567cvjab0r4ajrvjp9gm2i57fsyxx")))
   (package
     (name "maslow")
     (version "release_556")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09clj1j9avwxdidalc9gjrhrq0076g1bnbdisnpvs0z2iqgbk06v")))
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

(define-public mini-environment-admin
  (package
    (name "govuk-mini-environment-admin")
    (version "release_10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.cbaines.net/gds/govuk-mini-environment-admin")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0axgw29j545v2w6dpf9jrh6n9v5bs61rf24lxvc59hk0abscw26v"))))
    (build-system rails-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'replace-git-ls-files)
         (add-after 'install 'use-relative-config-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/bin/rails")
                 (("\\.\\.\\/config")
                  (string-append out "/config"))))))
         (delete 'wrap-bin-files-for-rails)
         (delete 'wrap-with-relative-path)
         (add-after 'install 'wrap-bin/rails
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-ruby-program
              (string-append (assoc-ref outputs "out") "/bin/rails")
              ;; Terraform doesn't support a search path for plugins,
              ;; and expects a single directory, so just lump together
              ;; everything which is used via the PATH in to one
              ;; input.
              `("PATH" prefix (,(string-append (assoc-ref
                                                inputs "PATH-dependencies")
                                               "/bin")))
              `("GEM_PATH" prefix (,(getenv "GEM_PATH"))))
             (substitute* (string-append (assoc-ref outputs "out")
                                         "/bin/.real/rails")
               (("../config") "../../config"))
             (substitute* (string-append (assoc-ref outputs "out")
                                         "/bin/rails")
               (((assoc-ref outputs "out")) ".")))))))
    (inputs
     `(("ruby-rails" ,ruby-rails)
       ("ruby-sass-rails" ,ruby-sass-rails)
       ("ruby-listen" ,ruby-listen)
       ("ruby-web-console" ,ruby-web-console)
       ("ruby-pg" ,ruby-pg)
       ("ruby-gds-sso" ,ruby-gds-sso)
       ("ruby-govuk-admin-template" ,ruby-govuk-admin-template)
       ("ruby-plek" ,ruby-plek)
       ("ruby-terraform"
        ,(package
           (inherit ruby-terraform)
           (arguments
            (substitute-keyword-arguments
                (package-arguments ruby-terraform)
              ((#:phases phases '%standard-phases)
               `(modify-phases ,phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "ruby_terraform.gemspec"
                        (("`git ls-files -z`") "`find . -type f -print0`"))))))))
           (source
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cbaines/ruby_terraform.git")
                    ;; hash for the support-passing-target-to-destroy branch
                    (commit "b858f9d7b1a5c193b94bd4bf38eea5fc3afd0295")))
              (sha256
               (base32
                "0714zhc0rak2wrb2yivy7a1naqlcccwgd90lp2vpjs7aj3j23i2k"))))))
       ("ruby-que" ,ruby-que)
       ("ruby-with-advisory-lock" ,ruby-with-advisory-lock)
       ("ruby-git" ,ruby-git)
       ("PATH-dependencies"
        ,(directory-union
          "govuk-mini-environment-admin-path-dependencies"
          (list postgresql
                openssh
                terraform
                (package
                  (inherit terraform-provider-libvirt)
                  (source
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "http://git.cbaines.net/terraform-provider-libvirt")
                           ;; hash for the for-govuk-mini-environment-admin branch
                           (commit "682f8a9865fe0bec16423f63018725f5b364659b")))
                     (sha256
                      (base32
                       "1wxiyfkmvflf6c669h6i9ym21r5w4mw55jiy6gma3cddlbhpjhhb")))))
                terraform-provider-template
                terraform-provider-aws
                terraform-provider-local)))))
    (native-inputs
     `(("ruby-rubocop" ,ruby-rubocop)
       ("python" ,python)
       ("ruby-mocha" ,ruby-mocha)
       ;; ("coala" ,coala)
       ;; ("coala-bears" ,coala-bears)
       ))
    (synopsis "Manage mini GOV.UK environments")
    (description
     "The GOV.UK Mini Environment Admin is a web application for
managing small isolated GOV.UK deployments.  The services to run, and
data to use can be selected, and multiple backends are supported.")
    (home-page "https://git.cbaines.net/gds/govuk-mini-environment-admin/about/")
    (license license:agpl3+)))

(define-public publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "14x5qqy5dd8kgqp1831scpfmm545a4dv3q1xfy3l526c5nvkj3z9")))
   (package
     (name "publisher")
     (version "release_2321")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "146w09gl79ngh0firqhdmsjp2pc5mbcxzhdkqp20f07adhp6sbla")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
          (add-after 'install 'replace-mongoid.yml
            ,(replace-mongoid.yml)))))
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
    (hash (base32 "0jhd46qy4dwf5wfh4biry8mg8azi323pd3qj1nmspzwnjpnbsg35")))
   (package
     (name "publishing-api")
     (version "release_1695")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0xrizxl0dpasw50gira10y3954492qhcglzlf7x200843qryggxc")))
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
        ("chromium" ,ungoogled-chromium)))
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
    (hash (base32 "11ds5x1bk1vlqk0l9i1a5jn83lsz2hbn8b0jzr0vyfjvvd18a99x")))
   (package
     (name "release")
     (version "release_650")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1idlcai5xndj4qzgqscar27p940csjvp3chnw51130xjs8f0lwfl")))
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
    (version "release_208")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0ap93hhvgwwm1nr4hb52l75hvrciizkxv6xn3q6yz2hcp1fg8d6k")))
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
               (setenv "HOME" "/tmp")
               (setenv "GOPATH" (string-append cwd "/__build"))
               (setenv "BINARY" (string-append cwd "/router"))
               (rename-file "../router-copy"
                            "__build/src/github.com/alphagov/router")
               (and
                (with-directory-excursion
                    "__build/src/github.com/alphagov/router"
                  (substitute* "Makefile"
                    (("go build")
                     "go build -mod vendor"))
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
    (hash (base32 "0q3zs9n16nbmafsd1qvj12rs3sgyfdaldlk8qfaxqdddbg0ra3w0")))
   (package
     (name "router-api")
     (version "release_309")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12ivkcfi7lsisiw12h5l2kbr33d2xxdc35mc01h70nglpz3aw3fw")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))
   #:extra-inputs (list libffi)))

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "00d1l41n11sj4cm8kgrc0hqs1d3ghz36x583mwl3psymw71jm2cy")))
   (package
     (name "search-admin")
     (version "release_406")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vx90l53zq1fh7h7hw5kj5fxiac1hsfw07xyf345mx7rsamw2z65")))
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

(define-public search-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1sh0pr8fdjkmjwxxj4q1r5ha4iv1w9cz4c5590m2a1lrjwkvabnl")))
   (package
     (name "search-api")
     (version "release_2561")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1si5np1qxqqhrllqiwd8q822899993my1wyvh7iki63ismvw0b70")))
     (build-system rails-build-system)
     (arguments
      '(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (delete 'create-hosts-initializer)))) ; TODO This isn't a Rails app
     (synopsis "Search API for GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/search-api"))
   #:extra-inputs (list libffi)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gzab10jhk9qlrj75ag7m85dr6x1x1a2vsbz4zz0arqa9p8w181m")))
   (package
     (name "service-manual-frontend")
     (version "release_559")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0yvcc6c1xxfb9bzvqhhc2hcxlvany0r2vmnxrlwdvxh1lcvk43ry")))
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
    (hash (base32 "1xzbljzvrl47i0wzsmnyahrhdvskqv6za9vv4h02l737cysaabl9")))
   (package
     (name "service-manual-publisher")
     (version "release_652")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z386p7a001nhlm93449csy51v3w7ir7wmbjqqmqzqs8kdf14pnw")))
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
    (hash (base32 "0grmxq7ixid1s3xj9kv3fhvpbqmwbb3bfh0cnjndph11jsxmz1fm")))
   (package
     (name "short-url-manager")
     (version "release_457")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hy5chiwf8rmd27nahn5h0vp0ppb3rz9m1ffxfdi28v2qf5zy9a1")))
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
    (hash (base32 "1fglfpssy1vc954x8qm521xbaid10v0srshblcf30x806bglyh7z")))
   (package
     (name "signon")
     (version "release_1498")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dc6lcfsghmkm5hcngjv7p37lpfxak5h0vkms5cn5jvhfhr5f2lk")))
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
          (add-after 'install 'replace-database.yml
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((location
                     (string-append
                      (assoc-ref outputs "out")
                      "/config/database.yml")))
                (delete-file location)
                (call-with-output-file location
                  (lambda (port)
                    (simple-format port "
mysql_default: &mysql_default
  adapter: mysql2
  encoding: utf8
  username: signon
  password: signon
  reconnect: true

development:
  <<: *mysql_default
  database: signon_development
  url: <%= ENV[\"DATABASE_URL\"] %>

test: &test
  <<: *mysql_default
  database: signon_test
  url: <%= ENV[\"TEST_DATABASE_URL\"] %>

production:
  <<: *mysql_default
  database: signon_production
  url: <%= ENV[\"DATABASE_URL\"] %>
")))
                #t)))
          ;; Ideally this would be configurable, but as it's not, lets
          ;; just disable it
          (add-before 'install 'disable-google-analytics
            (lambda _
              (substitute* "config/initializers/govuk_admin_template.rb"
                (("false") "true")))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/signon"))
   #:extra-inputs (list libffi
                        mysql
                        postgresql
                        openssl)))

(define-public smart-answers
  (package-with-bundler
   (bundle-package
    (hash (base32 "1vz9qkqvc9pr5l4dksdya8pr3x7bm7dpq75yvc5n3m7f4947f73c")))
   (package
     (name "smart-answers")
     (version "release_4821")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y7sini7gscxnnym94m5fvwa339fk9w2q6kc2shj084r7ka0vjqn")))
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
    (hash (base32 "0ng4dy0gwgf8cb8p1kzw3mh8sqbvvn6c6d323p9i3c638pc06yfw"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1353")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jffs9qdizq3sa9n0xcab59p28xq3kbcvizd41n43x389faa1pvr")))
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
        ("chromium" ,ungoogled-chromium)))
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
    (hash (base32 "0qn9dfb7832mnkx6z75fq6dk7zawyv1v8f54g46ghcxmjihh3qfz")))
   (package
     (name "static")
     (version "release_3549")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vwrznc2xz6bxi4mjvify8f2fg15jwqffncvka355s6fdz5f3434")))
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
    (hash (base32 "1d56f6mll3gxkih57m8dryiz42d66wg49c4r0lpj41jp1fvci4ac")))
   (package
     (name "support")
     (version "release_1001")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09mlb1jzjb9ca3h0dlj32lm334gvxwg82rx7ls9w22p0pm8pkjkc")))
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
    (hash (base32 "1agkc10vypjvfjlimb1f11x3bl8j0ndas1zp6z33lr5fn120iybn")))
   (package
     (name "support-api")
     (version "release_431")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0affa91pfa5cmr6cn60m0wzad2kx5p7bvi73llh9kzlxyl2zmkyx")))
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
    (hash (base32 "1alxcvixx71ki43kc7gmfwk6gygbpc0ja3hazyp16mdxwfwj6nl5")))
   (package
     (name "transition")
     (version "release_1127")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mah56hjbri6g49fi3r70clp2hnk2h3fpvj751qgx6smjrb468yf")))
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
    (hash (base32 "1kxxwrrjm5yf94yizahr7s6kc3mfbkfsm0x6haqk7d15k9c18gq3")))
   (package
     (name "travel-advice-publisher")
     (version "release_801")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mfs775p6d96gligbkxw95a62xgdhx7m55mkqbisawmz04sa1hwg")))
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
    (hash (base32 "0lr6h7zij0w1ic3dminkkk7yc1gk55sipbb5qcxmz98lkfhqjhxy")))
   (package
     (name "whitehall")
     (version "release_14831")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fh9i0q1r85nwvflxsma1mhyd3yfk2b47975cg020j67h4cbf7yk")))
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
   #:extra-inputs (list mysql
                        libffi
                        openssl
                        curl
                        imagemagick)))
