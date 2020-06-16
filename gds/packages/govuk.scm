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
    (hash (base32 "18xmxa0m6qs59qaynmfk2drazg48qxr3x323ry5nzvnqsp64l0ss")))
   (package
     (name "asset-manager")
     (version "release_482")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l6mk2g6vgqc0hr2ia97njlgk8p6n0j4d2d09i28kbwlz2d7hp2v")))
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
    (hash (base32 "1qcjxscd6sm279d61dx0fvl4zc3yd90b2590iggjjqsgk2lxa8zr")))
   (package
     (name "authenticating-proxy")
     (version "release_196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gh3szbvm017amlk01n68r3blj0nzv2k7imk6dmq8i7snih6wacn")))
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
    (hash (base32 "1zvpd2fr7fvghbkikhq2s3dm6dgndaw1d7kfy5qdhkrbnk4q5dg6")))
   (package
     (name "bouncer")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wsfzzi8wljrgcvf81jzb180vp4xzar0k1a56p09z4wnxr9cnh7i")))
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
    (hash (base32 "0c7qmrrnpi0h6bcff55c6kksnfc4y3if72fr519lsj99xcirbd8d")))
   (package
     (name "calculators")
     (version "release_732")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kyb7cng4djmhj3qg1cwfrzq4c6gmqfwn38kpvvfzgd4pxvp9dzw")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "00pmcypbsfr5vapdsx6s9bsw0liksyimwylsz08rs5dvg54inrgp")))
   (package
     (name "collections")
     (version "release_1500")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bbmibiw8hw6s85fhap0f77havx9jpzzgp897abiymnl5kna03mk")))
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
    (hash (base32 "0ip3aql5w4pymcnwiabwv7lfxjhj7hr40z292cxq3rac58cf05ki")))
   (package
     (name "collections-publisher")
     (version "release_937")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0nwx7xfbhcvl29nm80zi5vmdiyzmpmk45pwk9cr2yisgb2nqz54s")))
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
    (hash (base32 "1wzhmmjmxy3fdzndzqmscsrf08kpwz4jgbpk8r5429izrvwdgchz")))
   (package
     (name "contacts-admin")
     (version "release_735")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03975dnrnx8wmn58wxrxs4ih48pqkzfrnn5j40rrcbzs59450h3h")))
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
    (hash (base32 "0ysfw51gbvf1wwpyp28a3wf373jyiilgq1hs2x36alhlgv1d4j2s")))
   (package
     (name "content-data-admin")
     (version "release_686")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ggj5ibp8x70fzpbp3hpjpbkwy0da59wdml29928k7vv5vxckdvl")))
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
    (hash (base32 "1lk0kd16mks22wd6kc9cydmxnnlybgzla1hxmss5iqh0b1586a66")))
   (package
     (name "content-data-api")
     (version "release_1271")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lj6kccz8xkj2k4zq9qkgkqm4gmp68cq6lmlfnr31xg6i84xb3zl")))
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
    (hash (base32 "0qp2rwwh1f2d2gp11q585mrw9jhrggpc42jdigwf38y43afmxpf7")))
   (package
     (name "content-publisher")
     (version "release_1854")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h0s6xwx15myrjhl0idxf96hsvb86flb2g4q0vh9awrdq3n7sb93")))
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
    (hash (base32 "06zshlsnx6z1600fcs04dwl2wfq5vxpym6hcq895pydc8na4z8vl")))
   (package
     (name "content-store")
     (version "release_1023")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xysl8v4nm6f90hmp036qlakqjgg8i99z3nxwsrdg6dyyjv3fahv")))
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
    (hash (base32 "0df6kvlzkqyli57077mbphjpk8g7njvyk0shni77x5q5rszj0ylf")))
   (package
     (name "content-tagger")
     (version "release_1117")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1s53rrkhsqznxhc6sxgq1c5y4yfjhar5b1g78rqhsfs7mf65163w")))
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
    (hash (base32 "1wijiip1ksfl6l1sf531j2iicdy55vk26jy4wzm433r265vrgxbs")))
   (package
     (name "email-alert-api")
     (version "release_1187")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0866dw4r2jq8aldfxg146zhkpxs505jmvzadnf32jbqdai6p8c6f")))
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
    (hash (base32 "1h8qskjrcj586389r25i0zniy3ivy11rigpw87gkb6dqg4cm0kk9")))
   (package
     (name "email-alert-frontend")
     (version "release_613")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12ix26pdwbpw338yxynvvlvcljgxhkh3w8iicjdqdb3nkdg1w15k")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0dz08mn9f6899knb87mijif851wxysjjvrq16a68s7i3g3ykqass")))
   (package
     (name "email-alert-service")
     (version "release_353")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a2z53b3saph3qwlysisjlqdd9115l3idmixfi7a41qhx9f8zc2n")))
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
    (hash (base32 "1vrf24babp7pbp34vhpql1l0wjd9fnf2hxgbsm9ahvhr0irgjdcm")))
   (package
     (name "feedback")
     (version "release_874")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03d885rwv3m8fpvxaqgpmrvchz5fyp1z0fsf8293zi30ygnhz7dg")))
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
    (hash (base32 "1zz6xaaj9kki2hnbmrjyng12fqm8k8ap36v576gcaz71hswh2bxv")))
   (package
     (name "finder-frontend")
     (version "release_1821")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10fkyahd53cr7x4kfislmqgxb0ah4j2yxw1b2mp0mszd18cjngrl")))
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
    (hash (base32 "17gnl81k1kh6ndv9xhflsachgsbnkpcpcjd2nfjw5wfw4cj1wfjr")))
   (package
     (name "frontend")
     (version "release_3591")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05c96lizmixsffyri32n1ca8ycpnmmxqy02vf96s958pvr6v67c0")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "12hz8pmxysvammjy0wmbclk8hd277xwcq5248nkgz02f3wm3zix3")))
   (package
     (name "government-frontend")
     (version "release_1477")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gif3475p5x5ms4c8066pwsb25dxiz16z9gp2w1b1zz9cqq912s8")))
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
    (hash (base32 "12d9pbkwigmjpifmfdmfy5sv60kpyfcbqgpkdrjpaaaidd036lmc")))
   (package
     (name "hmrc-manuals-api")
     (version "release_455")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jiagky8gmps68js9ymc1zknmab98zbr0wdk6y0i4wrjzln7wr8s")))
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
    (hash (base32 "16dcr2a020348id144gqvrsk3kfm86z79firaw0jjhqcc5bv6vyn")))
   (package
     (name "imminence")
     (version "release_649")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14i4m6isd5q0xi1k6rm651kx0x8f7n9949cf9a244vggm07x4sh3")))
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
    (hash (base32 "007ysyrmvqm9nz3k8k3kii93x50v3s2img4ldkwz9sl6vz1fjwq4")))
   (package
     (name "info-frontend")
     (version "release_532")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "028x5idwpkaws062gnqqjh8zmib942n65jd5rr0cxvinv9iklfhn")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "11z8vhpbn8vc65l8fxs6qcv0sa6fjr2wy241cg1mh8v3wqgriiw8")))
   (package
     (name "licence-finder")
     (version "release_782")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19c3zv62lrcxm8539lrq1yi3wym0lz8vhdsqwizkisxc4f17hj9p")))
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
    (hash (base32 "0s6h2ylcwclkh72i8x4d8hgnx1zpchbvlkwhdpqhpw5jvihc4bgi")))
   (package
     (name "link-checker-api")
     (version "release_322")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1a3pwd54197735fynpcihidc4qgbmkrrnv4nhck1js74amvp8vfl")))
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
    (hash (base32 "1wa5zcygf0hj2ywhjvazxd4iki3bknhayr8q8kvrmy6ncq01nk0v")))
   (package
     (name "local-links-manager")
     (version "release_563")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ccvdk8n8lrijkiiwzjkrdxwag06qgam0l89vn37q3r2z8nyiadg")))
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
    (hash (base32 "0778bfs7mvizapk2wsdb68jbd7c8zjvapggip0j09rhy293k5lsd")))
   (package
     (name "manuals-frontend")
     (version "release_732")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03jd434is9s2ljypar8km6kmfj02ngxdj6s3lrrkfg30piskp2fd")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "08rxh2z6xvswqzqwq4k5gva05xhrinji0zsqg15jyg1kcinchsck")))
   (package
     (name "manuals-publisher")
     (version "release_1334")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kb5fc47ydx16nxz9yx4bck1d81apkja4dkrx4jvsr78mkcqlj9f")))
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
    (hash (base32 "1hn61xpq8ybyfzj6xz2maxcwcms0rdhds6bwb75w6b2xa06h4aw2")))
   (package
     (name "maslow")
     (version "release_551")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kaqhv1jkp9g0lys45w15w26gfgc9rhmjpg8dzlvpcdd2lhnbglk")))
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
    (hash (base32 "0r1sgzk7rp4wz53pn10wg2cmv2kp551f7paqwm948hapbnl9xsnn")))
   (package
     (name "publisher")
     (version "release_2317")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rsi7nfsizjxlfywm3x4k266dcirl8w3fq6hixs61bvhr6xnpaf7")))
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
    (hash (base32 "1wlywvdn1a35zgg7f95m1jl50fkyrhi95jwfabsgaiaf2gysdv3n")))
   (package
     (name "publishing-api")
     (version "release_1693")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1pjpcwrf3m5fg851drlbbrkj3cwwxqlgqa8hs75sjan1psdp51qh")))
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
    (hash (base32 "1rjaq0nbvaisww38549c3x1in169nhbc9as3a9xg0a2a0ac6g2zs")))
   (package
     (name "release")
     (version "release_647")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0w33r5a26rhqb6vgi4r9chf7slsadl4yh7w54ffyr3grn9l8ji5i")))
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
    (hash (base32 "0y5r09f2k5rqwm0d9p9ag8fzrprcad6qbsdby22kvggbwpsfbhh8")))
   (package
     (name "router-api")
     (version "release_307")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14q6aaclb0s1sl9r9ylaa33c13g19hcar3zck67xs60ahf00vixy")))
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
    (hash (base32 "1gsvnw6ifygknzn5r1nb9il8h4xnz9rkxrz1v4pnzcbdcb7jw20v")))
   (package
     (name "search-admin")
     (version "release_403")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gmp2ydy6rx5lc5hr5a895f07794jbgb038qws6r15k64wdv5dby")))
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
    (hash (base32 "0cldkbgf6099p6vgkmwj87bgkpzwiyhsfwqw0n7ccb8y3dc6nf8c")))
   (package
     (name "search-api")
     (version "release_2559")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "077lqjn290acs99b9s9g0kmmim968f1400m81nlji2hhc2mxbfh7")))
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
    (hash (base32 "0n94dc34x9dfba8qr6w6iranfz2xhljb0xkdkf9q1vjqv6vmm8n1")))
   (package
     (name "service-manual-frontend")
     (version "release_557")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yh8wxgsfa0qz19czasswjip8ki0bbay7h8k7lv4z83l7ss0qi67")))
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
    (hash (base32 "1fc5w3pf0n2vl7qmkfk9r8pw77mi9wx92yl2h35fz4sh3rhk2wmz")))
   (package
     (name "service-manual-publisher")
     (version "release_649")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yylvqjwcmlkwyfirj5212np5m6lkx0nly5flks6b3h2q5lbn9yz")))
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
    (hash (base32 "18bsmwhc9nynriwrwdbjfg8sf88krijy5wivxpvv7xqyk71cmnig")))
   (package
     (name "short-url-manager")
     (version "release_455")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "199q1kwg4prmx1dafz17694zc6pgiz2gxxgpi1y627sakppkrfap")))
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
    (hash (base32 "1k32cgj2yk7pganbsrf1vlja2gdipyhl0r7ifmv9h252ljgl1sdk")))
   (package
     (name "signon")
     (version "release_1494")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18ndinrw5v6l4bky8y60lcy8pv2hlyi322jd5k46ri3ymxgv9jiw")))
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
    (hash (base32 "0ms7w60i1xp61yhx2yh83ywvwfkh7yaw865f23pnp3l96pnww35w")))
   (package
     (name "smart-answers")
     (version "release_4808")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09ibfph7sk7pa7f8id60m3k46nb4500ahvsc5mfblccl2x5dbwwx")))
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
    (hash (base32 "0j4b5kzc4xfv7xlx6vgfrjj0svalaj0fqgsliw123dah4swcr2gz"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1349")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08gllpavc5hna7mifm3qlb7xmryd4lk752z1160rzv2ywk42dq1j")))
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
    (hash (base32 "1sp7jmvbn9947hwbbxi7gpbdc023wn2x3m4w4i1n9lqd23yn28yf")))
   (package
     (name "static")
     (version "release_3545")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zwggpyaz3rpplr9zlyap3xkxbp83lr82d75gc37wbmvxqh27qz2")))
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
    (hash (base32 "1ic01cr57dl73c5w58j0nq8aip1vjywyr907ahs49hdjk8d541s8")))
   (package
     (name "support")
     (version "release_998")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04n6xdd7wc10r0zfdcxc7azk94zs69wc4waw17jj24190gn38gf6")))
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
    (hash (base32 "0q7yavx5kfz0k7bvd73n7dj00sambpc968gi78kvjq53nsa92a7m")))
   (package
     (name "support-api")
     (version "release_429")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1j8czi1widd3srw4b2kdwx77p5vwnqzjj5sf9i8g3sj39r8w1ykv")))
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
    (hash (base32 "0qfs828w5r6ab0hhmmvrd8aq70wl3lzx2a6fdp66ny0b5bnjcg69")))
   (package
     (name "transition")
     (version "release_1125")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03as0k8675z5l9gxw3glrjgsi8gfphdvg9hk9qb7n32vbg0nm5m7")))
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
    (hash (base32 "14pb6x6xaf47xfn52a0ld78px33lzfpz53r0hhdy9am3x07nrmih")))
   (package
     (name "travel-advice-publisher")
     (version "release_798")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vibx5szl1livj7ibbc3rlnvvdr9vhixbr1gi7kkiwxs9hg6w5cl")))
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
    (hash (base32 "0qa1wxm4mrvx8ll2bn9ik082lvb1yx4l90nsmd09nncvc197k3jm")))
   (package
     (name "whitehall")
     (version "release_14816")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1f85c1gxm1m57yqg5df3f8srbflsmh63p0jycm933l94q9c2m5rz")))
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
