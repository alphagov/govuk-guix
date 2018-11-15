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
    (hash (base32 "1d8qym055q3l585r282jd3ai3nz8fq889q8w842ymp6v1lhcbi8g")))
   (package
     (name "asset-manager")
     (version "release_334")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03slzhz9wbn9gpmq06nzjbw7id2wl48rllmj259rvg0k82kr62q7")))
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
    (hash (base32 "1iil7npan4fnnmwdg2r9yd9j7dgcwa0jdv3kwvqbrhpfsx8523zh")))
   (package
     (name "authenticating-proxy")
     (version "release_105")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0nzvpzb9pyrarzcknlsljygypzhzpaf5s7ch3c4r14q66n67bxyv")))
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
    (hash (base32 "03zwfr32xq6nwj5sp2izn69h4jr1r7av6jn058n6ib2zvib059qz")))
   (package
     (name "bouncer")
     (version "release_236")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hpwc5n157hc5012jhv1dbl1aiydkpvran67k3w58gyama01ffmd")))
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
    (hash (base32 "0xfj0ydln8lfz91cy7w1nkmlwkyj2vynr57q5gk12g7sp1mjxr22")))
   (package
     (name "calculators")
     (version "release_413")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fh28m20f0n9py556j7v2ga3nvgklph01jzahpndz58nhvyri6m5")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0clcpnr9m2iapnyifplxhrn1095kxkf5p1ca277g4fcsiscg5b85")))
   (package
     (name "calendars")
     (version "release_634")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cd0hnxchyqwlamf89bgx4b4lsmrpwlf1asnmd58a33ghz94dgl2")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hirfwi6q4j3lnbr81ad1dg65z8k2qw1grzv177pwii0zd7pib88")))
   (package
     (name "collections")
     (version "release_762")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14pgmc5p9pxlpw79m2szf9acd4gnmk1gj1w613bqp42b1d69w6gp")))
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
    (hash (base32 "1221gd08mxrwf0i5vvhn5shwshh7c4ndand2y6kb5mjrxrciv2jj")))
   (package
     (name "collections-publisher")
     (version "release_500")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d0ambcpk6i9aqx5dmr5ncm23808h4xpy9afapdjam1hf4f41h4d")))
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
    (hash (base32 "03bmzwbxwjsbwi1wprw8440n286m82xxkgjz4a29xn6q0ff757yl")))
   (package
     (name "contacts-admin")
     (version "release_526")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14x1lybx1j7zyi1pa0wwsg04xjpm96i32jy6l5bda97agyxzbr6v")))
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
    (hash (base32 "0p64yv3scpgw647in0ij3bjbscay55nsqx730ggw5firzjh2mv7p")))
   (package
     (name "content-audit-tool")
     (version "release_527")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ksz7cnn3v5y98fdha0icm5q23gyp3sx68ih4s9glyn5sgwd989y")))
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
    (hash (base32 "1qih513nlp73caplfnd5ram55ms795hhh9whv1zzmyga2zadwdcj")))
   (package
     (name "content-data-admin")
     (version "release_184")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03lfdl5jb2fh7cffypb5x1qmvmdn89gx6bp9wxbb2whkq6vlgyrw")))
     (build-system rails-build-system)
     (synopsis "A front end for the data warehouse")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-data-admin"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1x6phlzwwi9r5cdxkls4qr93b8gqzd9803gzl1aa4a39af2iii2h")))
   (package
     (name "content-performance-manager")
     (version "release_841")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j3w6hacavi352ja2wqgzpqfc4fl7mdbh3ni5by1ckj6hkprpk4q")))
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
    (hash (base32 "1z9ab1ylr3l84zh17yl0r4446x37alilz02n1h3h783dbf7b7325")))
   (package
     (name "content-publisher")
     (version "release_420")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09xgryrp9r50w6vgpdw4zms6gbyc11lc4r154cra3gghv8bmd7fk")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
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
    (hash (base32 "0s5y22v8vlv7skrildf11i335skjwcpzzzmp43q4hfyl8wxwvp3q")))
   (package
     (name "content-store")
     (version "release_810")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17ya15cynsm61jxz118dpy853imkzw3lf9dwancl4qwifzrswlg2")))
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
    (hash (base32 "00bsl2jy4ljyx5m926m9x6mdyvb7c782mxgbczmh7nl60v23k34h")))
   (package
     (name "content-tagger")
     (version "release_878")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fhnri4cwzxyjxgn5vw3i9wswlkb8gc7qxvhfw5ig2yq1qjf01gq")))
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
    (hash (base32 "1gnimajlfc9bcdz85mwwabpq4pkn4l3pf4hlm6aqai8shmpspfbw")))
   (package
     (name "email-alert-api")
     (version "release_701")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13xcy43qxg5i159ibxwgmqlcxsz94fz2bw4qmzz7g8dvvfwc0cwc")))
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
    (hash (base32 "1jc70i861341zrhqx9w4im090i5g9x8bb8hyyjhxip6fsy0mkn7p")))
   (package
     (name "email-alert-frontend")
     (version "release_282")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x8gvcxbc423dmi59mgaip53lsd7sfn88c83ilqrvhqfjf2814sn")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1dl75qxnn1lxz0wp4wvxymjvm06bg91g1wrlbz68i64qvyalj3hz")))
   (package
     (name "email-alert-service")
     (version "release_196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09ay8idl5ij3biy0n69gmq088ccgynl0pz9k68rs7lzwfy7h67r3")))
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
    (hash (base32 "0qis8lq9wa6j7qwmgpp4q36i8q95v2p5d0yh8by67ha6wkkwv44m")))
   (package
     (name "feedback")
     (version "release_565")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mlmlasp1bfnx67cn8vaai7wkfshpp7i5a153wii8sh9wfg4ps1h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0apk6b67mc31y6gfqfnzbz5hbdq38mvzwjhv5zjvkwdczma8k9b3")))
   (package
     (name "finder-frontend")
     (version "release_597")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y33y4kcnjx94zzdc1vi0lqbv3zpd44n3ajbx2011azkzh63czdy")))
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
    (hash (base32 "0wrkg3y8s674ww5aqn88839v6cd2j7raq411cgj6v8sj1jhslnch")))
   (package
     (name "frontend")
     (version "release_3028")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "121vzb6jsc4nni60w1ig34pxckff9wvx2g7np0bs51wy4agxvgjw")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "10qa97x443imqd2d8c7mx5vhcai9wxdnnm59kim9r5irkw98jps8")))
   (package
     (name "government-frontend")
     (version "release_956")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04qg8cmmik0p8nil2h0g03y4vgbqan3zmq03idbb2m599y2rrvnj")))
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
    (version "release_825")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1vcr2dnza501d03p5i6wp0hadhlhyr4mrvarch02rx8adi5vc2ga")))
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
    (hash (base32 "1cm0z1k78alsrijxpbpa2gf5cwgszs05lxfcfkfff04v7q92a444")))
   (package
     (name "hmrc-manuals-api")
     (version "release_296")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "186jckr37ybds0l8ahjhn4sp0j75kyh6819r693z51d6zbyh9qg3")))
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
    (hash (base32 "0yncfb9zv3g1f5d7z2b3mya2ksnj76a78znlxj3f1jkad76ymxz5")))
   (package
     (name "imminence")
     (version "release_468")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vmylvzk9d8aw5clvnkzjvyrjwxxik8m1dq0rax5781a0gvh7hvw")))
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
    (hash (base32 "0220ggfjck6ph37p102745328is5mjwnp5jhh2d85jiy5v99vx58")))
   (package
     (name "info-frontend")
     (version "release_269")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1nm081r22jf3nwv6x4dpq20d1nkljzx47bwm1j0sivwisyhcq35b")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0514fvhl4qfwl8z52ja882n2z4sc65b2p8wyl5c3cnigv2a75d2g")))
   (package
     (name "licence-finder")
     (version "release_502")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09xf8bwwh5nib0ajj08h6a7b4677x2gbmq2fgpx622myhyypsl8c")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1d4z5cva7s81wv4n3rv7pr6kzywwcs7aaikc5074k8h0qkb5vzzr")))
   (package
     (name "link-checker-api")
     (version "release_165")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rsjc1ikpfq5vzmkjdlblqqvxcwj69722mxjjsgiiyvgp753vly0")))
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
    (hash (base32 "14cfqk59q0i7jbk3q5yj00w65cv08gbs1fr6j3ladyrmqbvn3d8r")))
   (package
     (name "local-links-manager")
     (version "release_296")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09jz4y46wcpc8asni24pqprqf6ynxs7v1rrd7r4gf8ck2gffjmk1")))
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
    (hash (base32 "16xy58b2hr0scn7f07ks1qh15cacrgw94gvc20hjcznhc2dbwpms")))
   (package
     (name "manuals-frontend")
     (version "release_423")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ai761041w13qm2h5gpl767vixrfmy8w46pq3n9czj7a3zcnh9q2")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0r85f5w0k16ipq0cgp0whq3pbczpwy4q1f12ygw9sngizjh6jzxy")))
   (package
     (name "manuals-publisher")
     (version "release_1147")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "097pm4a4h7a8h8jr7ddmidczir44nflxz0032i589hgcgdx2gn0a")))
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
    (hash (base32 "1ya640lcyqgkgsiwfay7gbka35gb71ypd9bdwz4r6nvxq1zq5j7b")))
   (package
     (name "maslow")
     (version "release_351")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12njx7xwkwkaq8kvli248fnk10ik5vq70xs3k4540a6ys1wwsk39")))
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
    (hash (base32 "1s3v8hj16c66zkdi0vczrhpfj6l0x7q0jsw05j4wcf7389vdv49f")))
   (package
     (name "publisher")
     (version "release_2065")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12vs3jdllnmsrvpavs73007n9xzx8g01yczsjwiqmmdm94n8z07p")))
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
    (hash (base32 "1d3hj09cwwmkn4ajxkmlmgng625v6s95pmj6qsvzabj3y0rqarza")))
   (package
     (name "publishing-api")
     (version "release_1312")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "06j3xk0sdi1spi1ad2m1mpwzgrl2fpx8adg0x2a7v3bmmx51jmbq")))
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
    (hash (base32 "18svx9wdjik06qgi509s3knasiik5gqcwcv6nwyisirhhfz4284x")))
   (package
     (name "release")
     (version "release_389")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a6fgygiiv6q8ymczljzh5bwlkn2jf9fgkrajbjrdcqs9rcjnqv1")))
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
    (hash (base32 "1rbh2860h4rxp1q0sgn2r77nk1w620d791n6sl2kpdhabz25i6zj")))
   (package
     (name "router-api")
     (version "release_194")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13j23flm7yi9c3r4br9wv4zw5lnk26b59b301da3kdapwq6xj8fc")))
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
    (hash (base32 "1zzyqps3i5p8mv0mq1yr4d6h7gs50pkbnvy3k0vvc428mn1073gd")))
   (package
     (name "rummager")
     (version "release_1830")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1m1r7qlhbvbas326g3d7waj5wa537ln771zibwdby8sfic513cx2")))
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
    (hash (base32 "18j0lqvv27j47x0ll1rhg3xmf5lz0cdgirqjivp9paq2n8cyzjqh")))
   (package
     (name "search-admin")
     (version "release_213")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xh0fnhm052j90p4jkb0nnf5nzxf19hn3k148bh4m10g78bnnf8h")))
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
    (hash (base32 "16nihn93lf8k22pz8fw0bq1krv2pv114ypb6cxw5vjja8is0v169")))
   (package
     (name "service-manual-frontend")
     (version "release_257")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07wa7cjnqxyw546r5kydby9zfhwf97x62y41gyn7r0gp0j88nlfm")))
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
    (hash (base32 "01xxh3bizz6f2cl9iijmq28hdgnf1glzari341r8mbjzpivlz45j")))
   (package
     (name "service-manual-publisher")
     (version "release_431")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1b86rrw9sz7s99n19inrympqrcjhvz0khc7x6rnnfp752nls2gkh")))
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
    (hash (base32 "1knbnfms1p84hwfyl4ypdw3i5ys6xcxj06bv494bwi1q0qrmqybf")))
   (package
     (name "short-url-manager")
     (version "release_257")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q95m1pwn3099n7w6gsjhfj7rqhydd4g1q682x2kwipialnq17mx")))
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
    (hash (base32 "19i1ydi5qvimir3jk50xajd0pxvnznq1wykalc7nvf7pc4y29ccx"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1145")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09fvfvfnlm85aj6xp2gwsv1bvjq874l0kdsvrjnay025yi7iafyg")))
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
    (hash (base32 "0j4alac2avg31x23bzv97dznzqcqgjrcr5c58zr4jx3hc54kahkl")))
   (package
     (name "smart-answers")
     (version "release_4162")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ibgajzm7xs31qqx0hzk97rsk67sav0gdyp8b3b7328s8qvm9d1q")))
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
    (hash (base32 "0jbgn3gkgm21jrnldrdl2hcpcvzd20varkym57dah3fin30p5dak"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1060")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g0a0smk7v2aakdrjnvspxlf1jqcr5dwz8rkwzm6z5lxy2k2wi8z")))
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
    (hash (base32 "13yzdi2gyik4dy240y838bb444yr13s1m2jv2fg4bas23sbmb9r3")))
   (package
     (name "static")
     (version "release_3016")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y3y7z20sw65qhmb099ya5pyx8riv58h17b96mbg1lchwpy2h6ki")))
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
    (hash (base32 "0nd0mq7mdkiqqbbj6k12wlkzh9xzm9cjmssf58r88l4v53y4c4df")))
   (package
     (name "support")
     (version "release_748")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "098xzs8mf6zwafb4plwkal76pw5qfva3p68is4czl6mjv6l49jm8")))
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
    (hash (base32 "1lp867gdjngnjmpsac3z7dzk40ww2jawc0gix8kbl52zx7dp4r02")))
   (package
     (name "support-api")
     (version "release_236")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17nqn8fm1s4prnq69dbq0pi4fcygkkj8byvx0r15j7iw2l79ikfz")))
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
    (hash (base32 "0k7blad2j003x02z9rwr9ikzfsr017q9rrsplnp7ajakw52s5bwa")))
   (package
     (name "transition")
     (version "release_870")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1img9rjz70gb3l5kxn90y9y6dz8zp0isfll1c4ijlw5m2lqda4vn")))
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
    (hash (base32 "0wbjx65gk32ax7fr99k7ry9gy1h7sh2ybgf2vl4d0jf8c771l6n4")))
   (package
     (name "travel-advice-publisher")
     (version "release_490")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05ldns5sxgacg4xj5c80sqsrvqaa7azm3mkwd0rpy9svc9zz254y")))
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
    (hash (base32 "1l3s9vfg2s4nxg9ywkpcmg01lhn5l9r0rism3mi0c9w505mknbl9")))
   (package
     (name "whitehall")
     (version "release_13860")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xilm9jkp6ddyk6g58bqgr4dbn2k7qjqkrr26n8z0ymypbwcdmbv")))
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
