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
    (hash (base32 "165agxdvqbysd1ayx8j48x8mgmlmlhyym1rf0cnqbp7s14cbpz78")))
   (package
     (name "asset-manager")
     (version "release_491")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03ndbpkb3sp9qn86lm3p75dqi40vga84sqnhpflsa97d2bii3m4r")))
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
    (hash (base32 "0qbm3k1s1bs8vpvh0gmh6nrbh4a5q039jpnypdsqxq3gizxbjpb2")))
   (package
     (name "authenticating-proxy")
     (version "release_199")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x920zn6vjd2idwly866wih9c239fwyfr0qca8rnwzdm98r63bv5")))
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
    (hash (base32 "05x742hw7558l1ngrqdlk3q04x5ybl0p1lrqn71l7d58khvz8a54")))
   (package
     (name "bouncer")
     (version "release_308")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g9f6x4x5r3ki0khn7rlzwdkzzx53gybawmdzzxxr3s1pnvvv319")))
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
    (hash (base32 "0f7lqyp29nvx9n6acwpf4pwlhfjl1dlma8fc68qbyyvfnaw8q9vx")))
   (package
     (name "calculators")
     (version "release_743")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g3rvgxx51rjhasphx54qhfh2ni6arn25pl2ciczfmmc7v1xxvjv")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "064h3mcgz0r7gx4a1r1h0r9w45ilkppzbhpmv8lrkclyf6v3l9ba")))
   (package
     (name "collections")
     (version "release_1525")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1iyf9qlx2w62kd8i9z042kjg5a6b4r0nb79zcpfqj0l00rlfhzl9")))
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
    (hash (base32 "03y9hj5zlazd8p61ylx4lhyi0s7yn8vcpdz8y340gnndvg1jms8h")))
   (package
     (name "collections-publisher")
     (version "release_973")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1grbh74maix7wvl5r5r8pyjy6yrkm5w2v9vfq7y3g0hzyv199qak")))
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
    (hash (base32 "0pmhy70q5drh11if244gvvz0zsy20j30yra3hp9k0g0xq2z9yd0i")))
   (package
     (name "contacts-admin")
     (version "release_741")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jbkqyn7rql9flwaqnkw0j6ggyp2b883npg3azpg0f1gkn0jfm5d")))
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
    (hash (base32 "102m7krg47rhkyp9dcmlq59fp9028jshbrgpyyvdvb0hf8wfg3py")))
   (package
     (name "content-data-admin")
     (version "release_689")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mkhbjf1d7rnywx9xwrqk1r6gafhywwdgzww3nnd3i4n0kn8s4fi")))
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
    (hash (base32 "1qpwr57ny5vbrl5a5nlwsd9j4971whib5a1yyhqqv8g9z3jr3rlb")))
   (package
     (name "content-data-api")
     (version "release_1275")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00vgxlsd3w1p1hi8wn4290bswn89ixh26d7vsnfzx3i31m4pwwap")))
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
    (hash (base32 "0nf2lvw97railars13nyxdy746czc5mwvaf706x3dp3mg758v52m")))
   (package
     (name "content-publisher")
     (version "release_1871")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gs3gh4vxfkq20qd2a3zg1100m81iwm745m26j4mmzj3k92rlg3j")))
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
    (hash (base32 "1a5f88215bwmxh9ncaghyxkjq7par89md9qijs2dd6qjks37f0dr")))
   (package
     (name "content-store")
     (version "release_1035")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fs9k4bvykkignhjs8n3n5bs9s6972j8d2nf16n0ii9cxzm2k51b")))
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
    (hash (base32 "0bbs8b5683nhvg4ava0zapxvfg04738b61wv959qblj837a256zi")))
   (package
     (name "email-alert-api")
     (version "release_1215")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0al6ilzy1vdszlcj1jrfisa8xyykcay3qb4m9bkg7pddpsd6r457")))
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
    (hash (base32 "1cwh5a36dcl71ma2psr3c34h1wa0yp4adaf4wqzr3xhcyna8n4yd")))
   (package
     (name "email-alert-frontend")
     (version "release_629")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c9kikckp8gyvdhx4p85b0w41w1b99vzc425jamvy07ls5lq576c")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0p10za70j19ml8cxv61vcxkkqf818imnvgxlhbz589ppx1ri5p7y")))
   (package
     (name "email-alert-service")
     (version "release_356")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g36idib61nay7va3f155ng6kf8dpmjzshnkvs87h32mq74xmzv0")))
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
    (hash (base32 "1cvb4fijiik0zb4z4l1baw6dc4ycpqixpvg33zf5i8hv6qb5zaw0")))
   (package
     (name "feedback")
     (version "release_880")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v765cz8z3zp3x0wsch2xbln04nf0nl6hk7dzah79fiqd4swjbb1")))
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
    (hash (base32 "024rrg9yw4zhjz0f59wv9hnsvmhm1xsqcyqz2s4l7i24rczdnrn5")))
   (package
     (name "finder-frontend")
     (version "release_1851")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qiyzlgr7s2k4ib18qgrsm9c9x29361jf38wgcllq8ngz04v24qy")))
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
    (hash (base32 "0g2is2zcmp2fh877nz0lyqp9i9s9f4il5ncnvfpaxnyw75lcn70l")))
   (package
     (name "frontend")
     (version "release_3614")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wj1gji0nz6x71rrpi7364khhzg8py8h23kmx519grsycjm3zh6n")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j23ysz7fkvgh41ach2hq89cswp58swangfaiwr6cqj3dvrawp56")))
   (package
     (name "government-frontend")
     (version "release_1493")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fb9dabjglg7a1xvs7qkn7i2bli4s0vmhn1ka7mzxcl1ij5f7h6q")))
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
    (version "release_966")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0fnvxq7ksyzxnx3lmc7p3pxvkalzzviphbricdiqr6nb02hz064q")))
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
    (hash (base32 "0lni1syd04chbqanlrgvs6x0qba2j5i9ydzwjjiwfrcq3gx84wlx")))
   (package
     (name "hmrc-manuals-api")
     (version "release_462")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1liyzi4q1ijlqlambpdhjhq6njpn7plfh78k8wp8x2ba0isys4nb")))
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
    (hash (base32 "1bc4q9ry0pws0z9a4dkl56rbnsfb8sv1pfj4mrbjm65cg63l285a")))
   (package
     (name "imminence")
     (version "release_652")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07rbj7anqvbk9bbbxz52bay4zayj18xyhlgl3phy8qybcxkcr1rw")))
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
    (hash (base32 "06z5gnqykncq5g1aa4zl2lnla5xd4a8a2s3jg22yzp1zp0g71cca")))
   (package
     (name "info-frontend")
     (version "release_541")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1a1lyhkih63h1y4qp0j8zk8aghyzr4f0bqziq57608lgapd285y7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gi0gp8p15xlk8x70v14gm0civx199v9xblrdqaibrv6hrl7cimk")))
   (package
     (name "licence-finder")
     (version "release_793")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lkml0jh5gqc0d47jw3qvrln9nmrrkbzwhaqm1cyi0mjjg6jc73h")))
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
    (hash (base32 "103h8virhakq36x9j4gj2w7bnskcnkk136k7z90kw867cgk4mkl0")))
   (package
     (name "link-checker-api")
     (version "release_328")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ljc5id1iz2qwr8fryy13w0g2p4z99jbzg05p5f7qm3cvdpszdyk")))
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
    (hash (base32 "02nq392hvhjy1cnfdwpzvcxzz03ifqv83iyfsp9axl036gwk04w8")))
   (package
     (name "local-links-manager")
     (version "release_577")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y7hxma4p6n2damzfyh0v6vd52jhpvarrxz05z07hf5na3a76iks")))
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
    (hash (base32 "0cry01xzy0pa7k7fbmcasar0bjc7ac1y6vja9xszwbw32ix1lpsw")))
   (package
     (name "manuals-frontend")
     (version "release_741")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1znikxvzc480g3m0s8cmi34cg92yxsr1z0n4d8m19zcl31p8cxvk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "18v70pk8w1rksyn2pj2cfzg40h3zhqi335k4lsb9j0jgbcwzagcz")))
   (package
     (name "manuals-publisher")
     (version "release_1340")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "023vlag75m0a7bsff360x0yy9bj52wvpgzhmzw8cn23mfdsc5vdn")))
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
    (hash (base32 "0k8g86max5ynashqjw0ihgz5dfyc176y8ygs7vxjd0wwkim5vkxm")))
   (package
     (name "maslow")
     (version "release_560")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07r5pxl0b97mg5c2mqa8cbc9nbjkanq84rsa3czr9xdf7jijiqkj")))
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
    (hash (base32 "1xrims79js21ama9i6pfxkrw6iybpdhgridvf1ljx1qbi3p3qja1")))
   (package
     (name "publisher")
     (version "release_2332")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h8fhgkhrw85bvsarja44hm0c676qmzi2x1smam61qhm34h6cfih")))
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
    (hash (base32 "12di3cjwh7vznrbxpdwkf2s13yp5zncs02q0wk8l4l18ywr8y2sd")))
   (package
     (name "publishing-api")
     (version "release_1706")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1lgsgw59pjjiplfazxf9vnq0xl6nhm4h9azgiagm5am4pjschp1b")))
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
    (hash (base32 "0dcimpsfh5hcblp4q8yj20p76ndb1rnvdbpyxfq786nfr6aif5q1")))
   (package
     (name "release")
     (version "release_652")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jizj3p4vml8zrwgxq0dbw3j1akkga28vv58vsirmxmnidilv2ir")))
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
    (hash (base32 "06v75fd069v7wca2yg4km2dv9nyvq345n1n7rb6fnr88avycvi86")))
   (package
     (name "router-api")
     (version "release_313")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vgzrip9k5b3ka8ny0xjrsj9pm1fz8x3917f8cykj7vgcdkxbz8h")))
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
    (hash (base32 "04pnflcxxqcm18hnwxbdrf0fy3246lppahi6adr0598zags4icxi")))
   (package
     (name "search-admin")
     (version "release_413")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1shffi0fqg1lammq86xrw3zhxf7yyfqfxs91kz5bnz60h1ksrscq")))
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
    (hash (base32 "0ix69p4xkbbwpzw8m1qm1zlqh1qs1sazinb745iyc8c4xcmsl3a2")))
   (package
     (name "search-api")
     (version "release_2581")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mkva4wx5v8dp7qxwvldyn2v0gkvfa5jb2yab0c0gplf1yik7rpr")))
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
    (hash (base32 "1mhsvwk210i890ccr9nvddahgmd16qjx45l7dgpk6k215yjn6yw2")))
   (package
     (name "service-manual-frontend")
     (version "release_567")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rycynhpxxd17hynllk6dy21qp4bdigpv698qj8h8a85d13yqnqb")))
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
    (hash (base32 "1jw7qhyvbdq8m2s969h8alcm61h7751bikhxnjdb9j2syxamrhpk")))
   (package
     (name "service-manual-publisher")
     (version "release_653")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00pvqx3sabs6r3d2z6wh6kmcm7gj2gxc3648l8c94j3if7widqrk")))
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
    (hash (base32 "1dblpkdh5zahyckf19m969sk429vbs667ss29b6qsqjj7wfmnn76")))
   (package
     (name "short-url-manager")
     (version "release_462")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "184c3avbhzy52ssn5hpkmxndb6dsnj5w5j0rnpbb5cljaia51gdp")))
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
    (hash (base32 "1199v7wiiwsc1c5xxmjhzdq2pgwlla497gjjsqcjw601pvhqzn1v")))
   (package
     (name "signon")
     (version "release_1500")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0niral1jbhifnv45nr2whkpvxzp6q90wvvxizrcc71ll49gic02z")))
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
    (hash (base32 "1dcjnbqy2lww7cb58wayrl2753js5jwj2a26idhdl9w0grzyyxb8")))
   (package
     (name "smart-answers")
     (version "release_4888")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g0ivlxb1hd5rp9xl5hmzrdbxlnv0b5fw2i9gx0yczy3svmd999j")))
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
    (hash (base32 "10r3an8c3p6xk44rwqg1rmal8skvng4121mg9shj65ppadfs5z01"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1356")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vmww1s4mhilzacnhm437k5qm9fj1xpb31vsvx76v3bymjwm7bkf")))
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
    (hash (base32 "093zjn92d1sd2kmgljk74rph7yd6lgnwqwc2yz5fmfisshq94nc3")))
   (package
     (name "static")
     (version "release_3559")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16gmynpgi88azvy00rz5yky1g3hql6gz37hb4c4cm09gmcdn538i")))
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
    (hash (base32 "1xh0s30dq82apkh52dgh466wmvnn651wmx07kf5i8cd7v22mvis1")))
   (package
     (name "support")
     (version "release_1002")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bffkv8vxjz1lc84bgd4414m8nsvf1ldl9gf75z0kymg8vrpbjkh")))
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
    (hash (base32 "1j2dad95m3bcqyyx3w33jpa41j5i940jc2rq8dl6sw572yn1x47k")))
   (package
     (name "support-api")
     (version "release_434")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rn81i1i3pj5j3j9z9n9lqw210c7jr5zj9wrh139bngajxhlxi0h")))
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
    (hash (base32 "0ngfclygqv1iqy6wwyyp4hm8hpszc5wc0328n0hfv3pdqc5j09i1")))
   (package
     (name "transition")
     (version "release_1131")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qsq4cb07s3d2q02qry31wlsd76x14ykml99cnag5allx04590f1")))
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
    (hash (base32 "19cgxzk72d525lljx6dvnzny2mkvc4i9rh4kfb5pgihws0q6zfhk")))
   (package
     (name "travel-advice-publisher")
     (version "release_810")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xhnax93kqsljbhw9hr3r37mws0vk9iwp3z8lcwilczzvynlpv81")))
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
    (hash (base32 "09aa5jvix38d8svg40f20kcm3qxq4sa2iz9dfa6smpv0z3cy4m55")))
   (package
     (name "whitehall")
     (version "release_14852")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0si01ksh87b7iknf0vjakpa6ivhvc5m2qxkkfb52gx69m7djwjd8")))
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
