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
    (hash (base32 "04i3vc89xsv7hjs3pzixp5v9g6772qp0wzcbcsdl60hwfa4348x4")))
   (package
     (name "asset-manager")
     (version "release_498")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10q9lsmfzf5wf0866510rrfny87h758v63dl6w5arqz6002y98lr")))
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
    (hash (base32 "13wf5djys2i8bb70d1c5wz926781vs2rndlyaw97ypggnq2z8ddk")))
   (package
     (name "authenticating-proxy")
     (version "release_204")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1m2d73i484kbdcjkp89jcixrpmcr6cr0rmsl53plrpqd8yykx3ln")))
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
    (hash (base32 "0k7k6zi2c2q5p8pvbad2nnspgmvdsjks4ylydd6m1x78a7ib4mks")))
   (package
     (name "bouncer")
     (version "release_310")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1f8jncr88xb7n4558qzp2rvi8g89wl1i15mvl4gaf5zs8c7nql2l")))
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
    (hash (base32 "1z933dpsa3gf74y80vjz4g04nc6hkg33p7sk2g0bx1dg8dk44935")))
   (package
     (name "calculators")
     (version "release_757")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00583hi1whv1frmbp7hpj57m9da3cx4k6pp37hlyc2yg29hbyb3v")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "12mk0cii42dzcs7g8qrw9ky14f0ca8mqbxrribzsg28ml4yg918q")))
   (package
     (name "collections")
     (version "release_1578")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qq2ii8k3xq2wnn4vnyd3ywg83dw82wd0ykr9j28xb3498b8avkn")))
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
    (hash (base32 "18g4fan5w007yhki8s2f2hvr2vl4qd7fmq1mfvzgdza5rjysjvdd")))
   (package
     (name "collections-publisher")
     (version "release_1000")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zy17hcrf2v7viw31swrlzfx0db4i1qiqmsj70izmgmkr1j8w6ra")))
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
    (hash (base32 "0dzn8q12jch9xzdv6040mjjxbnr7ym7mvp09qly486791y89xmlq")))
   (package
     (name "contacts-admin")
     (version "release_752")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cw2ijln85zch91q04yk9736w3ry78lykvrcnb8ja5lz9pyr9554")))
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
    (hash (base32 "0f0pxszpmmwrrp966zk881ym6qzqg8n1n5k8jjd2c9xskyvnwm65")))
   (package
     (name "content-data-admin")
     (version "release_698")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cfxkhm43x8kz3n3phn3406q37p5fsagr5grfa328ai051s5cm7f")))
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
    (hash (base32 "1b71vwbqcha05sxxfd777rdhbwfcgk7qghzxsv3qj0qa8f3g1333")))
   (package
     (name "content-data-api")
     (version "release_1298")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p2cb14idcabznnd0p2xwyx1zwv1pbava60lnn2r13pvg1fnr0mz")))
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
    (hash (base32 "0lbmhxqwlh1apyjkk1prkm5yy5dvwigi61zsmhyx83lk1g1lsi9d")))
   (package
     (name "content-publisher")
     (version "release_1900")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0903q8f3g1gm58s5iarff469li1cgfqzdqgfryn4xza41pckcmzz")))
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
    (hash (base32 "17ndz0rairw7gxfk9dhlkpbna96m73y4mx256yc334myl3xnmqy1")))
   (package
     (name "content-store")
     (version "release_1037")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rm7g0spw1syzvlcwixqwlaa5vxn5m97pscz6smcm27fb430fr8x")))
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
    (hash (base32 "1r7i1jijyxh1dsy4g6kclx1p8mndlkxaflq3d67drvwjqflv5096")))
   (package
     (name "content-tagger")
     (version "release_1129")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09b412647j5piwkzw4fb0xhb9df2c63c5gppm5whb4l7lshjv0vl")))
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
    (hash (base32 "10fh65k5zbqrjvq5jcjfn51z8f9b0znm44bppabqsr697gr3sf15")))
   (package
     (name "email-alert-api")
     (version "release_1274")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07w5nxmfb613gq1r5789s34p4b131fipdmzr843c1idazn9n6lq1")))
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
    (hash (base32 "0100qwwkyjh6cf18aw7dy6f5162a8m7qiamx5gqcfyw5a9n393kh")))
   (package
     (name "email-alert-frontend")
     (version "release_652")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08nw2sqh0dr7ji9qj8hxkggapmbdnrp4yxg36ks843vhxcz5b5hb")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0wn18dw5aqs9x5r6kfhyj3x6c83hby0wndn04ilkklkam9b4h61v")))
   (package
     (name "email-alert-service")
     (version "release_362")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gw3mdjdc88ff4c1r4mb1j5szc3hs17nd9gvvm9yg5r4h6iclzr8")))
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
    (hash (base32 "0m63v086bgyf43pg8w9b3dff5q9glng6nasbfy251x7akafndnzb")))
   (package
     (name "feedback")
     (version "release_903")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bjz4brab1cfphp1hf3z5ns2flry7yjafzd83pahcrx2p89gv35r")))
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
    (hash (base32 "0lsajpqygfrz4vd7pq7dcdisf2lq5v7xjg67wrqb46v5fa05d8x9")))
   (package
     (name "finder-frontend")
     (version "release_1881")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rn2z79ay7qq77kvzszvwshx2wl50hl9vl5zc71c62kdllm65sri")))
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
    (hash (base32 "0lcp0v4zhzqkw0lfdykzr0jjc17k3mzd3msywgjyw2qnf52jyaqg")))
   (package
     (name "frontend")
     (version "release_3643")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "099v24ays92k2jzwxcbl5sxs3cgm5q9kmxjn2vwic7y7wcid0r90")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b36zzymx2lbjiw5hq640nf3l2q9hy63pqjz7rga4q5cb6ni1csr")))
   (package
     (name "government-frontend")
     (version "release_1519")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1chyi1wb1h6a2i21kc3vkg5vlv9wjl6p7nyhdlv1krrpkiwbjb01")))
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
    (version "release_969")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1l7bw6vmd2mhszxxh5d54v724sbvqzy920cmlqgp5jjdl7vsm17d")))
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
    (hash (base32 "16rcxaf8asxff4wjxixz77iqmsxw78y9mnnsd8kj8mln1jd1h9wd")))
   (package
     (name "hmrc-manuals-api")
     (version "release_471")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pskqicvs881hcszzkzrg75kbpnybnpm3n902xxasnib5d31bj64")))
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
    (hash (base32 "1bfca6s4h36ik4v8pv2pw5ygqx5iqlwa1dciwh14qy8jy9rqdrgk")))
   (package
     (name "imminence")
     (version "release_661")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1znav7wah9zi84xj8pvnmpfbsinvry9w5ln2598sdpk198h8gli2")))
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
    (hash (base32 "0sy21dvb5bp5mnaq1nrvg2fx0axca17fmfj6jiakillcy8ak550x")))
   (package
     (name "info-frontend")
     (version "release_558")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zgl0c07j10lhb1ggkjzf0746kd2rc8702a5fcc0936hza06w0yc")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "10gzx7iv8myp57a0jpq1ji7h5qllg7x7z20d5wk2wqwfrb14ra8q")))
   (package
     (name "licence-finder")
     (version "release_811")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09pwrc7hhfnh0wsiw8w0mzbcdb50dchyb75x2klc7wdrpklp5n21")))
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
    (hash (base32 "145gr9lxrjq7g26v5g3sj3ysf51smik4dadv6kvka80syjb09ngi")))
   (package
     (name "link-checker-api")
     (version "release_333")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ajrl6a74ws96k072sdvbj24pap0nrxmgw1xmzl051c8kpkh8iyx")))
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
    (hash (base32 "14b5mkkilzirz3vff0gm3bk8i2j14h2xj7mfvk8zdwglxf684xzr")))
   (package
     (name "local-links-manager")
     (version "release_587")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ggc4rdg99z8mywv07hd9sq688z6fcb75ypvr35qny6yy2awp86v")))
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
    (hash (base32 "05jdjqxrym7khwzh2d0xcay79rx81cd6b7lpqwzq64lrygzrcyyj")))
   (package
     (name "manuals-frontend")
     (version "release_760")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "057rgh9i4gglkwwznvjbnhwc6irr8ngd3pb2qkq8dcvsv2kvvjbj")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0i1vyl0iyns1ag9mm6v3pf85cjkcasbv81i7wks0cfw9krvwanp2")))
   (package
     (name "manuals-publisher")
     (version "release_1355")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0nhbahh41zi59fx60j48xn2gg8zr075rin09q8y7nmfr9jwyh52b")))
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
    (hash (base32 "1i1papql2cv9yajrgg7irfcyavin0y4j7cc11vwk5gmbz9mrh0b0")))
   (package
     (name "maslow")
     (version "release_568")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pgnir5br6ylliagsn2c4jywrz6qrad460pm015yd6wi0a4i25ar")))
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
    (hash (base32 "0zqrji9zl242ndc1m9qhm4jy8ljpkkym5ppgrg64k958nj8dp89q")))
   (package
     (name "publisher")
     (version "release_2354")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r36m02nbp1jwb89c5x840fkbhcm93v12dll2svlqjs946qkhw8d")))
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
    (hash (base32 "05936gyz2qgrk1qr7apd2i01ird6n2nwzcf0y5s12f3098lwi76y")))
   (package
     (name "publishing-api")
     (version "release_1720")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1mwzsxx7bq1qk61bfwxx358h33v49fx3ll2bvvcf104wnpqm5gyl")))
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
    (hash (base32 "1m2c3xfxiggawv8anzxyg208h6s9qxrc4385a4ayr2h6fsaq1wj6")))
   (package
     (name "release")
     (version "release_673")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vpjpwycvh9vq0pk3jw6vjvqr1z70hxg3h8jn7ir1a99g904ijgi")))
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
    (version "release_209")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1nlx16w6fxg8rbz9f6xr9vrc419nsl5bs6r4r207s6nyr7aia222")))
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
    (hash (base32 "1lwzi9ql4nm0c5mnllgv7mnzxbla49pfzd0rn9mzlxp30366b5j7")))
   (package
     (name "router-api")
     (version "release_320")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0srips5df8sfz4zjm1lnfqf7i0mqh828wnz8q6zcpsganr2kc1b8")))
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
    (hash (base32 "1l7k11zbmk731gw57mv1xrphr934cfp7pcfmhav0y8mn78595yvn")))
   (package
     (name "search-admin")
     (version "release_426")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j8izx4isiq967sad0wirqq3dl0faw59yxqh4a73d12h7x13v9f7")))
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
    (hash (base32 "0z3gjc3mx059km9dhbmdij6iikbhrg7d9594jl9flzvrihp9mwjp")))
   (package
     (name "search-api")
     (version "release_2596")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jvdgr6p55saj1ylyvf1crd4k8q45r1pk1p26ndmwqp3km0skgbj")))
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
    (hash (base32 "03aybvmkyq4lyyr2hfk1p58c5f8dydy5xh3hi7a4cfm4yvb9pbl3")))
   (package
     (name "service-manual-frontend")
     (version "release_581")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h59pk354hp9vyixr2qvhj41nkmf4vgn2rn876p84rbz37spxbkp")))
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
    (hash (base32 "0hdbmqk7s09q6vqk4ar6s5r8mxq5x42a40yrsqvy6s9xcwxgk8w5")))
   (package
     (name "service-manual-publisher")
     (version "release_665")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0iwk0pnqb3k9545sfb12qkdrr5588sy0jnxf4wj49s2dg83qxv5f")))
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
    (hash (base32 "1kd1hir1yrrynb8yx2xhrfzcl7pqk34j7f29g47bzcsw7l54zxa7")))
   (package
     (name "short-url-manager")
     (version "release_472")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01r5wpqgsckivbfvyccz0hjyj65gmy2ls32ln6dgl0aqa7kxyzg3")))
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
    (hash (base32 "15gmh3cyzjvfyi9gxy0ppm4k84qlmibcdy1gg4xh6z29ldqrrlav")))
   (package
     (name "signon")
     (version "release_1522")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z18kj0jcd63jb7w6sxk29fw2zw8alqca3g3ji2ximddrzg2wdhg")))
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
    (hash (base32 "0kg4hp6bs85ikp937z9b4pdahmb0qyw5k9a2r4jn5r9y451gls9i")))
   (package
     (name "smart-answers")
     (version "release_5011")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fbqgl0z75k5im9ipgf1xia61c3j0n18p31bsnby28gqz5npaz59")))
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
    (hash (base32 "1l0p6vz0jdvjfg1hqlzi1vpcazw7g36alks88xnfas7bmpq1g880"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1380")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c2y7h3w7pnzlvrhb60x8bdbjlc2a8k0jlxm805lpk8b6nbwgpls")))
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
    (hash (base32 "0i6911vma5pxpm195mn9x7w42i04c6v1rxg8gng42vgm46lnv2js")))
   (package
     (name "static")
     (version "release_3577")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10pfxxw5r0f0d9srcvlz37nf7cm0h7126if7aam4d98w2fwkxqs4")))
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
    (hash (base32 "1jj3va5pk7kkf83z3i38lcvjssm007hpqiacbqxnncfqbv2k1kg0")))
   (package
     (name "support")
     (version "release_1013")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0yzmmzpzw16iakaqzwwccw4akaac7fxvkvccaxjfnbbk99444si7")))
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
    (hash (base32 "13x6d00dfszxmyxzspr4x1lsmafimgpas4957mypckfmxjmg5ia1")))
   (package
     (name "support-api")
     (version "release_441")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bm7vnm949fqldravdbv5d99ms4almfyn1s633iwx5glxj3b4nld")))
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
    (hash (base32 "013ygq590lbmh76xxlkrhw77s4plnqsp2zg97akvczsqfslypk1i")))
   (package
     (name "transition")
     (version "release_1141")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "170mh8q20pv3chybi7af2wkb4s8xp530lcvjkv993v6hqi3s3va2")))
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
    (hash (base32 "11g5bm2inybnbzcbfc2z7wlyy0fs4f112jc51w80vhnfjg4p8m52")))
   (package
     (name "travel-advice-publisher")
     (version "release_839")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06cmcada7qk57vib2wdm2zgcfy6j5rxgmqkgcrbpccpxl9v4sl4m")))
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
    (hash (base32 "025p3744r06qlnmvcdwqrb486iw0izd8zv5sbm3lydg4g1k87x0m")))
   (package
     (name "whitehall")
     (version "release_14900")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z3wfzbz8gnwzrhpig79vfpldzfi1sg4hrlnlfbrr0934rswaqns")))
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
