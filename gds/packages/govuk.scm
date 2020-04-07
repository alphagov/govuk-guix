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
    (hash (base32 "1jk95i4n3zbhgfajcvzx03r7df65bg5s5rzdn2iwb3xnval8iqhk")))
   (package
     (name "asset-manager")
     (version "release_457")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0scfm4f05xgcprsavh5w9skax0sg61lhysf1p50xd74w15k0csja")))
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
    (hash (base32 "1jg94m1nzpaglrrfbb9kxdvnnn3w4912004a8jrnjwmc0kk94w0w")))
   (package
     (name "authenticating-proxy")
     (version "release_189")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bynwpsv6b24lxdp34iv9add8cc3g58i8vn7pm76frckmqyyd53s")))
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
    (hash (base32 "1zskjp81b42jamdcr9y61dsjvvxwiffcbssrcf8sjnlbnlgxfjqc")))
   (package
     (name "bouncer")
     (version "release_300")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wmdn9h13ml9xgfzbyyr9swsb8jq0px4x3k3l03zq67p7lv26wpa")))
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
    (hash (base32 "00642sj7b3s1vma8d2gl530pb0vcyk8mkirb45wrzn4xzg0ncpg5")))
   (package
     (name "calculators")
     (version "release_682")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01pjavkr7nzlwi0adgia2z47h2xiry0rfzq54c60h42wmxsmladw")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1h0xbblsgf0yj5jd6knsl81jhvhmgwx740dclsz05dqv9rr6iinn")))
   (package
     (name "calendars")
     (version "release_908")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1has1iz6492jawf18mwiihgmqrm9ksyiny23gaya6z273xz9k3ll")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b65pvzpihip80pwq9zja78ca6hxry5zwsfb7hbb38f49lwmz2mv")))
   (package
     (name "collections")
     (version "release_1376")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cv2bk7yvkival79gyi2hy98ljnslz1zqnbi1ss9mqyrpfsgfv91")))
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
    (hash (base32 "0c7h7dn9c6jicg26lzk6db8c4xmbxmv30s96gfmgy6hp4329nyin")))
   (package
     (name "collections-publisher")
     (version "release_887")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1c6qdcdzlx6blq28c6h5r709glf7nvlvhnvz5dwsvxpvy5z1fqcr")))
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
    (hash (base32 "1jsmxvassiy0rkjmksjpbyigsfrnyiigyxa055jqn6xdnmqimyb4")))
   (package
     (name "contacts-admin")
     (version "release_712")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1nr66nq1wmdinwf8f1h9gd79g6p2fcpqc43jbs87biy9kbcc1k33")))
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
    (hash (base32 "07b5n9l681666ag14gs10xwpz4dw7zxlflh6ip588pnwdjgdzk49")))
   (package
     (name "content-data-admin")
     (version "release_659")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06srbgf7k6vpc8y9ah0rkf3hsl2q0v4nbryzqfa8aw5xmnsp634b")))
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
    (hash (base32 "1zz5j59kyqk83j80jn47wmc06pk58q4xlp8wpd3qd31w7y2wz7r2")))
   (package
     (name "content-data-api")
     (version "release_1238")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1j97fjwjjp7zn943znycgfk0bfy8h2jg9rq1d4xskb9rwfazws18")))
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
    (hash (base32 "026w05j1njl9y80xxvkip7bv8mmwy2z8m0jz6nvv3k49fm1scwg9")))
   (package
     (name "content-publisher")
     (version "release_1765")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ybnxzwkivi84ziz1m1xdyg7jykyzy82s9l7fnw2yy93y9pdp8zp")))
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
    (hash (base32 "1p1k7h60g5s3p9xcy9xk6mzjahry0hrw320dv51r4dzkryqdi65y")))
   (package
     (name "content-store")
     (version "release_1000")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0d2xy0f92ya4lam3kslfd75galb844vyi7a0aln8mfgyzxiz1nac")))
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
    (hash (base32 "1kib0lj9fiqnnfb3pwqs15jmsw3nh8p2w895jn2gjh5rsimv499a")))
   (package
     (name "content-tagger")
     (version "release_1099")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1k6p5i3qnz38w2374m59r1m1z7082mxxdw17wcrf05fgcd1wk1xn")))
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
    (hash (base32 "18mdmj7wvx86y04bk50mj1439wrwh53l5fkwvxvvpkn2dmr49mch")))
   (package
     (name "email-alert-api")
     (version "release_1108")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18sz29xq73x49dp9vcg6gdb7fqm0kc8w0sm2awrfqg8fa41xyw8x")))
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
    (hash (base32 "1xzkmaayx69filpmyw4mvk2c8s7qc3sshy0imirxfncx74vkj6pd")))
   (package
     (name "email-alert-frontend")
     (version "release_552")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pkw4scr6rh11wxc4c822gvbv30iy1s1jpc3n18hpdfnpwsjf0hi")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "09i6y1z1ij26c9igzy53bkb4hfwa1bxi4dkhxxpk5pyqq0pnld23")))
   (package
     (name "email-alert-service")
     (version "release_326")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11czckbp137is8c28ahmqvjq29i4g4mz04mz94npy9xjkzp6z4l9")))
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
    (hash (base32 "18w0964dc1k9bbi2jdq15ipc6ay91fmqkqaa600vnn914ghhwzvf")))
   (package
     (name "feedback")
     (version "release_835")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17zmjkmndnza79fq1i65ya939vc7yd5qr85divnbg6avm2fxpmxb")))
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
    (hash (base32 "0km5cxyx1i6cmkyvnhpx6518fp2li5yh77lwlyscr9312z6akfwy")))
   (package
     (name "finder-frontend")
     (version "release_1757")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12lclvgy5zdflwwbbqs9y9vilwdk4zz2if2l0fn0ianv4032gw91")))
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
    (hash (base32 "12sycfrk7jd1pkgbs2g8hk1xp888q0klfqc6aqn4sgxs1n8m2716")))
   (package
     (name "frontend")
     (version "release_3515")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12dmkasx64jpkhbkgwpzxp7nq7ziqbazvxqaqpgy1gl9al3vf72f")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1z1w4g3sl5c5lln4s1bq2m3adr09zls4xim9lr9dall1nmcxi0v0")))
   (package
     (name "government-frontend")
     (version "release_1408")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1aak3q442mz1rjql8mmjamknhcnz9w1vdy26zw5slf3vdkxlp6h1")))
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
    (version "release_950")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0ijb04xhfan6fj7vjnlc88v344sh3cag5dd1l91b2h8rcarw7bzd")))
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
       ("guile" ,guile-2.2)
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
    (hash (base32 "1sp4jmy4lmz79rn209c835li5gnlmv9hqaskh4y8z88ab41r7hxc")))
   (package
     (name "hmrc-manuals-api")
     (version "release_437")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rgw2xqn7yz7c0zd062j373m40n0c9047srfc92nnv039bgjfqvr")))
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
    (hash (base32 "04n9a006k0w9fafqjfiqlcf5xwcaflvrlfv325i289d7ibnv8va8")))
   (package
     (name "imminence")
     (version "release_627")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pfhqga43mgw7f8gq4v3dpqfvc1s0gpv5gcrfwrywn2f0126v12d")))
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
    (hash (base32 "1drnmfqp3m6n30ghgcwrhn77aw4v65840r8fprh2pf4jx73g62lb")))
   (package
     (name "info-frontend")
     (version "release_499")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "182rn0xsjdz8ywqm3m2b0ylmhbvxaqbsv7q3ajp709zniw6q2yl1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1hhs1jih2yl6szn597q70j2n5n8jrki5w0aqkj5was2k8w8p04jx")))
   (package
     (name "licence-finder")
     (version "release_733")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rb26dla244q4sspaqwcjjay538lirpb2mg6id99mw5yk0gssg14")))
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
    (hash (base32 "108si1y59gl1j08fyfzywhzsqdy8sjlg3ln0xwm6k0hg5wi3gzpf")))
   (package
     (name "link-checker-api")
     (version "release_297")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01xsxmnhdpc8a4zkccylhyip8xz6dx5i3pxffck4bz0w4snnvpj8")))
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
    (hash (base32 "0lkgk38r8j6ispcka36q762mr4gg31vdyppdpzvypm6nacjqba8j")))
   (package
     (name "local-links-manager")
     (version "release_520")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q9q2np975gmvcbg6dpi154mz4ylxn871pda970c5bfqdaw7a8j9")))
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
    (hash (base32 "1swvzbcxxk0fx2vqlk8v10qbq50napan3crjdxj9dwjx0709fc1b")))
   (package
     (name "manuals-frontend")
     (version "release_680")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00894f9dfbxzyx2db3068dcg7c5ypyx7crlnl9qm9khyiz69dy8r")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "09dc8xmg2qq9rrlkda6gnxan8mwrl4vc5wlsbr3lsbyjhi2lwn0x")))
   (package
     (name "manuals-publisher")
     (version "release_1308")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h8zs6v6599aj78l4zh6dyi0ljgd5pz74xsxxs47hd51mjjv1n1h")))
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
    (hash (base32 "0n8gn5rx6bmg7gazpcwf60lnlyqrii154wyr5qv17ql7gn7h8pgz")))
   (package
     (name "maslow")
     (version "release_527")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fkzvs1bdh9q10sd588lkfs57hm1sdsa684877hfc8h8jldmlnad")))
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
             ((@@ (guix build ruby-build-system) wrap-ruby-program)
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
    (hash (base32 "1n6jv7bwisapi92dc853jw2crqh227dm6bvf6kpxvglf0p8242ks")))
   (package
     (name "publisher")
     (version "release_2273")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1952508vnviqsbdx5fmh004gph25vs0v1d3b0xv35vmw0llb4dnb")))
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
    (hash (base32 "176grj4hwm23q1pa3f50xmjh52yam00cacgla7iyzaks3y2qpsdy")))
   (package
     (name "publishing-api")
     (version "release_1639")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0d78cx1ijv0yg2dasvqz2p82w5byc4pkmbwhkxfl8iyh64jn6dk2")))
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
    (hash (base32 "0njx2pd2zcrsgv2ds7psbdyipblwrdr42a9pzcchm5i04vdbzfsq")))
   (package
     (name "release")
     (version "release_607")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "091zy6dwwz4yks8ir1mamcc311cyrpxkrhl2y4iv4pi0kmhqj8mv")))
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
    (version "release_206")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0r8zqracbipgydgnqdpkqck2w1drgwq10g79qqpyjdxlqhwg8aab")))
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
    (hash (base32 "01r840qj245panv952pnhj8ryq9z592c7sigljspppk1nl5lpc4x")))
   (package
     (name "router-api")
     (version "release_286")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1srllzxlaalp8x9bq0ylr1q433r5fd16ki0pcbfb9an2a4ivzms6")))
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
    (hash (base32 "0f3x9488qnn6vygz3idk51bvcljc7p8578p5m272iqhdpgbx8145")))
   (package
     (name "search-admin")
     (version "release_373")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13p8bn51jfmdlpvbdwh8w1fdwpw5x4ar303glch0qadrnrj0lkdr")))
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
    (hash (base32 "1h4d1a1w7zf08j5c421qxa5zzn71pw0704rcqdnp5y1f0cl7nym7")))
   (package
     (name "search-api")
     (version "release_2509")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02j793zmsvdafvzi2f9hggsd75111a333pgqf2d60rqmk735bwni")))
     (build-system rails-build-system)
     (arguments '(#:precompile-rails-assets? #f))
     (synopsis "Search API for GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/search-api"))
   #:extra-inputs (list libffi)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "11ljjyvi50dpd55j2zymcbw6siiy7h12wvwm6zdspnlbcr9xmjdv")))
   (package
     (name "service-manual-frontend")
     (version "release_505")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16rq6km5q9dsafcnjhqksv9as4pgj7x0pw5m8y9b72kc161wn53x")))
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
    (hash (base32 "0j341286igpkcl9ncnjwilcxgl3rw8m0spyr5mgf2m55fir6cnam")))
   (package
     (name "service-manual-publisher")
     (version "release_616")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bspygqhz190pa43rcgl4wb9dyf7a88nffmg653xjx93fk977x68")))
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
    (hash (base32 "1526y4r8cscninscyj5333l4qxvpx76cs3bj148cn8man6m3p2bg")))
   (package
     (name "short-url-manager")
     (version "release_428")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18jzdkl6x0wmiz9nhbq2awsgxmkrg3jrc7vxqlhfxknfbymng64h")))
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
    (hash (base32 "1z51gpp4xrl83fl2b9fgapsgxqmrrxwb5gm14wyy14k37bz2p29c")))
   (package
     (name "signon")
     (version "release_1460")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p24sdz1d0jv8llnjw54krdp4bkmaarcdggfbmj83gjd50h2arj4")))
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
    (hash (base32 "0h8p5pjcsk78ym6nlghd7yym41bbssv3h215mlfj73ym1945gr1d")))
   (package
     (name "smart-answers")
     (version "release_4661")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00qg4b9xip0i4q74bcly0xik1x0mnl4ayyb80qwj2z15rv81yfn4")))
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
    (hash (base32 "16k6v7jklnilabzxzqsjv6fi72h34l3ndpj97phqc7q5076hxw34"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1319")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0asa7d8rhzn67yz10ph1r84flqja03y8qd5br86dacbdab32jfbd")))
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
    (hash (base32 "0ifzyjlkdmbcyjz0gfhvf7n6kffc6fs459qpg6hm8qwl5kv8s1j8")))
   (package
     (name "static")
     (version "release_3465")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xkmpa49f2yw6midcbqd2hvmzsizav1xpasmnkakxcsl4l95nbj7")))
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
    (hash (base32 "1h5r2z0z806vzsd4dmsb11mw3gspwg6a00aidz3hxk3vjyg7xqzw")))
   (package
     (name "support")
     (version "release_972")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "020p3wpw1vk5xbyv9yls6ks0f02dzzr5jcni95lgrn794b1mvd5y")))
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
    (hash (base32 "0dpmxly8n0i0fqns890c52bhsn70appvynbzvq4dvhwl81v90l7v")))
   (package
     (name "support-api")
     (version "release_405")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hfzfw16r47n3p3zs4q35pywxhfgxspjd1c2qzxd59m3ngj4v6lj")))
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
    (hash (base32 "08fgabpm53ciwifyz509v4hywc52yy1xy2dasx18lw2220nncqd2")))
   (package
     (name "transition")
     (version "release_1087")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sxmynn7am3bdxj99nv6f80fhv0szmk4di78rc606lhnjnhqknwg")))
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
    (hash (base32 "01jm66iwv7cvcc9mi1y40s2yhb23zyrz46a6v2377agjc2hw20gi")))
   (package
     (name "travel-advice-publisher")
     (version "release_767")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g1wwdn0i96dqscf3vm9pjmi2imn2wwb5bxhwh1z50b48zw4qina")))
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
    (hash (base32 "1ca33hl9whvcyrgj6isqvq4rkk700pybq9pph4z2dwjsn57l7nx6")))
   (package
     (name "whitehall")
     (version "release_14689")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xvd884l6w14cznp6vyn6bx8db47v5690iwzhccglf1jsp3zb5r7")))
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
