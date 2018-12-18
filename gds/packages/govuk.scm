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
    (hash (base32 "0r42ifhjm2qyvbnkm41cvig13zy31ihachsfhzbszz3m5md1k3s1")))
   (package
     (name "asset-manager")
     (version "release_343")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0np6vw319ha3l7wcmqappihq00p9xsm23w78iy81l9xxlvl1g0g8")))
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
    (hash (base32 "0ygarjl2w8bdil7h4id5a5bcl75r3iwvagsdk3c1scyiqidgjzfx")))
   (package
     (name "authenticating-proxy")
     (version "release_109")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l262fssqi83n0jnfnlxjvk2swcbqq0v2yw8wplnygin1r2bir1v")))
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
    (hash (base32 "1k1bnc7bqia93spjkig592axw7ln26pfbaz3vq9afrzdxnxpkkcd")))
   (package
     (name "bouncer")
     (version "release_240")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15qxyrl6da1ggbw0xqkrvwss04n22m923xi97i0hhb5nn8xvgf63")))
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
    (hash (base32 "0nkxh1v9xpmd0n0qmkrfrhiqmfqxax4jqgfhpn05aslzbn1012qa")))
   (package
     (name "calculators")
     (version "release_432")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15sxynrdj1fkvaiv0jjyhjcb4wcma4g2rrb7g13v1659mzw8avdx")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nbwh3d8rr3y01949dji366qa0ldxnp223khmfq2gpprlqi8qv0f")))
   (package
     (name "calendars")
     (version "release_657")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qf9sjjxijrwq6xlk6bs92202ibw53798wp4bnaywv6mjsjnkzx3")))
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
    (hash (base32 "0kqxd7483q2kdfcvlzc0i0amfhrwv258srmw0df9ckbs4mxcx50c")))
   (package
     (name "contacts-admin")
     (version "release_537")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xncm5lvi2vmv95d32yw15529wr6qdxhr51fmpxzv3zkzzl5n46g")))
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
    (hash (base32 "1hzrgszlwdza1v89gybik91yng62jwdm4m45wpz3hh2rgh4542q9")))
   (package
     (name "content-audit-tool")
     (version "release_542")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1p50lwha01f4zjbci3asc2cz6jlppa0zwzp5kp3ah9mnvpawf6z7")))
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
    (hash (base32 "1cad65vhqgcawddjvz6j42qsfnk332cwg78m5rzlv1v5jzafghwv")))
   (package
     (name "content-data-admin")
     (version "release_253")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05yw6nvxlck0y1k01hjv8s4mhgvagskmd40nlijlbv8y3qb6327b")))
     (build-system rails-build-system)
     (synopsis "A front end for the data warehouse")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-data-admin"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0f76ipbw8nsik8az4500daxwcpriyybn323a018sbp208q5kbvl9")))
   (package
     (name "content-performance-manager")
     (version "release_889")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1flxmx9jadc7kvk8kyappbxpm5w5jb7650cy0ic8snhhl1lkw882")))
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
    (hash (base32 "0b2wd6i08asw9l920mahdwfkn3qm3vj22mw5adnn3sywbzbh6ra8")))
   (package
     (name "content-publisher")
     (version "release_553")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "090s86qx6aq95w7b4n5q4g61i798dd3f715mjswia88g209nyw53")))
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
    (hash (base32 "0j6hc9lfl6phnbdsldh28n68zvlzkgiqfkhyknddfqg5k32s9q2y")))
   (package
     (name "content-store")
     (version "release_817")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08ivgpva0m9lb0h0il7zpir43lkqbfpllrwpxabccf5wfhff65ga")))
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
    (hash (base32 "0wnbh73gj6kb6r5qpva7b96gak55578bwzgzzh1a1a89y87zkmjf")))
   (package
     (name "content-tagger")
     (version "release_887")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lnx3kv2ickbfyqzn7a2a4wwgq9zijdk2sqz0pwzxy5wb20fdxjn")))
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
    (hash (base32 "1khqh2vd0lh5x6ww60arh3pnks8p27r9vnms330cwhfs1n8pwcpp")))
   (package
     (name "email-alert-api")
     (version "release_719")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09pvz7xyxgzwsk9byqfin05fnzkj085s2q4kpw0savvn175dlcvx")))
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
    (hash (base32 "06naq9fh1vk69n8xdc0lk8wrzzz7145hrc1i6370rp5wksyliqnv")))
   (package
     (name "email-alert-frontend")
     (version "release_295")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05hv2kf9vjns4n5qy02j4mfwhsgnldsz366kmh442gq7if6a0m5j")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "101ahvgq1flk4kx0sjwclilv7srgxx81vc9dqphmw11zkpvpr6q9")))
   (package
     (name "email-alert-service")
     (version "release_201")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ggfp0s9iz7gsvjqs02rs7ixbdrq18mwnisrpgi3pcw96xqfidr3")))
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
    (hash (base32 "0mqld5aahj9yikdj24wjchms3dj7mzkxh7dl02vm3l2ycb6zvida")))
   (package
     (name "feedback")
     (version "release_581")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cpjabfs9xpbr5dl1i9jm4h7dizkcizqj5vi253n1s58l7mv4sbh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "089r10maacvdg2fk49liyabjh9cy2shpih8jlk4i1asxjbawxc12")))
   (package
     (name "finder-frontend")
     (version "release_671")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16rijnpd4p1kzz30qr5rkym82bvzmrcdjk655i7ch038m3m6qdn5")))
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
    (hash (base32 "0dcvyirair6dkdvaxjy17s00bsz59cmppb7795ks98y2shpi3njc")))
   (package
     (name "frontend")
     (version "release_3050")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jl7q8pdcdaip2zf5v9r5mhira2j23rxyry96kjpv3j0m5nc8wk1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0c0wlm7ikav344263n7d8v25fg5mba46jqnbif56ry54rk6wvd3y")))
   (package
     (name "government-frontend")
     (version "release_980")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mb3f885mz40kp9i2rzmy27hywrmcps0wz6awa9cqhsppi53msfd")))
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
    (hash (base32 "0qna811gfyqljll36xx0gs1i6cmifixzm4npip3nf93ddi259skf")))
   (package
     (name "hmrc-manuals-api")
     (version "release_303")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jmlaxa2v8ajw7nbyhwwqi51ljc1ffcxzf1mf9cp6zmfg64yzaw3")))
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
    (hash (base32 "14ch0wdchb9kb40bbsp0aijxs9paiypnyy63qavplkbl8mcvxszq")))
   (package
     (name "imminence")
     (version "release_475")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d9xb1afz8r295g2r497j42gm5rcswcwxz94qsq19jcz0a3zzcw3")))
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
    (hash (base32 "0ayc6mdl5r3q0j0vz2274fpms6l8d48x1654n23hn7c3479y0sib")))
   (package
     (name "info-frontend")
     (version "release_286")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r1m7pj2lnljdqjzk1csg4656cvkrz01vlwlfbmk5vbv2csma9a2")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jr5vinqw53897x76rxfmnml5gv37nl3b0di2j3xz85fx90zsqyc")))
   (package
     (name "licence-finder")
     (version "release_516")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07yjzq7m426ndsa59kqvy71mm0lfi99zb29014rpkw37gkggsa1z")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0iq69viwzfwr167lx5lx55y23zj69q6lr8gs7v29pa80xbk9gxxg")))
   (package
     (name "link-checker-api")
     (version "release_170")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06rd64hm8mcyd9d3wxzvf25q7f3y4nrykci8hhzkkcvh7kh14npr")))
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
    (hash (base32 "1hk4xc6ifyj08l59z63xcgxpvjgd1w8vk9rdskv0xsfqjcahacci")))
   (package
     (name "local-links-manager")
     (version "release_304")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0abilihhin3dj08vq6gamc1fr9pd5jcsfzhs3najnjmck9hlvkfb")))
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
    (hash (base32 "17kza2ib77mgmphmgy4c2vhffywvj72j5c7dqkw9x6c5na3ygzcj")))
   (package
     (name "manuals-frontend")
     (version "release_438")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n2yk9cixw06kr1av484mazsc2181scgq44wjlpw5w7l80w3afnk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1657wxwpkhw7v7jc0xz47wd0h26wlc4i896q2ydsnczi0jinghrq")))
   (package
     (name "manuals-publisher")
     (version "release_1157")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mxilzg766cvjs7mjfjbg6g7fm02f5l9hc820pl2wvhcvnxv3nw5")))
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
    (hash (base32 "0b2j837n7ffab82rzda4ffnqs8dbb8b67s8j95j8bb27yqxawlsd")))
   (package
     (name "maslow")
     (version "release_363")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pycjs5xwskrqiibvcsqn5slj0dxjhxrz9s6pkjbfa02bj31qmfc")))
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
    (hash (base32 "0qajmxi4bm7f05ksqm4796il76zx3mhlwcmii9b73sc718pd7b87")))
   (package
     (name "publisher")
     (version "release_2076")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16hlz7gqmpwm3984av4kz07lhjh2rqihq804y3ld2fdcg75dbqm3")))
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
    (hash (base32 "0ysnkii8hpsnjh3sk2c07mx0dab49maxz256v6cli7ciifxdq1g3")))
   (package
     (name "publishing-api")
     (version "release_1333")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "08z4xfp35z8wq0wn01pbp733s1jgwgh8rmxc4d41hsz865017sx4")))
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
    (hash (base32 "0vq5p24575aazay80l0kxv8q2z6b4g3sn86k1cp6rkpjw25s57ba")))
   (package
     (name "release")
     (version "release_399")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fm7iz8zganw6ygmd6n5pn4dsd6yxdwz6na1fclbikif9x5jlq4z")))
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
    (hash (base32 "1c6c8lhjvi9nwy4i4jp22qxpqimlfa8a2r39vgfn5g0z3a57m232")))
   (package
     (name "router-api")
     (version "release_196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lbsx7lxxal0xf7xldpa8b6yrc1skahrzzpz4ijvqx2bd3caqqll")))
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
     (version "release_1888")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0drsikxr2rkmp4k4s8k42pbbj958ai84bxb6b6rh4y56w4h8alp8")))
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
    (hash (base32 "0n5kb8pxg1xpb2sc7q0ifr6gmav4salacnckw1d53f9rphkim8l4")))
   (package
     (name "search-admin")
     (version "release_220")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gxc4iyvscqpvg4hhn6r2520r2x4b8pgs8wbii0f3vxw3gy5vi8x")))
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
    (hash (base32 "0i0wvffjlkqljcjjdmxhwvdghc99sxqkmh2wrxiz8giiw659rd8b")))
   (package
     (name "service-manual-frontend")
     (version "release_272")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bir7pwnm8303vxi9xy1h51lvszdiqkjfrblfl7aaxh3qj8njgq3")))
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
    (hash (base32 "151dhk0dccw3sc6gbamxlcbcifhycrn9jdq92rc9ijcx5hdqs5hp")))
   (package
     (name "service-manual-publisher")
     (version "release_441")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jm0az6d31628anlgf3awjpjyr44fmgskkzx59wnwybfscijyl93")))
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
    (hash (base32 "00b8qw6kq7rqs2ydsmgrqg8sqiacv36ja5wv0rc7g70yk1mshpih")))
   (package
     (name "short-url-manager")
     (version "release_268")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kdpjvxv2dasjzlfm70lh72f6wlg5ygi3lavd6x3kwvrl8328dn6")))
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
    (hash (base32 "0yw228g7cvmnlkls6xgw65l6zs9g9619mzf7yakncxxzjam0d7jr"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1162")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10adwmycm9r5r7ak8y41hfljmmnmsw3piddzqg01m2ijzmjvmriw")))
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
    (hash (base32 "1h0p9nzk6v8wd7aln4xfgsysm0l9mixf6gxnwax6hwk6lqzwi80w")))
   (package
     (name "smart-answers")
     (version "release_4187")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dppci7pn9ilwary4frpbh5ahdbckqyhxhln4m6ckxda3s5wh49v")))
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
    (hash (base32 "0r6kp95rcmq75yjhyiyfp1hcj0va2vmx4wfv7gnd6jsw0qxhzjrw"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1076")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qn12d4h3f8mn2clgs3sad9p5mw70iy1zlxanh87mlbpkis5l055")))
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
    (hash (base32 "0hzay37p5f5b20qycljzl8wv7w5qcw6mqi7nl7bbpbgni7d2rwbz")))
   (package
     (name "static")
     (version "release_3029")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1c16vhslc927h77rws1svacvhkyypbmi8z0zaapfhg7nzfah8s2r")))
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
    (hash (base32 "002575sf1n3ga9yyngh6y0i94g330rd9qj9k4sjc8pfdgnns7h1a")))
   (package
     (name "support")
     (version "release_765")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1j25yzjhmjc1ccmnvgp0dr55719qkn690i8y2vg1nzgv0wnbhl88")))
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
    (hash (base32 "1yzz2vlipx6x73ml8d4j39cj8xdqys6z5xqjmw8aa060yy3cr61y")))
   (package
     (name "support-api")
     (version "release_240")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "008kiymh0r9sq9n2xj44qsmvknnjajh1bglnmk53cjh84k0xa31h")))
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
    (hash (base32 "1296lpc0ykv0k4ppi8c6mni1xzmx1fg3k8fb0bbbsvnz8vrvk9k8")))
   (package
     (name "transition")
     (version "release_879")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bi9lws2f0pfjvqsc30y5b7cc86j5kvmfk5amxl9qrvmlw447d27")))
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
    (hash (base32 "1ild25msk9g9zw04fmzb91fq86m1q81ddddl50azjip18xxy6k6v")))
   (package
     (name "travel-advice-publisher")
     (version "release_508")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kr3mhd156l3z46nsm70nlkriknkl9vais1v5ks92rshiklnwjs6")))
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
    (hash (base32 "1rnnx2vj51m6i32yqry7xrqpdcwdm173xqhja4g4zfq0v4ksdk3w")))
   (package
     (name "whitehall")
     (version "release_13898")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03lrj51mzc023s3vs4g94ys47vpqvx54lxxhlzch7bm42a6mschd")))
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
