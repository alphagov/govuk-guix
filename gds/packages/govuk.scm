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
    (hash (base32 "0n80i7ax4inwpy7kyjrw0vw25y2w78cnfx6sxwqzvh9fkp9bvxpi")))
   (package
     (name "calculators")
     (version "release_434")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ih4wgan7z7in3ns6vkqgn5ygn5ir22zwqv020lp5vrgp1bxbri8")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jky73nrmqfrvnqg05vi12g4fqp562hblin9lmbxpaap0v168zlb")))
   (package
     (name "calendars")
     (version "release_659")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00v669ngynp978jk399q1c3w95pi84szjmwib7wr5bw0z4q2n8nw")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1k04y77k9k8s63k4p1489gzp36gm1n652ylsw3p2dhxpc9bzr3n8")))
   (package
     (name "collections")
     (version "release_792")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vvshl4m8pkdlfhyn9fr32azcy9854gpd3q45sknjwlb43rqz6pl")))
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
    (hash (base32 "07fdhmvy6hb59d8hn0pwnb14idi464k1yfvjz64sslfckjr6dw7b")))
   (package
     (name "collections-publisher")
     (version "release_511")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02kclndz0yqkv3kcxllzbsj3ygpd6h0ym464cangpx8dmnf6gqwb")))
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
    (hash (base32 "16h6q6k2q39zqjzggq1n5dmzfmzdn87kib1682xd2f7dd9xlfbpm")))
   (package
     (name "contacts-admin")
     (version "release_538")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z391bzmp42smi72zsj4whcn9i48wsk9sdmprv5g4879jnyl5gi6")))
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
    (hash (base32 "0yi7w233aqna6bjwqbgsjwja293fp2x34si01fbpcqpk63hi7c12")))
   (package
     (name "content-audit-tool")
     (version "release_546")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pw5w14p20bvlr03bf9dmslcgns04caz80s9cp8kziw29yfl8hq8")))
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
    (hash (base32 "03g3xqakk9rhrsafpdw2p0pw5p8771y4sl9zizbi8ym6z0c59788")))
   (package
     (name "content-data-admin")
     (version "release_259")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qckjiy34khgd9w2769b1b2ina8azqalm2w91hygxlls5fb5ly8a")))
     (build-system rails-build-system)
     (synopsis "A front end for the data warehouse")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/content-data-admin"))
   #:extra-inputs (list postgresql libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1gg82n8vsh2602llraw5gqvmwbmzsr3hrs4bivraqzlp6qlb5f0n")))
   (package
     (name "content-performance-manager")
     (version "release_894")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1p08876wprm8xbvw6vljcnd1srkc5ipc4g9cy3jvglgdw7jv72s1")))
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
    (hash (base32 "0fgl8c6g1lmx0msp75nx2y3dh3mghpr0qm2p5dsazdbkr884575c")))
   (package
     (name "content-publisher")
     (version "release_572")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n7jb6vp2a9lq2b3vmgzcfg3ivsj7cfm7nvanyv5v68hdf9m7qs7")))
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
    (hash (base32 "1rglhgnjd6jfkz6aw698afy78vbr21lbm55lwaf19b161l3yjd9k")))
   (package
     (name "content-store")
     (version "release_820")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v16qv8dxw2pp95kxbbba4nb8p1iadl1sfwf36c6px9rd520jv9j")))
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
    (hash (base32 "0rhk79ji63p0fmaa892lcshk8r79xwn5rbqgrvmwxi7d1y3ygmxv")))
   (package
     (name "content-tagger")
     (version "release_888")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1k7haln9z23vs3h1p2ivnriqjp24b62a64jkjl9hj74q4vb102ji")))
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
    (hash (base32 "14l94iqcwr32mikd1qnd9gqdm7yqffagq06s4l4vzlmh3g3xaqw4")))
   (package
     (name "email-alert-api")
     (version "release_723")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dsa54ll1h2nqwknfmf0ppf5jdnilxvhidcncpw6smmzyzly8bn9")))
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
    (hash (base32 "1xswj4qjqqabhp2xzzh50mcs69dxip7b7knag8w54bjjxh69y3l3")))
   (package
     (name "email-alert-frontend")
     (version "release_297")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18rm37b4sj74k07sgclrllqmavd8mw32djl04kgywwjcgv567509")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "137n72kcypa4swjj9a8jsw9mf4w3hdmypnamq8r609vl1lsi1mmv")))
   (package
     (name "email-alert-service")
     (version "release_204")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qxv22xz0sk8fsk2k9sbka97b3mbxkgy7yhpz38kqkf0sqgmxwzl")))
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
    (hash (base32 "0y2dqlz2zhq6npy9758qsp86r50avxmjk9vb2nxi1wgdc2laj263")))
   (package
     (name "feedback")
     (version "release_583")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bh0c1cb802pyk1s9q8n2sf39js655biyfvrjss22sb7q8s1dnf7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1dkxyb8i3jc0ffldic101mc2zpixg80fryg0df6ryna6ik3nvknk")))
   (package
     (name "finder-frontend")
     (version "release_679")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i3xfcpzml1zijpaz59wvq69nv40fq1vqxvpkwmhrag6z86mw4qz")))
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
    (hash (base32 "1wpnrixsyn78ajnlxbj6zcbq8zms2vc3xf4agn6qrd5wx5ycp322")))
   (package
     (name "frontend")
     (version "release_3052")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "094gwdkc1jy7nipz9pvxnfnhy67drjhg5j2ig7ghvh7zcvlpszvd")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0rcjffkl0z1626mwcmlvlv0rb6vqg1z24wmi8fb4jmg64agzw9di")))
   (package
     (name "government-frontend")
     (version "release_984")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "086y1lyrm5lank0f7hgf7nn8r6mdj8fykawrd7g70hq2wgzcmz84")))
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
    (version "release_840")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "001xri8y56bm287bkhhih6qjdgrsj36ymvc721870nf5h9405d1a")))
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
    (hash (base32 "0kz7pbcm4cay229sk4rhf493589jqcvfmj7na5xlpkrgaykprmgr")))
   (package
     (name "hmrc-manuals-api")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kw47zx5v78nilmk63rjwfwfwyzyar6rr4qhg6clj1h6mkrwsd48")))
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
    (hash (base32 "0wbkw1nkw54xxcr66qnpf3zqwzmw5s7nrhi686mw2wmk5a2pvcb3")))
   (package
     (name "imminence")
     (version "release_478")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pm7qpc8r705fpfxvng0pmwrliwfmk1cyxgh5gr4kjp3glsblkqn")))
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
    (hash (base32 "0hr2nk42jwzvnbm5rj03kdckhnghjkiypdp90z07d4vlqp8cha0i")))
   (package
     (name "info-frontend")
     (version "release_289")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vbax8y74rd7vpv8cz0fl4asyhy15ng1jpk089xx6df1a213i0x8")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0915ban44dfrkkfjnj72ajc0mk8s1iksv06nha96x3f5gbavijp0")))
   (package
     (name "licence-finder")
     (version "release_519")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13d0mn20jwwnbdsh9bj8062dxj53sl96hkza1gf2kbiskcjafhag")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wrgh4nl86mjihfnx4248gg0rarwzzg6zxa17yg3kcrmxb6cmp7a")))
   (package
     (name "link-checker-api")
     (version "release_172")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k51ipyl422v4c2zxicw1k09lmrljp8zc32f1pjpha1lcpz61rfs")))
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
    (hash (base32 "1wmxpm18zkd7zi4pp196a99l47l82ym461icwincdiq8b3lg7fbw")))
   (package
     (name "local-links-manager")
     (version "release_307")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zn5h18adhczxaib3nh05jhws2lk3a1mqa8vi916npvd04cgjaiy")))
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
    (hash (base32 "0zrrpab2fjk22azlb0nxyqm8ybmwsz34g0rw88b0vcphwkmzbfh3")))
   (package
     (name "manuals-frontend")
     (version "release_441")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k1zb2gkg9fq8n2jfh0hd4jgw0zzi05csl14nd38csf81mkv12hr")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0b8c638723hjiaw2ws9zkpnxsvllzb6xjb3mm3b70wmxr6l395g8")))
   (package
     (name "manuals-publisher")
     (version "release_1158")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1c9v93hkwz1yl2xl004x3x5krsyag6gpl0adjms3xnnd7zsja59a")))
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
    (hash (base32 "14s2sy9ac7k8iaq6pn7y8kbwsf7bf9jh9hdx7a886z96lbbjvr1w")))
   (package
     (name "maslow")
     (version "release_365")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r45y2jbz8717xy4iqa0fh597hznr9rjwy56mqphg4dmgj9qq7ls")))
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
    (hash (base32 "1db96f9znvysh9mb3wdbni13p0ykh6b9j40rbmn6cicph1kkd7sr")))
   (package
     (name "publisher")
     (version "release_2077")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a01vh00xr3kgyxx925iwlngfqblzmgmzisa8y9k5mcclslphyiw")))
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
    (hash (base32 "1vbmqpdhf8lnidn7favah8gp2yaq6jq8fwbliz1vjm4waqy8j310")))
   (package
     (name "publishing-api")
     (version "release_1338")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1fqi1g02mr57na8p7mfsw2kxi08y7lsvlxzyzdyy51ihd4g7dzvx")))
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
    (hash (base32 "0g01p5i7l1zmp6g7kxj7yykql8l77pggvhbdnsl05kvlwwnvx6s6")))
   (package
     (name "release")
     (version "release_400")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zphp2zqaknc62va04pp66nad3sphz4pjqclkflgzgjncajqb3yp")))
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
    (hash (base32 "08d2hmfhn9k2xrzkg4p9ifdv08r3lx7hby9iyfs35nl6vmml71da")))
   (package
     (name "rummager")
     (version "release_1893")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gzxj5q136is1pmg73ph2rd2cmj1khgcxrv3fs6abw63mb77g95y")))
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
    (hash (base32 "08bqw7ghl4j8hln8d0nrcayh0x3lfdnay4swz9r3zxdkk77vq24v")))
   (package
     (name "search-admin")
     (version "release_222")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jfz1l66xnnh5v6jdigwqnjsqfp7gqay6ckj087nksbbkvbgj79x")))
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
    (hash (base32 "07wy7pkh09ni9ia1m5gvsqc7x6wapqiar38ik9iwnk739y2b8jrn")))
   (package
     (name "service-manual-frontend")
     (version "release_275")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04hb30hxp8p2h8ky4lmvyjn81ccsnqkwrpf0hlic5cygfk2vp5pg")))
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
    (hash (base32 "1avazbqniibl1a8kcpsglc51zkvsr3pkbkyfy3y3kqz4siim7f0k")))
   (package
     (name "service-manual-publisher")
     (version "release_444")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04sqnwxar8j8dl1551nrda3ql4hixhr7r2fmb31kivhbrcgbszc1")))
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
    (hash (base32 "09yx8qc06sgb2slbwhvvm09s2qmp266z95smcb0dplb8lpdrlp96")))
   (package
     (name "short-url-manager")
     (version "release_270")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i3cilral7aij5r1d6jgwdlcq6jdlfv0c8mk9vyxqw83xqkfgabw")))
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
    (hash (base32 "02a5kw22c8d2mqmq8mjg3abn9khicgib2lw6rj8p94lhj1rr5jji"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1167")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ckhgjd04z7095mqrji4hdmwcl6p8cd5jj852wz2vsi7zf07f4g0")))
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
    (hash (base32 "10hypy5yq7n6d190rxz119iaap4497fhb9g5mi27rpxynfgkfpv4")))
   (package
     (name "smart-answers")
     (version "release_4192")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bz5703aw0rnqgxa2v643r5nrbls6i41bkns2m9a2by4z3if8r2c")))
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
    (hash (base32 "0g0nxgjk9xh6n37vx738blb6cfpiimrng50pyk1k36q6dp2n9vl9"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1078")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lh36ybjlbdfszh5zqcsc6c3s29pn58yfjmjf47nydz2p6cafrg7")))
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
     (version "release_3030")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vx8gxgma0nmnr1ib3ry391g92phnmi3lxycxdgvmdqng9270is8")))
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
    (hash (base32 "0h9hbn5g88kvvyxpqvaqfblxvmmxi7vcxksar9h1ln2ffbvbk1dn")))
   (package
     (name "support")
     (version "release_775")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07jz65928b27hf0kwzjfzymzadxc6v4879q76dzrrdssmc318w8m")))
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
    (hash (base32 "02d69pj36pl51d9f2haijbgfjpn39y5p8xkwqip6kcjisln5nqyq")))
   (package
     (name "support-api")
     (version "release_243")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h6gm8xwdm0awypqyp7yfjs5rdnzqd770sl3h91km5c8sk3n4i63")))
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
     (version "release_881")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rz8ji2bc4gk8bxn2kfwnb6qpan76vg4vqvc88lmzlsww557wl48")))
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
    (hash (base32 "1jr26m0fcgjkvyzxfc21cs13sgmr76xza4fzbj3qh1pkjjkqx3xj")))
   (package
     (name "travel-advice-publisher")
     (version "release_515")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ml56fhp2f6znzwnm7bjjbqss0mpi1llr7wc36x8v9clrhwf08qj")))
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
    (hash (base32 "01lhyls8gafxjix16pwpgmf6cpbvmxy431pc68k5a5nr5p32z6yl")))
   (package
     (name "whitehall")
     (version "release_13904")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lpczjlkg5k6gr0lmmli3hhqnvsnprq62v2m88a83gh8drr6lzk6")))
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
