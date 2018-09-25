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
")))
     #t))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0fl4y2q664xp5crhap80bfzxxyp97fwr1fyz0658s4zbh5c1jhlk")))
   (package
     (name "asset-manager")
     (version "release_326")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11h8q4na0mszhkkadp84bbdcwnmqkyx3nsrkmqdm280xpxfk3s3x")))
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
    (hash (base32 "15mvqa877sbh8shaq2b9wyb3xip4bz5xlygqcm3zr380pfgy9l0y")))
   (package
     (name "authenticating-proxy")
     (version "release_97")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sijnygb85cf0ahsfljg6r90alsjwxm53275bvq15bgl6h8mdg4z")))
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
    (hash (base32 "05qmvgwn4d23ajslkb1jj0krfxnak82wpjafh09r7h67fv1s1f60")))
   (package
     (name "calculators")
     (version "release_389")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "174mwrb2yw9jjfgqdyvzsv8864f55iis62a4b3qi9ak3mzkpwb60")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "01ywyxrxc6dsv4q9bdl1byy7nk9brg5rll7wcp9vrlvsks4081h5")))
   (package
     (name "calendars")
     (version "release_605")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0117vdpfnjajlir9fkk837p2bkjl091k1gx18gldd1ac8lb9yn80")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "05f1cxx56z94cr113q0xc0gs22sj9wlnw12pq290g12vqbwg1q5v")))
   (package
     (name "collections")
     (version "release_721")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01g0z4la7vl1f39ywpj7674k8aac976bvpdnihc43kq2a71h1772")))
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
    (hash (base32 "0fhlymv75z7yv6br8af5kddpzipqfh4jg3cirh6kbjmk8jb4kiax")))
   (package
     (name "collections-publisher")
     (version "release_479")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vcd62c2sv0rhh9bzmkpqf5063326a92g1lah1r5d49vwszfny30")))
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
    (hash (base32 "184wp8hkwqwgf6kv14n921c0wag3j2dvh5pbyz57chaq5irnyc7i")))
   (package
     (name "contacts-admin")
     (version "release_506")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h50ypdkfrvx7z52nibsyv742gcw21w3bdwspxvgx20v98n0aa2x")))
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
    (hash (base32 "0b0jxn4kj230z78xa9zrhs9w91hgb0g0bcnhm8r0gnlm6dq6anrh")))
   (package
     (name "content-audit-tool")
     (version "release_506")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mlxjr7rzr6wr8pbvwm6kpssrp5x32msxn7qv6zprcz62r2677x0")))
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
    (hash (base32 "16cpi2cxz7gdzwa21vwq15inhrflx0xv1b710f7b93afmspgbxr8")))
   (package
     (name "content-performance-manager")
     (version "release_772")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y2ds380x4h3yb2nmkyvc1sipwkcwiiaxnnmx1xx722bjmhqm7n3")))
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
    (hash (base32 "1qabbbnh5ipik3621hvwnw1c6x1r1nlw2h3sh51lkk1w698hv4kn")))
   (package
     (name "content-publisher")
     (version "release_292")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16v5qapljpf53a8hh8sqlbv2msj6ngr3m1zhag52yfdpcci4jxmj")))
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
    (hash (base32 "04pzsjc59bf27dv1x6p4mrm956m45i9lmgrzz9v57w61n7g9gqyg")))
   (package
     (name "content-store")
     (version "release_797")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10h6qx9dvwgj8b2biqgjmyk9mj18v62zp7w8inxdfiqzfz3aysgq")))
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
    (hash (base32 "0h7saw2n0nplf4lagqpy3kfpjslf3gnisq79s2k3x2680a6l9v41")))
   (package
     (name "content-tagger")
     (version "release_865")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0aqys6pjdhhmbmaw0n4zlcl2hag495rn98sms18v70rp9ryjqgj7")))
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
    (hash (base32 "0qjfi8mfcf11ydv2jqvgcg5nbw26hn95jvv3iarzlv9fp3x2hkpa")))
   (package
     (name "email-alert-api")
     (version "release_682")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1iw4b230iqs511r7bllnawma3fv9bp27kjn9xis0zy5j6ym4wsx1")))
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
    (hash (base32 "1x2x07lbgz6q9p90sk51s4fcfsmabq17if7c7g1a19kqw6w205g8")))
   (package
     (name "email-alert-frontend")
     (version "release_258")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wkfbyy4kdndmb29c7dlqjj2mak6w81hg6m8n6jfmlrcg1klrf6y")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xnvvmjcrblpq06ds015sc49d3x8lmhr2zd5pq6xcsgrdfjkjgzg")))
   (package
     (name "email-alert-service")
     (version "release_181")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13i88jy2k1vmrwxvphvpjml51ih13y79g0ghvdfx99ckwfj2i2xs")))
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
    (hash (base32 "0jlqlszws67r785afxdpavgdsbip0mzyiqfrq4qhw2zfgwzgdp2m")))
   (package
     (name "feedback")
     (version "release_529")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pm43iijz4bm0s2cavqlg0gl16x804a5jqbrm8yga3cwwlsbi4fg")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "05azmglarw63lfx1j26gcgk3iyrs121kbn1da7idjgafbk2mj4ng")))
   (package
     (name "finder-frontend")
     (version "release_554")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xvigm6gqc2pkzy8hkvqc1fhja1vv4wilp3kslzjljavavsy2va0")))
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
    (hash (base32 "0sw92ip0afnlix9swh7yagf7i61gyji0l9fg6vbr08m4hj65mv61")))
   (package
     (name "frontend")
     (version "release_2996")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09wvmvnjrjvhk1f4sbbm8rzvzp3i9q8jh7w2yzrzkgdmd5bas3f1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0rxwc0y6y5lfp6b0aryay2gpn1vks3zyghgh720s6rjg9gf74m90")))
   (package
     (name "government-frontend")
     (version "release_909")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ya22rmfmj6bxzs2afkkyxjanxhy9nkx2wnwmrbr7pplgf3lsdiv")))
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
    (version "release_782")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0ypl5fxkjphcgmdrydmm60xycriahqax1d81m1768ncc3h8yq8nz")))
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
    (hash (base32 "1c4kd94p9nkzbdgbdf7rwn1yyll62p1kja84n74qw7d5j89zgws4")))
   (package
     (name "hmrc-manuals-api")
     (version "release_283")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17n3j6qa1nyfvd4jsf29d2s1pxgadldqx5naq3z8cjklnfk1c4ii")))
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
    (hash (base32 "13jxxfdz60vydbqb30cv23jfidxj9q08i9fclz2z4y6640cxxr64")))
   (package
     (name "imminence")
     (version "release_454")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00ibjbxpsjp937kky39gvvp8i35nwnp964y428fm3spnb8ikcn6p")))
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
    (hash (base32 "1hslj5sza9djqwxr8k4a09sw3nnjzvk5vp20bjjids8wfwi328zd")))
   (package
     (name "info-frontend")
     (version "release_239")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xj70p73n5xgq7qlpgs04i8w4h1qld1yxq076vphgsabv759k7sx")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0xscab97451323szk6gvc7c4caniahjqhpykbix4hhks7783m9wc")))
   (package
     (name "licence-finder")
     (version "release_476")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xz0m9r860sxdkbick72hx5m5pmcnlrvi29irdswilcv14xv8b3n")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1z2ala51i0ypjyzrnx30lwizcia8v0bl0s760m2gg0312jz19q6x")))
   (package
     (name "link-checker-api")
     (version "release_154")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bx3z4zwsip5r7vcma4vfigvlgp683py1mlcm1381q8ifhj5dqxg")))
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
    (hash (base32 "1i1h9bqa0i2v3p3pi4q9ykr9fhhx3sdjlnyz6nxbhfmbyql43afv")))
   (package
     (name "local-links-manager")
     (version "release_274")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mwgj1x0ijmshb0lpghdvf0n08nlxlnvh74l7dm4m93qzchkn42a")))
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
    (hash (base32 "0lr5vrxndbc8bc7vfd9vgah1ar8n9cyqajmdy53vgfqszm95x6a1")))
   (package
     (name "manuals-frontend")
     (version "release_395")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zw1g91m37jb5cdspidwzydji85djpqkmk14a29nsn5mb4c5qa6h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1w54qynxbn9kp13hirx3jfa3sp6xypmzmbzi4z2mkkq7pgbcz4fp")))
   (package
     (name "manuals-publisher")
     (version "release_1135")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19pf2hj1cqrj3h9zkd6mhkkn77dpymkw769l6vgnp4rh55njakgf")))
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
    (hash (base32 "1y4fy4n63mjrgdmia4mrjdiavp1smn0yyri7maaqz5qf65m2jk16")))
   (package
     (name "maslow")
     (version "release_335")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bsk2znfnj6s9p2iavx5wnr5fgikcm86skvb8hvy4gd7h840l8rs")))
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
     (version "release_7")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fk3izvnjib95df16437941byf5di440hf4sz1dg7r1y5d26h94h")))
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
    (hash (base32 "10q8zlkmqdf8c0jg42140iw123ad558n3jm2cprb69bwvqnij502")))
   (package
     (name "policy-publisher")
     (version "release_311")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1629d40jzmxswa0pcviifjdic5abxjxj05k5fh8qgwf0xm7wfhjh")))
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
    (hash (base32 "1r0xzskygzd2fhvygqvkbv5pza90cfq1fyrf4zpf96aihvsvxqi2")))
   (package
     (name "publisher")
     (version "release_2039")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "077kbr03qfj7a0f0pzqmrx3miy36x4vx458bap1wq7g2j93vq3js")))
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
    (hash (base32 "1g7gqd58zzl7a3jvlfdbi30mhawmw97xwa40j6nbvqilqqj3n5vl")))
   (package
     (name "publishing-api")
     (version "release_1272")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "142xl9b3nyl0m4y5z1x6cxkchp9gkkq5q7pgrjc77r0pbjmqd1y9")))
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
    (hash (base32 "1n1w0x6pw85f2mcwai4haas42fs2asblmgl6rmjwkihimdhmcvvr")))
   (package
     (name "release")
     (version "release_367")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0nx685ihrpkrkz6ylxcz6971l189rh2wznkzdh6q10k08n0r29nx")))
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
    (hash (base32 "1sprzc58hpq75cdny9vf4648b6zp1vmc96fwjka62yzrrwh8vwg1")))
   (package
     (name "router-api")
     (version "release_185")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0f1xl0fnz6aw4w0i51hsyinfa5y6psk7grv045jqracz7lk0l7ga")))
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
    (hash (base32 "1bmm1fqs8ikhhrxjrdvxyvjqqkpm92998ad9syyfgspali6w7qa4")))
   (package
     (name "rummager")
     (version "release_1801")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02z404hncqsmx7isqwfkcygxdsi3fvzjl1mfqn7913a1zvdj0k9f")))
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
    (hash (base32 "0pghpz332mmrd5qj64jr94rbbda407xq7szkpc609adfi7kwgg3h")))
   (package
     (name "search-admin")
     (version "release_202")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ipd76zm4dbxfavqkakd3h99m6mqhwancjp3qv6s9fm38jchf5d3")))
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
    (hash (base32 "124pwq54gpmzjqmqqca62jq2karglm4p5347rk37c171cbkr836c")))
   (package
     (name "service-manual-frontend")
     (version "release_233")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l5frbxwgf2y3wx4q0bnfz2m0kj6qgamsz0g1vmrbnxiynmbbj90")))
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
    (hash (base32 "1a1pab4md1svpz8zr1hlhxsi5j3jsgx24hdb8ml4wszqd3afh0fq")))
   (package
     (name "service-manual-publisher")
     (version "release_413")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cdbxzc2vghzxc3z5ncg2zk6lvclwdy2z91k2i47ylbfzfw87vf4")))
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
    (hash (base32 "1rsrhi32zisl59r6v2gizkvn6nh5z8i632grahps2l42bjbjw9c7")))
   (package
     (name "short-url-manager")
     (version "release_242")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lzv5asxd0chr10m8m226rpix2jhbnlpnx4dpkwisvbvz026lh77")))
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
    (hash (base32 "0py17ajz355sw2vxx4fmvpiksjz5q7fsm1bhjgxnpffnx0rjmbbp"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1087")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1azfhi5n7a6gdwsz1yp1acqzwkgg121ni6nfqmrai4mr7n36a81j")))
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
    (hash (base32 "0hw7c4rqzc00i2yxmrni7xsrxi458yxxb21qn3lbsq7g1nbxywi9")))
   (package
     (name "smart-answers")
     (version "release_4116")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ra1fshsn4wrixgvx7dgicrw4i4csgv3lav2smlzl0h1vrs661d8")))
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
    (hash (base32 "1n20ljg8h822mvixsgnywy994j1yf972zip7m1dgy9i7x21pjxhh"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1030")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n3248d08kwd8lv4xcar9xhazlg6dyk8203s7spbgzfc0yxdvqkp")))
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
    (hash (base32 "0karm86f2yhv73lq903lxq8pzaiv789i16cff4y6k0ch9q4msy80")))
   (package
     (name "static")
     (version "release_2982")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16gckhvsv4ggywh8h3334sj0ccixgrx86yxbk9zszd2kwfg5ivvq")))
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
    (hash (base32 "018hvdnzd8fi71jyrnjbx45qhfcakq11sr8yn371gf2ycac21qjb")))
   (package
     (name "support")
     (version "release_731")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bqa0fpn1amswpza1ccsfqvj1b5m90pkl6fy3sgf20wzgd5s149y")))
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
    (hash (base32 "0ck1nm5sya5kimpvjsinknvs7zkvwjqwsynism1w00a54mzzq8w3")))
   (package
     (name "support-api")
     (version "release_222")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fmvyxscai1xgqjxgk62gqpnm72dddfkbps2dg9rk65j95zkhnyl")))
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
    (hash (base32 "08367nvpfphm12i3mv7bn05n54bqypw009mfyjm02b2iwkfqgmqg")))
   (package
     (name "transition")
     (version "release_850")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g7qc3hci7zycibay1jivfvsraagkvfjpiyzfspflkvrp95r9bv9")))
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
    (hash (base32 "1rp3qi4wsyg8lrfc7hd29bilfafidbvv4q0mnil9d6dlw8kq0mmr")))
   (package
     (name "travel-advice-publisher")
     (version "release_460")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c7bawka37549344z1325afz2ln9v8hws2anz6jjpa39b2fdkj69")))
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
    (hash (base32 "1gvy2pv83m13fznirkgr6ycrlkn7jf2z8jzgmzlz4dyafkczijwy")))
   (package
     (name "whitehall")
     (version "release_13744")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17jamp11r39q9caxq5sppklsq94jbwq7qk5wdfx4zc53m5sg4ak1")))
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
