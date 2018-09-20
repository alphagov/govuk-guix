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
    (hash (base32 "1x4wpggqinnx5gb9b1jpsj0fl9pm4k34da9j1chaq1fn3rqsxbzx")))
   (package
     (name "asset-manager")
     (version "release_325")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1k31zlwvg84kzai32dcf2qcwyjhq8b5hmnz2lg0a5vj926j7qkhw")))
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
    (hash (base32 "0m046wlnnd918phwa38h09yfab14wz54m63dzma6xyj1gpdkdv8r")))
   (package
     (name "calculators")
     (version "release_384")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "006ljxpq9cx39q7l9c5zkap9q8xn6ixa5xrwc01jha8j2njac8k9")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wzdnd18n6ipi0gbx04rrclarbvync72vrfbjkqvrbnl6rkiiv51")))
   (package
     (name "calendars")
     (version "release_598")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rc47pj5xh15iqmgdyn3lhbz3vaifhsmxm4rr7my711haq05j5i1")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zc56cik1vdgbhzjqlfb4j4vb44lzbyyz8pscpnx3nbfyx6x3dq4")))
   (package
     (name "collections")
     (version "release_713")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r1d6h6vc4wsdwinqn5alfln1q4al6xy6071d1bdk1cmgls9wx9r")))
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
    (hash (base32 "1kl6vzalavxwir35nvmpdnaw3w1cy20i9ndsigq3wk6bfrs31yp5")))
   (package
     (name "collections-publisher")
     (version "release_466")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pysfcilh0lzvdgj425lkinx8zz9jgphv9cihn18q88qx2h3yrwb")))
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
    (hash (base32 "1pc3cazmdvzk3dakk4339idj12amflzzzcmj0gy5fvgq7g6s6jws")))
   (package
     (name "contacts-admin")
     (version "release_503")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ryjvrl79y5fm6ddic7pxh1mxyhx7m904p2v2m1pwksm2b7wb4vp")))
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
    (hash (base32 "04q524wf7wpd69axj50sm9wrfbpfxs3q6zgpbxmqcxf8ji64zl3i")))
   (package
     (name "content-audit-tool")
     (version "release_501")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1f3k24mmqwfwy435jrizsl397p5ccypsvx1wrpp91kkqjdlvld88")))
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
    (hash (base32 "1n9z8zzcgr6xm7633hpdy7sdcdn9jigsqfpp834vzyvlhh96kfnp")))
   (package
     (name "content-performance-manager")
     (version "release_762")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "025p8jk7y5l9dmfrs2yxx3bsa2ksnpmsnmgc2jij3r82krb3myx9")))
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
    (hash (base32 "0g7ai6282gz35kqm1fs25657bbvh7ypprw2jlm31ip5i8ajjjv9d")))
   (package
     (name "content-publisher")
     (version "release_274")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xhrqbh7b3hyx2146jwryrig23l8mipjy4nah0w6dlqd9hz040jn")))
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
    (hash (base32 "01ss730w3fg70sfyklbw4gcqrsbi5sim2lgx32x9zizvvbbxv0ci")))
   (package
     (name "content-store")
     (version "release_796")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1s2k5anm9jxb44n1dkd3panycc0jqlzphb3zzhrlsvns4p923i6w")))
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
    (hash (base32 "1w4kzaqvsnrvw7sjjawnq5r8yxwhahiy6x28kndpiak0qmdsn6yn")))
   (package
     (name "content-tagger")
     (version "release_863")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wi28345chiq4a99973vp13jsw5pv54r2m0qpaxbqby08lp6zzr0")))
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
    (hash (base32 "0ngk16qhk2gaznj6521fh4a9sbqv68z4gjcvk7781scr74lgfidx")))
   (package
     (name "email-alert-api")
     (version "release_678")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gsywl743zcgd5liz86829db8qgvhlhnzrrp2dgypz2c47q1yvqs")))
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
    (hash (base32 "0dq9b1ripaf177cm6w91zyz0b6gk63l8j2hmnh5sg002m38bm7ci")))
   (package
     (name "email-alert-frontend")
     (version "release_253")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m3a64wrq2g9k50vllcc9w4k15i9msrlgmdldznjy978ngrq5z6d")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "11l6lvsbvlhwx2yha02gjd64637ymr8qkm6756yj92k1vqcrj012")))
   (package
     (name "email-alert-service")
     (version "release_179")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q648zdgz8lr17anws7494qy0h3pwfnskg51n1jlpllkk3yfbnmy")))
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
    (hash (base32 "1x90vwzc2q6k7frfi85hgjh9ydzy70jh5z224k8xlrqn4qn3ly0l")))
   (package
     (name "feedback")
     (version "release_523")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hb4j44z10im8v5y2x1l9avhsnxwff06x5vj209fn0fpy589rncn")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "165lbwi4vnilvg9ysg918gic763fj978zrrkjsg2acn2cd4xzddl")))
   (package
     (name "finder-frontend")
     (version "release_548")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q9bvqfg9vf5057ywr7vc85hmpihfw724n48fsix8baxl5bv0l0w")))
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
    (hash (base32 "115nbjzh9vw3phppka5243wr2vjghp7avwazgr0vqj19g7bdgm6g")))
   (package
     (name "frontend")
     (version "release_2992")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pbqwyqs6vl3aldblvhkpyca0vwc1h4v1f6z3vkz3yqzrjqzy1m5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0i6zy4458fjyzys7k8fzxfq4yxzmgkx9qikixh7aj30i4pzh78i9")))
   (package
     (name "government-frontend")
     (version "release_903")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k8din62pjlghs5p2yqqbrnf7z8wq9s569jlrrkc468anw0rqbj3")))
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
    (hash (base32 "1v5rainxqnsfv0c4kdncr52qqpckn265dg650lpds3803f0x9l0s")))
   (package
     (name "hmrc-manuals-api")
     (version "release_281")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zchh0gkif6dk4dsb3576rmq5j8gyfllp55w69q5d09b59k14wgx")))
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
    (hash (base32 "19cczhi0a7pk1djg03dkq8ld75i96c46wmlxkcc051yqhwsjq06k")))
   (package
     (name "imminence")
     (version "release_453")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1iq9xccv9p803fa00gib3qr62jyfk2mls08a5fhv621d9rlp79v6")))
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
    (hash (base32 "0bijsif338zxsr0zvsd8lxvgy5ji4j35lz894rsg2zi7gjsrf3mz")))
   (package
     (name "info-frontend")
     (version "release_232")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "103xzk451mpkkvg4vkl4plgnn597n629w5izd9sx34v4nj1hlfhi")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1igp3r30fgdx0gvm3qs7jcml9vdxs034hkd1p3j6izga95621p5p")))
   (package
     (name "licence-finder")
     (version "release_472")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m3fjjjp6wfbcivl4pbx1yqgdr277ddkwbkldz69xjv35a685jwv")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "17gfsmrr3hkmkdhnp67iix2hq0bvzqywiklliw76jvsr1pp2yr2j")))
   (package
     (name "link-checker-api")
     (version "release_153")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cviyj71jifagwhyll9biycgjwb1634ahdkbd0f1aqkk865c6wvg")))
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
    (hash (base32 "0v7falz9yfz6fib4amcbmnj3jmnmznz94ww44hcd311hp23gdig1")))
   (package
     (name "local-links-manager")
     (version "release_269")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v8g6ch7ai1y7r1jhkc9ydja9d209jxm029bh328n1i5ac7fxql7")))
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
    (hash (base32 "0mp3qg4q9993a9w1qkqf7akp2raw40hxfk8p2v1mi22klvj0ybrw")))
   (package
     (name "manuals-frontend")
     (version "release_391")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jid3lmkjc0g7q06nl90hnv8sd0gq5ik8fjp3jnka5hzjz92ppyk")))
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
    (hash (base32 "0pvjl2zlll94m6q30cqwrz441vwrqz2dchhi9ksbyx9nb4vl1j0c")))
   (package
     (name "maslow")
     (version "release_331")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05vgk996k2wfacknnli0agngjs47nnxc3phfmxgxmbza9fwv973m")))
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
    (hash (base32 "1ilhk1pyr122j4i0cr4swqj3rgfmqcfprx172r5q3576cbi6163d")))
   (package
     (name "policy-publisher")
     (version "release_310")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g51n6mipzfpk8jjnw95scs71xzpm9wkh6iyf89jhzp3z8ycy4af")))
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
    (hash (base32 "0pjhlwc34kvlb88h7w7zickcglsbqfmj2xf9l5mdipzzkn05h05q")))
   (package
     (name "publishing-api")
     (version "release_1269")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0c4vcjcvp336498k7f2909l7n30gq0dvqfph9rg798qrhvpk1zhx")))
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
    (hash (base32 "0znplxc90c6kcnaz03s72wfbn0c83xywin513mnqxsid8qriscm4")))
   (package
     (name "release")
     (version "release_364")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18d66jgl1cn1c50s8pw9zdsm70myqsfdvsnvdgciypkg8zib64ks")))
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
    (hash (base32 "1ihzgq5zc8fvf9c2kl9nkszkj3i83gb1ihg8p2adpl481vlpb5ca")))
   (package
     (name "router-api")
     (version "release_184")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1aqla5zd1jsphzifr41dbqp7385h3jgi8acxjzyippmxfslhmvbr")))
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
    (hash (base32 "04wlkncav9rqm5xc4vwrw9fh0gmmsc2rrpqigr14x52d3n08nwck")))
   (package
     (name "rummager")
     (version "release_1800")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13fnj6rjhq0blm380b75jqqbvvrn31m5k70kz5vv497zc3007j4f")))
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
    (hash (base32 "1j8kb4cr0rcxkghykw712v5f9npn0jnl39h4kjxzm5h1gh51jr38")))
   (package
     (name "search-admin")
     (version "release_201")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rrhm64iq5m53474y8pnhhnvlvmh10nrjg97rw5mhcwry0r9j096")))
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
    (hash (base32 "0bqmcbm9jcry53wfai4wcwkrxs0kgjf5vx2pkf5nr43ir9xyywz6")))
   (package
     (name "service-manual-frontend")
     (version "release_228")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dpi2bxwqb2amxi5674bvzk6rbbvhay5mnfpbvln5qd1ba0sd2cc")))
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
    (hash (base32 "00f3g62nlhbyb7ih8nz28wh1anz27hm5ys5r852bhy1vf16c2six")))
   (package
     (name "short-url-manager")
     (version "release_237")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13cyvbyqmipzvj6v9qi09nk3zmgzfwxvkf5hbd3xnd06ygxzwfy9")))
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
    (hash (base32 "09j2snhv3nk0lk4r8hia11xl4n9fymx2gmbdj8cb9ll3vp7wi4m4"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1084")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ashx6idknkbwq0dfjcdd3rbh6y7dm1pgq3nzqb6322f85kyydcp")))
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
    (hash (base32 "0jzd8mzz2g9y46vqdazgj94a9wb400faaj7fa8ylmcp5rgspllr1")))
   (package
     (name "smart-answers")
     (version "release_4110")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "020q81b7vgzxp9sk8f1ql6r862xamhv3a6sb3p9da4insnmz9314")))
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
    (hash (base32 "0ivypn36b0a1qwp4380507nvqgjm1vd0h65vhma9m4lg0qdjalns"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1027")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pbf9hnb7z69n1kgm2gfbh6q3sjcqzp3h3r2645ibjir0r1cqan1")))
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
    (hash (base32 "0cpnpd1gh8nlnwr0gggsj4qa95vi62zy7lpa9f2sai0zirrj7qf7")))
   (package
     (name "static")
     (version "release_2978")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02j8rwjw1qjc8k88cf3bf110mqdnvcrghs0l1xl2236nn4rg0q0q")))
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
    (hash (base32 "1w6zr0fpwlkrdqnbywy5yvfcfl95cf0w03fj4mv2yqdmki21cxwx")))
   (package
     (name "support")
     (version "release_728")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jgqfwx18ar5c67njzvhlrhnh6bsz3y7164qi5iiai76i3xmc70s")))
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
    (hash (base32 "0520vqrxn4yyigissb17jpa62778w1n0xda2ixp0wvwbgpjn0is5")))
   (package
     (name "support-api")
     (version "release_221")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "101n5d6307nwimrz6b7p425bx2ckrhjq6q98x00kx2aai0m84sgq")))
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
     (version "release_849")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fa06c00s71gc44lyh4pi47ns9mib8mr9hbj0s3vxqwgbf01chlh")))
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
    (hash (base32 "19yqldcz23srkqzk30j77q7wbsm2ksqkmjrazsr3bhfnqgbag1nv")))
   (package
     (name "travel-advice-publisher")
     (version "release_456")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zyd22g7j2kgbhjpjflfb6nvx80wrmyzqhjx1wnk8kgy5swpl4rl")))
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
    (hash (base32 "1jmklx2n8fzk774crk25vsb958hcgj4hjlqwllh2vdmhv188bzyb")))
   (package
     (name "whitehall")
     (version "release_13735")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i56h9c6pf27r9ms9slnid5xjz60gkm1357w9c13fizd7ps759k1")))
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
