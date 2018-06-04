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
")))))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pwf1n2cwrzxlmv82xakai2xxskv1587sm9amijiik426cprb0ji")))
   (package
     (name "asset-manager")
     (version "release_293")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y962d0g01zi6s3116996gzi8nc6dc0jqf4gjm5pscwjp6cd6q36")))
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
    (hash (base32 "0w7jzizrn46zil5ys19ahd6s8mg8b94saaf7rn2dg4vdv8nnsi9i")))
   (package
     (name "authenticating-proxy")
     (version "release_87")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12bmqrhgi7m4xslsyvqgwripg9fmfsp6zaasdk58119bf6i2gh77")))
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
    (hash (base32 "0pff81chkwyrj1d4z3lsf8ssldvzrnwx08azqvkqdxhh2fpvs3g6")))
   (package
     (name "calculators")
     (version "release_309")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p9hldbggm5jaidi92wvr5hbzlhjdiw5jmffl1i7i84bvnm0ghm0")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0lgs0n1cj35inf6x871gvm4micl88rhk2hvwr8w95s7yvin3gnz3")))
   (package
     (name "calendars")
     (version "release_532")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0368zry5wbrkz6n1snyxvr6w15a3zg9w2ngddkxpr33spzxhydxq")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0a9xgxvyn49ma6bwi6mp7g628sccmqzjzkkq5l2w0fb576x9xv8k")))
   (package
     (name "collections")
     (version "release_544")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13nhd40fwvknqsjwrcsg0i11546k2d3fgkipny5a5n3dq6rgh56m")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zz19p2v99g5qd7ii44k3as2v97ax3489sln8ss16swlripa5yxy")))
   (package
     (name "collections-publisher")
     (version "release_394")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dm0xhn1hib2nkin53bciiafq4bgvycw5z6vzc7yy4kyrid0w7vp")))
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
                        libffi)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "03yhwmiadrfnn2sn7lvk7b3d17i25y93chrng5yamdvwfavfwbwm")))
   (package
     (name "contacts-admin")
     (version "release_463")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09h2bgyxiampw1j2h1k2wa1n7qa9dnif9ksl1vfwwwgd7xcmzjwr")))
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
                        mariadb)))

(define-public content-audit-tool
  (package-with-bundler
   (bundle-package
    (hash (base32 "07k628pkpzhy8g1hbllkdxhanwmpr2chagjkqbkw60x54myi5xwl")))
   (package
     (name "content-audit-tool")
     (version "release_440")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03v6q4fsghlndqc67c1hq82yfh7pq364ma8krjc2cdh0n45n5pzv")))
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
    (hash (base32 "0m2sbawc2qf1c3mhsxs22w1sl3q9h5s7zpgh8xdljprbvkn3pn91")))
   (package
     (name "content-performance-manager")
     (version "release_586")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h4zilchvm6k8k8m4v4xx4khw2aylcb1qjryh424g3p322qd8dhh")))
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

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jsjhh49xchi3lk9sgdlikki60ymijlkm7frzs94iyyyis9d0i9z")))
   (package
     (name "content-store")
     (version "release_767")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d7zfpzh25cp6qrnbv4nw12rj0yrd2z67vbibq9zp5i9zrr0sx9s")))
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
    (hash (base32 "06gxqirmcwkl1mzvdyh3csrfp9nqyilza26zx3nxwsxfl85yif5d")))
   (package
     (name "content-tagger")
     (version "release_797")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ynw6vrznqnpbg9lf14il695i97zpjgr8v5lf5xix3fws51d5bb4")))
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
    (hash (base32 "0k099ma37cw0rss90m73k8kn8n8m8gdkp0hwljc5a4p8wr3n1cpp")))
   (package
     (name "email-alert-api")
     (version "release_593")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ngdycbjgair7vraaax7ai0397xnnp3n5w2fi4j5w4wgsim59yhi")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser)
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
    (hash (base32 "0wvy6s7adjyik1lw21hrzpwqpxsr77barjc87kqbmj3czar6hvhh")))
   (package
     (name "email-alert-frontend")
     (version "release_184")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19r857ix3yvbfi55jfn63rzn4z0h5sv36lnh289w3vmwgc36ja7s")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "01d3rzgs87996j3xa9q0q107pmfq90r6idyhasfx8wycgrrb1fmr")))
   (package
     (name "email-alert-service")
     (version "release_155")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g9jny5sc2lwwpvrwjgx2qm6m2zh6fqy1jjqma2iv810dq3kzwv1")))
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
    (hash (base32 "1z4v7s1c0bl54cwihjs9nxhzzzsxv3r20d66yclpv6xbfhfmcdbb")))
   (package
     (name "feedback")
     (version "release_445")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "123na2a5djpfs8fs3wklwr5dzn2m06fdrg2lpxahpcmgg1f08h37")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0qvpkfb5vqai5i21mdrq271423hi0cnrji14nhis3xhbxan6n611")))
   (package
     (name "finder-frontend")
     (version "release_464")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qrfa3s0s9dpwl5nq162rdn0xp6j8m8aarhxxs4bkk1qhbsx3lwy")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1w642nqwvnpbcpjvapn3qhilnn74r90k1i6n4k44rp94m8lxjjvs")))
   (package
     (name "frontend")
     (version "release_2908")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a9dqivz06mvb33pfvrfaln55y2smm1abgdaxb76i2p08g5v6q0z")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "00k0cg401fwm2aavngng8i9aa084f998n380q7bli53adr8cragk")))
   (package
     (name "government-frontend")
     (version "release_763")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zdfirdv8f4h0d76vl0n2qvqj5kxfsngf5yjgzfhg0494gkf0gwi")))
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
    (version "release_737")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1z8wfhnlmbx4s1q6cbww180h3jx660v8bsv2nxsq57icjn8g7gg1")))
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
    (hash (base32 "01vhim8ipayl2rf5ln5had0f1hsp6did7h05aqzwpl8yfqgsa3bq")))
   (package
     (name "hmrc-manuals-api")
     (version "release_263")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "050vrwixz3nqagsgl73i3z3n2avklcgyjbhkicin6dsdn568d650")))
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
    (hash (base32 "01dlp4kqyjv91yh826apmdl0l80dddz82pq4kzr659z2gv97xs6h")))
   (package
     (name "imminence")
     (version "release_397")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01ncwkyijl8n5mbifys0z2y1cin88z85j5phgkf080c8ps9531f4")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0dp0c6w1d6h9qkj7nlqihs0cidbz4q38h2s54pqbdwxd1yqi8am5")))
   (package
     (name "info-frontend")
     (version "release_168")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mm6vx6zpmr1vwsan5zrmf4apw7vk46f29ggjhvih67jrnh7hb9k")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ccnqf8xyz3ks21r6db5rpj1yf6g1hk4cr6j9548yw8nzfjz8k2j")))
   (package
     (name "licence-finder")
     (version "release_399")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mic710n2chyb5jjkllrswsm8am3xzq424yyw54wbgmrks560446")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "11h8b4mlwadi4zcb1vs02w6q6c47f6s45h5z6xndf8hnrh41md4w")))
   (package
     (name "link-checker-api")
     (version "release_131")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v8n5g08cjkq7cm7ydxdkmgjg641xq3cyg8gw67az4q12pmmpa9g")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
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
    (hash (base32 "0b4y4swz4qz8l0300pnp1gssfh0bsgmlz8j37cxl5nggvh44hncr")))
   (package
     (name "local-links-manager")
     (version "release_220")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j2nwf91jr7a0bbniagq4cf8ybxdhlq2xpzw9wa67zs7257rayi6")))
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
    (hash (base32 "0r0kb15y9zfcvyksswchg1d75j7azcvlga65zcdbl349m0b79cv6")))
   (package
     (name "manuals-frontend")
     (version "release_325")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "198c9bbkd9ainw0jnx1raddbkd2rfl5nylvma9asd9yx5s7v759x")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "09inhhfp00sqca0cr9vp626dj7j8lqpcvjz6jcq38bh4ya3m4r8y")))
   (package
     (name "manuals-publisher")
     (version "release_1098")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07smki61695dafj3zmnlmkj921dhvn10x84a8gskc2zr3vyqifh1")))
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
     (home-page "https://github.com/alphagov/manuals-publisher"))
   #:extra-inputs (list libffi)))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "03q3ycbw8slrmxc553v5ycpkkh1v1aavzkfgfmh7i6wfgj7kggj1")))
   (package
     (name "maslow")
     (version "release_293")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08snjzp9cy9465zhvrjmg4chdrp2pcvw15a8rjy432g7h1dhnwps")))
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
    (hash (base32 "13mwdahp7cixc2cjm14kbgk0jx42d7c389ifllygn4p3pdn8hpg6")))
   (package
     (name "organisations-publisher")
     (version "release_4")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "086fa00pv852ilmi90s5ciabk0x197wrqw6y0x5ib625ya5yqrra")))
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
    (hash (base32 "0zs906yj7vsziygpkvlcz314l7fc7x4qacxq3j5lm3ycy2a6ri3l")))
   (package
     (name "policy-publisher")
     (version "release_273")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sygvxaygbcq63f9rn2swxnga18mn8jlzmby8g61686kla0zz5ik")))
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
    (hash (base32 "1s5z2rp9jpy1dv2dz6hbi9m3ws7mffl5g6ca2kslg7af2byrdyxp")))
   (package
     (name "publisher")
     (version "release_1980")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q5dz6bjp7avjq008q7l1mfvl668ssy1rnkki5jqvyxj10q2bsf1")))
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
    (hash (base32 "1ldcsxk2jq7z3zybc08ycvl8pg1yxxj4bgrs3pjsh9bdy6jr06p3")))
   (package
     (name "publishing-api")
     (version "release_1188")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0xp5zjc5qrbaaxjdnlw61xsvrblahji8n45m6cm7cbrvwwi9mzc2")))
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
    (hash (base32 "152gzhizfsn80qmy3jbkdy6817fl7jmvhjrcmhw3r5w7hknmi6ac")))
   (package
     (name "release")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1i82a73pdp9bn7fz35isrrvgi19dzwmhhxp9qn4hdg5p2k7mfsi3")))
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
    (hash (base32 "0sdnsi3cmh4z1bil000b3a27qfcfym6b9zw4vwlssgjjhhlra5l1")))
   (package
     (name "router-api")
     (version "release_168")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0k36jcjq68yq6z5q2f9pycx50f1kazvwjljnssvqry80spakaiyv")))
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
    (hash (base32 "0gk4x352gf3qz8wcw9qw05nzngjd15i5lasxvx59080jygkss99m")))
   (package
     (name "rummager")
     (version "release_1754")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xyyw2c8pyljac0dq5996ln947kpfrcw2bdanw33xfghhzlp2mdr")))
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
    (hash (base32 "118l027b94sm7p2ng3gbb3565cjrbmi1vv6whlg86q940y24pilq")))
   (package
     (name "search-admin")
     (version "release_176")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mcm1hrhknykqnsg3wrql8158jv70sz2fa7906ycfvhwl1c56s06")))
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
                        mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0x81qa8i4ysf9p0r11ii2m8ms08gnfvrfa1f4y1icz6405s3dgmq")))
   (package
     (name "service-manual-frontend")
     (version "release_163")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v30lja04qllgz5klha50lrjpc1rysi8027ndwr3sr35zc8xivif")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0s473gqpip05jwawmwrrx4q3cm59b9iinjnk4afg61giksf9wd4p")))
   (package
     (name "service-manual-publisher")
     (version "release_366")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r1vx5c679g5b64hq7p9ja077bg97byr7lrrccxafadsnxi6yhlg")))
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
    (hash (base32 "0nnq7bpzbfvrgwxh1wpcji4dlj8val4w3bnq3cg9kyqdhajjajb0")))
   (package
     (name "short-url-manager")
     (version "release_205")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j65jbmcgdz44w9l91p22mps2nik54vk5qd2d6zpwia353615gjs")))
     (build-system rails-build-system)
     ;; Asset precompilation fails due to trying to connect to MongoDB
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'install 'add-govuk-admin-template-initialiser
            ,govuk-admin-template-initialiser))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))
   #:extra-inputs (list libffi)))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "15nx2yrvfrah4lzy9zhr3dcrgvlis44rwj5p2da1j0kvyxwxvj04"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1037")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "034yqnb3rp9y59r2129pdcz53sv9j228fp8g34qbnjqkf4i6z1qv")))
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
    (hash (base32 "0ryz16qhcr85i1vibd0qfryzwnvq1rppa276zgpdak4b2xcz35vy")))
   (package
     (name "smart-answers")
     (version "release_3981")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l0mwkxjb2iy32kghch5zsaqw9fxzhp5hj5vl8sy85fx3b5h65j9")))
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
    (hash (base32 "1c89j53d0b7zca2zcd5h8r7zc63hhswkj3lqqn23d2kx9bvpbq7n"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_961")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fy6hj3sng9s3baf1fjfgxllwnqn00f7xyjsyffd2sj04w4jwzwr")))
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
    (hash (base32 "1mjjhm521nnn0xlq0w1mbs20ld6rv6jaylr4ry314qxz4dxywv8k")))
   (package
     (name "static")
     (version "release_2894")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wp22bk2l0ynljv1aicq12c2rwghjy459hfmlyrf4l6wgs5jx6r4")))
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
    (hash (base32 "078ydva7035mkmhi62xg4vigz9ni62syz0bxbilhrzvh5q383f95")))
   (package
     (name "support")
     (version "release_686")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "139rj5azywkr52yi070is5bxc7j8iffssm489kxr5w8x7d1nb21g")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails,
                                      ;; as it tries to connect to
                                      ;; redis
        #:phases
        (modify-phases %standard-phases
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
    (hash (base32 "19srzmh9q2jwi4c6jmvymzprxdiral420qvkxd4nw7wrfwf080ad")))
   (package
     (name "support-api")
     (version "release_196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1sa55bfrm1d6c34knx4cp6bcapcmbf1fic7hsx187s9xzr4ss2z5")))
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
    (hash (base32 "09fvrlaljq069wc0zl492dfidwv0hsvcqjpbls5pp3j4vrh8zrsf")))
   (package
     (name "transition")
     (version "release_847")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g6d3wa7gwi8k1gzpihl1fi6y71rcy6yvk8s17k3r8mvrhg4lrfd")))
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
    (hash (base32 "0p9iprifnw52rb53vlb97wd38rfycpsk0c1adm17d3xy08b3g4in")))
   (package
     (name "travel-advice-publisher")
     (version "release_384")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kxilx59za0xz8xs4d8pwc344xiq6dbrl0bqm06hapj6584fq0vp")))
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
    (hash (base32 "0qgqn1gg82zwdy9lwps1p8920isdxpqm2gbk48b2a666ghgskfna")))
   (package
     (name "whitehall")
     (version "release_13507")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "076rmhrywa0s0hnxjmhlsn1gr6k9c0p75wbmg1l7mzjkkvd28wjz")))
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
                        curl
                        imagemagick)))
