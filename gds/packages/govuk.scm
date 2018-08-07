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
    (hash (base32 "0344vi4x80a121fsrwwqrlalk5w2f2b80ffcq3px6v45a9c4alsb")))
   (package
     (name "asset-manager")
     (version "release_315")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xip3zq2a1rycqbr45r1gkz28h04k5iin5c307cnp2f3ai359mj9")))
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
    (hash (base32 "1lrh1iwh0fh9yh1gmbqh9xrj41yrkzxn2hjrz075sah968jsl7ar")))
   (package
     (name "authenticating-proxy")
     (version "release_95")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10rcpgy7sc3z7hvsn7wgp3wm4bn644j8fszdpz090x5lagiv3p6s")))
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
    (hash (base32 "1m9yqj995fmlyi3brprlyvjj24gznxd8rfp2xhndm2j91dgawxc3")))
   (package
     (name "calculators")
     (version "release_349")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19ww847wnwc9sradvh0y3qi8hxcails6ly8w5jimw0grmb8hqcjs")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zdj7hcym7l0ng38bgqfjxp4hmyr3v8dv3r3z5gf8pkb1i2ccx22")))
   (package
     (name "calendars")
     (version "release_572")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1drwf7y7cbpdpgkwzrk24n6pslf9vfs3va84azszl1bzjwjz1hxh")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "05iarxwxasfbm59qrn95clgbk99dvmfmkkn591pbv956d01n2rkd")))
   (package
     (name "collections")
     (version "release_676")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vvr984xy089vkss286khk7cppp8hb0mz5i7w88548ljhlylpvia")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "057l45fg46v9mqmw428n11qrmrjkbp97fxn3g3z40dls2cdcz9k4")))
   (package
     (name "collections-publisher")
     (version "release_429")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06s4w6j2prmzhld9jccc5sg7f0xbzipv0h11n400m957xjn24lx4")))
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
    (hash (base32 "0lr3gnj7rj9gd07pzlh54ras8yc628vy891qw30jhzy9vbz54wrv")))
   (package
     (name "contacts-admin")
     (version "release_488")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1g21g41b7nk8haf48m4fx3kbaz05dyd7d3xdk4aszk29p2f90gh8")))
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
    (hash (base32 "0wmf6lw93x3297iaq9r1hwv2c0l87dgbysn9x53gaxb9lg5nf2gn")))
   (package
     (name "content-audit-tool")
     (version "release_470")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05ysspacf3acmyxx1b5r5aaypjvr6l2z8mzgd34pycrqica9h2jk")))
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
    (hash (base32 "0lwinhlgxm9kc4flaq7g9r13kgldwn6fwigh1x86wla9c9j4ci4y")))
   (package
     (name "content-performance-manager")
     (version "release_690")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02v1p39r4mxvbgjlykahxgi372s2cdijvkw3626f3209mixdwfhg")))
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
    (hash (base32 "1jypianq1k72gl2r6fglpih9l8bz4s1w6k843rmv8kq0z2lpaw3g")))
   (package
     (name "content-publisher")
     (version "release_88")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wzijcw9qvwwaagf2c0blm50pz3nv9dqkixz7v0a9x06fz0rjj4n")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
         (add-before 'precompile-rails-assets 'set-fake-SECRET_KEY_BASE
          (lambda _
            ;; TODO: Active Storage seems to require the
            ;; SECRET_KEY_BASE Not sure why, so set a fake one to make
            ;; asset precompilation work
            (setenv "SECRET_KEY_BASE" "fake")))
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
    (hash (base32 "0knqwn28h9dssylyphq1hiswll9hlyzjjxrsazy62pa2xl455pwm")))
   (package
     (name "content-store")
     (version "release_783")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bvz4agla6khc6810xci38cy5sz946c39n9h725qr75m5dwz0w1z")))
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
    (hash (base32 "0j75rc28g5zlhps5vzflrvwh63hvxb7m4w2kswx70dn3ilpbal5p")))
   (package
     (name "content-tagger")
     (version "release_836")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01dwjf76zpyn7m2v0sraw33qgd0aarbl4rbb4l9sp75f0kklj4hd")))
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
    (hash (base32 "1ic1q31gyjjk29zn1i7gd3pva4rpxx33pzy7jcs1mkjxczy6g7j0")))
   (package
     (name "email-alert-api")
     (version "release_638")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15fv2nw8fnvrz3hby8vp01cr72a4ivcnd4vik7r4lqjdl0mb5hy6")))
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
    (hash (base32 "1x42ymgx4f67ki3g29676wkcr5hpzpkl5aqvnwld2cgag699z3a9")))
   (package
     (name "email-alert-frontend")
     (version "release_229")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13zk6dr7mnpx9n5n66wfbkvxq4d864zlg30jnb40lmp4isw545p1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zxqjjh2lx2a2zvk3dvj51wl2yxn3b7dyga6nl49cmb25df9awjb")))
   (package
     (name "email-alert-service")
     (version "release_166")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pg709f8y3r9g4abx1dsff1nsgqgl67qw8zapbbdkwml411q9rp5")))
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
    (hash (base32 "0ssxpbmc7ykyjc4n5s46k201rpn1rzx8y2dwxpim4wk9w98a4f5k")))
   (package
     (name "feedback")
     (version "release_492")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11q2glfxs5i9zs4l5xk3wmf8cjwix0j7i2bg0ygk2prfk85sx67c")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hd0m3v7iq61mjls80fafxravwfwp7pk9rg6fi1vbz7plzi43vqr")))
   (package
     (name "finder-frontend")
     (version "release_515")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b261js74in5jc19j2mj4j73dm51af7b863cig16bnc3ji04pf7k")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1pghl7m29xp0a5bs9hd9cc5cbmfdgk6b8k5mlv2pwq3bxy5kp3py")))
   (package
     (name "frontend")
     (version "release_2955")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bfc4d9kc847fg6gs47vgaiixm2iyblk34sgf9a7h9a9z0cqxz3c")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1c76fnf1f6ch6smxaj6l22vixkdqzn23g0mglrcnjgjrghfw4rdl")))
   (package
     (name "government-frontend")
     (version "release_838")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05vc3ab28ndmxa00fx0n5vn27lkj7jn3a6pscb187d9lmbls14cj")))
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
    (version "release_777")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0m4wbkiss7l6s4y81wyk2wr801qj0nbch6jshrqw10i3f5jdvj3b")))
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
    (hash (base32 "19v6q0jqrrxpcwxs99l4wga0cycp0fgg0y4vwl0618490g1x8vfg")))
   (package
     (name "hmrc-manuals-api")
     (version "release_274")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1w9v8hkf5zbbnmkxix3zdgij8hg0hnj3dgk617nc59bppgb8lvx8")))
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
    (hash (base32 "047v5w3mqm29p8j73syy640spxqv69szhvfj25z98j5hx93099w3")))
   (package
     (name "imminence")
     (version "release_425")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h75b7zph04iffl44q4n6fi4hrh7hnk772xvqgfyw66hs8bp7zdi")))
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
    (hash (base32 "10mbprzhs8jjavxxikgg997dqnng27iym1rdgsfhbnydl9jrlqc0")))
   (package
     (name "info-frontend")
     (version "release_208")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0aczjjwd0c086m9phw9787wighnlkip6kr1cv452gqk96c6af542")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1626xl8jj5df9dlkc7r0d3z6xhd3g8nfv8q3i6mg1dyfxa2i0nvs")))
   (package
     (name "licence-finder")
     (version "release_443")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fz0vk1siy045nx53shxhnlyaqlqm84j1s0cjwn0r31ryzh9pg9s")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0lp7wkyz5knsl2mmv7x4v08ysi6ikvvlr2wqhxh450a41kidj592")))
   (package
     (name "link-checker-api")
     (version "release_145")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09flrwn8ggcbp7fb0vgwkbm18qyhbk61fmh6gm4ykfi7aa7z5xzn")))
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
    (hash (base32 "0aggqs3klpr3ls3dyngy0h866aczk9jvz4xz90yl9qvxbccfqzph")))
   (package
     (name "local-links-manager")
     (version "release_247")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r535yqp66j6zn8n14y0hj3fpkfsmqia48gyc98a06lx9cgykfln")))
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
    (hash (base32 "0m2ck75kj58h5mf0xhl3694v21kjr6sxwpwp5fk9cwm5k581x1di")))
   (package
     (name "manuals-frontend")
     (version "release_365")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gng0f7877j2n7v1blxibl87kpxj2xbzww6kqmx1j9557f9xvia8")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "139hlkxv0n2rn1znilah72c8dyaqycgyjkxs6hd0bw1x2glqpnjw")))
   (package
     (name "manuals-publisher")
     (version "release_1118")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0skp6bbmvawc30dkgi6yv32rd7q33mcm1wnbpv7dp88pz0psv2kg")))
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
    (hash (base32 "1mc8247531qqwcmhmd0940fp4dg2zqs6129z11v60bj3pmr7f56g")))
   (package
     (name "maslow")
     (version "release_314")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "105nq4jci2nfliqbkrj8y56z73zsn4f10nhbb5jxgh2libz0inin")))
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
     (version "release_6")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y13mvm8jw4k3c50bazfhngm537r50msq7b31lglz50763c2qib2")))
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
    (hash (base32 "10iakpwmlmdkb52c6vv4x9vw4nhxghzz24k2ca7s61w9gvhqaxdq")))
   (package
     (name "policy-publisher")
     (version "release_291")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jpzp11knjikw2zziccv5p7hkvaz7cv74a346n74wsw5k6flylbc")))
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
    (hash (base32 "1jx2skzz2rjifkx87681lf82l7qz491893c1c4xahbklsdnz5393")))
   (package
     (name "publisher")
     (version "release_2014")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rd0pckysdaakhcydwnr27g2kppp1ygi5a7w9plygpyks79j8k93")))
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
    (hash (base32 "1aqfp739c1vhcm4svqc8dr5dzh8rhqff11wndwgynary6xr6mmjm")))
   (package
     (name "publishing-api")
     (version "release_1242")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1g811ag72gddvwzy2x2rr8xx4izb0q4hjhrnxfhhzy0lnxn4gkgh")))
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
    (hash (base32 "08c81glsfqkx3dy9w9900ahc2n1arc9ysza8f1mskp8d1dvjgv5d")))
   (package
     (name "release")
     (version "release_344")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00rvq3aas2jhmm9hpxv0bl9bajvzyk1qqlmvh1injp53zac2vvkw")))
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
    (hash (base32 "04s8zvdkvlcync86czm13kakjkq19g9ahxyjr30hgwx3ijy4fiir")))
   (package
     (name "router-api")
     (version "release_176")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0d733zvi46v4khr0siqm08f6vplgzlrmq3b1zpjj5p0qm9d36kll")))
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
    (hash (base32 "1gmdy2hx03x5r86scr882lahm82pc43mbzb7zpwil7kq45z0qd51")))
   (package
     (name "rummager")
     (version "release_1784")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mz5qyj4pgny9fqdwq6h2bbv2ydb1ncpblmwmxp1m0p62hlsj5vq")))
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
    (hash (base32 "0pz2m4lprh9dkjm6s9jz67g0806l8k6ng9ym8xl9dldagv2wvgq9")))
   (package
     (name "search-admin")
     (version "release_191")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cv3x5f2ndhaz1gyry4pyk1mfr0qf68jzi8danizwy05z1di7z8a")))
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
    (hash (base32 "1skb483b85mjz99z1ak3kfacza5wqb7m8c6cp447d2dxbg0big93")))
   (package
     (name "service-manual-frontend")
     (version "release_204")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bwg5r2631pdc36wrv5y371v9pgycd6fjsp3l91r0590aj0nbd6v")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ipc2d3mwqi6hbppsbzz0m2fnvi87m85rcbr81w06sm2n34dfb33")))
   (package
     (name "service-manual-publisher")
     (version "release_392")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g4xi90jy9836rzpg54jq8nymif34xkwj6ap8rizn5bjzw29xqk1")))
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
    (hash (base32 "1lzrrcald10yk9axlfv4kr95riq88d9nslq6rfssm02zgc5rghqg")))
   (package
     (name "short-url-manager")
     (version "release_222")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ydkhvq3jvwifahc8zm0gyahk5d4mwgfg88sv03669pip3c73sji")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'precompile-rails-assets 'set-production-rails-environment
            (lambda _
              ;; Short URL Manager attempts to create a 'Test User' when
              ;; running in development, which causes asset
              ;; precompilation to break
              (setenv "RAILS_ENV" "test")))
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
    (hash (base32 "09r1ahcbkdi9nnf4lnbgs7rmd6036jgmrpkjkxgafdxgajkr5dm4"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1065")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r659iflqfbhxfryilh57f7g0xhx63hbl36mbnnmb9pv3fw54305")))
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
    (hash (base32 "1vhrsjqxlsdgiyxb7whwkj0ysk0sjf46560shwcbcv3a4fd6w6jj")))
   (package
     (name "smart-answers")
     (version "release_4053")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09qjpnhzd9hhz89m8dik2j9rfd877b6g84bjny9jlzssnnf0xmmp")))
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
    (hash (base32 "0zpbjcsg6cnmgs30siv5hnb751az5x3yb2lmy7f2vy8jlr87plc7"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_1008")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wzlypswksay1mbxkc8n8xbszipfs1z0p9549njc94dky8bxprk1")))
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
    (hash (base32 "1gjn68kaywki07kd5jcvmb9q8mlsmxgx4s0mp05wd1yx7r360agc")))
   (package
     (name "static")
     (version "release_2943")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1863qcxh9nqlpyafzyjilb65246rwh8fdmij8y99h49am1zrikz7")))
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
    (hash (base32 "1vbkpd89ywlbfpxxkmbxr4jscxps8s0mpr53dfzi40gr39rddxvy")))
   (package
     (name "support")
     (version "release_708")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09rdkj7q34r0yvqi9d19i0mcqa3dd0m9zaaifz7aa5nqn5sp764r")))
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
    (hash (base32 "1bh7q7ajx9f1zhw92cyy3j7hg5vkm7xmimiy8ngw2kswaczxygcc")))
   (package
     (name "support-api")
     (version "release_211")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j1qx7dymsinia8y4d4qkp9ynvry36bpw97dla0fphvwnfc7a0wp")))
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
    (hash (base32 "1lhw3dz8rs1rnldk3wair8xzcqvjlv7ng3bn6skrrwzfjm0ilsfp")))
   (package
     (name "travel-advice-publisher")
     (version "release_421")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zmxlgrciniah3c9n20srniflc515ls8igk6cqwzzh5c3b8k0lcw")))
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
    (hash (base32 "083jwrm76bs61y9lmldf09mvqhj2hnfh0wvgrmlay7x8lff5i2gs")))
   (package
     (name "whitehall")
     (version "release_13655")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qck4g949dhimqw4idk5gry721i9cyyky0g5i4vrbh2w4b5fn875")))
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
              (invoke "bundle" "exec" "rake" "shared_mustache:compile")))
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
