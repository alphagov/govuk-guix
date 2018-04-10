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
    (hash (base32 "1hagv7f547rflr02n1b7by4m42rihag1sx0jqz1mcisdv8llzyk6")))
   (package
     (name "asset-manager")
     (version "release_282")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18qpvqzd1vgz2x9379bnv7bzgm5jmlaa78v8mwwrz4dmwp61wfdm")))
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
    (hash (base32 "1ivgn9jwv0lr9126snsclnys7pzjj6chb1skvl00a1xmdqldr8wh")))
   (package
     (name "authenticating-proxy")
     (version "release_80")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xk7qj56hbyppn195grjbvg5pp4lqdb17l7vazq9ks4hk7mxfdy0")))
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
     (version "release_225")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07phpircplajqpjx75x668880cjkg3n8vp8wp01dv1w55wggmknv")))
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
    (hash (base32 "0f04rn8qp6wni8iw7xw9l3nis4xcr2xn8jd8wki4y70hpf85m850")))
   (package
     (name "calculators")
     (version "release_277")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dpwrwbazy84dic296ydr9vldliqs10cahs2f38w21bk2rv6xzqj")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1psf3c7ar489y3zk0nd9fqgs1hjmvjsvskr7gmdsz8b9kcjy1yj5")))
   (package
     (name "calendars")
     (version "release_503")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18w0nxrzfy9inw51xplmzqvpdypycx6dxvczi50x878gg01n1k9i")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j0g4zkxql6cqi1l87z1a1pn9s9hda0r28mbirllnpfm1mqcd5wz")))
   (package
     (name "collections")
     (version "release_494")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05ysd1hsh1cwfhm7ccqxka9nw5val5z1wywww3lfpl00jx3swnlh")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "19ihjip0rdp3a5xxnfkg7f9cin24l04fqy0r38w6x1v2dg6r9ixa")))
   (package
     (name "collections-publisher")
     (version "release_379")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fb49svwmdcyqrjp5rh354dvgn1x7k3k4pn3kz146ykj0s1m8kcc")))
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
    (hash (base32 "14g0cdnvvbjp342cbdgq4avpg6l92q1d42wn4irfxwy6qql1cq1k")))
   (package
     (name "contacts-admin")
     (version "release_434")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kvff2d2vl3y52pjrh8by2m7548a62625iy8r3q6gclwmbz0vy5c")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(#:phases
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
    (hash (base32 "07kma079gy1f8zgqpbvsqa0n5pm9qi950cca65hbs8f57gkircl2")))
   (package
     (name "content-audit-tool")
     (version "release_417")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wf4d6y3022xmzzzc6pmmdjf201kdppi1vdnpllwb07l5jy3l3r8")))
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
    (hash (base32 "11ah65rg8hzy0y3wfnwl1msasybm4n6vinh7sva2w4sd14rrkbwv")))
   (package
     (name "content-performance-manager")
     (version "release_503")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02j6yln7i1ipf9mfjh7b54xdg8qkz3jg6cz217kxdlmmcnl1dwvs")))
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
    (hash (base32 "0mxlzldnz4z2kh1g048ssc3dskz424r9mbfp8y58a8snxj5d4afi")))
   (package
     (name "content-store")
     (version "release_752")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11dss5nw5kjlb509a562888xigi29zkf42x5x07xr0l3lkjkwx5d")))
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
    (hash (base32 "1wh6j5fny4zzqja5s8wax2vwspprkv3n5l7rsr8n34fgdh8lhrbj")))
   (package
     (name "content-tagger")
     (version "release_773")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "197kxi4aw7v96d310yj6m9p515ymhnm7zb14bh6lla5pig5zjfmf")))
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
    (hash (base32 "02lf3d3qk3yjyjh9m5k20w196hg0ny0g0r4yb2lqh5vm6bm7jccn")))
   (package
     (name "email-alert-api")
     (version "release_571")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01gqy8whyxjgyvfzid6znp7sbb5wi8vs5qpyv22n251xgsgn7bgz")))
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
    (hash (base32 "1pwvncvy3qcnpg7r3v463my6hzibqm1jc868f447blxd9fdrhwv1")))
   (package
     (name "email-alert-frontend")
     (version "release_161")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jxnabkykbzbn6xxkvja747wn1xy0pcs6z833a97vp6867bpq42p")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1721l3fl4c04qmxch1d9v0b68x0f8bjard1kradmxmzxrmh9qvfv")))
   (package
     (name "email-alert-service")
     (version "release_148")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pdhr2d58jrkpxqzzvjrknv8s512z8wxhgg90f6v73pi9wlpcv0s")))
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
    (hash (base32 "0m5rg3jjylb21sj1r3m8wx7sr98n4zwfr8f1hrxa9mw94fscchvi")))
   (package
     (name "feedback")
     (version "release_410")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kfpnl307igdbim1sq78gs0br2qfgc7q7v6pma1imz7ygys080hk")))
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
    (hash (base32 "1v06cz92xvnmmah9w6mys8ib81j30rj2gbq84irigshffbb3dwqp")))
   (package
     (name "finder-frontend")
     (version "release_436")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13px60c2zs697q327scxxfzffdh3yfg65p3hibps500sl8f2i9j7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0qkrph892cl280cpw4bv06jck8y5wcpf8q6i7wbrs2bzgaqfb7h2")))
   (package
     (name "frontend")
     (version "release_2874")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1823qj30lhallh819n7j4d84d56bc5wirhr7z8yzl6j7g6n0lhha")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1n7hcbsqpcw9a1jblfykfckd1wknsh67r6cf1ag2p9rvn2ndgxlp")))
   (package
     (name "government-frontend")
     (version "release_724")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ba4mhq51ir86z4b4cr2s4g88hhfvfidmnp6086gn3yd7xcrvg8h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))
   #:extra-inputs (list libffi)))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_720")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0l9l0n44hrb9a4ghbd624fq4yr9chk2pbl5zi6npax5mmgsglhap")))
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
    (hash (base32 "1kd7vchzynvncr1jaasc0hxs12qynirxmjh54c7l72cahs0i6jdk")))
   (package
     (name "hmrc-manuals-api")
     (version "release_256")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mwswv4idk8xgkcpwyvaw3mn8pzh85wgrb6qzqidizm8plrwpg1y")))
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
    (hash (base32 "1bh0mhmkn9z02nzwnjxl1a5lz1hj303l6kvicw94n5jayp3xrcg5")))
   (package
     (name "imminence")
     (version "release_380")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bxjnasw1y123ia5vq4h2flaii6c8m5ff6chw4nswd1hmhhy5wk5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "05lsbmnzbpifd2jscjrvik8h9kzsbli2d7ya6chxq8n5ga6hmvvm")))
   (package
     (name "info-frontend")
     (version "release_142")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bv231kbxaqzall9aislszj50sihy5sghqardh64zd6c9fa5fq28")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0yylyyhr0w46p1m25jv6jiyq11lq3c0ma5lgg308wh9xhjq3vn70")))
   (package
     (name "licence-finder")
     (version "release_370")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19s929h6a5xbh8g70kcnb55khpk2npvj5z48ibji45h5z61vhjfv")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public link-checker-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zqw2g4qbjbxcjxbflwxli9g5ii28jq5417d2aa9h6bkx66pkxp4")))
   (package
     (name "link-checker-api")
     (version "release_116")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "036jrg8rxhhlgvnm24f92gsc5vji3br8j9zryp85izp1wx4qa55i")))
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
    (hash (base32 "1ww8q3l3yj4kfav9yyfgfikcafppnrz4as3wmrcaimg3rbv21x9r")))
   (package
     (name "local-links-manager")
     (version "release_200")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z5xyg5r3v91wbnji60l9rmzxgkxnr3lyvpxdjs9y6hl96ns21sr")))
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
    (hash (base32 "0njp7nka5q3hyxbx25r5jxcqh33sg1y6lkz23mnd55197f6g4pay")))
   (package
     (name "manuals-frontend")
     (version "release_304")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i5xja5ihq36sjkwfhb90q52j77k71x7vqvwcdg7j8pvzn0a5l0d")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1swxkgvh1cjnimg6p8dr6mz62c2bpv8vjaynik42l2pizdhvjphp")))
   (package
     (name "manuals-publisher")
     (version "release_1084")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17qwn0lyrp3rc18c7fnacbcjnmgw1k92jqri0l1lslw24i34dr8q")))
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
    (hash (base32 "1yda1c9mw0z6j1v1r1lk91qwz27mamp6zpca0lmqx0bv8rgn3vwc")))
   (package
     (name "maslow")
     (version "release_274")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04m1iciqq4kgi0b3wmc1ikav0x1ms8mngc5dc73m84s2gwn1xzw3")))
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

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "19yplx03w08q0ff6vasjlzidnkcfn4z1rq2jz8c4gbqm0xifpsa0")))
   (package
     (name "policy-publisher")
     (version "release_259")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0dal9j4k86gghndfv1n5gmwpnij6jvwdaczw62wn1f8mz7dxaf7c")))
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
    (hash (base32 "01dxw2ajh3qr1am71bg0iv99k3pk85fdn5ghlzx87ibgvsf4i3l6")))
   (package
     (name "publisher")
     (version "release_1946")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sa9fs8vc3h6l74asgyxj21k0xab3nh8m4d0v1rlr24ychkfqx0d")))
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
    (hash (base32 "1yk6i5rgjigqg719a23lk33m3kbw4rq1kjcblz8l28xlm1iyfgpq")))
   (package
     (name "publishing-api")
     (version "release_1166")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1w24nk0y9yk9vp2bf1ww8s4dgdbqwzycnlvhh7nsjchdzmnqr441")))
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
    (hash (base32 "1533kb1rswqpidhs1znd0icggdv40i6vy5hndqvl1zvb4hr0cjyl")))
   (package
     (name "release")
     (version "release_287")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fc67h3p9yyzn3l4bj3lk29mbghyv65lzmq5i87v3f22npxqnsvi")))
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
    (hash (base32 "0xcvh0ralbydg1y0ixrpnnb8ld5b3kwbg0cvihhdrqjcbgk94y2c")))
   (package
     (name "router-api")
     (version "release_156")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0phn69vhahi40fzc52spnz39zs71n6sq2s5f124zjpm9hafx4rmh")))
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
    (hash (base32 "0jyhg5jg9gz4zwqpmbbi0bw6v4w79w9jghsji681xwpn9zgfwgz9")))
   (package
     (name "rummager")
     (version "release_1735")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18a26zzrpfl7mgd1y74dwsk9z2008i6v7q7vy1wr36gb07a17b3n")))
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
    (hash (base32 "0zxrm8q1rc6xvzrgh05ng0dkg44c9c3li0qlcnlkyf8d4dyadlma")))
   (package
     (name "search-admin")
     (version "release_163")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0javlx72j2v62jiw5dkpambn9l33w1ff85yyjigg9hp4czylbayi")))
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
    (hash (base32 "0pjvpcl0kf3i2l6vycz0ih387x7ljac5ivcd687sd172aws91b53")))
   (package
     (name "service-manual-frontend")
     (version "release_138")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h6p75sdajjvn2nyyww5pjnvjphpaw5x07fx98wmlax4pi06h784")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zq79zd435rwf0hkqpchmanlqas8wfrgnck6qb1ipwcrxm8bllvs")))
   (package
     (name "service-manual-publisher")
     (version "release_352")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xiadikykqrr197skynrp698n2pzkkrjzs34sifvdvqjcv2k9mmd")))
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
    (hash (base32 "0vk5b07i7s6sl4qir16dhl927kbsm62hicihhjiwbq7cpmki588s")))
   (package
     (name "short-url-manager")
     (version "release_190")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "111mc193xynnash85n01ddcv32nwd7zrdx3nwram1rq9hjslfc9n")))
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
    (hash (base32 "1q943sjyz1290fwafj4vhr2kxbys59g7f4jfkr68grgxdrpi2b9s"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1016")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xb3w35fcljf8ww0mq8nb3wdzsw6vrr5wjx0zqprf6x088pjiv13")))
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
    (hash (base32 "036kq84l7x02siw4a1836bzffdfahk3n3j6jh7s29mw3x76ha6x1")))
   (package
     (name "smart-answers")
     (version "release_3924")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pvviwga03s75xxz5qxad5vsg4fhayvjczz6jdyj2z94sg8w5b3z")))
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
    (hash (base32 "09alxfz8cg0qd8ii9wcs9ly6glbdgwhdfyhz1n8j6728vvkfw0z9"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_945")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fm2q286m2d3dhij6b87zciwwvz4kypx4rddlmdzdh1basi26p4k")))
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
    (hash (base32 "1r2yrqrih37pgwwnq8va36dqdwkmjz3y2j9rl0966smmpd3qfa9i")))
   (package
     (name "static")
     (version "release_2847")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r5pagwhmqppb824w0cy79p3hdjfzd7jra0ma2sdgq583hk2a0iy")))
     (build-system rails-build-system)
     (arguments
      '(#:phases
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
    (hash (base32 "16kb7r99wg2fynpcip2hrp1kb30cvqyd63wrfbgx7kd5ai52yyy6")))
   (package
     (name "support")
     (version "release_664")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0w1w93gj7misymml9kj095visnndid1k5bki1x772628av1l43a3")))
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
    (hash (base32 "1qrlwdx2agasbjn1pjbgy3mzjmb0s6d50qwzksic6mk9c7qv9i2a")))
   (package
     (name "support-api")
     (version "release_185")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rqzr8b2mgpkqn55q7l1y5i8bs6z90g7plv8a8sa0gbx3c9d27kp")))
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
    (hash (base32 "10z19hlidic9ni1h23jgjcx5ky50csgkyk2rpb2hp3jbx5fbvqf7")))
   (package
     (name "transition")
     (version "release_845")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "182iiy3grf5yqhlf6sjjys16ikdv6spzmg5lsd3rh98p2l1b50w3")))
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
    (hash (base32 "1q42vr1lqj35zjmricjfg9jbfi8rjvmmnj8pb2h07vh8ywydc0fv")))
   (package
     (name "travel-advice-publisher")
     (version "release_365")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0amryfispraf49qdnm7qiqsq4qlwxd5m3i6pmach7ba8sm534pmm")))
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
    (hash (base32 "1darsvyqbgy2lbkls6iry50c6s8rivbqbi5silf72yhlw1sf5ybp")))
   (package
     (name "whitehall")
     (version "release_13408")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c3n4ff0gyfp2710434hd8ny2zc03b1kfq89f9y18fc52wqpklxy")))
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
          (add-after 'install 'set-bulk-upload-zip-file-tmp
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* (string-append
                                     (assoc-ref outputs "out")
                                     "/config/initializers/bulk_upload_zip_file.rb")
                         (("Rails\\.root\\.join\\('bulk-upload-zip-file-tmp'\\)")
                          "\"/tmp/whitehall/bulk-upload-zip-file\"")))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/whitehall"))
   #:extra-inputs (list mariadb
                        libffi
                        curl
                        imagemagick)))
