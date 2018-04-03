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
GovukAdminTemplate.environment_label = ENV.fetch('GOVUK_ADMIN_TEMPLATE_ENVIRONMENT_LABEL', 'Dvelopment')
")))))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1a15fh4ckk9s8rypvb5gn6i407xhyq1imslybnpjrjifqlvd03fb")))
   (package
     (name "asset-manager")
     (version "release_278")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wwqx9aybvhf3wmgabyb87gw1jc4dqyy1x114143j9dq8xrml3r9")))
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
    (hash (base32 "024lblg563pifsk91nh1wb74pl1xj3h2v9sid5cdy4vh3knxkp8b")))
   (package
     (name "authenticating-proxy")
     (version "release_77")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pcngyk6qk11j0acys4i4iww0x43ib6mwp7ffrw494b2jfw1cqb0")))
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
    (hash (base32 "0nw97d786a96b1vz0vg19mj1z8rwr24avlfq3hc3yxnad5j0wf7g")))
   (package
     (name "calculators")
     (version "release_271")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0myj7185a9x130vw6chqzvq2g0wfm1ahh1yijk48g6w7j6rgz4dh")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gg1smx0ryx692f5q96d7nl6pcmnzzhf8q6ya3gn34vz3ffngj0n")))
   (package
     (name "calendars")
     (version "release_498")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08322vc6vvnpixlli8ygia2scwx9qbgh9ffcjwj8a1k8ljgn7ihh")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0qyas9hn7zfw62iwcd4scx1gk32ljwisavcp4kp7a74g2v7kfycl")))
   (package
     (name "collections")
     (version "release_487")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l4g8cx3b8x6yrrfv1z3as8n1bzh8mjpkzxvm2cj2ajw5gg3hw2q")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1j20ljc1xcjiwc4frd0qn5qp54n7d69x4jh9ybbykj9qcgzazii2")))
   (package
     (name "collections-publisher")
     (version "release_370")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bjyyg5zmh336bm6rayp3cz1mrfkrggz0p1f0xqb9masd8wdg8gw")))
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
    (hash (base32 "10m705gnp0340daczm5an9kvwnmv4gk9hrzilzyapi0dfk5ianzy")))
   (package
     (name "contacts-admin")
     (version "release_427")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "050g5pljw3pc16hawngxsbcpq5w8qvb8gzs4ia91cqbsg9n88aax")))
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

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0wmn61d5yjak3dx96pp3n02x3xxdgzzs1wd3gr1q5y4qj65yqckm")))
   (package
     (name "content-performance-manager")
     (version "release_495")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01vb0jzbm84gd9ama2qnzpl2q7qxzrw1qiq2dy2ia9s3c0q6ziab")))
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
    (hash (base32 "183zsd5xm6vw86qjmrddgg5ndb2gdn5p3lzqcrpk4cm6xls2kivi")))
   (package
     (name "content-store")
     (version "release_747")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "188k24zhb16hzd8818rpkj5y051wmkh0hi7qy3vwjrws853a2xqa")))
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
    (hash (base32 "0zimzr6mrvgyxqrx12jlwsd2sfvhgp4v1fndyppi9x8zwc00lg7i")))
   (package
     (name "content-tagger")
     (version "release_764")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10zhglphng0wwh6ddif390zfrxgcqm7gfzpk4hz5mp0p8b2wx2za")))
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
    (hash (base32 "05vjjvmis8s97xl7zdvyimjjhgyys9k9lmq1vhdsi4bynkpw43bd")))
   (package
     (name "email-alert-api")
     (version "release_566")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yc9drq4nzckfq3amv92aznq5vqqyrd01z3wisbn211s1dpqzpw4")))
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
    (hash (base32 "1hg2k1275k6djk3skd49p07wpz9sv6phgchyhnz8zj6cpk6hbk5l")))
   (package
     (name "email-alert-frontend")
     (version "release_157")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l827pip8qm1xqlyhgdvxgzds2awdh029zvi7r0gxs2k4vkhvjpq")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "15l0sylr6h2l2xw7r8a367acdiy7569afknq6n2bnrzfxyzv1y4b")))
   (package
     (name "email-alert-service")
     (version "release_147")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0psqvshhjv1bmiavcvkvcfjj1l47lab9vnmjhjg8v5gkqkqdxdi7")))
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
    (hash (base32 "0wvcibw19wh68ah0gmgd888lhdcrhxpi53c7932iyaiz07ssvj7v")))
   (package
     (name "feedback")
     (version "release_404")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1p8idypfmm7qbsw2a00dn01y3fmzglhfacpk8i2jg8hsd7aw5mfq")))
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
    (hash (base32 "11dzd6xwm6l27cvvp9z8gfy8w74p02f5n239bqz7q9r80ygbrk4j")))
   (package
     (name "finder-frontend")
     (version "release_429")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xj9llczbavjya0vkmlw79j28iwvnbb6vksab108qmpy4bxfy9np")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1yj39jxkjp0jmw8k88lp2364bsdnwxlpv9a1dvgn5zbq2c5w08hc")))
   (package
     (name "frontend")
     (version "release_2866")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rmggw880z6ksl2dnyyadz7i888ary9s7pq4jw9dhir8qazk86q1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))
   #:extra-inputs (list libffi)))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0llb57d1za3gz9c9c9dk2sqz5yahmh0qy2msrbvcx71a6gira282")))
   (package
     (name "government-frontend")
     (version "release_717")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06skdpsjnrb41ydjal4ip4lkkdp61xp4jm0q8mhv30z1h2dz5qwk")))
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
    (hash (base32 "0x5cygsikjah1hrj6awh4gw0jxsd29ncmqihln603bfwnv6lwi0k")))
   (package
     (name "hmrc-manuals-api")
     (version "release_252")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1p29i8x33f4vf408i75l9g9pnav2ghqkyfaqlmqh53xyg7434d3x")))
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
    (hash (base32 "0dv6lmfdsiahhvp8m8s15vy10hld1hfihsyyyg5jsk64kfwafgix")))
   (package
     (name "imminence")
     (version "release_372")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zigd1420fgr3ggm9nxh9gxd3lgldrppns6apw7whd7qgbb76d2i")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0g04hba8p5s82m1z80m09jfpdh9nd3kl03jwbmpfxsf02d0jrrj0")))
   (package
     (name "info-frontend")
     (version "release_138")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1sxnpxbwkapginnh5licq05jawdqzj6k72pgkd8l0cwg4kwj5vqh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f3vxnfjmzd9sqhalgp23fraxp8aq6j43qka2dps5rh7wpfyf7fd")))
   (package
     (name "licence-finder")
     (version "release_363")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1w1aii877p0c27vr0d23f6vvmfsjr04x4vxfac4k9mbczgwlknba")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))
   #:extra-inputs (list libffi)))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "08l0nk244sc18s39saifbhk40k0ryjg3z0yx2l76fkcmzays5qvg")))
   (package
     (name "local-links-manager")
     (version "release_194")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fqfyi7529m133hsfrp9w6140g04q1jh7j2m0x783zsiid0acxcs")))
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
    (hash (base32 "0rk3gcn3z19i83brhf1wjpsmybfhwxsmrgs2jxnpjzsj0pripasb")))
   (package
     (name "manuals-frontend")
     (version "release_301")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xi3xl1kjza7fzfwkihcbvdkwcz91q70zcj6c0h6nsjdid2kn5ly")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "01hk0m5yvwjy7qwnf4dd20443l9zn513mcawjk6jc1kfv8hycccj")))
   (package
     (name "manuals-publisher")
     (version "release_1079")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0689fw6avn6m1gvvcwa4z05m4z7q92gk9b6ijnrk4jasp8d7rz4n")))
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
    (hash (base32 "1x1lnqx7bfwq7hq8mf8qa8fvb5mccvwq3v8rhf540m49rsz7c6j5")))
   (package
     (name "maslow")
     (version "release_268")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "012qvyc793wmnsrdwzcmmv9pg58b9znbknmhiaifxx61pmliafzd")))
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
    (hash (base32 "0yn25lmynl05ynnascfgawii7m7n816wixivdsbrrvb641ghhkg6")))
   (package
     (name "policy-publisher")
     (version "release_253")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0yw27f8mfzcgw9i2p4d22fb7542616hjdcyyrh2jsrvsgdcpn6hi")))
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
    (hash (base32 "0i1qd76qxm08qky0abf6cg2birppq247xfb200rz3wpqkxhq6lq5")))
   (package
     (name "publisher")
     (version "release_1942")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hw8w6pqkv741fg10nrrysp92gv37qvzawy0yyfdvhkzzz6594bg")))
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
    (hash (base32 "1iw938pmh71k46fim24xr4276byph0wrg54hf3n0nwh0s5nbjkv6")))
   (package
     (name "publishing-api")
     (version "release_1163")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0r493gmsfp32jl90a6hj25387an9ysrrvim4r3j1ds0hwqrqn52x")))
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
    (hash (base32 "0zs9c6dik382jpnxhsdsxcviz4i6w58d4nr8vj29rpzckc9h6kfc")))
   (package
     (name "release")
     (version "release_283")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "085h0lyxyg4vxsqnsw0m68l0qcyx2sjbvm0cmzz7j5lrr50mld05")))
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
    (hash (base32 "1vabvcybd7h6xgnbwgi4gyx390fnyz8fhynsx1y5rhx3laws5yyp")))
   (package
     (name "router-api")
     (version "release_151")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lbg1z7zaw460l1nr1mpl4dsl6y42vnpad5rnx58r64pvq727hzc")))
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
    (hash (base32 "0fwj8h5hcdy3bwxlay5vv81161fq26ly8n6pbfc7rmlbk5dn5bfa")))
   (package
     (name "rummager")
     (version "release_1729")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09sy1nz861468h8kmy7fjhf66dlb08bg02yry3n9qdi24x7iy64l")))
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
    (hash (base32 "0ydkv83gcn73kai6kjmi44jhi3vvwn1n5nkmr9kibdn7zf7h2izn")))
   (package
     (name "search-admin")
     (version "release_158")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xa9825wg3kqk24lh49432s4qsy2q3kx6k0ri0ax36ijw1mxw5bz")))
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
    (hash (base32 "1897cjizfqg4gazghh9qilrb2spm3wq3l14q7a6ad66k19g2haph")))
   (package
     (name "service-manual-frontend")
     (version "release_133")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "093bm2rbw3qnn9kkrf1icjp8ps5syy4kcldf4mmaab1ywl3yb5g9")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0mhibmxa55c0aab38wdylz1ckrx2yzcaz5amp304z9fqr1y8bn54")))
   (package
     (name "service-manual-publisher")
     (version "release_348")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pfkbrq66zid1a3mlp8nv7iwq1w6gs76b43hcn3yyxl7l5908c1f")))
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
    (hash (base32 "1shgbrklxrgxn91r6rc0f2cwnrmsxnppf0bb43f4hp1wkqqm6qp0")))
   (package
     (name "short-url-manager")
     (version "release_185")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1m4nz0djy544brnv2mxwh3hsbzf53jnhcarhb1r2iq78n0bi06j5")))
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
    (hash (base32 "1z76dgrssirpwjj82hicqr90r94p4kzg6dam09z3bz02g1vg9rxl"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1009")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0825181b8ydwzg13g59afhs9dfj2zpk3kfipff64hd5q6gp6na2i")))
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
    (hash (base32 "0hmx0z4jr6vrxwfklr3rr2nmgc2v70yci0ixb70rbzf7rs86f0iq")))
   (package
     (name "smart-answers")
     (version "release_3914")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x7p09vzml9h3ylcz07zq1kpwyxrz1nqhb3ywg57pv9xwy90ws3w")))
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
    (hash (base32 "0b6v278a31192s3ap4g2cygayn6vrrbrqs091lydrrfpcwq6igay"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_941")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1s2bcvv8cqzgskgk2rhaqf4dvyjdh6fglsmwrxbjh2vq2rqswbv7")))
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
    (hash (base32 "1fd5cqcgc7in1zik6iaxb2dl9jh63cq3hapr145lbdqwsmrp9dmf")))
   (package
     (name "static")
     (version "release_2841")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cvwldz4wgss96pijxzj47jih0nj4gizr68gfvsvrpa9d2fsj7pr")))
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
    (hash (base32 "1ss1abs076bw2x6gxiv2r4yfk88ics958vxmwxgibkcr53jsys4g")))
   (package
     (name "support")
     (version "release_658")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z3rr5g97jqy7as4snfamcsddbq524f6hcdg1mfvh1hz135qnnsy")))
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
    (hash (base32 "1dz0y7kwaa4hddh0xm1p6pywdzqrdzad7jin55zr32qfqvnr1vyi")))
   (package
     (name "support-api")
     (version "release_181")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mj9mbb4sig1g1255fpyypm43i69idcd6paz2q5p9a9n8xif5vih")))
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
     (version "release_844")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gw7gfik22a7z10gk05la5n9bd4zrsvif99c8ni4g8pcgcmvhfys")))
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
    (hash (base32 "09g6z3nllljy2nh48hfvn5ir832mj5j6xj5ps1xv4sh8j2w3f6gj")))
   (package
     (name "travel-advice-publisher")
     (version "release_361")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11awikdgfgq3kj4y27il4bv91saw2ag4ffn92wv061skf0pyda1p")))
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
    (hash (base32 "178sgnngaw7hbgmfr9iw3l0dgc7am1m0w1wp8mvhwa7p5k79njhr")))
   (package
     (name "whitehall")
     (version "release_13395")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bjz6gk0r3q7ba2i8zc5zrd6pixk25wz9vsjsb2wf8zb8l14jzzc")))
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
