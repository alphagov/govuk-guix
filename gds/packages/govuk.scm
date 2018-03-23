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
    (hash (base32 "0czw5q5gz2r0dyziar3bwps5667wvw8szywrarzcqy8xh8krpd54")))
   (package
     (name "asset-manager")
     (version "release_275")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n01nndqjrhh9rmhh1fvms2006j4as3spmlwxhywr8nd45f0yjag")))
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
    (hash (base32 "0cx5nv9k72s01qay71z7djx5p5aghz7lb9kvcmkynr1w6my12q95")))
   (package
     (name "authenticating-proxy")
     (version "release_73")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xbasx8rb6i7isrwcfsjkqx5ng22ini133dbyin31gpcy9srs7ic")))
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
    (hash (base32 "1n2llx4z3rvqzak89mfa3zw57lb63ychw4v3i3qk2100b5gxkqz0")))
   (package
     (name "calculators")
     (version "release_262")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x0mw79w8qkyqxwh3w7xykib8xcxm4d40ap7f63hbdfyxv39aq6w")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "011cvxkah3a8mkzv2yh7q2dnazrn920n9xqsqhyl9yhf7pwjkqm6")))
   (package
     (name "calendars")
     (version "release_489")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16s2hvxxxbsxhsi6h3b2xm8w35hv5azhz0zbx64zbg2slqnpxj0y")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nwmrhna5fisl3z6p6azkdzq2ns7lahn67l6damwy78z2nbj9pg9")))
   (package
     (name "collections")
     (version "release_468")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0msgqjkipdklwgld5hiai4r3j7n3qlssbphlpal3gz0fln140866")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ds67ajg86gjpff335zy7j58fkp9aajl7iy6cw3fqdfan6025r1r")))
   (package
     (name "collections-publisher")
     (version "release_356")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ym7g8fhfc020i0h2i0q0dwpxb1jpvqsd7wjsq8l29zyw45nxqfv")))
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
    (hash (base32 "18vsd4zimy1hnzd33i10d9xggprhgd1nrpbbdgri0z2w7prd7a7w")))
   (package
     (name "contacts-admin")
     (version "release_422")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0f37gsalx5nx4la2fii1c8wa8v2r709az0qnxqwfnx0k8qmcyf9x")))
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
    (hash (base32 "1qny1pddra74kdpzc69kcqr762ysi38hm8r2gy7rwxrb2cmj2qbn")))
   (package
     (name "content-performance-manager")
     (version "release_479")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ykfyg5kag2srkm3fq9qfqzh33fzgl8vlg03s4ikjzbcfgrwwc60")))
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
    (hash (base32 "1c1hpdn4rxnczp37rq99awmzq2jvpbcgn6lp003plya028i7qd11")))
   (package
     (name "content-store")
     (version "release_743")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1933ygjvaqp69v4vr0597ky0vvhq36zwd0vf1qyqaby8669696yb")))
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
    (hash (base32 "0w4qnm662fgbs297nqdhrxsj2gk3r5iw2sgawdkzd61vqwg47n9f")))
   (package
     (name "content-tagger")
     (version "release_755")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17n6cwsjxxcjga5mw7nz8rympa81g7za547flj1gd7d2aaphjc46")))
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
    (hash (base32 "1mn4lmr7zjrkfyzhig61nd8as9jpfhbva91z6i31yd3fi888lxqv")))
   (package
     (name "email-alert-api")
     (version "release_549")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "069rbmlacdky4m81rcpg8r2jp2ls2rg698v89gd1n2x70q00frp7")))
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
    (hash (base32 "1dvy2jkl3r6172rgzgxxksw8ysll6bjj2cjqirj9gsg5pa3fywlk")))
   (package
     (name "email-alert-frontend")
     (version "release_146")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ccfn14c1bmyy2f2116daz8w43lcknmb2kvr6fd3l7j32n6gbw5r")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1lkjwz3rzv716nfsfjlikd9k830x63gn3acxwfpbapw10pz8fj5i")))
   (package
     (name "email-alert-service")
     (version "release_141")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hvw33ignv6wv7wwxc78d7bal10d5afr11x0zgba8fpiwykl1js5")))
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
    (hash (base32 "1m8v2w9mwn4r2dykqfpcki3g0l0cxgc2hqqjnsdghxjb4dq1ry70")))
   (package
     (name "feedback")
     (version "release_395")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l3hgl1svdxvmhz4051361b908bjpyl1fjamigkawlnqsmxm8kww")))
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
    (hash (base32 "0hwlp3d2rsy17l90q1vgnsy2p58iq81m6z4781v914xac14n7qwn")))
   (package
     (name "finder-frontend")
     (version "release_419")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kfh0gik9fbsn72zhbix10d7l5aadpdmdqf7f8bra4922g01arsj")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "13irmcfl712x75i26sgn5anyim9h67vsyaz6g1v2wrq2lfhzkv1n")))
   (package
     (name "frontend")
     (version "release_2857")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "087jn1fykb25ww8cskqz26bsvz4nsvfrcbp6mnjqm2lqdjg1d8zf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1j33nn6kypw7qx4v9pqpdd7yhr96xghl9n48hmblvs19vbrf12mh")))
   (package
     (name "government-frontend")
     (version "release_699")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r1llr5b06fr3jgv4igyj0w0g5l0514pdwrqk98naayh8fwxcfbb")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))
   #:extra-inputs (list libffi)))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_719")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "131fsna2kxjghxzrxh1m7z7bdx5xj0jh3y562ikch69kj1z1l5pl")))
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
    (hash (base32 "0i1gycg69bipd4h3n5knl6b5lyjr0q038z5nblpwxyv29sbiryhp")))
   (package
     (name "hmrc-manuals-api")
     (version "release_248")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y3sc96akp0janh2kikb7zp80vjv8c4l113m99066zly0dp1xv7v")))
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
    (hash (base32 "00c3x4imr1xp223m4ha5p3599395hch63bybh2zcq39a1mcyj4k3")))
   (package
     (name "imminence")
     (version "release_369")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ky4b163y8cfzhva456v64wa1wdgqnnn8cn5ii0i5vk00h83sn47")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "03nd6rzqhbcmyv7g2mc55hnm03fhnl4jydv3k7rzzhdw60x9nz0g")))
   (package
     (name "info-frontend")
     (version "release_132")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "029hp9miy82m9f1x1mpqwzxy39wpr9qkjkw38wq7m0iwh0vjw1b4")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hlqr2mij9pc2wn5phkni7glz3vyrpsp4kfy8vkp44l5dqw6v6pz")))
   (package
     (name "licence-finder")
     (version "release_356")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1570ikfmp4gh2vjggj1wlqlq2xnyczy8aqi54s7b5n7m12a1i9ss")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1j69d1w9nv4izxsnn6rnc94sykgq59k5lplci7j1p0z9awb17cpb")))
   (package
     (name "local-links-manager")
     (version "release_187")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mixj2fykrjnqi2gf7hsf7nlg8fmmcblazvmfqvhdckdvy5y995s")))
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
    (hash (base32 "1shss442xz4n76f8w4hsdxi2c762ckch4h4xzy9fryc1cjb60di9")))
   (package
     (name "manuals-frontend")
     (version "release_294")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lgvla6py6s8icxqb62l81qq2d2779lkgh24z4ym3vvv1y2821h7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xfqhdrni1wnqqr4hvx3wny50pd9fksymnkw1r226wdk9f0a87x5")))
   (package
     (name "manuals-publisher")
     (version "release_1074")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bmcqv8l3ma3b2dks3f32iz2icpgjr0c3iar8ghsnp3733h9xyd2")))
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
    (hash (base32 "1ib6bp5wbjhcvfs0k494061cfbf9p78qh61hsa6l7dnldx72vn16")))
   (package
     (name "maslow")
     (version "release_259")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xjqr86andl4wv3q575nfi8fgj73bxnmykfgkryzwc29nmf619xi")))
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
    (hash (base32 "15bb557an28z69x4xlyanb0vvji9hwvk05kzidsi0qmkfszkrqm8")))
   (package
     (name "policy-publisher")
     (version "release_249")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vbcp8hxl8yn0fcm2xl7772yybb8f9pgxhjwynhjq99yfap1lm03")))
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
    (hash (base32 "1rl5vdxp6241mcqlqbx1vmavvk3rzkpwf2sgyj0sisd1056i50i7")))
   (package
     (name "publisher")
     (version "release_1934")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1i0hmzb51zqffys9dv38436sf2791xk4x1727i8a9y2s3j6krvdh")))
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
    (hash (base32 "0ilpwi8zazdcl186q01p5315vb6w0wal1ydwvinzwgc4bimf6h6f")))
   (package
     (name "publishing-api")
     (version "release_1158")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "18l4immlydmdj6adw0ws5bxawbxwrf0vzcf5883ch8g4bg859m4i")))
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
    (hash (base32 "0r5j83ygpdiz60vf0m9zmigcm02lnnz7ig329gpcs79fwkh5g03p")))
   (package
     (name "release")
     (version "release_278")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mk6m3pw2wgbkkjv31m1002g1k3v6mgfb4x91nz51x2yv4par8zk")))
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
    (hash (base32 "0v9cvw32jvw6lp7mvk5s5zj3sbf18c0vnd2dkgv2k8lwrvkcks0w")))
   (package
     (name "router-api")
     (version "release_147")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qwr1ynvhymb5ikb6mnd13pkwrngk29k7clc9sf2jrzmk2h8lpri")))
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
    (hash (base32 "0j4dz9pzmq6l3arlqiip6dqn4k1d3nyd71rvd3v45dzlahzdyb6c")))
   (package
     (name "rummager")
     (version "release_1724")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1i1zfczgh8hjj55yssq9ml6rzsmxnl574avlzy7q61pda7f4b41s")))
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
    (hash (base32 "0fyja1swv37n4as6qyf74lyky2jqhmhvjjzj6m3vlfdg6viq69xr")))
   (package
     (name "search-admin")
     (version "release_149")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06ccgrvjvd0aad11ca4ba9pb7lk8qdj632kdr8675rxg5q8gs8ak")))
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
    (hash (base32 "18aljbjk2k4xyjwhj8gfbmx7m368ckd2xsgdiy2jzfch98lnxa7h")))
   (package
     (name "service-manual-frontend")
     (version "release_127")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yjbdxk05pahgfalbz7v4iik0bh8qjwndfs3ixqr6jqjvql89l9l")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "16l281pvkxqqcqfd8q9sha4abf069plwcnx9r4gf8l32lc0z035h")))
   (package
     (name "service-manual-publisher")
     (version "release_340")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rl4xgpzs1af925cizy3vl4n64zkzw4f30z8ba6v9a748pdi1k04")))
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
    (hash (base32 "0hz3icawy8lz5nhnar6fnyj4li9vp8y6r64fc92h27k1slaqf8la")))
   (package
     (name "short-url-manager")
     (version "release_180")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0svbbhvjbydm9gr47d94nj3i84rn80hmfgj6fz76aqlqkac1mgpr")))
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
    (hash (base32 "11zwlrl00sxs8gi1wrgbrq5gcvqy7if8zvqjyk48gxl68haln2as"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_1001")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0s16llsn6cgvr9mg34mk6fn7f0zpvi0harwq2my4wxwhcr1spifd")))
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
    (hash (base32 "0n1c5xw1pjhvcp5vxnx5kbig9f5wxanyzxsvlxgr77zhk1pqrawd")))
   (package
     (name "smart-answers")
     (version "release_3901")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xnm243ym1msba6w7h4a9xqd5rf6sccqjq4qcygii41fsfb22kws")))
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
    (hash (base32 "1spcx16rq3xqni9kxl890in9v6h9kspwaxzmrdm3ph85q13y0f9y"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_933")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vijbzq8wl7pm239ijg0x25ja1fawicvz3kscgpacwwch20mdsvq")))
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
    (hash (base32 "18n98slmhmans05vxn6dhy87g61g43qzg7yq8knkpsdpq4p9p2wr")))
   (package
     (name "static")
     (version "release_2829")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0dgd4yz73rv78c256gakh96n35vsacw46wxjjzpjh6x2kbkyp0zb")))
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
    (hash (base32 "0ld4wvmg65141m4kd9mpc9k5gbmrzrw14aq4cpdabhkd3r4mcp7a")))
   (package
     (name "support")
     (version "release_650")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13g4ahdbc1x2im11sl6ji826masq62daygijl0rk9xzjp37sjlfc")))
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
    (hash (base32 "0w9s7dd9iw54nvapf2f4syjvppbnjlra92ms9qxwfb7hn2akn2i6")))
   (package
     (name "support-api")
     (version "release_176")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0vmsddiydplxihnn39yw0bnkgvhy7fxq6cxzvs5icgdc8v4na7fx")))
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
    (hash (base32 "1wqycwrsmp9xjm0xqjq01j6kzjjkhrxr3pspbh7k6daf08by3nfh")))
   (package
     (name "transition")
     (version "release_843")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05ljzzj6jc6m65y7allms038vrl695sk8l62n8vvz31z97gj8s44")))
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
    (hash (base32 "07fqvhxfyzdvs3c9pb8bp9v0s774qgdlnrvldcpihdqmk3wxjdgw")))
   (package
     (name "travel-advice-publisher")
     (version "release_352")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rcbzvqxj87dlprswpg2cj7kpaiiacs066rwr0z7ymrmi84npcb0")))
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
    (hash (base32 "1b9pmmddssbni9441cigim10i3s9vx0i2c8akcbr6xsgd5pfn2l9")))
   (package
     (name "whitehall")
     (version "release_13373")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c859mmqmwr8nyjwiymc0w9m2c4mqj636svyi3ly4cxc2xifhnay")))
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
