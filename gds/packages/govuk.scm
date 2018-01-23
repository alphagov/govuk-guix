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
    (hash (base32 "1hkr6a3fg6f5bc129mmxbkvcmmlvk91iwhhjl92d5nl5zkjp2nvr")))
   (package
     (name "asset-manager")
     (version "release_230")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wc1bxbxj2ycs2xlf6nmd5nqcc6a72ap19bj7lxk806l6kprlz32")))
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
    (hash (base32 "09cxv0nln2pwipb4yxcf84p55lgilp3h2pnfcdi7ln95j1na4kqr")))
   (package
     (name "authenticating-proxy")
     (version "release_54")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mb91gkrmh920n52a9rwx4wqscnfdjzqpky72msc5052z6g1cyyw")))
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
    (hash (base32 "1yrcpvhy6sjrs06p050l5bwpzg22mvjkr7sfw6rdmajcnmmxggw4")))
   (package
     (name "bouncer")
     (version "release_221")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16z17qya0wl48mbw290p37jfbra3fvd5bfr9yf00a7mwvk1i8z3k")))
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
    (hash (base32 "15v4gdskm0fyc80qqmr0wq5m231wvdj4l4afh27ar01d9a7bgssd")))
   (package
     (name "calculators")
     (version "release_229")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n5vi0xp2nlddfb5mvalrzhc6kz0qq6pg7l64slxwnnf9vba5mci")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0mj734a7iainjnb471rga2n942dd6rfmvcbmm3ikrhj2qwdpg3m6")))
   (package
     (name "calendars")
     (version "release_452")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ixzmn90il6qa4a2q3dph3v3ywrlz88nn0fr55nvsvzs3bnw74jq")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hj2n8j9w9p2gg45a9s4sg7kbxpb5rv951q80jiabl65vnvbq38a")))
   (package
     (name "collections")
     (version "release_433")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1b1g597q0crscigfaw1v71zjyqxw6iwyzsnihlsb3nns57m4nh28")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0sr2qqpzy60i3dhr3srr67iysn8bvrv303rk6xgf6j8ww1wvrlgc")))
   (package
     (name "collections-publisher")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06ixv3vandxsxz3s59xzvgwxi9s6jxi78p8nxj7n7qvas2xybi3v")))
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
    (hash (base32 "05w6scqk343lkivkxaspv68456mzbkdzrss395bv1nzkqpb4zr06")))
   (package
     (name "contacts-admin")
     (version "release_388")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kp3hdk9knkq48d34lnvz7yv7wy8i78l1xq2lxb18vn810mr8xc8")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(;; Asset compilation fails as it tries to connect to Redis
        #:precompile-rails-assets? #f
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

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "00w1l1yc64cdag9kws7l0g9qzbdkyxzhgym3vw8xfryylxvdkgrp")))
   (package
     (name "content-performance-manager")
     (version "release_364")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bbsamdcf91lmh2j5jpsdd29lb8f54qp0ynn3h4fipraylx40csv")))
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
    (hash (base32 "0rhb0dxw65z6cvm45ybbwz2fmll1dd1dicby8i3dxnp9ilc94dsp")))
   (package
     (name "content-store")
     (version "release_705")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09w2b0fzgvrzz877hwc11visnsyzysz513mgigxbg0cx3shhgfb7")))
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
    (hash (base32 "1rmyz9yjva9p4xd51q34hi9bsd7lw4nr0lbmhl904hmfc31mibcs")))
   (package
     (name "content-tagger")
     (version "release_680")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17cb5ypzxs45q3rqwzx98n7l32gqayssfmjsnbfwwlvcg684lajw")))
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
    (hash (base32 "1c42gcq98pd9hxk2iq0rnvzjrwnyq8vxa31pbqwsbkjzxal057ib")))
   (package
     (name "email-alert-api")
     (version "release_370")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x17v0qbhyf4vg9ccil0bkmwd6abbr56l643h9sl3faiwkn2hyfi")))
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
    (hash (base32 "0p1nz0gq81rmzm22flhvx6mzs6n3hdwza23jg6cvrfawgjndsj79")))
   (package
     (name "email-alert-frontend")
     (version "release_92")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0nvcxqrny40qfglz5pfqzdl0yf1s4sbl9p6xixni18bbji6aj8v5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1190by8wppfq9qky3dkrln1ic902hbs8cvhrpy5z7q55flrjxz2l")))
   (package
     (name "email-alert-service")
     (version "release_112")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02dinwy9jfxv3nnas03s64d0szbiscmsawp60vrpr4ywlzgazbrr")))
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
    (hash (base32 "1hxz1riw1sj1fzxsr9byby266058jabx8m86zhlp31vkqv9i65pp")))
   (package
     (name "feedback")
     (version "release_359")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g6yhrlaqxlzy84nyx6068f9f3gccdz5rhnyalyk94gb7mixrnff")))
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
    (hash (base32 "0w2fk3wcn9z6kaf5gvb0x7400bdnsdfwwlrmmzqgfg9m1ar7j34f")))
   (package
     (name "finder-frontend")
     (version "release_374")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02lx2lpn6m37jqx9cml5n0b2bd9yhwgi0gazklhih74gf9my1cxl")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0l7j830ibrv25phfn4945piwpvjlfkkqcnzmhm12bggf4cchsdsl")))
   (package
     (name "frontend")
     (version "release_2812")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gqknxvhiqzmv3f9bv4crix7mfpqc0w2f9zhpcfhbnc7hnp9xykh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ic1zj47j13mvqp7n3vfxjp0a8jmf2ki74yzw7my4cj9y6nw35ip")))
   (package
     (name "government-frontend")
     (version "release_612")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p40v751aqna0sykx5d4gxjzgmvdy7p9pas7qnx46bdyi4sv5ybf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))
   #:extra-inputs (list libffi)))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_682")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0svxsd3dvvsckjlk3j1fg9b2im0ha3qmfym1sx63qym3zammhvv7")))
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
    (hash (base32 "0w6vjsizywdhgpwz9pbcvxi8wyyrhk4d2bwhincknrf2dzsp6gqm")))
   (package
     (name "hmrc-manuals-api")
     (version "release_224")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hqqq8cd08nxhx9ib4vqwrib3ilbir4h6wj49nzznv2nr9rx1wjk")))
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
    (hash (base32 "0n6k6pw0xs4q0qajp8brjv7i22h1bjmf7b90b12w9v1hz0kyjwfb")))
   (package
     (name "imminence")
     (version "release_333")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dw5c0527kif7hi62wxrjcbh9zvfin8lch1zfnz3nfbnja9w84hx")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wz3d6vk0rlr3gkgq2n4a5zanydn57z4mxhgjmxrpgp2zxa7mdqb")))
   (package
     (name "info-frontend")
     (version "release_99")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q1j06m4mlp38x608s9sa0jw43mdp5m7fffrm9ys0mklq5ay6bik")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ybcd5f2dk3ls58g9ha4bwgpv2bkab0zsfmj21w8zlf6gm9nck2a")))
   (package
     (name "licence-finder")
     (version "release_320")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fs8vxbagcjkxd8xqm7s6llb4270lbzc417q6lcmvz85n6fgl986")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0rc6md5c3786rsw7dg0wylxlf8nh0d7w398qx6bqss8fdsnhi435")))
   (package
     (name "local-links-manager")
     (version "release_157")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h5mjw13vy1gf4z5bj1ix1zq9jxlmyhyw7bd33y7yxan3hai57cy")))
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
    (hash (base32 "1xmw2fwqn6bc6wqr4vr4ijslbg0slybj439qgwmz7qwv67rpw9pd")))
   (package
     (name "manuals-frontend")
     (version "release_250")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xwyf56q568c6q8dxvxgalkiyj2li0jh3ln9rnyq4ywxkr69yk3c")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "150ml4jlk0lc72k1vxx2p0wkwfm0akrfqlrkfz4gs3n9wf1ys7k2")))
   (package
     (name "manuals-publisher")
     (version "release_1042")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "133vbrcjq9vy8yqq8akvkjgq29b7f9aqz48kc6rcfwfr53x4bygb")))
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
    (hash (base32 "12b4mfc5fj5h075zf0p3qm6b4fc2fpchn7icaal3szn04c4a5vwq")))
   (package
     (name "maslow")
     (version "release_230")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1glh9w6qvlan9r1blcf283r8r65fwhdxfzm0avr8sb1dalrky8s9")))
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
    (hash (base32 "02i12pw9bs711f3ipzzb07q310imhc3v5as5gx4mzic1g6f7lgpm")))
   (package
     (name "policy-publisher")
     (version "release_220")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p3r105vdd065gwqwc8l4rv5rf926riw9q9xhq3vknnyv72p66qm")))
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
    (hash (base32 "06fi7qqi57g1rrcyym4i65bhk0mjdpd80339mq9bwxv5rc676qi0")))
   (package
     (name "publisher")
     (version "release_1898")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mfg5ybpyimv4j6z6j8cnbap6zb8s1imj6wrdfnp2r233s0b66bc")))
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
    (hash (base32 "11a1r5xnkb3fn2dak596h5q791w0wjcf6ndx34r2ll51l98gg3rh")))
   (package
     (name "publishing-api")
     (version "release_1105")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1xlhhq5xm4rgbjjcxwwhlnx3im4aj33xcj010vw6x5m5dzpafxxb")))
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
     (base32 "1ysv8781prbclmf6ralanba5bf0wbwyvv046r9fpacpy950k3yj4")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "d04943e00ee58c20b49cab00c608dbebdd065700"
       #:hash (base32 "1w34myh9gd4w61dg2xlwvzmzlz6hnil0bqs56fcylknrr8rhdiqw")))
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
    (hash (base32 "09s8ax69xk6c744lql8rkkgpamcp5snwr70w70jwjha30vw02s0k")))
   (package
     (name "release")
     (version "release_263")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00fqkg75ay4r6vikv03b78l530az7v7nba3fg993v7fad5p8dj1g")))
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
    (version "release_182")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1ri06l2c7hibm5ds994lacdmbkp5965jzq0mf7zj8r1n7gn4rpw7")))
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
    (hash (base32 "1j9xrwmxbr9m52aan78idnbvb7kjwqhl7fj3pn94s04cr06n11cw")))
   (package
     (name "router-api")
     (version "release_133")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06crqjphh0r1xn6f9z1xpcdb5ljnbqxzz31s6lk3slyi6zg2hkpg")))
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
    (hash (base32 "0ddh58n7xyl50qcs9y1l6gy7xgpm7wslsgv5n2bxh9k307c5lw2l")))
   (package
     (name "rummager")
     (version "release_1685")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x0s5sh2rz81wig8vxi6rgqjk90ck0wa4q9k7n5wwskycb0yla4m")))
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
    (hash (base32 "1vrs7w0lsbbx4sfcq9b81ml19nra1a3k9fp5f5nnilwdfs7a9gs0")))
   (package
     (name "search-admin")
     (version "release_130")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0syzmmhxffy016x3ph2lnwkcy6bvz0x53rg552c6h5jq30vsx2qw")))
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
    (hash (base32 "0mpnc37gl0x0l9wkpbp3j71y4dnsy0rz9x9cgav7p908hyw0jlzc")))
   (package
     (name "service-manual-frontend")
     (version "release_107")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14jqbm6ypbw8bngal6pwaysxjglnvz0kbmwr9m6fpah3wvsck14g")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))
   #:extra-inputs (list libffi)))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hq9bvfvhm15paq43b3vxviqi18dfn29diybg8v696sjd5bv8cr4")))
   (package
     (name "service-manual-publisher")
     (version "release_308")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z9k24s3gks2gnk071wi37lxjv83l0wnm96a2jfnzn10bglmycvf")))
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
    (hash (base32 "105amxxg9xmz5zg8yb03iy79cxlakciyj3rg43mgiiz5riaxqkbv")))
   (package
     (name "short-url-manager")
     (version "release_152")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zqmsfszzqk2sd14lm4a6ibaka0jvjhlbwh580h02h6qsjj05221")))
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
    (hash (base32 "0701gqwn0a7d7bh8dk3zf762flk4qwqnyv21xzhps8qwc337bal8"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_969")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03hpd4jm5diyhwiasbwgacaag2b924a99k2pmjkyq3wdp9vrnpya")))
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
     (home-page "https://github.com/alphagov/signon"))
   #:extra-inputs (list mariadb
                        postgresql
                        openssl)))

(define-public smart-answers
  (package-with-bundler
   (bundle-package
    (hash (base32 "00sb57s6ix4vhl1wpq9babxhs5azwnvkdz2dp7ml4lf0czgd3pf2")))
   (package
     (name "smart-answers")
     (version "release_3835")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v18vmr6ibxmq32wk18nzbzk90abk9vccnpyarnwn3y3g6i06lwc")))
     (build-system rails-build-system)
     ;; Asset precompilation fails due to the preload_working_days
     ;; initialiser
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))
   #:extra-inputs (list libffi)))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "05xrd3r7590dyb0iqahbzqw3m1jv07rjcyxmg6ksfxrprrviyk6r"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_892")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ifyhgrnvbgizaxpvy1ki9bahsi4hgdcc58pziz6prjmanm52h8f")))
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
    (hash (base32 "0dixzn4nn4218hpy4iglanyjxqcm4x4x2w911dfns6y7jvzbdy7d")))
   (package
     (name "static")
     (version "release_2768")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1350n6h3nwxcad4y35pffnqvm44ccjp1cj4gddx0xahi2ac74a03")))
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
    (hash (base32 "1fi33mdmviqfw9d4qs3ja8pwi03kig430m1s5r4q2lyhyzfifjmg")))
   (package
     (name "support")
     (version "release_631")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "031ksjl2iih6znf0ihz4h5mfvbn0gj6h4gdjdxfmcjlv7f87knkp")))
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
    (hash (base32 "1qhpv8qf2csydbvkz7m4ryzcd76s5qz5q7ss5c11mkrzwq0ydhbm")))
   (package
     (name "support-api")
     (version "release_153")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0digh5c9n0pjyvwinmsh76jpj89nqzqwb5k2rbwchhlmrnddixcq")))
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
    (hash (base32 "1cbwda5byf7nrzxlnazlpf6gw415x3fjx0r4ad98s167bihn3iwh")))
   (package
     (name "transition")
     (version "release_831")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n6dsrm5rp0h4yil6d63v585ncrp5kgnbz321x9yzwgg13nqnfla")))
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
    (hash (base32 "058z5rwxamsa3mdzgq1nqr11njc2psv7a9ia4hdlgrhv0cg2x3r2")))
   (package
     (name "travel-advice-publisher")
     (version "release_332")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0akfdb0jaxz2apncfl3css7kq8ks1l1w3i0l1il0d51d5xmbgy9x")))
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
    (hash (base32 "0k84r52r9knn1n1gdbm9ki2hi9vwa2wp09kwxlanrra884gc1n0x")))
   (package
     (name "whitehall")
     (version "release_13222")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jv2lcp0hxv70jrql588gf0q0vjflbbd6cbsby3r9sh2bvicqvhb")))
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
