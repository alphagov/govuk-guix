(define-module (gds packages govuk)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix records)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
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
  #:use-module (gds packages utils)
  #:use-module (gds packages utils bundler)
  #:use-module (gds packages third-party phantomjs))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xzrsf3bx666x21q78smj46wb7fh7v5c1dxqxmx8syljjql9s77j")))
   (package
     (name "asset-manager")
     (version "release_92+4")
     (source
      (github-archive
       #:repository name
       #:commit-ish "66574074ded971daa96b1a3c97b50d50c29c2c10"
       #:hash (base32 "02rich8mfivxgcv41a0284m6521gg0wv9c01gfgph419syf1nin5")))
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
          (add-after 'install 'create-uploads-directory
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (mkdir-p (string-append out "/uploads"))))))))
     (synopsis "Manages uploaded assets (e.g. PDFs, images, ...)")
     (description "The Asset Manager is used to manage assets for the GOV.UK Publishing Platform")
     (license license:expat)
     (home-page "https://github.com/alphagov/asset-manager"))))

(define-public authenticating-proxy
  (package-with-bundler
   (bundle-package
    (hash (base32 "0al4dn0qjfhyap1cha4r7ix3vz3z0n2g3hb9004l5ifk520mmyi4")))
   (package
     (name "authenticating-proxy")
     (version "release_37")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1cxyrmr1klrh1kn9c4414g1k6whlk5hf0pv911q95zizvq4x6nyk")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
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
    (hash (base32 "1v6bcgmfsb1ffa6071qz7kqj5g8ihq5rryxl25k9dv6rx3n4knds")))
   (package
     (name "bouncer")
     (version "release_205")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y6s4hfvnrf7vs9z3hssvk8lc3jx4jrs855n9s2n7naxaqyq6dq2")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "1a6ffra7dlp681x8wx0ipj5c3ibpmqq2bf58icd4sdsgbcf8w96c")))
   (package
     (name "calculators")
     (version "release_187")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07004a5c2l93vkg737033fj0dwh3pkcad6i0pad6995bf2rwqm97")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0n5vzakccjy4bbw9lrvszakaximcq7sa9g6ayzd8imhs6j1xnml9")))
   (package
     (name "calendars")
     (version "release_422")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y3y4s1scrq9h0idas2v54hn5y6pd1r65ar24y2jw4lvlls7syq1")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1bac6pxn20g2x15935bghqr1lysiz67kimrl0bv6k1rx52jyv5m0")))
   (package
     (name "collections")
     (version "release_329")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0i18f4lbszahdbkbr8858f9cv5bj02sr5i05q4lswd0np8aq9kk7")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "19px7wgq1mp9ryrv7p8s1a0icln6lx35hpjk021y4xf9yiwhsza8")))
   (package
     (name "collections-publisher")
     (version "release_260")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0im5ykn7j9qyq5784cbnvyaa88vi6cqwx44vpaps3hdfgp84f6zx")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "Used to create browse and topic pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections-publisher"))
   #:extra-inputs (list mariadb)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "05gi6jcaprvbwfi2nrhyw9hsml5nycndqwp7c098dghk7mq1mpxb")))
   (package
     (name "contacts-admin")
     (version "release_351")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yqciyq1mmnbcv6n8jp9v0ynwzmjyr7infwghp0z1z12dibm14w3")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "Used to publish organisation contact information to GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/contacts-admin"))
   #:extra-inputs (list mariadb)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1snza9nmb79yw543k7pxxyiklrgvaqly69mshf218smgmr0q3j91")))
   (package
     (name "content-performance-manager")
     (version "release_99")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13ay5gdlybkw15dp718plmil9wv6x36ibfyqbadycqy07j2q2v1j")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
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
    (hash (base32 "024l67my3qw27vrys9mw1yl8cx564sq6c1qnimj692lahv8h5d4d")))
   (package
     (name "content-store")
     (version "release_641")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rwm6frzdzrh7wz2lllr0gp1qg5d2jqgpnd7z1w7wkgaxzfib14m")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "1niyy0raigfq9jf9ayy4r7v2lldnsn2x795sblfn1q6jx5awcj1b")))
   (package
     (name "content-tagger")
     (version "release_422")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0j90nhxagqzfkakd36c3f73ay3sdnynfix3fgn41k9n3yqhcl9cq")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql)))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "0n1bz9c3nmc571wmb0n96hbbfzk3mfgsw4d6bhla22p96sfjpam9")))
   (package
     (name "design-principles")
     (version "release_871")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n9bh0lx8s9l95m0caffnyygh6fggglk8x6wlqyax3ycx8jyzbdy")))
     (build-system rails-build-system)
     (arguments `(;; Asset precompilation fails
                  #:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/design-principles"))))

(define-public email-alert-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "132fq8d1cwzdsi5rh1jbwsm1rh0rkggsrs74d7fii5xmlmp328xi")))
   (package
     (name "email-alert-api")
     (version "release_181")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07k5zcmi29ygj9s4sy38sj0ihhw2gx4glkdc7f9dik0dy37yxbih")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-redis.yml
                     ,(replace-redis.yml))
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-api"))))

(define-public email-alert-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1c6qnaklid2vsg2iyc21nsza7my7vsvxh2lp5s0x18d9dwqd9d31")))
   (package
     (name "email-alert-frontend")
     (version "release_44")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vbml50gsqp5lfg1lg34q1pqpwq8njjpw93nvb5ivsnfds2wf4qv")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "0jfl2a0sdn9qvp0xkfxd7c4cpbn73gqnzcnpcy7r057px2bj8plm")))
   (package
     (name "email-alert-service")
     (version "release_78")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h0fs8ad5140fnjbwhv795pbvrs0l03lpr661fknx6k8zy0sbkh8")))
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
     (home-page "https://github.com/alphagov/email-alert-service/"))))

(define-public feedback
  (package-with-bundler
   (bundle-package
    (hash (base32 "0c40z42zlh0aj1gbapqzr71hhlc4m23yadi9jw3l42cqgwh3w6wx")))
   (package
     (name "feedback")
     (version "release_301")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "118znn3b63i5pcbk4p262yryjdhx8bwvqa8sri4n8jidwai6hc0p")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0mxs91wzfkn8wxck2fgi2njjq5amqygh2vv4r33p6lg0nz1pw9p8")))
   (package
     (name "finder-frontend")
     (version "release_309")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y9zdk81njsp2nslahspl62rwdq7da7il0my46200z2jxlxxyihc")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gqvw1dyaypfr6cqichq5zmqg7k0454734mw6ryl74ncdq8ywy5z")))
   (package
     (name "frontend")
     (version "release_2151")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x9dgcqwpcnpkbsdjib4wd5xjg0cq42a8z86nj1n8l8s32mv824s")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z0vjlsnz5xsh38p1xvynsnx99mhpzz5xfcxzvpdd65yzqqm5jwl")))
   (package
     (name "government-frontend")
     (version "release_362")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1nzw7cz29dla6v8safypxjdir2i3m9zfzq347llg05aswmnss2b6")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'bundle-install 'replace-ruby-version
                      ,(replace-ruby-version (package-version ruby))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_606")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1h1xqfx9s38x1awszz41h6p60mgvrinb503cg6h13p2xlg1wb265")))
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

(define-public hmrc-manuals-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1qbjqy3yxxnqg8lxnnm9qj6dmhjspbcy9cisf1hwrc5v9m09wc7c")))
   (package
     (name "hmrc-manuals-api")
     (version "release_181")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11l0wkny9nxrpsdnqnhlfb7ihvzi0sdr7ywvvj4vr4m68jvcaddq")))
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
    (hash (base32 "1fsmkmwgzblgwl5y4f68xy1kk5jp64hrz2f9q1rv00hz37x3mfjp")))
   (package
     (name "imminence")
     (version "release_318")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1a4x2kgfx4vna9f2cv8x8kriqxmaqn1ni0rx4r2fg0r1ppxsn8dg")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "10c8npzfidrs9wk29nb4b1vba2islkf83i57c410a3z0xm2hm51k")))
   (package
     (name "info-frontend")
     (version "release_75")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0aqhghqyli8pbw612k1a42m2v7dsy93xyfqxqh5mhgk7x4mxsxzi")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1bbnig6i77d22d24xl36p4hyb1gyn836875mxpchpzb82y81i43f")))
   (package
     (name "licence-finder")
     (version "release_282")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1idvi9zb7cqdhyszkmm2jwjfq6w2wh6lv2arppsrh2bbgy3ilwmk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ga8dz194gf5l7xxdacgqj298wvlgfhkh36lba18jlk8ja5mx45k")))
   (package
     (name "local-links-manager")
     (version "release_118")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xriwj3y64izjn4agi0sijksa5xv2h5iknb5ml2vhf63p5a9mz4b")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/local-links-manager"))
   #:extra-inputs (list postgresql)))

(define-public manuals-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1s22a6aa77410h7zgq750g9n2mqrviqgdnnak6p39lv4k9fvyd6h")))
   (package
     (name "manuals-frontend")
     (version "release_201")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1snvnkx4gf58b8sx7f5dbrq2alw6l9m4vk6v190qsmgr37pf7j36")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nyxa2hz3xbsirv3cprm7yl729zmcljbf0p7mm1ibw9s0nz8qgy7")))
   (package
     (name "manuals-publisher")
     (version "release_971")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zi5yrf1cviqfvdial89g9n0isxnbfwwplrxjyxn77bh560c6lmg")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml #:mongoid-version "3")))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-publisher"))))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "07mxwq9rcv471qz0r7pc7dmrrmryzhy6g8sxy00wvwvfvag84dbh")))
   (package
     (name "maslow")
     (version "release_197")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n161a0rvbl93ncyzgk8k421fhqc8ail5kkwdh4pwzf9igc8a494")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml))
          (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
                     ,(replace-gds-sso-initializer)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/maslow"))
   #:extra-inputs (list libffi)))

(define-public metadata-api
  (package
    (name "metadata-api")
    (version "release_78")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1vh4xdydvk8mc3cmdpkhmcdhlaigxmn91fp1np6sh9alzgc2i3cr")))
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
                           (cwd (getcwd))
                           (gopath (string-append cwd "/__build/"))
                           (repo "alphagov/metadata-api")
                           (build-path (string-append
                                        gopath
                                        "src/github.com/"
                                        repo)))
                      (copy-recursively cwd "../metadata-api-copy"
                                        #:log (%make-void-port "w"))
                      (mkdir-p "__build/src/github.com/alphagov")
                      (mkdir-p "__build/bin")
                      (setenv "GOPATH" gopath)
                      (rename-file "../metadata-api-copy"
                                   "__build/src/github.com/alphagov/metadata-api")
                      (and
                       (with-directory-excursion
                           "__build/src/github.com/alphagov/metadata-api"
                         (zero? (system* "make" "build")))
                       (begin
                         (mkdir-p (string-append out "/bin"))
                         (copy-file (string-append build-path "/metadata-api")
                                    (string-append out "/bin/metadata-api"))
                         #t))))))))
    (synopsis "")
    (description "")
    (license "")
    (home-page "https://github.com/alphagov/metadata-api")))

(define-public need-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ibaajh3z5i921lsqlm94xgc5lvhbr1b6c7yc2r99cahygcwhj8j")))
   (package
     (name "need-api")
     (version "release_146")
     (source
      (github-archive
       #:repository "govuk_need_api"
       #:commit-ish version
       #:hash (base32 "1mgz29xmi4sgbbwal9z9rbb5f5drjgq5hj6kalbhvn8pin0glzqr")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml #:mongoid-version "3")))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/need-api"))
   #:extra-inputs (list libffi)))

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "03ipdp9zk36mb7rlqm0wa4vl469j60yd60iaivprn8h5dl8niiyv")))
   (package
     (name "policy-publisher")
     (version "release_174")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1p2kfciq1c7ykncghrhfxiwqrr1lw5jcfxj4ikrdlcvxv01pasq1")))
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
   #:extra-inputs (list postgresql)))

(define-public publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0v6wnvyfqgcajxsk0s31ck7dmclwj1hdknfxq7d09h94khyin64y")))
   (package
     (name "publisher")
     (version "release_1795")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bdq7lid3zy2z9qn03zr2k03kwniziz3b2n8xnlwsar8zfi7gyzh")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml))
          (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
                     ,(replace-gds-sso-initializer)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/publisher"))))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "06gki259z2b2zivxrjf5k8cpwmx8qhmaaw25acj6dy605gpyhm09")))
   (package
     (name "publishing-api")
     (version "release_910")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "0qbcaynbvhd5561n3d8mpj8y4rms5nz3kggirglr28n3ap62lnkc")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
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
                   ;; Required by the pg gem
                   postgresql)))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "17svvb2g3g3s06v16y5pm7jz3anrjn2yg9xjwvkn7s754v6xswbi")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "d21b889b9e9af2f8dd19d944dc12dd25f4159c15"
       #:hash (base32 "1jk26a7c5d760rgrbb22xrkxy2sqsvlan2wcgvw9slff69v95w10")))
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
                        #:log (%make-void-port "w"))))))))
     (synopsis "Suite of end-to-end tests for GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/publishing-e2e-tests"))
   #:extra-inputs (list
                   ;; For nokogiri
                   pkg-config
                   libxml2
                   libxslt)))

(define-public release
  (package-with-bundler
   (bundle-package
    (hash (base32 "0qynlzmxm25g2h7wc4mm72z62mq50gfpyzz4ingdy17rvwdrrnqr")))
   (package
     (name "release")
     (version "release_257")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15gcg7ss7yhanynz6k3jfsdlkrq1g69705kf0y77ccrk8naw66vn")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/release"))
   #:extra-inputs (list mariadb)))

(define-public router
  (package
    (name "router")
    (version "release_153")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1sw3g0r2kk78c8l369zmkfjn9d4p5nh2nlfc2s2g233s1bnyg5ga")))
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
    (hash (base32 "18xi02azlxjqll2h9fqhy3rgqm41zhkkh464psli0d6h3hgw3phk")))
   (package
     (name "router-api")
     (version "release_116")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01ny27vkshiwhkav5h52bkclkzlvnxdawalkmb9sw8y3c32shal7")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))))

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zzr0sj9pmhrfyjlfhxyfk0hh6177xyi2w17sln6sdljz8m3hpz9")))
   (package
     (name "rummager")
     (version "release_1356")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a5f3dbrsy457svygdvbgnmywwr26p14cds7p9qsmw6ykhbrfbal")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after
           'install 'replace-redis.yml
           ,(replace-redis.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/rummager"))
   #:extra-inputs (list libffi)))

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "1yqjh9hf2j4phkmyp3pmpgc8x70hl33yfnypkjqgzj5s9rb1m4wi")))
   (package
     (name "search-admin")
     (version "release_96")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mg46ba4sbfcyf4fn141izz0ba501ldpq5jpj8j0whfphd6sj6bh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/search-admin"))
   #:extra-inputs (list mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "114g4mk8mcfg9szbxr6avpp4skw8b5nd3j2rfvm0v11pwn65010i")))
   (package
     (name "service-manual-frontend")
     (version "release_102")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gjzv2j7anb5x325r233wr24r8si3hmv9m8ngjjsj3cgl8k12ggw")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vxji9g2vkwncm6m4l2057071m6802nyzy2lvdhpg28p756k7hc4")))
   (package
     (name "service-manual-publisher")
     (version "release_297")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "040k0lbrwr41ip596pfi92hwq5izxxff33pdhc1ndcx02dyy78zm")))
     (build-system rails-build-system)
     (inputs
      `(;; Loading the database structure uses psql
        ("postgresql" ,postgresql)))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-publisher"))
   #:extra-inputs (list postgresql)))

(define-public short-url-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "155mp2xi968hhzkk3rk6rj5rd4qgcp0rb6n01aim01515xzy84il")))
   (package
     (name "short-url-manager")
     (version "release_108")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "106izjki844w0bk87ds1v49n4d9bvp1rn97aqknfrsz497ckk8v5")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "11yz7ngd38b2skvmlf6qw6m1vj493i2jqlji9sz5rn124szlhpqm"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_911")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14r9jhbaksayn0nz03w646a2j1j22s338vypsjr2iw35hpcxjlrr")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
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
    (hash (base32 "191j4ds122rnmvhwbmv1q794mzcxvjhk7rr4j5zlkn1m9zikskqp")))
   (package
     (name "smart-answers")
     (version "release_3607")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "109hqqq9k7jkac67vz56j1g4flwl936smfr6dwyhf1m6w4620z5v")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "01d1v3ays6r7raac5b66a77p4bmiyfbym4737wizjp8sm0ywk7ia"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_814")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ghn3dmc0xby6pypnm6v31rcc34z4z1d29ydkxbp0n32jjbviiyl")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
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
     (home-page "https://github.com/alphagov/specialist-publisher"))))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "0b0ss0mpwzbjppraxyjhfzmaihi7lpqz99n2qzp3qhfxzknnzsca")))
   (package
     (name "static")
     (version "release_2599")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07ss90rzkfb92cglppqfa7yy7i8vcs9159wry4i3whf2q4v9zjd7")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "06i55afi48lhr6cz46cxm80gdf08zm9b9rdcl1696afrmx5mjgg0")))
   (package
     (name "support")
     (version "release_591")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cxvmbjg0pcdw2hbn438aym0ck9fplnx6jip5bjswkzk95p05ncl")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails
        #:phases
        (modify-phases %standard-phases
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
    (hash (base32 "1gndr0jpd0qqic6lanbgm0xk6pqk1ljzjlkrb51bw6gmp28hiy6w")))
   (package
     (name "support-api")
     (version "release_126")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0mxqdbkm6m8ifd05xsprvikjq9g62j13lhjlyffrj5gxsc8mbf67")))
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
    (hash (base32 "0hvx8piz9sj1dq2ky6q22zzsjknw7x11ysbzipwhn9a0agd316qp")))
   (package
     (name "transition")
     (version "release_788")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03lhnscblfwk8fldipjwllmwncisb6k14lkn6zkl0dnq6h8l6mjh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0biin2wsjl6gbcnxxfmsyfqg0lcz2nfz3r215nc1kq39pmaf1nf5")))
   (package
     (name "travel-advice-publisher")
     (version "release_256")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0f2lz99mkgxzlj75dcc3vl1szl4id6ywximqvp2cm76h5niv68jy")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml)))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/travel-advice-publisher"))))

(define-public whitehall
  (package-with-bundler
   (bundle-package
    (hash (base32 "0l7dhl6wbqyfg2pvy93abbbnqi0vfm79p9pqnjw8b8xjgy964db1")))
   (package
     (name "whitehall")
     (version "release_12823")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14xww51gp6pzbdhdd340ncacl10bwwwjbdx666mr81bcss58b5qx")))
     (build-system rails-build-system)
     (inputs
      `(("node" ,node)
        ;; TODO Adding curl here is unusual as ideally the gem
        ;; requiring it would link against the exact location of the
        ;; library at compile time.
        ("curl" ,curl)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
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
