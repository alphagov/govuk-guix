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

;; TODO: The native search paths in the ruby-2.3 package from GNU Guix
;; are wrong in the version currently in use, so fix this here.
(define ruby-2.3
  (package
    (inherit (@ (gnu packages ruby) ruby-2.3))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list "lib/ruby/gems/2.3.0")))))))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0a9akv1lvbb4gdfw5abqrf5ldq3szyqn9gaqakywiihqnzawvqsf")))
   (package
     (name "asset-manager")
     (version "release_185")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "062r9n95sj9ggrvy41ppzf5sw0b3vdajzv1phnsviwiszkw2mjm9")))
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
                         (mkdir-p (string-append out "/uploads"))))))
        #:ruby ,ruby-2.3))
     (synopsis "Manages uploaded assets (e.g. PDFs, images, ...)")
     (description "The Asset Manager is used to manage assets for the GOV.UK Publishing Platform")
     (license license:expat)
     (home-page "https://github.com/alphagov/asset-manager"))
   #:extra-inputs (list libffi)))

(define-public authenticating-proxy
  (package-with-bundler
   (bundle-package
    (hash (base32 "0fmrm4alqs2yl8wfry0pawahibxkk6l4frvkhgdfszwjffy9svy1")))
   (package
     (name "authenticating-proxy")
     (version "release_41")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0dhvipzya2gylm0lwqn9jicwf3n9z67bj0za6pi39c111h9jq8fz")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
            ,(replace-mongoid.yml)))
        #:ruby ,ruby-2.3))
     (synopsis "Proxy to add authentication via Signon")
     (description "The Authenticating Proxy is a Rack based proxy,
written in Ruby that performs authentication using gds-sso, and then
proxies requests to some upstream")
     (license #f)
     (home-page "https://github.com/alphagov/authenticating-proxy"))))

(define-public bouncer
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gr9xds4rjy97mv9llc974jpsnr2wc0h3znaqaf6v3132gg67c72")))
   (package
     (name "bouncer")
     (version "release_209")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0iwp4y2dh894x4zdfwlq7w7d6j7wn3lfzm5gwwjh8rf8i624mmnp")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))
   #:extra-inputs (list libffi postgresql)))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "1l08pn17nxdd0h1mzmchfh1zcy8k5px9gc25w9sik7828x6sk9vf")))
   (package
     (name "calculators")
     (version "release_208")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lxg9njd5m66dghc9r7ivnbzf9wzdb3bi5xb2djz84qzv7zsg0v4")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1kpgwng1hfngyk3w8js2byh89zk685cf9w5qfvl4cnadmcizpawr")))
   (package
     (name "calendars")
     (version "release_434")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bfmdd6z0gg0d32kn1s7b31zrk1xj54ylvpz8l1fkssqfpymppgn")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z27yrmim32ydvls3ysnd05vlnh1pmdvjqxf9cd3650m46w67fwx")))
   (package
     (name "collections")
     (version "release_393")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v74kccq99xs2l8l1661088sd488i28hw48rl25lm75wvx7s85c3")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1rl3s71wrc81dmp8gay73r9abnx3aqv8mma4m7j81xm2q0n9m0kp")))
   (package
     (name "collections-publisher")
     (version "release_270")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b36xpiik4rik7jy3h163rxhr79jbcnz28wc83qa29m8xw5v85sy")))
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
    (hash (base32 "015c5c59l9nb3v5ylqr9lin1vv2ny4yh6kwf5nn1i2z9n01s3nma")))
   (package
     (name "contacts-admin")
     (version "release_359")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v0v5dfy7mi4m2nmwr0ca3skxf6ain08qvzp2zr5m03ivaswjbqz")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "Used to publish organisation contact information to GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/contacts-admin"))
   #:extra-inputs (list libffi
                        mariadb)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vd34rq3vciwvkdml7z1wnn3mm3g6mqbrgszl8211x8qvc9f4cdf")))
   (package
     (name "content-performance-manager")
     (version "release_314")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ffw23mx50daadp3vlrfddfiiy7iyyyrqw2qr7gvszqmvvwa8mcv")))
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
    (hash (base32 "0m3d9wvlg5k8vq3jpw6ppzxlrq1amcpsp90n839j2wgz0mmglgvq")))
   (package
     (name "content-store")
     (version "release_677")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q62qvdjlzccmw13lgwgq05wyjrliwyqbagfdgp3mr7l2miz4jkp")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))
   #:extra-inputs (list libffi)))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hrsj6p17i38idpfdl7jy42y7f1939kvb01xh2nsmw1bknr4ik6i")))
   (package
     (name "content-tagger")
     (version "release_598")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1sdksx2mc7qkihbmj1a7s23cvmnfj0x0wcfcrrals698i48zv1ss")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql
                        libffi)))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "01gg6yy8wa1g1zmf4072xa7v1lvyhdqhhpi8w29krl27anfag3qa")))
   (package
     (name "design-principles")
     (version "release_876")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1brak8fq2406i666iqq1drm1lamcvfdjrw8lh25gp6y60d96nnif")))
     (build-system rails-build-system)
     (arguments `(;; Asset precompilation fails
                  #:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/design-principles"))))

(define-public email-alert-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "09p16i17z1jv6mbm9a45b96qp8d9zyw4xlbfmi8lxzmmqplgq21k")))
   (package
     (name "email-alert-api")
     (version "release_281")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q88abn2hwj5h8x2xll1759xcn68m0h8sllga4zrrd72dq6hhqak")))
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
     (home-page "https://github.com/alphagov/email-alert-api"))
   #:extra-inputs (list libffi postgresql)))

(define-public email-alert-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ns1awapzmvajzvpd5g51lmbhndj4whfhlb63hchnsf22b54335b")))
   (package
     (name "email-alert-frontend")
     (version "release_58")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kqqgxf8fxyzix0nqjbhrrhdh776xwqmz3www0ygzp7xnwxh2qnf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "148mbqdwinix2w2m71ag8hayfhxdmpyn7mwlx29vvkg3sx373ih8")))
   (package
     (name "email-alert-service")
     (version "release_92")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d0nzqgl7w6fdh3jcxrbsvhn0w0h8wflpfqi6v5qa3qjg8arx981")))
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
    (hash (base32 "17bh9xdsk0hky66d028d8p4x8q2dsa4afb867ivkzaylcfwwh8xh")))
   (package
     (name "feedback")
     (version "release_340")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03hrgyqpf4p914r8mcdqiz906l3jra5n8p5893abr7zwd6a4xa24")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "18m2x8yfmpf7hd4lks4sqv1xckdcqhy8rffp02a96y2gwdvyf4n6")))
   (package
     (name "finder-frontend")
     (version "release_332")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09dsc1ld0ykwk2ncjkmdwmdw33afrw7g5z7hlsi88zm9x5s5wdg2")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0sxwj68kqi2gr229d4hnqciqzm85qi8rxx6pj8588cp1w7cfaq7q")))
   (package
     (name "frontend")
     (version "release_2769")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0dq97pklf0fvkpzfxcarpj7d6gljp8qzljsd4m0lawrwn58vzang")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1v5bp1pqwq4qg8f189simn1kfdviv2253b0rp22x4cc1fxcy0j9g")))
   (package
     (name "government-frontend")
     (version "release_510")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08q4b1vx1n7lr8v3l06vhyza43c9jrgxs4lqkpbxh6bwk8svnbhm")))
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
    (version "release_652")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0dj3wi9qckxwm0jrz6svzvqcabq7dr9zwxcqj98pvrp8fqphsv0x")))
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
    (hash (base32 "1f0rvgbdx2xcdr5111wjdnrgmh8kazgsw8f2ap03h68wqj5dbqf8")))
   (package
     (name "hmrc-manuals-api")
     (version "release_202")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19wkgxk62c2nnzz8mdhxl4x87mzlaml8vm4bripg0jhyznlqi8xv")))
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
    (hash (base32 "1vxlmwqhg30l4zkpd0rfnfl4scgx9w2ir00sgzrsh9p19czflyld")))
   (package
     (name "imminence")
     (version "release_322")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hr93nfg3gnxm411l0v8skff9bkl77mqbb3xkmyf8gjlm4izmxc6")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "17yzyzddcqgz9z0vf3h7m97qivf2fsszz4xpziqi6j6wj8nz63yq")))
   (package
     (name "info-frontend")
     (version "release_85")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ix4ri739sigj7ycm8ylsdp320cnllr1qvawkdkb8bqc27azvyxk")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))
   #:extra-inputs (list libffi)))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "06rhgch7mhia739fhjpf6plbqwpaiy23k88sjxwlmsvp7gvswirv")))
   (package
     (name "licence-finder")
     (version "release_298")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xrp8m6mkq2y60d6jggp16fcpplil1hf0y7hrjbpsfh6qfclgaxs")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0wf6f7xv1apdbxlzsln4xqrp5j4g48x4ga87zv479g5i3cq32w13")))
   (package
     (name "local-links-manager")
     (version "release_135")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l34wirf9x9fkcwg419kk4lzrx8zi09p9imbscr66r03xmvsgz4d")))
     (build-system rails-build-system)
     (inputs
      `(;; hostname is needed by the redis-lock gem
        ("inetutils" ,inetutils)))
     (arguments
      `(#:precompile-rails-assets? #f ;; Asset precompilation fails
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
            ,(use-blank-database.yml)))
        #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/local-links-manager"))
   #:extra-inputs (list postgresql
                        libffi)))

(define-public manuals-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1y78gy9kd26ijdsff7wpiwcvp85zjvkvzpa63ci5barbf8dbkr4w")))
   (package
     (name "manuals-frontend")
     (version "release_219")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yz7bk1286kldnpl5zg50cmx368znqy6x4ls1jrjmifyl3m66hp7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "05zw9dmmhq0kc4rp4kax0i70wh3fia9chxwkrjgfb7s3s9r4hka3")))
   (package
     (name "manuals-publisher")
     (version "release_983")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1maf925rcb400zd4i4nmlskjxn8ky90504lp6zkf8kkx09abwm0a")))
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
     (home-page "https://github.com/alphagov/manuals-publisher"))
   #:extra-inputs (list libffi)))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "0l5kwbmg6d863yn6jl2b5gv5m74l5yabsf6wckja6qz92vycaqdw")))
   (package
     (name "maslow")
     (version "release_203")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gx9b0xm8n7mkv5d0qs4hmd4xkdrzbjvkzizg78f937233kyyymq")))
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

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0c0f9p83ksg7pixv64c58bachylvb5dfn0r0h9n94l8dlljv5v06")))
   (package
     (name "policy-publisher")
     (version "release_180")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15nrys9i7g0rd9k8jqk2mwx02yy2vrpa0jcg5k046jvp1b5cbqpj")))
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
    (hash (base32 "05k3zhbcsl86hapg9rybdyipmmh8d27fq9qb4c711faigci7wjlj")))
   (package
     (name "publisher")
     (version "release_1830")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bid7hs1d862l4n0irly1kh73pqg7gnhg8lr7h5pb909q9n7n115")))
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
     (home-page "https://github.com/alphagov/publisher"))
   #:extra-inputs (list libffi)))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "15rp5biv96fs9gxxn0wspxbvvlg7jpna8lh70p1hylhm9gbsayja")))
   (package
     (name "publishing-api")
     (version "release_1037")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1xh1cwx8jap6gn1h908c81x4p3j2qkaqhgshxqvs77w4y4qiwqrx")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
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
     (base32 "0638s65jrd0dqwdira5mppidn6ycpcrxhr1vzj185cp81c403zjn")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "e07cc17fa88ee958967e0219fd2c715ee47c277b"
       #:hash (base32 "18mwrkwqpiwgga6kyyf2syp1bxjqkvlvpj5ddr3lnhnhq04i0w4c")))
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
     (version "release_262")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jqi7wxxjaa4yjmlzp2c0hm56miq84dzcsqqs0r41598f4xky1rv")))
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
    (version "release_179")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "104anjzgyymcrqs5lyhigk6sig228p53sa17a8fvn6iprvlqh932")))
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
    (hash (base32 "0hm9gkhynr548bj4xhzx8bdaaxd8h8z61flmg05ar24ja4wgjlzv")))
   (package
     (name "router-api")
     (version "release_126")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zg6b6xf5rxj1z7n9az0hm8698kcv7kgpdy5gpnllpxbr25f5x0j")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))))

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wag0rzhp1r37p38a19s2b2vdklzjg44p6h5glzddwnb506wr6d4")))
   (package
     (name "rummager")
     (version "release_1590")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "160rkl72df4lrq1x7szsamc3ghmdq9vbxrmw70fmla45py0h0g1s")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after
           'install 'replace-redis.yml
           ,(replace-redis.yml)))
        #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/rummager"))
   #:extra-inputs (list libffi)))

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b08nkhlcc9b7p440bghfzrpx4cb8z7f0w1df58jj51xfp4pin60")))
   (package
     (name "search-admin")
     (version "release_99")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vrv8zk25v6rp595havjglylqjkppqmg9pp05pw386cxm1nnrwcj")))
     (build-system rails-build-system)
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
     (version "release_106")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y41qjikhaqj8c7gwfbcr4hzfqyz3sjh57i8b96r0ixhsliqgb6h")))
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
    (hash (base32 "1pvmggg6gpiylr4610c35k5w119173fpqffmjgc8p78057p92k2c")))
   (package
     (name "short-url-manager")
     (version "release_131")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01v0a79rff4z4vvv28brfbfiyd9hqasca7xjilwxvm7ivgaq176c")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f ;; Asset precompilation fails
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "1y0xqla6i058q4x3sx667gxrjwp2380jpamdng7x5jsdc7a31nhd"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_947")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jsqba4qbkrv2vgpj6cjzw41qf44x1m59ykmn4019fcgpysh6rsj")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-database.yml
                     ,(use-blank-database.yml)))
        #:ruby ,ruby-2.3))
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
    (hash (base32 "13anm7755dvxql8v92yiripw3f13cagmn92345k45lnh1ja428nb")))
   (package
     (name "smart-answers")
     (version "release_3795")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pb26g50jqjnwgmza3q723imm7rvniq2vglawhxnnigkdb18h96n")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b94hfm41r7zl1mp2ag9n4vgvp1gsxf7zbcw5z0hb2bh21njbvll"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_844")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1s3yr93a5144x4n5g4dcyn2xzq8xizaan0j0vm3rsbggbaxb31fy")))
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
    (hash (base32 "1gjcqnbp61q88rhlnsrq42fcvkfkzslx479xbkh6crd76b1639km")))
   (package
     (name "static")
     (version "release_2726")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "072dwbdf4dd24ks2r4815gncq8q25bk67ansyn3dk9115x6pxvsg")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))
   #:extra-inputs (list
                   libffi)))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zi4hwmwjz5625s5z7mqrfp59xis5drncpjc4lic32dbxqa6wwab")))
   (package
     (name "support")
     (version "release_602")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13cy987gn45fc4vfniyai43gw931xd79minxcnr7zfd7bqqwagh6")))
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
    (hash (base32 "09d84qlgvbw8yvhz9fkr6k6gjn9zlcl23j1jaq8dhq21hw8nh0kg")))
   (package
     (name "support-api")
     (version "release_134")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q23c6wkjf6nlif3lrnrs3n5qfc9pchd6cbp7xm2pm7apgawskaw")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
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
    (hash (base32 "0nah4mn1ckinsykv9qdm4sylcfdip0vavgj6x9k5zsrifhi4chrd")))
   (package
     (name "transition")
     (version "release_797")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gmh8d6kyxj1560wkf82yhfzfll364jsycsqxkgbzj0b73x1cz19")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))
   #:extra-inputs (list postgresql)))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1v49lqv6lbgmkifxgjj1fliqi0d7d6gx1hxmkdfyg3iq8p62h6b4")))
   (package
     (name "travel-advice-publisher")
     (version "release_280")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1nlkz9hbcpagp3b6w0gjqcrzyc3928qxlw98hgy8pf32mvhj853f")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
            ,(replace-mongoid.yml)))
        #:ruby ,ruby-2.3)) ;; There might be issues with Mongoid 2 and ruby 2.4
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/travel-advice-publisher"))
   #:extra-inputs (list libffi)))

(define-public whitehall
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zx79r2vplz06nh7xdxqvxdrv9m9jjs6rn05wlayc3lrzaln41dn")))
   (package
     (name "whitehall")
     (version "release_13118")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fjpl67dxhh93y0g8hkdr1jg0xy5iyhrn33h0ldidg38rh272g0q")))
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
