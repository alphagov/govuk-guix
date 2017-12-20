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
    (hash (base32 "0spf54n75cf8qi4blmwdw2220jji6v0m1qbfzfp3y244sqfjlg6z")))
   (package
     (name "asset-manager")
     (version "release_207")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "093717ysggkdy8940z8giwxxmaapm6fk5r9nzzbjp45brk4ng95w")))
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
    (hash (base32 "0rc8gbsp48jxcsag9pd5av6y9pgf7zsdaladj5r6m73d1gh0ybjp")))
   (package
     (name "authenticating-proxy")
     (version "release_42")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x333hjx5yb5nyb7jsr6132q9gnb4sylss907bkx27a8jmcyy21b")))
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
    (hash (base32 "1jgclkyfgicr3wjixi3vdg2n9bxl0fklxiwnsffhpb9bnkgf2nxm")))
   (package
     (name "bouncer")
     (version "release_219")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lymfpwvv899p7pbi2ppd16wagws2x7bzangzhfaxm37khhdj4vr")))
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
     (version "release_209")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x8kjr2r906iazbnjgjm540xfma5ksk1pn74900zc2my8ad1acf6")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "14ypz2j5gwb6rfxc51q2l9mni3f2fgph46jjl678rs9ya2bl9xwh")))
   (package
     (name "calendars")
     (version "release_440")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h338z1bsphbwc347mz97gsagkrg5m01p8lj8ssipaidiy4mkz6h")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1x21l6wxgzd6qhp2c3sak25i8fnf9iky61grf8jf135655yazifw")))
   (package
     (name "collections")
     (version "release_411")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ggv0y1gsn7jkn6byqjlq2h9wsv6hkh51bbqdbdysvbbw5qzg3cy")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1sjwxvd245l9169zys4mxypihw450idlh11cwm3vavbvgs6661iq")))
   (package
     (name "collections-publisher")
     (version "release_287")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02f8na0gw77h2avgbd4laxz83hfx1jinnrklfp8qpgbbns8g7lqr")))
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
   #:extra-inputs (list mariadb
                        libffi)))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zgv0zap5269v60gnb37pv8q9a2mdc8g9abpibp5lgc1fmlvf23q")))
   (package
     (name "contacts-admin")
     (version "release_360")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10w21nl7jahsfn7l39irgi45angcz2dn12rm9wgyks84zalppcj8")))
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
    (hash (base32 "1iz4lxfz79ykwzpii8bl6dw664hq1jxq60vdyqwv2asnl9cff25p")))
   (package
     (name "content-performance-manager")
     (version "release_349")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1yvxsy43kz4j88vfhna5g6xvc8vp83lq5sdrlxqb5jlajyzrbn1j")))
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
    (hash (base32 "0vrs4g00ynd7y05700l9rcfj51s1richz6h3x36xaw9yyz8dgh5v")))
   (package
     (name "content-store")
     (version "release_691")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05bwdy0i9cdpxqji21bqbd1chsb08pqc3pq5dwa91dzh5a7dv0g4")))
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
    (hash (base32 "0zqnkxlivgi5crl9k6qdajyxgqxningz67nnd8fl0h0llz0ml3i4")))
   (package
     (name "content-tagger")
     (version "release_635")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14k9qb3qagvdvfm0xv9qdf73pgw2w0wvrrd41561liwvcljdaw71")))
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
    (hash (base32 "0x0i8h71ak7q9pfl7pc72k32pxvn8fpdikf7fn0brkamcj04xh5y")))
   (package
     (name "email-alert-api")
     (version "release_347")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "07diwpc2ywai4q8wim8a6y7gjmb2iqbfnkj0pgkjg5jn4rwl5ldw")))
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
    (hash (base32 "03mrmahh5qcl6q6mq5mxmiajmgvhvclmw4lwlbp7sjjs50c5jzpc")))
   (package
     (name "email-alert-frontend")
     (version "release_75")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1sk224ld4cph4688b0psh4j7zw5x5lrvrd8q2mhzspl1x6x6dflc")))
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
    (hash (base32 "1vlgwjljwx3gmh724i6mn3fq8ijqifxcr5zkm0pyky3y59lr625f")))
   (package
     (name "feedback")
     (version "release_347")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bfcajmxv3gkws27fzhlpzldn54sgwv256a8hqzmziv14jp3s7fi")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0gdsm31p7g4z94cniiaim4wwk3dan6938rjmx25x8kn2kjy2c3vc")))
   (package
     (name "finder-frontend")
     (version "release_345")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q71pik5p8w1iv4mvhzcr0i77f2cw14k19njaaiwxy62b1rg31xw")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ppgmq5ymcb92hvc35vdy37n86f7bxghpf29sxz8xy8l0mkfal4r")))
   (package
     (name "frontend")
     (version "release_2789")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ilp585gjnz88a2ckbdf9gp9ifnr96g5myq01sbw2d868r107zrw")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0jyix0rlilfn89cki79cxl6ff734lfjmnk5mwl6b6m6ailkllhsk")))
   (package
     (name "government-frontend")
     (version "release_554")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d039jrgnkbz33xmfd8kqwxy1fd0nxg73b8a072bnj214xs3la7w")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'bundle-install 'replace-ruby-version
                      ,(replace-ruby-version (package-version ruby))))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/government-frontend"))
   #:extra-inputs (list libffi)))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_670")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1gvkwya5628ygiahalb827gw5vvsjncqr78azxywww3kmb4n1ag9")))
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
    (hash (base32 "1wqzpsgzj5401kg4jx1sn5gm8jzpf59pdc5sc6qp3rnalgk2zpc6")))
   (package
     (name "hmrc-manuals-api")
     (version "release_210")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "04v44bmjpjjv0y21cy2qx1fwvkjz0hp2ggfz995rnkxpi7dl66vw")))
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
     (home-page "https://github.com/alphagov/imminence"))
   #:extra-inputs (list libffi)))

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
     (version "release_299")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wdnapihbisd5519r85zcssaxwcd5c3bd2q485ihgsx4q2lrb66n")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1vvvxmb4x8kvb1nl0cl20v9626kn0jk5vayqszwrgzn4g3azkbsi")))
   (package
     (name "local-links-manager")
     (version "release_137")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1w58avqkw0gxd1h6k6m4rzjksd1712ya4xl61by44xlw9nj4xdwx")))
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
    (hash (base32 "0z3zv3lf04pjv2fmg4fyv8a1yfw6v0yrpi6y4nzkzq9sbdi07rgc")))
   (package
     (name "manuals-frontend")
     (version "release_233")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1aplp309nyajz4afasbi62j934glsjamcc19fq67p5vdfyd905vi")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "09bqfbh592ap6l4wwspl63hzvaq5139h9q9m2d7l5iwqcncq7m4p")))
   (package
     (name "manuals-publisher")
     (version "release_1004")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "195idg01jfx0famlvl47qm8jv73w20zwiad9wm5zh38vbfb8b9bs")))
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
    (hash (base32 "0aybyvalhmprnjxcfysxpa0qh92b54zkpz40dq3nkkmik2q89rm5")))
   (package
     (name "maslow")
     (version "release_204")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1z2bx6s4fqbgi07b4m5znap6w7nz0li6b5f4jx301v1i3w60fnjr")))
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
    (hash (base32 "0gy5xcxmsr2wkizfcbxhlwcskz7zzyg62azhbh4m23dqh2rdnwai")))
   (package
     (name "policy-publisher")
     (version "release_205")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "068zl5q3znxd9mxm58grig13zaggqgmkbg6q0pfxm18rhdv1zaqb")))
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
    (hash (base32 "19hra9lxd0adlfcx792by4ygiswfw7m009rvlsnpir1smf50dr6y")))
   (package
     (name "publisher")
     (version "release_1850")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c6568javna216r7q9dzz0pax6wvj6nli731nbigkabph8rwy00b")))
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
    (hash (base32 "17ay9m19bsilv6ckp9xw9kkdvrkvjj3klwnblbrpj8bh2q505hw4")))
   (package
     (name "publishing-api")
     (version "release_1066")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "1j7avnbjjr3679gf671x5kymlf62yka8aiahynkxjlg8l24i1ldq")))
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
    (hash (base32 "0j94db78dxbay3mkklg89w174lzwzbyj9nbnbndkgvb8s9y2vn2c")))
   (package
     (name "router-api")
     (version "release_128")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0695qshhp9d4c2cl6qiivhvypi4g95zdqkvf8qwmak0a634sps2f")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))
   #:extra-inputs (list libffi)))

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1hc3pd1r7kz30nhpkyjr686a3z2plnajrfbnrd8q6i579jhjfsvn")))
   (package
     (name "rummager")
     (version "release_1643")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "093kwfrjdd9fq04k0dvpcxcgf7l5m4p7wkfbzlr54r3vzfy75vdr")))
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
    (hash (base32 "0bddzccha8pbdlmsyhbdh8ji9pkf1ncxpipnna437hkpfs7xddgk")))
   (package
     (name "search-admin")
     (version "release_108")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l3rpkg6rsp24a3ivl8qff92fani8dfl59dccnkqlhdgmqrlfmvd")))
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
     (home-page "https://github.com/alphagov/short-url-manager"))
   #:extra-inputs (list libffi)))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "1v78zad5gdix0bgvv58dmz2iggg90557d4rnpsxb4mjh3n942jmv"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_952")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vwckccf0vz7dxqfmwcab790dj4x6ckgmlyfwskxqhdbn3iny170")))
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
     (version "release_3806")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zqvihbi6vk1hwglviqls43lv4maik3m9ryir0f0h3p58laqcvz0")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1m4ax9gqn3gxyai8d0n437w0m4hfjw6wib5ybrqik9npww51chwm"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_856")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0iakil2sdss568zfx1asx17mc90i1fr1k44bcz97dfpkiwqklii6")))
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
    (hash (base32 "1igqv68i5l30anwmlmnpbkrs7lg7y2zksxfgy63fzyvdp12qxyvj")))
   (package
     (name "static")
     (version "release_2744")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "193r1y6m1xb4g3dh39rmaq5rz856npzh6kqilvnamc9bvaiykhf0")))
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
    (hash (base32 "18nrdphqqi6212zsgqzkwl166hdl44h6qr3z63qmk6w4ak9c95qn")))
   (package
     (name "support")
     (version "release_604")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14f8fa86pa46hfp6k0jpxf7v63cr5msc9c180228z0wmylq9g5h2")))
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
    (hash (base32 "1whlhmrvnirpp81x0qd948a51hlk19p8hbivslsxkhr6k8ppqhpb")))
   (package
     (name "support-api")
     (version "release_138")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00344x7cbmsgiqs4lyz8gwbnfjj33j4f6zj7syl4vd74037mjlp3")))
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
    (hash (base32 "0qs0hzy0768iplclhbzq1nwcdfyvnqcnsvmh8wq0wk2wnfgcjff9")))
   (package
     (name "transition")
     (version "release_811")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0h5wx145w8bdhw378x79cr5svg77vpxhnv5c8svqgjlqdl0lkwy9")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))
   #:extra-inputs (list libffi
                        postgresql)))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0lhhc1x6ykgb3m6sh4d68hg17gvszw5cwz55nm1c8ngd2kh3z7jl")))
   (package
     (name "travel-advice-publisher")
     (version "release_304")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jqssvxngb91xa8j8nfghyh5yflfk1zqqc1h6cxc38jb8pmw3h96")))
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
    (hash (base32 "08ki7085ixjk03dx6iaki83h77ha7yz2951a07wlr1p5pikjf3fc")))
   (package
     (name "whitehall")
     (version "release_13149")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "13zjfv5599zl0k4kjm97l35y9nc55qf6brv41ix0widbffcpsch5")))
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
