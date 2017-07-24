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
    (hash (base32 "1bwbrkpyg193hc79mhbsndvwbma2iqz1wyv8iac9b56snis5m07r")))
   (package
     (name "asset-manager")
     (version "release_99")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fk0x1f4wz9xgcbrzzh9yjb4k4zn1zjw6rnmmc5bw4fc38l48pir")))
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
     (home-page "https://github.com/alphagov/asset-manager"))))

(define-public authenticating-proxy
  (package-with-bundler
   (bundle-package
    (hash (base32 "1wagrzk07fks423pdjsk29b2y7786cp6z3gnqpcpvm53xb5g1cy2")))
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
    (hash (base32 "0lv0ar6hi3cb1mh5nn453gy94gc50pgqzglaxpdpyraqzcq3a3c2")))
   (package
     (name "bouncer")
     (version "release_206")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1gpyryh2f49ga6wvg3mpw6qrgdkm327cnydh8ggkiyzd8sgypw6y")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "18qkd6zfhm4q6rnpixmdp0nri3x4p9gsvpp7974gk6vdmx8vx66b")))
   (package
     (name "calculators")
     (version "release_197")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kw5c6sl9fi925i9fypgnv6babhqj3vphsav5w5lfxm4db2bkwgw")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "07nj042cq2nxyw8g1r1qgvwqd5hp7d7f2kyknwa5iiqbrbixzk7k")))
   (package
     (name "calendars")
     (version "release_425")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m88lp53wd5ivb945dxlnv6h5pnbl9mi2rryjg2kmzlkr9q8dlbd")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0q92qsh21y2mb7b2akkqr4wv4bx7bnagy3v01scbfvv23dbkjr80")))
   (package
     (name "collections")
     (version "release_346")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pxkdh19lg6hqqf599iday3s3r4qgxncfak682zlnkn72zmb2xda")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "13dfn9pm7rl91qcj4n0v84fliq2g2g23n688mnm34z1qqjcaq1kv")))
   (package
     (name "collections-publisher")
     (version "release_261")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16dpqclb8iidxrwsbxc4iqrn70vagzy9ci0sdg0lgxsigiz6fcxx")))
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
    (hash (base32 "10h2vbjf3xw3xh8da2n6y3751rmbmnk5r03xnh0a06yy94j6lq6b")))
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
    (hash (base32 "1rqbksnjg0s1731g002sjj8psqp0k3dqkgir8g11qqjimjw8c62x")))
   (package
     (name "content-performance-manager")
     (version "release_159")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1jfbd3ln0vrwd84a2zllly1s2j68j2242gcqark1dwxnpidab3vn")))
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
    (hash (base32 "0ygmb79rb1cb8gampgyy51jda7s6hfmr7nga937zr92nvfpmldwy")))
   (package
     (name "content-store")
     (version "release_647")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1h7c255052cb3p4qj4yvj24p52ih8igs4dn7616iwr0nrpcz66ka")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "009khlprwij7nd5qj7cp9fm2a8mzqvxkd6bla784lkch0azsyp1k")))
   (package
     (name "content-tagger")
     (version "release_455")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r48cy8b1x62a70vbkkanmsypl52g1s17gmc2yxlfzpjj97kyqpn")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql)))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zd74v5dvywahq83ih2jkcsbxmcksn9ql3vcs3w3qvpbbscnpxsq")))
   (package
     (name "design-principles")
     (version "release_872")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1w4krlinpl2ig8sk2dz34r0xqp3i820w7c01h2rkyr1gpb7as08l")))
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
    (hash (base32 "0dbw1z8ww1ydidvra1ljgvh8cbk900m9vbnf9nfz2awkp4ia5h6s")))
   (package
     (name "email-alert-api")
     (version "release_182")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1xq1bjd2sbxs9mmmhqcr11gkk1ff7ifwnp2ri2nxdbdim1sip1xf")))
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
    (hash (base32 "04cbcds7j6rw2bhi54fjmxs6v7b6scizr879lvfizd3sy0dl50vg")))
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
    (hash (base32 "0fhn3fxkb56i0h15np740g8k5mgacshxv5cidvniknnrx22aks0d")))
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
    (hash (base32 "0v1f6drjazns2jfj7valqva8nzg36x04lqzs12fl10x376a8z35g")))
   (package
     (name "feedback")
     (version "release_303")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1c4lzmh79zv11yl1k9nirjgllq71v0l9qp4nvybqhdbhrypyh9ys")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1kf29sqb997pqrqvdzjilxddzl68s50hmyr99ri6mpky14r2aw27")))
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
    (hash (base32 "196pfajm8q565vpmb4kafrs4ipm3ca7j8rrmkl9apqhmkbvj4shg")))
   (package
     (name "frontend")
     (version "release_2168")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0f7nscv20hlpfl64z1r62sfjjp604lr4l16d1vnvmhdsq6fm7s6h")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0kjwcigywzi2xaqgm6vfxr5ynnlbhxv6h6vslliw1bdpvx21qfyn")))
   (package
     (name "government-frontend")
     (version "release_385")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jdd3q9r4hpzrnk12782flcpk9qqrmdmb4kh2nw1fphvv5xkm178")))
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
    (version "release_615")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "00kl11alwy5ii9kcngp1grlvcyxb2p1fw59g3zzmgkjpv0vvmqci")))
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
    (hash (base32 "0wxqylhp5rfmkhd9aylg6wb1m2iqjfrap3f8p6nmykbqwld5sy4w")))
   (package
     (name "hmrc-manuals-api")
     (version "release_186")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q1kbypv3vsp4r8qy0xc7d6a3swzsnwx7fri3yn7as3gr2csvj8z")))
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
    (hash (base32 "15p4lndw5mfr2z1daagi4wsjcqgl0fzgn487swzvcs5dm4si7ygn")))
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
    (hash (base32 "1a22s5q11gqw9l79p89dln5pz72rl8kypa98lrh8941b37gq3gni")))
   (package
     (name "info-frontend")
     (version "release_79")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17apwkjy3jl4l1i21ih1zdpwsy5vxxn6lfvy2vvgwqjjmzq9zldp")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0r2hhnbkans7sylj0dxra1dm9lc3idgandfh9zqhl1jpp7nxp7k8")))
   (package
     (name "licence-finder")
     (version "release_286")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1spcp77bppjmmk8gs4g5fl6wv89ynkcn343f5sr5nsnkw48mmsjg")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j6qb53a036jjrga34s0wc3q7qzrsy5vkwdjg5fwnfr72khd0c69")))
   (package
     (name "local-links-manager")
     (version "release_124")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dsp5fxbcrf6yxqp9vg03nipfhxl5wgfag695zaa8f4n5fv7jrw2")))
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
   #:extra-inputs (list postgresql)))

(define-public manuals-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "068kpz528jiyzva1481n6qdxl4h0d9mrqyfqsmcm9118kmvw6826")))
   (package
     (name "manuals-frontend")
     (version "release_205")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qf1rf55phw858609zjz6wziglf3n3wkznkc8lz3hngj6aaw2x5h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0f8ls9p55nhrzmb37x8x77lpzmqm9hh71akjmj487g6rdfrbpafp")))
   (package
     (name "manuals-publisher")
     (version "release_975")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1s3gm9f2hvd948gckggykwmaf1yjyk0dl6if9dh75rhpwh2d3q8v")))
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
    (hash (base32 "1nwl8nvsyljb5vn6gpz85zhv9l04m09zfxn972fabdc64vir0kya")))
   (package
     (name "maslow")
     (version "release_199")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cpn46n5qz8bqp524j9fcpdpbilf7f39hy0vvrn272l1grgr823p")))
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
    (hash (base32 "0w62ydia3j5d0s045dsk1bl88gv5vrhyvbri370al81v8zrrrabi")))
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
    (hash (base32 "0r02dfl7rgccbm3kggcyacyfndybzvj6ljliqga7zx1ciqff3ydn")))
   (package
     (name "policy-publisher")
     (version "release_175")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16yldwhymz3qsvpjpnq7cf4hcb6jpfcbsbdcgcck94hvwwghd886")))
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
    (hash (base32 "1s8clpgc3ggzlf4zhmlwnzpvr0fi2kadg3n69fv325gl4k675lbx")))
   (package
     (name "publisher")
     (version "release_1800")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1105zh6yl0p5aj8linbqbw1f3jc00j4xq2g7dhw733ka7rrd0k7l")))
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
    (hash (base32 "19hh5488y1fsn1ckdr2cvg3cn997lf8swv84jjshv2bw6jg5q8l1")))
   (package
     (name "publishing-api")
     (version "release_945")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "046msin8vydck0fzzc05d8aw9cf076vf6iv5n7frflxyxizyg402")))
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
                   ;; Required by the pg gem
                   postgresql)))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "1lngabhlnymaxilc6ixdy8pxp6w669dg1n75v8agxlkgsqrkcvrz")))
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
    (hash (base32 "04kimq4w7yrhlcnmmqw4brr6kwppq2igpiy9z2gqlcppss2b16nz")))
   (package
     (name "release")
     (version "release_258")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mxqws2h5rwlf54b8zbcmb2v1wr851ah3w6m4ww0k9jla06svmwq")))
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
    (hash (base32 "0ymfjvf37fg7w4zz8jg3nsgrjxllg6mjj0d287zzhxky75wnl9mn")))
   (package
     (name "router-api")
     (version "release_116")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01ny27vkshiwhkav5h52bkclkzlvnxdawalkmb9sw8y3c32shal7")))
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
    (hash (base32 "09hfrrw1jqb3gp6mfd73gia5dq5q96awrmz3hq36lmqzh2v4i0m9")))
   (package
     (name "rummager")
     (version "release_1383")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0sxq230dwd7lf6dw1znahzmpnsigjq1rn2hj7a3acaw3cpjjvxlh")))
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
    (hash (base32 "18m2y66ww28k8zqm5js1wlh6xls2359z3k0yn9d0vcf5bdxs7jka")))
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
    (hash (base32 "13fpbgqnbr4i8qz71ws846n146ffsvvdy6d5ai39n4imxph49kg5")))
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
    (hash (base32 "0p28dsiw5zr19pwsd30xs9xa9bygzvb5af5xl083q0d39zf7r5jw")))
   (package
     (name "service-manual-publisher")
     (version "release_299")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0azg8bsx9migqda539jyr69qaq5y69pfbaf2pkj71qqavrbml4sh")))
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
    (hash (base32 "0rbs194crac31b1hw4drpyl69ha1h4d4knmhalw46clrma1hq2vz")))
   (package
     (name "short-url-manager")
     (version "release_120")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1r5dcndb1lw5b107k5q81cz7ym0anj5alwniscrf8cp9gxw63xyi")))
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
    (hash (base32 "14n8ialjsasj0cjwmqy446kw7d194lqi3v1j6d3yhm1bsqhrldq1"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_916")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gfln22ysfvfgmshz23rlkz5np1k304qzyjqy8fac1jwgb4lagl9")))
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
    (hash (base32 "16pwx40cknngjnx0d2b02rhm5ywzd09vs110f47n5sv0ya7mm338")))
   (package
     (name "smart-answers")
     (version "release_3675")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p57xbd0ykyv8jl0mls81ynglaqns69xcp7118g80jvgjp798yqj")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "08vvhppxmqq0g4dlys52m8dy4w0b6nvhn0w175p3q8ik3kagqfz2"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_821")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zfa100wkw8jv5vv7hydhhw5lf7ngga7d5babhy12q922j6ngjvx")))
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
    (hash (base32 "0dlvrdbd1ywmyygpfp7bcn1vnyawih513faaxvzgm4v2yxcya1bn")))
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
    (hash (base32 "14kgaf4gg9ypfr4lqwc5kgpkcpd9j0hhk7p5irjqrn6qywr9hars")))
   (package
     (name "static")
     (version "release_2624")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m79gdj0jyrhqndhs2vjqfv5d6r3c83bf9d09avwj39wf6x2x93q")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f
                  #:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "074gwgwnmz7irgazjngflh8rcsk2krv6ksq73zk1jwazh4w9xdhl")))
   (package
     (name "support")
     (version "release_593")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11nnb4sr0w4zjkfqbawkf167ncv76ilrag0qkwgmcghvn6fvsq25")))
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
    (hash (base32 "1r8igna3pz5xgmb6ym8j3y5mn2czmn8vl1kan2rp1n79wa0kwl55")))
   (package
     (name "support-api")
     (version "release_129")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kn3pkfywik5k85mz3zk9r12xyypvwp0jrc7q2nm0q50a1pr9xfa")))
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
    (hash (base32 "0ik7a4v2dq6yvbbaq464yr78kvap4q76izbzjpjhxbrqnafb53pc")))
   (package
     (name "transition")
     (version "release_789")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v2acm00k286ihxq3vwac3yhndvh9g7kkk707z5kzinb8lxy7hrb")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ai70b1gl4jhpi4xygzrdbn38mm80vd6jlfpibsjl77ki9w4y6mw")))
   (package
     (name "travel-advice-publisher")
     (version "release_259")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kkkcscsvlxz458qi3qcb1j6w9l98k1hjvncvays53kfdz710bmk")))
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
     (home-page "https://github.com/alphagov/travel-advice-publisher"))))

(define-public whitehall
  (package-with-bundler
   (bundle-package
    (hash (base32 "1fy91xzl4x116dlk4vdnry27wxwa4wiyjgyyx57ihlj7988z6kf6")))
   (package
     (name "whitehall")
     (version "release_12885")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xlrn5464k909xpn51x67rbyflx13zq7q4c66370rwdck4b1yv97")))
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
