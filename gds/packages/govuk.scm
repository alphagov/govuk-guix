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
    (hash (base32 "14z77p14vgsl8n36xk16wbw57fx4z6f1ld0v6k494n51vq6hpndl")))
   (package
     (name "asset-manager")
     (version "release_132")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "145igwl3bqgz312fibjh3imy450kbz838cingfmjpljjjl74nzaj")))
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
    (hash (base32 "1javq4cbfc5974ncqk2r9zf4p48xsbzxrbymmfrgfjyh5vqzd0gc")))
   (package
     (name "authenticating-proxy")
     (version "release_38")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1cbri1zh9ydpdpzsqyqzbp9c0822r9nw8bx4rd3vkz0dlyr4r519")))
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
    (hash (base32 "0zffwip1x8fr5r9zmp7272d1frhl417vnpyqz2bj1wgd0xg071n3")))
   (package
     (name "bouncer")
     (version "release_207")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "031zdinc1rpa32yi5nimhjzl9dcf87xn3fg090i9ghcvfdijhi96")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hlpl55lvcghy0h13ad38z6kgm0bxw5cnwkbvshdc0wnmc3bv5xl")))
   (package
     (name "calculators")
     (version "release_198")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z21lrhvm27y8xksind6z89yqsgfdih5nb0i14bla596qqg6nhws")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0h7k8iyy6sg37rg7364dwzpljwmx9m98rvs762kf0641n89k3qwh")))
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
    (hash (base32 "1nvaz45kmsmrc0q8kd1kvdvcw3jims81wqwmanb0d5bjkwzhr5r9")))
   (package
     (name "collections")
     (version "release_363")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1n16pzwi7fmd9qj6d8idr6jzr987jgk84xp34igh6ygkgmcvfrvf")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "179li5vy9ykwlib3a23n87yij1yzyba3wsv6pgkqygjvw5pqb6hg")))
   (package
     (name "collections-publisher")
     (version "release_264")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0904pbziwvsr1sgvgwsg4qiwid765nb5ca4vs30fwzsf961bs0df")))
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
    (hash (base32 "0gqyp0cckmz2wczzi0nxsj1dzvm793j0b4m04p82mx98ymixzhzj")))
   (package
     (name "contacts-admin")
     (version "release_354")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jd75s9ad7gbpasq8lvcpqgb3jyx22awqn62l1arn78yd64qnzdi")))
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
    (hash (base32 "06f26j2wym71lm55h6xr1plsgjxmfcbq6qjfrx6j8jsrr8l5x261")))
   (package
     (name "content-performance-manager")
     (version "release_251")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jdn232v5azlq2gj36imnh3r3w98bhhhmcjkjpbyrhcz10i21zmm")))
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
    (hash (base32 "0kf1x5zjkfpyxcd5asqg3n65yixfx3jyq05g39d83nsnp2y08r8a")))
   (package
     (name "content-store")
     (version "release_659")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01jm78pwgvhsa0myw540qwvmql6ddd6290ifmkq9fqpvg4n51qdn")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "00fdbnwh2zlqg2pjlbs6vrxqjrmn8ggx8qmpp3ar2ivdyygikg1p")))
   (package
     (name "content-tagger")
     (version "release_505")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1cy2l9zaqp0qqlk3s8bg35h7xiyk6s4y5ia3x18ihvi5m2ygai1a")))
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
    (hash (base32 "0vm3bpbv936c45m707ay9fcywygbhlkpi2r3hz3shb2ah4aw2ddi")))
   (package
     (name "email-alert-api")
     (version "release_194")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d98c9flf6jcn6gyxrljwsslj39rp62dz9cw9kj9xklnwzhl7irb")))
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
    (hash (base32 "0qzr2rb5kg5009mjkny2w7191q5k0358vmzrb60sliirj8g3b9pq")))
   (package
     (name "email-alert-frontend")
     (version "release_47")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16ssl3y8yva8bwymkhrixc15w7v4l58yqd919jiyvvkria35wkc2")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "04v3rwmp54zl796rcpkgjzfhr7jrxg2chsmmf69nbbmjkd7r4hpl")))
   (package
     (name "email-alert-service")
     (version "release_82")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17dfbw85026y5mhvywmcvbdsih9nv52f1j3cknzrx492m67w6v57")))
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
    (hash (base32 "0zbfm5nq3bwygkjv3mhagrc7j8qzyf5x06c3n6das8qyxakishr8")))
   (package
     (name "feedback")
     (version "release_307")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1c3hmafwrqr4a4liqv2lk8smpdicz05yr389w9m1jn3n9aq99m0y")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0p8w07wlpb53ha5cs9i26npksaj9x5svh1i57r5ra7is304n2zmy")))
   (package
     (name "finder-frontend")
     (version "release_312")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "175pggm3qwis4qfv1k6n5y7asddljwrc8ivqai2xbg3nbf4njxyv")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "11352844s8bmrafci47w6wmgljfsa7c5m738xck1bgr0yq98zibf")))
   (package
     (name "frontend")
     (version "release_2726")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v3gakg8x6yg0c8lmzq1yghvi3z70qilmlc14p5yd8ssx6h6hr9w")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f59g1pcraxp5r50pqkhlj5zzn4j40pmizkfcd3zzjl1mzwhpbs7")))
   (package
     (name "government-frontend")
     (version "release_445")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1681k8a67n0k302vmagyy4wlvqj6rf93prh58273j7scfp8yckrp")))
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
    (version "release_624")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1pddsinwydnmwrlf43yfm39ykh8dq0wfkkvdvlliv4nyj7j4kd5h")))
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
    (hash (base32 "1zis0hy8b1bxz6xfafqmyjk1ghirykbd3sdpv746vxrxyy3cqw41")))
   (package
     (name "hmrc-manuals-api")
     (version "release_190")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qbzh20hhqxryj68zcdvx5hkr9zqgs6i24dmdn64l5zxx53is4z0")))
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
     (version "release_319")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a0fyd2s4d2z4232pcc9fgkp0s9ifcfhi0hkwkmk3qskvqlima4k")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "11bhg6vprkyrd6kv1m1nxnnjarrb7vgrqpjhz3mwnav2szkik30i")))
   (package
     (name "info-frontend")
     (version "release_83")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16kbfj9fpfif8rvy2s8x2ysz7jiljb4kx9ra37xys3qwq193bz4a")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "080z3hpfjgl6hpi710g54jwk06rr9npljfz38j1aj24xzv9x0mi4")))
   (package
     (name "licence-finder")
     (version "release_290")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lg06jw8m5fdz91bw0rypsrw3xibgw002jkhx63kqpaq3x8yfyi9")))
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
     (version "release_126")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1pw2wrgjbs42yjpraxvpz9zk0vb3hinn9xii3kxnd8yvx84czk2q")))
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
    (hash (base32 "0fkx7gslmjwrhb6p1sagp8v8x9m10j9mjpdfvrri91imhgs43s8r")))
   (package
     (name "manuals-frontend")
     (version "release_209")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dpa1hdvgvml9wjh2zxzxiagk84dhi91vm0b1yzgsmcnsf8zf5ff")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "030kxzwp6z9lr5h9bw9nx82d2g7rrcxpnnbmcg1ahcsbdrk71n2g")))
   (package
     (name "manuals-publisher")
     (version "release_977")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0l371g8h7v6xn62sjhx94pdvlzdi34a2nxavs4av8wxs6m68n6f9")))
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
    (hash (base32 "0jziaw43fsnqqzmpbfsldqwpqwwhxjmmslmd0rjs3v7dln011xfq")))
   (package
     (name "maslow")
     (version "release_200")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lhdn7nk4ps7695p1qp1p2xsdyaz5glaj0pxpzayg62hyiv2jlcs")))
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

(define-public need-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0bvwysm8q8kbc70ajdhnicyssxm6qbjz4zl1mhnbir9z6dwzd2vg")))
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
    (hash (base32 "0rmljc8xz11j9f13v450rifshklwjxxv9in8zgmzgnhv6pa8k4jb")))
   (package
     (name "policy-publisher")
     (version "release_178")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0pkz9caa8pn55f8cbhh5zji848a3vq26hhyjrixam1rawi4vdqr4")))
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
    (hash (base32 "06h6v785yg7y0xs01ppssd4m2v9651nhd42jqz1n45ah312pvcj4")))
   (package
     (name "publisher")
     (version "release_1806")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vvijvc12xna6vzp6ga84cv222gbwawqyzkwrpda18spq8fkl32l")))
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
    (hash (base32 "0gqnp5kwdh14zybxx15qzp7q4lqfdzmb24d63k8pbcqb4z03y398")))
   (package
     (name "publishing-api")
     (version "release_1001")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "052y0yybsdq51sg0dfamb75kj5k7ziz093hl2wy8h7xg9df49f30")))
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
    (hash (base32 "1zfb8b1slp4kg4vz5c98p9zr239mzh76mxyr81qyc2g6m1awk9s1")))
   (package
     (name "release")
     (version "release_259")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18wzk1xyy3h7zziqk1ykiydb1if0qhc65i8ffvvq539dinn24vyw")))
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
    (version "release_171")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "09lnsncxxc0xj4y8w2aymw76yx79z7yzwl4c51hrhc8q7n7kiklv")))
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
    (hash (base32 "1fp6kp01wf6gv1qzlvr17x134pw8yymvgyyys45nz25chjwk8c08")))
   (package
     (name "router-api")
     (version "release_125")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02flbrrppyb7l0qcacw6i1fl9l7d5q8sn8djvb70mln4kw8cwrsl")))
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
    (hash (base32 "19m24df091j3ivxzb6sdbiz36c065r5fd5mfccv4sjssnaqr8pb6")))
   (package
     (name "rummager")
     (version "release_1463")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "023f7bmg1scsi0f66lwgdvx4c5j045ylnbz9m19ilsw6lvahxqmd")))
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
    (hash (base32 "149nnadd5484kl941jhl1wwsn2yahkr8jzkx2bhd3522hngdnp8q")))
   (package
     (name "search-admin")
     (version "release_97")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "123ql5xylskirn3ig31ksqn821jnp1ks35dl2gkb4jnwxpjx5vlh")))
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
     (version "release_103")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "151cmar36w166c6dbygans8yj9a1jmiiy4rp3p5yi184f05104qm")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "00hf1b8v1hvjzhamfjpgh2zxp98jnp7vhrr0rb8001izk7gzbqq8")))
   (package
     (name "service-manual-publisher")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0dda2jsrbzm9asg9vwjjxwnzw101jlaq6xgw29ffp7a8lksp9rid")))
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
     (version "release_122")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0n8s19g43rkfx7v1la7lm601gipw973m4jfhv6d9i093llc0rv22")))
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
    (hash (base32 "08w5cpxnb4s439ssrpplkwr2jqlvlg6xrsyrfgam14q1j656j7hs"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_922")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "098n798vxfa4rc4zif371xjkawcwmbvjzc795wp40g99zqygz3r1")))
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
    (hash (base32 "0mbn0inavgni1wzng3hc5hcvs4wjrclpbmca5w4bcbl85isdbfsr")))
   (package
     (name "smart-answers")
     (version "release_3740")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qx4g4xbb3y3kfgb0gciqb52a8qzxicz0by1f09g38ijj6i1arbk")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1q96hs0vckbsq0fnfyhkdwfhd1q28y28dfcw9kmlvm4w65qy2wn3"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_829")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "196phbq34yif5fprwy7h47na7l0cj2r8mvyrr23j0236gb6n4p3a")))
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
    (hash (base32 "0nckx3gzwx8jnybzhbxq62asbk1xgcmi6wq01ifrcc9z012q9zbc")))
   (package
     (name "static")
     (version "release_2668")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vb20g3pygis0yisbqpsjm975cglnrv1jqbxgs8ikwlj7pq9skvv")))
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
    (hash (base32 "177dd003by0xq6h9xb2g4difqgfwk29a4b6da9bsjcajapk65hbi")))
   (package
     (name "support")
     (version "release_595")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v4sqm6s0hrvs5gh84v00rwdxlqhx6rzm28zrkpdj0c4xdry555j")))
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
    (hash (base32 "1qjj6b1yn594khcchmwlsvfcvvbi1hmkjb2nkfy6sgkggzg8dx84")))
   (package
     (name "support-api")
     (version "release_130")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19i8pd16vsk192lsic04yavgsygnv5hrfl45cbx88r8ymgsk1zr0")))
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
    (hash (base32 "19nnmvxs3m2zf01hv408lb3ijhzpwjd2c7fyq518s2q5h1h96q97")))
   (package
     (name "transition")
     (version "release_791")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1k82m0xgah5y2yfjb0rdvhv4ag65f9f66qfa89l032b4y6k2djzr")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1i50fyr3fdk2zwd4kip4v1zx11p41v997wlrwd917zhhc72wyila")))
   (package
     (name "travel-advice-publisher")
     (version "release_265")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0818nz5ijbsr2hshych24zbdnr5zzcdaagmm9yiw2hyrwpxpxlhv")))
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
    (hash (base32 "04y1w0vc8hynk7hqn6vaz0g6fsiisbf86w0w9l65v18zcnc172gs")))
   (package
     (name "whitehall")
     (version "release_12980")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "169shf780a3mkikq0b0ks1j7lfr1024zc2xggamp240fc29ir1wp")))
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
