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
    (hash (base32 "1v9mf7qzkrq0lapfag897x59b3ng2md9gwm2xvj3c3252315qbgf")))
   (package
     (name "asset-manager")
     (version "release_92")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b7qxzckzckwgvvw1wq3sgzkf0yj2yp947gz1d4jndz5g4b5j57r")))
     (build-system rails-build-system)
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
    (hash (base32 "1s29mszvj796swgf4szs9pnv92j9ilk2ipmy8if5lkrsy82710pw")))
   (package
     (name "bouncer")
     (version "release_204")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lv6vh88s56ys1p82yjxc25801jl90fq3smdjbphpfqpxgxbkwys")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z05c16a4rz0kf1wq3cqykfm3v1b48kbx32g7zyhniq5ax9hhka2")))
   (package
     (name "calculators")
     (version "release_182")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19vvkkcc8qsb2gly79bbqrg0vp1hivwsvg5ks497707blhs1qbks")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pb8viv6cvp91qg1a9n0648frw5v49kf3z5vn3585fdmx6q9jd9k")))
   (package
     (name "calendars")
     (version "release_419")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0c88ginv7fwhwr0fk5gb894c16icshnl4fdf4ikms20k4qz3nbzm")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0hvdhfczljwlcbhnvykxip8w014ggmv5w8aw1zfz21dxhc39c3vf")))
   (package
     (name "collections")
     (version "release_315")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1x6352vyw7l7v9vymmwjm8hhk69gd2z5hnc0y0z7q4mqdsns642g")))
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
    (hash (base32 "1rg9889j8mqrhz78n6hn7csph625090dvzri6kx5yp55vhwk7m8z")))
   (package
     (name "contacts-admin")
     (version "release_347")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0v49a4mvlnx0g4w7402jwdz41dl5wszm023c9jm1pgyhm06gkyqs")))
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

(define-public contacts-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pabhmanjs1m0b5zdkwng7p2fpjmwjipvwraimywyq8dikvxl8jh")))
   (package
     (name "contacts-frontend")
     (version "release_73")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00bvw3jadh4c97g7j14q1p4m2q9bygxbb5xkvqns5kgxyys7d762")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/contacts-frontend"))))

(define-public content-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1iaf3vyiff00sybr0ysqx20j68ml0kzzkb9sv05w4xy612shbyva")))
   (package
     (name "content-api")
     (version "release_402")
     (source
      (github-archive
       #:repository "govuk_content_api"
       #:commit-ish version
       #:hash (base32 "1hhabbbvma9pk13hf38c5jx9zm3zdi2683sifghg8hg9wx79f946")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml #:path "/mongoid.yml")))))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-api"))
   #:extra-inputs (list libffi)))

(define-public content-performance-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0fbwnx7j4qvn4aj5vi4qr78n7n1a2mhhgj9ayhv8d5z8d3cpcwi8")))
   (package
     (name "content-performance-manager")
     (version "release_30")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12z9800nlm2vl5r7nznh8ah0sv1768id0xxhkn38m9wwjxlq53qj")))
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
    (hash (base32 "1chs8dym0izp367cczbhqsdj2d2hvnx7pfb7bax60d1w040jyamh")))
   (package
     (name "content-store")
     (version "release_631")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y9ycdwbs9l7qg2d6s43rrry7l27sj74lkf7hy3jnjp5612wwjhz")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "0syxhixz96khcj8rmjy0s9yy14xal8d75ja6ar69mh9ylr9argg6")))
   (package
     (name "content-tagger")
     (version "release_404")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17ykp6zbgv9j88zfhy7v4vpmyaw533d6923jyqasabw3lk4kmy29")))
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
    (hash (base32 "17nswlp66kkx0vja4yf94zrccgpwzgr7x4nhrz46brbgnv226zn1")))
   (package
     (name "email-alert-api")
     (version "release_179")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10bf5hkv4sklr5jvcf8nl8n9c84vk2bw1yc81x7sdjpzz75bpfvl")))
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
     (version "release_43")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x9mrskx0k784brlv4g0dk1ic0mnq7g4gx1wbq83gshl03wmzpn1")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zp0wqjf721fg6fpnwhw30960ill070zpj4lh690s5cdwai1rfja")))
   (package
     (name "email-alert-service")
     (version "release_77")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "082il0cfjr9syyx9pq9yjq7bdby1hrfwsidlqkmy6cnaiang5z7x")))
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
     (version "release_299")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15ywyv575gw1d4cca5qi0v5cycl6wimvkw657w7b8z4yydi9386i")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jh4rnfqpnjzckjxyk62qs1rrg3drj3fzy920iycdy6aqk2d9958")))
   (package
     (name "finder-frontend")
     (version "release_305")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16jg36iplh3my718gs7lv9ny6ad21jph0rlkzfgym290q8dj0yff")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vkasdpgvz368f924qfk57786qmc9m2ysxfd6gjziwrdbvkq877j")))
   (package
     (name "frontend")
     (version "release_2120")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02b8k1gvsr57k2kfjb6wdr8mn07yhh55c4pdb2adfdij0lb2rskl")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0p1i7kbnpv2jix5b6s0n1z4lx43c23zlqhw2czm9lh3b1i1pjsh4")))
   (package
     (name "government-frontend")
     (version "release_323")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ih0134nlx2aca3lvgs4ik38jlfqc6z6dgibzxa65ayzllcqmyab")))
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
    (version "release_588")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "10bk14cm7jh9fka51fjx3prwbsk060xsw31gy10jmgl2930pwznq")))
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
    (hash (base32 "18av10h8wbxmy1kdfd3n5raiavvxadpx88qa7fv7rln1pa3jk26n")))
   (package
     (name "hmrc-manuals-api")
     (version "release_178")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "16zg8wwjxl99pphzw8sjc6x15ljfry59afmls8wlgi5i52b8hkag")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/hmrc-manuals-api"))))

(define-public imminence
  (package-with-bundler
   (bundle-package
    (hash (base32 "1smwll8m13215dd8m1dsxii847cd054vfq896xsivrqy73fs0asm")))
   (package
     (name "imminence")
     (version "release_313")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08ryh2i48p1sfwzsjxr1znnbpr1bc756pc38kav9l3l3vq4xkn4m")))
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
     (version "release_71")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qj5q7fv4x122x41j2ln4zg1sk017930ap51650pn5g6jdb3pcyx")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1cfgxbm7qhhfn2w27khn7avn3q0w7961zpj88i8vq6vhlvdn5qzj")))
   (package
     (name "licence-finder")
     (version "release_279")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jgsravvf4ssn6mf5q1hpiwxvnp8ccgvv1cj80254a0fbix2qg0h")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0zs7gk9kf2zwal8lxlwp9b9ih6iii7jgiyap4vfqva5la9hhpwsh")))
   (package
     (name "local-links-manager")
     (version "release_113")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1wphikd5gn7jsi3l24dlwnmd0634i9isjdzccxi9z4bsnlqfx8v1")))
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
    (hash (base32 "1j0zs14v7ljxn0d9cwladid8xwbjfwqcykq39p3x1ly41wh9hx4d")))
   (package
     (name "manuals-frontend")
     (version "release_200")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "146ll1pngyqya76iwvynkz5llr6s506x7pxy7j1ifhiwvm72zssl")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0x5q5cp3rm3syljnl8jvr9blzc8aka164f58lk4z299slnz3himb")))
   (package
     (name "manuals-publisher")
     (version "release_877")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ng4sf6x71paq3kispg2nzbbd72plsnp9rb63b1z79s90zw1n9h5")))
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
    (hash (base32 "0911wcbx3ixbw3l2yqsmcjqxx8lsfqja3skbj17ah8dr22pszs5d")))
   (package
     (name "maslow")
     (version "release_196")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vvsr01payhmfg3b7xn6ib75vm7nkldkc5b2si52djjhgbrp422f")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'replace-mongoid.yml
                     ,(replace-mongoid.yml #:mongoid-version "3"))
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
    (version "release_76")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1fc1sih5cd6r50m8zgyz3rz19hv4m5893q1n2nwy35hsynn56a9h")))
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

(define-public multipage-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0h7g4s2yzwp9a77fd5rpwj7sbsj0vfy0216g98j2175nx75b8ic7")))
   (package
     (name "multipage-frontend")
     (version "release_56")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17wybzky3ykcrfgxrphiqgbnnhh2lm4cxa7vkrac55i3xwcrsvca")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/multipage-frontend"))))

(define-public need-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0h5rfpc2xcf30grrklzb3grcnglhag1rfc3h7a6mvnzas6sv1dw0")))
   (package
     (name "need-api")
     (version "release_145")
     (source
      (github-archive
       #:repository "govuk_need_api"
       #:commit-ish version
       #:hash (base32 "1m19rix2mb89lz195v02n56vq4452g18klbws27zwrwrj8yd9b1f")))
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
     (home-page "https://github.com/alphagov/need-api"))))

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "03ipdp9zk36mb7rlqm0wa4vl469j60yd60iaivprn8h5dl8niiyv")))
   (package
     (name "policy-publisher")
     (version "release_173")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06pqy1wi4z9ihlvpkzsbm1kdyfg81rvjqicqfailm48v55fb3bim")))
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
    (hash (base32 "15w37ahqjvyqkw7ihh1i6l3z7lnrqbbnlvj491nyjbmg170bh0af")))
   (package
     (name "publisher")
     (version "release_1786")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z69ak6vm0gxk834mi122wagix0md5dc4s5kw5q4x5gvnsnd5ra8")))
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
    (hash (base32 "1m0blbl0bjlzxp98851rkvff1s6j9k5iddh2jq1isgn87yigrrwq")))
   (package
     (name "publishing-api")
     (version "release_848")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "050qw6078f98wxc985qvmsnjb8zl0vpgvm6xx46sfa6l3ivmpb65")))
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
     (base32 "1bcs0j60pz620jn19n94sbkvp6drzanw60mmd361vqhgnzj8pmff")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "44058ef1421b81a9c4c11f5a7dba40d0404de29a"
       #:hash (base32 "1xlgs4a7k49h54nv19ax3cd4a17jv74g13zn05hbrv041css3my9")))
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
    (hash (base32 "0vwkj1ihf2h98xpdgbn2wrmgwb9lw69v6pjir840c6q5yzpd2gy2")))
   (package
     (name "release")
     (version "release_255")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ar9fvxcwzmxdah85xala8q3kal9xbngh1k01h62hgr444mzdzmk")))
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
    (version "release_149")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0v15kj1a71xy1dz5warichwabq368wym8168szph8qyr153s5rkj")))
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
     (version "release_114")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1iqk3436m5370vq6dj5pvhjbv48z71fzn5z1xqxgln5gawl50g9y")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))))

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1905s1pw9l1jv71z2paqahdy6hdbywprxq7n64929vz927zwykq9")))
   (package
     (name "rummager")
     (version "release_1330")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l67f6zi77zc5jgf0fic2cmxhrzx89c4n4j3kkw9cwv048jl15rg")))
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
    (hash (base32 "06wy96q5przv7pnc1wxxkl606jyayhyn3si3fi93v9c3rwaiv0sk")))
   (package
     (name "search-admin")
     (version "release_93")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0b0w0p9rimi9g07cidpk13yb71vr9qka37x8agfdxz3xz5rzwlbb")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/search-admin"))
   #:extra-inputs (list mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0jqqd5rpypkrjjpcikx7fps41dzff69b7vi7q6w7hyfk295p7h4d")))
   (package
     (name "service-manual-frontend")
     (version "release_101")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1nzxzrwn44315pddvhzv3mjy2zm3n3f0sg1cnqlqvxsdqwx96n3r")))
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
     (version "release_107")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dh9fshhk93vh1s2xlwv2xkrkq7vn8n4w93k9zm199hnmq0v2siz")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))))

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vidm9aikp74yvdnh2infmf45xyx9sfiya27h4g8s6lj06dx6p2f"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_905")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "059aqq3rx7skwlgasaqyn470nw4bxm9ld3ggnhmlc03ax8xcg2xq")))
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
    (hash (base32 "1947y25sc73l7f3ib64cr9rk90l4mwwmcyw3smnl1b46lnqq9jg3")))
   (package
     (name "smart-answers")
     (version "release_3559")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qkx1mwcwbc5wc5q10zkz2q005w75p58ka686bfg28ysfwfppv97")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "12nc71g807ryi5ywvzcj8r0c5fd3fyc8rx1nj32m3kk5xp07gwx5")))
   (package
     (name "specialist-frontend")
     (version "release_186")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19drxh24yv04pcxkj473x65nwybymscm3bvhk3rylgdkxllmrrhf")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/specialist-frontend"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "01d1v3ays6r7raac5b66a77p4bmiyfbym4737wizjp8sm0ywk7ia"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_804")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06872brbpfnmyk0i0fplpydv70lixsc4hz5xzzf2jsxrxb0xkykk")))
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
    (hash (base32 "0gxry4hdp5scn5majhfmq8zn7sz6bxpfqmxq6r59024a9biybawp")))
   (package
     (name "static")
     (version "release_2552")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vq2jb5y170mm9nhry252dfsdqfxywf9jr7xdvr9qmlah6ws3f9z")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "19sda3i4x1idpja036bnwdvs9l00pim0g1wxhmfh2lkgrakfhmld")))
   (package
     (name "support")
     (version "release_587")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mvkvg041czn949g6ykwcap026dxmaiiz754h6m79iynxfnlcvyr")))
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
     (home-page "https://github.com/alphagov/support"))))

(define-public support-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1n9fgi7hsyzbdx06nmfqswvz199g0gy4x3izmb1jpx8ijlzsj0cg")))
   (package
     (name "support-api")
     (version "release_123")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1y504xxni3bhr6byjka9ls7m60h3vcakjk2khsyf9bs407kp6k79")))
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
   #:extra-inputs (list postgresql)))

(define-public transition
  (package-with-bundler
   (bundle-package
    (hash (base32 "1rg9889j8mqrhz78n6hn7csph625090dvzri6kx5yp55vhwk7m8z")))
   (package
     (name "transition")
     (version "release_786")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vk2spmc7h6afq7q15qh39ahhy4k2a45zs6micdq9qilzd3kjhkk")))
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
     (version "release_251")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zz8nkrxn1f01mnk3mbqzxk6gqa043mwgcdzk5xilmqxj6mh1nf5")))
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
    (hash (base32 "0rsxiil7zjjv18gwdjsnm2r94i8xq8b07bkkrablq6215y234pbr")))
   (package
     (name "whitehall")
     (version "release_12727")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lcwfxd3b6z17vbc998bjjyjgliaq41xga6krraz9nws1p1x3sq1")))
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
