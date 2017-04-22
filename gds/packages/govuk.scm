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

(define (make-govuk-package
         name
         source)
  (package
    (name name)
    (version "0")
    (source source)
    (build-system gnu-build-system)
    (inputs
     `(("coreutils" ,coreutils) ;; just for ls
       ("libffi" ,libffi)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("linux-libre-headers" ,linux-libre-headers)
       ("mysql" ,mysql)
       ("node" ,node)
       ("nss-certs" ,nss-certs)
       ("openssl" ,openssl)
       ("phantomjs" ,phantomjs)
       ("pkg-config" ,pkg-config)
       ("postgresql" ,postgresql)
       ("ruby" ,ruby)
       ("tzdata" ,tzdata)
       ("zlib" ,zlib)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  (ice-9 ftw)
                  ,@%gnu-build-system-modules)
       #:phases
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
         (add-after 'install 'patch-bin-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute*
                   (find-files
                    (string-append out "/bin")
                    (lambda (name stat)
                      (or
                       (access? name X_OK)
                       (begin
                         (simple-format #t "Skipping patching ~A as its not executable\n" name)
                         #f))))
                 (("/usr/bin/env") (which "env")))))))))
    (synopsis name)
    (description name)
    (license #f)
    (home-page #f)))

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

(define-public bouncer
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z05c16a4rz0kf1wq3cqykfm3v1b48kbx32g7zyhniq5ax9hhka2")))
   (package
     (name "bouncer")
     (version "release_203")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19vvkkcc8qsb2gly79bbqrg0vp1hivwsvg5ks497707blhs1qbks")))
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
    (hash (base32 "1717fy92ww92ghhwbfwzhwm86plyi5dkx82db2mdfh84w8m00xm2")))
   (package
     (name "collections")
     (version "release_294")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12dz2m5j7gry6g24lcbadnr1104r8vn7972jhzlnzqc3ksbdjkhm")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1am82z6arb94w8gzmnfzadl1fqza6rw3yvsay3f9kmy8zx8w7h2n")))
   (package
     (name "collections-publisher")
     (version "release_255")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0s5s1br84xcd0gg82v0f2rabbb8ryc5952frs3syjjmzgqvm1ykd")))
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
     (version "release_343")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1vk2spmc7h6afq7q15qh39ahhy4k2a45zs6micdq9qilzd3kjhkk")))
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
     (version "release_72")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "155fb85hpad0i0szm1k9w4a254ydk3bzlmj4niii8pc0icghcs0x")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/contacts-frontend"))))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "1y3az4pqy8a3i712niccvsib3932bzpj99pvfqij9r8ha6v5a78i")))
   (package
     (name "design-principles")
     (version "release_870")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1b4kqpjn5dnxa8y6f7ncazz1sclbrkkhklk8dzrjsfgsq062js31")))
     (build-system rails-build-system)
     (arguments `(;; Asset precompilation fails
                  #:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/design-principles"))))

(define-public email-alert-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "13x9n88f56x4cx18fl409dzkhdr6k8rc2sbvx85jbydwyvvbqy5c")))
   (package
     (name "email-alert-frontend")
     (version "release_30")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0yxjc15dgnszy44zdvcqm84x8nq7y34jx282rylvs630wbd38gac")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))))

(define-public email-alert-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "17nswlp66kkx0vja4yf94zrccgpwzgr7x4nhrz46brbgnv226zn1")))
   (package
     (name "email-alert-api")
     (version "release_161")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1bznicrxasrz7r7d0pz6iy388hy4jax5l05rg8j6c25p55bwk04p")))
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

(define-public email-alert-service
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "1zp0wqjf721fg6fpnwhw30960ill070zpj4lh690s5cdwai1rfja")))
         (make-govuk-package
          "email-alert-service"
          (github-archive
           #:repository "email-alert-service"
           #:commit-ish "release_65"
           #:hash (base32 "0gvds70g8x8qnc13biqj121z0r3p6b9vjvbfhizffxi73zpdfw57"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after
              'patch-bin-files 'wrap-with-relative-path
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out")))
                  (substitute* (find-files (string-append out "/bin"))
                    (((string-append out "/bin"))
                     "${BASH_SOURCE%/*}"))))))))))))

(define-public feedback
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f3jl7i0yiy4svyhg6h74v9m6bwjmc50c65j02s1ncnxn9hb472h")))
   (package
     (name "feedback")
     (version "release_292")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1b8nl2bv5ffgi995c4wwsl0ffscflk30nj2ih7vh3sr6lalnqcf3")))
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
     (version "release_301")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1sczxdadl47z2lzbg2cakd96insvv32yb5fx4wgx9hb1ks5qybvk")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public hmrc-manuals-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "18av10h8wbxmy1kdfd3n5raiavvxadpx88qa7fv7rln1pa3jk26n")))
   (package
     (name "hmrc-manuals-api")
     (version "release_175")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "12c2dm51yq19449xvq3fbdkrbbbppkivlkadldav7zk4p3rpq573")))
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
     (version "release_307")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bnc5x79qlmmhjg81jpwr91235qggkmc1n6hy3sch97gv1a7zn46")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))))

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
    (hash (base32 "0avy99bq44wlclap50nfrkrmdsnmdla0zz718lrvdm1npqdbln90")))
   (package
     (name "local-links-manager")
     (version "release_108")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "11ykzxrrlx7lwjqnv45z0z5x1s26k354ksll0y2w88bnx79p4hg3")))
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
    (hash (base32 "1ka2mwjmbsia3ib64ndjwz4918laxdl7aqgcrwixm8538kfma8ql")))
   (package
     (name "manuals-frontend")
     (version "release_191")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1zvqsrmrkcrvqglinbyrghi7fw1zfc0r6hpgc7yqk5w86g17dj45")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nl2lndhiw9x55z0chwz7ndg86sh67xhk1c13in7na5dcln8jyvn")))
   (package
     (name "manuals-publisher")
     (version "release_850")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "040wwsiajqjpxlzrkahmhcyfsd7x8jljk92g8w4x762qbxzs0yvr")))
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

(define-public multipage-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0h7g4s2yzwp9a77fd5rpwj7sbsj0vfy0216g98j2175nx75b8ic7")))
   (package
     (name "multipage-frontend")
     (version "release_55")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "055ga8s0z9ljmcfkrwhl6if6qshq4bn303dhqr5476a10mgdghf7")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/multipage-frontend"))))

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

(define-public policy-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "029rdnjh0fdx7arijbi1gq3r1hc34xpdi386rfw857n40f1p1zwi")))
   (package
     (name "policy-publisher")
     (version "release_167")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0gzbm31xbkw66bmjxjifxwvhpshf1bvnyl2d8frgd6kwjzi7x3qr")))
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

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "06wy96q5przv7pnc1wxxkl606jyayhyn3si3fi93v9c3rwaiv0sk")))
   (package
     (name "search-admin")
     (version "release_91")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1kr3rlwiydnpdrcw809rmy1xspz9m5i8a1dsqm7fpfyyyn2v4is5")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/search-admin"))
   #:extra-inputs (list mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "023jq5606gvj0wa8v8v7maxhqm8z0k3fkz2bbnajv6wcjd5dm0hf")))
   (package
     (name "service-manual-frontend")
     (version "release_99")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fsgx2p91j43n70mwzdi31s7gi1smdm5y0q9zyp8nby1dxf5qhpf")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))))

(define-public smart-answers
  (package-with-bundler
   (bundle-package
    (hash (base32 "0k91z124p6x17wpnvy23bfr9x24hvp04v0cblg8xg0xi32imb0bh")))
   (package
     (name "smart-answers")
     (version "release_3521")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1q3fnbvw86c4ca4brwlr0igi01a9dd2szb6n5gz7q0fmnd19304m")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "19sda3i4x1idpja036bnwdvs9l00pim0g1wxhmfh2lkgrakfhmld")))
   (package
     (name "support")
     (version "release_586")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "144g3vkc9nvwfjgbxx5c5xanwj2b7k7f64sy13114fz0r3mxcjkc")))
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
     (version "release_628")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0rxb0gzzqp2fv1anjsi0xk9s05jvbf67g6a1195c7dnb2ziqwpmk")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "1q1h9bpshrijrzzrzgfgsrqs017rfmzn4zxrdvqbz0cnj3igj1af")))
   (package
     (name "content-tagger")
     (version "release_341")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "17xgyq9ir72v63f3xfkvjjlsq8w2s0psx9frxg32jv167m0fy801")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql)))

(define-public release
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mbhlvv6jigbd1v04nhcp31n1zbbdl4pb0d36ad29lynsp85ri5l")))
   (package
     (name "release")
     (version "release_232")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lp18bzf2x9gsi12h8kfnn3rz4snyp6b3lp8b51nrmi8zy036xws")))
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

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0anh8zkprbxs2rikwf9xbygxj9fg0x0dysjy3dbhw8f9lq4zhr9z")))
   (package
     (name "service-manual-publisher")
     (version "release_284")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hgmk3y9x7nnq9amy5sy5sk71cqvdmc950vv8mvdr1r8mc2hzvr1")))
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
     (version "release_103")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ll9kq17iaa5jkj7k0ck9r2gydw79j27jjvplbaadk31jsmh87jy")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/short-url-manager"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0iwgfd6ppv40iwps1m3li1al6glh2ckinfby8vs7ckay5yr1bg7v"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_751")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l2ncdw3p1a4kygp5gc2gjzbdh2prdygaqrhvp78q5fr1pk315lv")))
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

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j0sms0d5dwgdlj6y9cfq7ay2r7szk5ih5cs04hrhlqplilk7q1d")))
   (package
     (name "specialist-frontend")
     (version "release_174")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/specialist-frontend"))))

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
    (hash (base32 "1gjd5bf1h3xgy2j449sinzpgbmmird3a75spwfv5vhicw25ij4id")))
   (package
     (name "travel-advice-publisher")
     (version "release_238")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1rp6chk0d2yjb1zbl2yzksid01l9hik0vw03cw2cl5i7dkfb5zy7")))
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

(define-public content-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0csjlacjx7r9iv8j82vl2fbki824bq76hvsjzvbxmabpwwbyn0zk")))
   (package
     (name "content-api")
     (version "release_397")
     (source
      (github-archive
       #:repository "govuk_content_api"
       #:commit-ish version
       #:hash (base32 "0smvrqf81c5xhcrw6bf9b28hj848d4yqibh5chlkm8hflj2d14bv")))
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

(define-public publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "00g07lnd16cz27h46r23pjgg7wjx12y2n0vgj19ch7480aq9va85")))
   (package
     (name "publisher")
     (version "release_1779")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0qwli0c2hd3jqjr3rjlgl3xp4cn0q0g10zqjqczrnqal1nim0182")))
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

(define-public signon
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vidm9aikp74yvdnh2infmf45xyx9sfiya27h4g8s6lj06dx6p2f"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_896")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bsg7pk5g4h2if6x1253rv48hssnm1xsqfjiyyg46w5ag5m13dl1")))
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

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "179brwqjg8hxkqc4r0ga194nc5alfajyp9ms21hbqx7ykh9vwhan")))
   (package
     (name "static")
     (version "release_2441")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "05rbf7x2n2kzz0n1bww0190l7ly9cvvl10j3mdy9gb891sm74n2h")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/static"))))

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

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "18xi02azlxjqll2h9fqhy3rgqm41zhkkh464psli0d6h3hgw3phk")))
   (package
     (name "router-api")
     (version "release_106")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1f2gycb51mi7cfvm83lldczi56l9j6ra9c5db4b6hmm2wigwh53d")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/router-api"))))

(define-public router
  (package
    (name "router")
    (version "release_144")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0lq5zhvsahs436aagsf89bzs9b7ydhysng4kj88is7p69i6f1h2i")))
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

(define-public rummager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1905s1pw9l1jv71z2paqahdy6hdbywprxq7n64929vz927zwykq9")))
   (package
     (name "rummager")
     (version "release_1328")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "19a896x7sv5y468ya51cp84p7s9npbgw1i9sh6nzda37sxfdism5")))
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

(define-public metadata-api
  (package
    (name "metadata-api")
    (version "release_70")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0jdwvx546f4fd7gvablvpg38srja04klw9m9hv9ix2zp94wdrlns")))
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

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "0911wcbx3ixbw3l2yqsmcjqxx8lsfqja3skbj17ah8dr22pszs5d")))
   (package
     (name "maslow")
     (version "release_194")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0npk0nj9mfw7fgs5rarhd8fig3359c6vnj6614jvmpwspqraqdxv")))
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

(define-public need-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0kfxvx2cw42zkc07vayymra5a6sssbqlppsnh73q7hvmr20ykyq8")))
   (package
     (name "need-api")
     (version "release_141")
     (source
      (github-archive
       #:repository "govuk_need_api"
       #:commit-ish "release_141"
       #:hash (base32 "1ds6mp42fflmqm7jx5aw2jfgwr33hc6r8p1krfj0jczsll7r70f9")))
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

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "release_587")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "13hx85nyvycd5nqw6a6ds68pyzw51ix54lnva1vnd5hgwgy4qqmq")))
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

(define-public whitehall
  (package-with-bundler
   (bundle-package
    (hash (base32 "024nibhwc61g1awj3v7mqgq80wmf7jmpa5055ryqkv9xfliijcl2")))
   (package
     (name "whitehall")
     (version "release_12554")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "15c894aaz73657c79nlws6d9243lqz5p9cngj8vqr98m1nqxbxhx")))
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

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nwb9qdz4iybf640rbdfg6v155gh0358c4ly77zzkds9sg79a3c4")))
   (package
     (name "government-frontend")
     (version "release_244")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zlxr1340ax3xpvnry48v3jr9sv6vlr268h647svg2y86a2avm0i")))
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
