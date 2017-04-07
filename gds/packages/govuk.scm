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
     `(("bash" ,bash)
       ("ruby" ,ruby)
       ("tzdata" ,tzdata)
       ("gnu-make" ,gnu-make)
       ("gcc-toolchain" ,gcc-toolchain-5)
       ("linux-libre-headers" ,linux-libre-headers)
       ("nss-certs" ,nss-certs)
       ("postgresql" ,postgresql)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("zlib" ,zlib)
       ("libxml2" ,libxml2)
       ("tzdata" ,tzdata)
       ("coreutils" ,coreutils) ;; just for ls
       ("libxslt" ,libxslt)
       ("libffi" ,libffi)
       ("pkg-config" ,pkg-config)
       ("node" ,node)
       ("phantomjs" ,phantomjs)))
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

(define* (package-rails-app name source
                            #:optional #:key
                            (precompile-assets #t)
                            (create-tmp-directory #f))
  (let ((pkg (make-govuk-package name source))
        (phase-modifications
         `((add-before
            'wrap-bin-files-for-bundler 'replace-relative-spring-path
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (files
                      (find-files
                       (string-append out "/bin")
                       (lambda (name stat)
                         (or
                          (access? name X_OK)
                          (begin
                            (simple-format
                             #t
                             "Skipping wrapping ~A as its not executable\n" name)
                            #f))))))
                (substitute* files
                  (("File\\.expand_path\\([\"']\\.\\./spring[\"'], __FILE__\\)")
                   "File.expand_path('../.spring-real', __FILE__)")))))
           (add-after
            'wrap-bin-files-for-bundler 'wrap-with-relative-path
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (files
                      (find-files
                       (string-append out "/bin")
                       (lambda (name stat)
                         (or
                          (access? name X_OK)
                          (begin
                            (simple-format
                             #t
                             "Skipping wrapping ~A as its not executable\n" name)
                            #f))))))
                (substitute* files
                  (((string-append out "/bin"))
                   "${BASH_SOURCE%/*}")))))
           ,@(if
              create-tmp-directory
              '((add-after
                'install 'create-tmp-directory
                (lambda* (#:key outputs #:allow-other-keys)
                  (mkdir (string-append
                          (assoc-ref outputs "out")
                          "/tmp")))))
              '())
           ,@(if
              precompile-assets
              '((add-before
                 'install 'precompile-rails-assets
                 (lambda* (#:key inputs #:allow-other-keys)
                   (zero?
                    (system* "bundle" "exec" "rake" "assets:precompile")))))
              '()))))
    (package
      (inherit pkg)
      (inputs
       `(("inetutils" ,inetutils)  ;; The redis-lock gem depends on hostname
         ,@(package-inputs pkg)))
      (arguments
       (substitute-keyword-arguments
        (package-arguments pkg)
        ((#:phases phases)
         (if (null? phase-modifications)
             phases
             `(modify-phases
               ,phases
               ,@phase-modifications))))))))

(define-public authenticating-proxy
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0al4dn0qjfhyap1cha4r7ix3vz3z0n2g3hb9004l5ifk520mmyi4")))
         (package-rails-app
          "authenticating-proxy"
          (github-archive
           #:repository "authenticating-proxy"
           #:commit-ish "release_37"
           #:hash (base32 "1cxyrmr1klrh1kn9c4414g1k6whlk5hf0pv911q95zizvq4x6nyk"))
          #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
                        ,(replace-mongoid.yml)))))))))

(define-public asset-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1v9mf7qzkrq0lapfag897x59b3ng2md9gwm2xvj3c3252315qbgf")))
   (package-rails-app
    "asset-manager"
    (github-archive
     #:repository "asset-manager"
     #:commit-ish "release_92"
     #:hash (base32 "0b7qxzckzckwgvvw1wq3sgzkf0yj2yp947gz1d4jndz5g4b5j57r"))
    #:precompile-assets #f)))

(define-public bouncer
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z05c16a4rz0kf1wq3cqykfm3v1b48kbx32g7zyhniq5ax9hhka2")))
   (package-rails-app
    "bouncer"
    (github-archive
     #:repository "bouncer"
     #:commit-ish "release_203"
     #:hash (base32 "19vvkkcc8qsb2gly79bbqrg0vp1hivwsvg5ks497707blhs1qbks")))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "0z05c16a4rz0kf1wq3cqykfm3v1b48kbx32g7zyhniq5ax9hhka2")))
   (package-rails-app
    "calculators"
    (github-archive
     #:repository "calculators"
     #:commit-ish "release_182"
     #:hash (base32 "19vvkkcc8qsb2gly79bbqrg0vp1hivwsvg5ks497707blhs1qbks")))))

(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pb8viv6cvp91qg1a9n0648frw5v49kf3z5vn3585fdmx6q9jd9k")))
   (package-rails-app
    "calendars"
    (github-archive
     #:repository "calendars"
     #:commit-ish "release_419"
     #:hash (base32 "0c88ginv7fwhwr0fk5gb894c16icshnl4fdf4ikms20k4qz3nbzm")))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1717fy92ww92ghhwbfwzhwm86plyi5dkx82db2mdfh84w8m00xm2")))
   (package-rails-app
    "collections"
    (github-archive
     #:repository "collections"
     #:commit-ish "release_294"
     #:hash (base32 "12dz2m5j7gry6g24lcbadnr1104r8vn7972jhzlnzqc3ksbdjkhm")))))

(define-public collections-publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "1am82z6arb94w8gzmnfzadl1fqza6rw3yvsay3f9kmy8zx8w7h2n")))
         (package-rails-app
          "collections-publisher"
          (github-archive
           #:repository "collections-publisher"
           #:commit-ish "release_255"
           #:hash (base32 "0s5s1br84xcd0gg82v0f2rabbb8ryc5952frs3syjjmzgqvm1ykd"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
                        ,(use-blank-database.yml)))))))))

(define-public contacts-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "1rg9889j8mqrhz78n6hn7csph625090dvzri6kx5yp55vhwk7m8z")))
   (package-rails-app
    "contacts-admin"
    (github-archive
     #:repository "contacts-admin"
     #:commit-ish "release_343"
     #:hash (base32 "1vk2spmc7h6afq7q15qh39ahhy4k2a45zs6micdq9qilzd3kjhkk"))
    #:precompile-assets #f))) ;; Asset precompilation fails

(define-public contacts-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0pabhmanjs1m0b5zdkwng7p2fpjmwjipvwraimywyq8dikvxl8jh")))
   (package-rails-app
    "contacts-frontend"
    (github-archive
     #:repository "contacts-frontend"
     #:commit-ish "release_72"
     #:hash (base32 "155fb85hpad0i0szm1k9w4a254ydk3bzlmj4niii8pc0icghcs0x")))))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "1y3az4pqy8a3i712niccvsib3932bzpj99pvfqij9r8ha6v5a78i")))
   (package-rails-app
    "design-principles"
    (github-archive
     #:repository "design-principles"
     #:commit-ish "release_870"
     #:hash (base32 "1b4kqpjn5dnxa8y6f7ncazz1sclbrkkhklk8dzrjsfgsq062js31"))
    #:precompile-assets #f))) ;; asset precompilation fails

(define-public email-alert-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "13x9n88f56x4cx18fl409dzkhdr6k8rc2sbvx85jbydwyvvbqy5c")))
   (package-rails-app
    "email-alert-frontend"
    (github-archive
     #:repository "email-alert-frontend"
     #:commit-ish "release_30"
     #:hash (base32 "0yxjc15dgnszy44zdvcqm84x8nq7y34jx282rylvs630wbd38gac")))))

(define-public email-alert-api
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "17nswlp66kkx0vja4yf94zrccgpwzgr7x4nhrz46brbgnv226zn1")))
         (package-rails-app
          "email-alert-api"
          (github-archive
           #:repository "email-alert-api"
           #:commit-ish "release_161"
           #:hash (base32 "1bznicrxasrz7r7d0pz6iy388hy4jax5l05rg8j6c25p55bwk04p"))
          #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after
              'install 'replace-redis.yml
              ,(replace-redis.yml))
             (add-after 'install 'replace-database.yml
               ,(use-blank-database.yml)))))))))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zp0wqjf721fg6fpnwhw30960ill070zpj4lh690s5cdwai1rfja")))
   (make-govuk-package
    "email-alert-service"
    (github-archive
     #:repository "email-alert-service"
     #:commit-ish "release_65"
     #:hash (base32 "0gvds70g8x8qnc13biqj121z0r3p6b9vjvbfhizffxi73zpdfw57")))))

(define-public feedback
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f3jl7i0yiy4svyhg6h74v9m6bwjmc50c65j02s1ncnxn9hb472h")))
   (package-rails-app
    "feedback"
    (github-archive
     #:repository "feedback"
     #:commit-ish "release_292"
     #:hash (base32 "1b8nl2bv5ffgi995c4wwsl0ffscflk30nj2ih7vh3sr6lalnqcf3")))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jh4rnfqpnjzckjxyk62qs1rrg3drj3fzy920iycdy6aqk2d9958")))
   (package-rails-app
    "finder-frontend"
    (github-archive
     #:repository "finder-frontend"
     #:commit-ish "release_301"
     #:hash (base32 "1sczxdadl47z2lzbg2cakd96insvv32yb5fx4wgx9hb1ks5qybvk")))))

(define-public hmrc-manuals-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "18av10h8wbxmy1kdfd3n5raiavvxadpx88qa7fv7rln1pa3jk26n")))
   (package-rails-app
    "hmrc-manuals-api"
    (github-archive
     #:repository "hmrc-manuals-api"
     #:commit-ish "release_175"
     #:hash (base32 "12c2dm51yq19449xvq3fbdkrbbbppkivlkadldav7zk4p3rpq573"))
    #:precompile-assets #f)))

(define-public imminence
  (package-with-bundler
   (bundle-package
    (hash (base32 "1smwll8m13215dd8m1dsxii847cd054vfq896xsivrqy73fs0asm")))
   (package-rails-app
    "imminence"
    (github-archive
     #:repository "imminence"
     #:commit-ish "release_307"
     #:hash (base32 "0bnc5x79qlmmhjg81jpwr91235qggkmc1n6hy3sch97gv1a7zn46")))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "1cfgxbm7qhhfn2w27khn7avn3q0w7961zpj88i8vq6vhlvdn5qzj")))
   (package-rails-app
    "licence-finder"
    (github-archive
     #:repository "licence-finder"
     #:commit-ish "release_279"
     #:hash (base32 "0jgsravvf4ssn6mf5q1hpiwxvnp8ccgvv1cj80254a0fbix2qg0h")))))

(define-public local-links-manager
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "0avy99bq44wlclap50nfrkrmdsnmdla0zz718lrvdm1npqdbln90")))
          (package-rails-app
           "local-links-manager"
           (github-archive
            #:repository "local-links-manager"
            #:commit-ish "release_108"
            #:hash (base32 "11ykzxrrlx7lwjqnv45z0z5x1s26k354ksll0y2w88bnx79p4hg3"))
           #:create-tmp-directory #t
           #:precompile-assets #f)))) ;; Asset precompilation fails
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
                        ,(use-blank-database.yml)))))))))

(define-public manuals-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ka2mwjmbsia3ib64ndjwz4918laxdl7aqgcrwixm8538kfma8ql")))
   (package-rails-app
    "manuals-frontend"
    (github-archive
     #:repository "manuals-frontend"
     #:commit-ish "release_191"
     #:hash (base32 "1zvqsrmrkcrvqglinbyrghi7fw1zfc0r6hpgc7yqk5w86g17dj45")))))

(define-public manuals-publisher
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "0wc6gfffn1hfsxcp479icw1r4rlzz4wgj9hi1vmfs3kng0pyzxyn")))
          (package-rails-app
           "manuals-publisher"
           (github-archive
            #:repository "manuals-publisher"
            #:commit-ish "release_744"
            #:hash (base32 "1zj6b0kbpf29g78q330p2chh04n8fzzy2587py5qr1zm9f8k4bvm"))
           #:create-tmp-directory #t
           #:precompile-assets #f)))) ;; Asset precompilation fails
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
                        ,(replace-mongoid.yml)))))))))

(define-public multipage-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0h7g4s2yzwp9a77fd5rpwj7sbsj0vfy0216g98j2175nx75b8ic7")))
   (package-rails-app
    "multipage-frontend"
    (github-archive
     #:repository "multipage-frontend"
     #:commit-ish "release_55"
     #:hash (base32 "055ga8s0z9ljmcfkrwhl6if6qshq4bn303dhqr5476a10mgdghf7")))))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1m0blbl0bjlzxp98851rkvff1s6j9k5iddh2jq1isgn87yigrrwq")))
   (package-rails-app
    "publishing-api"
    (github-archive
     #:repository "publishing-api"
     #:commit-ish "release_838"
     #:hash (base32 "1j3v57z556ryyd1hskrhx88kfgc4pww2l264a7qg3bqfw3xacb9q"))
    #:precompile-assets #f)))

(define-public policy-publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "029rdnjh0fdx7arijbi1gq3r1hc34xpdi386rfw857n40f1p1zwi")))
         (package-rails-app
          "policy-publisher"
          (github-archive
           #:repository "policy-publisher"
           #:commit-ish "release_167"
           #:hash (base32 "0gzbm31xbkw66bmjxjifxwvhpshf1bvnyl2d8frgd6kwjzi7x3qr"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
                        ,(use-blank-database.yml)))))))))

(define-public search-admin
  (package-with-bundler
   (bundle-package
    (hash (base32 "06wy96q5przv7pnc1wxxkl606jyayhyn3si3fi93v9c3rwaiv0sk")))
   (package-rails-app
    "search-admin"
    (github-archive
     #:repository "search-admin"
     #:commit-ish "release_91"
     #:hash (base32 "1kr3rlwiydnpdrcw809rmy1xspz9m5i8a1dsqm7fpfyyyn2v4is5")))))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "023jq5606gvj0wa8v8v7maxhqm8z0k3fkz2bbnajv6wcjd5dm0hf")))
   (package-rails-app
    "service-manual-frontend"
    (github-archive
     #:repository "service-manual-frontend"
     #:commit-ish "release_99"
     #:hash (base32 "1fsgx2p91j43n70mwzdi31s7gi1smdm5y0q9zyp8nby1dxf5qhpf")))))

(define-public smart-answers
  (package-with-bundler
   (bundle-package
    (hash (base32 "0k91z124p6x17wpnvy23bfr9x24hvp04v0cblg8xg0xi32imb0bh")))
   (package-rails-app
    "smart-answers"
    (github-archive
     #:repository "smart-answers"
     #:commit-ish "release_3521"
     #:hash (base32 "1q3fnbvw86c4ca4brwlr0igi01a9dd2szb6n5gz7q0fmnd19304m"))
    #:precompile-assets #f))) ;; asset precompilation fails

(define-public support
  (package-with-bundler
   (bundle-package
    (hash (base32 "19sda3i4x1idpja036bnwdvs9l00pim0g1wxhmfh2lkgrakfhmld")))
   (package-rails-app
    "support"
    (github-archive
     #:repository "support"
     #:commit-ish "release_586"
     #:hash (base32 "144g3vkc9nvwfjgbxx5c5xanwj2b7k7f64sy13114fz0r3mxcjkc"))
    #:precompile-assets #f))) ;; asset precompilation fails

(define-public support-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1n9fgi7hsyzbdx06nmfqswvz199g0gy4x3izmb1jpx8ijlzsj0cg")))
   (package-rails-app
    "support-api"
    (github-archive
     #:repository "support-api"
     #:commit-ish "release_123"
     #:hash (base32 "1y504xxni3bhr6byjka9ls7m60h3vcakjk2khsyf9bs407kp6k79")))))

(define-public content-performance-manager
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0fbwnx7j4qvn4aj5vi4qr78n7n1a2mhhgj9ayhv8d5z8d3cpcwi8")))
         (package-rails-app
          "content-performance-manager"
          (github-archive
           #:repository "content-performance-manager"
           #:commit-ish "release_30"
           #:hash (base32 "12z9800nlm2vl5r7nznh8ah0sv1768id0xxhkn38m9wwjxlq53qj"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
                        ,(use-blank-database.yml)))))))))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "1chs8dym0izp367cczbhqsdj2d2hvnx7pfb7bax60d1w040jyamh")))
   (package-rails-app
    "content-store"
    (github-archive
     #:repository "content-store"
     #:commit-ish "release_619"
     #:hash (base32 "1zgb3fzjrv7rf9qh0vfzayjh77ch1zp6rbd8n74jsd4w959xy57n"))
    #:precompile-assets #f)))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "1q1h9bpshrijrzzrzgfgsrqs017rfmzn4zxrdvqbz0cnj3igj1af")))
   (package-rails-app
    "content-tagger"
    (github-archive
     #:repository "content-tagger"
     #:commit-ish "release_341"
     #:hash (base32 "17xgyq9ir72v63f3xfkvjjlsq8w2s0psx9frxg32jv167m0fy801")))))

(define-public release
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mbhlvv6jigbd1v04nhcp31n1zbbdl4pb0d36ad29lynsp85ri5l")))
   (package-rails-app
    "release"
    (github-archive
     #:repository "release"
     #:commit-ish "release_232"
     #:hash (base32 "1lp18bzf2x9gsi12h8kfnn3rz4snyp6b3lp8b51nrmi8zy036xws")))))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0anh8zkprbxs2rikwf9xbygxj9fg0x0dysjy3dbhw8f9lq4zhr9z")))
   (package-rails-app
    "service-manual-publisher"
    (github-archive
     #:repository "service-manual-publisher"
     #:commit-ish "release_284"
     #:hash (base32 "0hgmk3y9x7nnq9amy5sy5sk71cqvdmc950vv8mvdr1r8mc2hzvr1")))))

(define-public short-url-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "155mp2xi968hhzkk3rk6rj5rd4qgcp0rb6n01aim01515xzy84il")))
   (package-rails-app
    "short-url-manager"
    (github-archive
     #:repository "short-url-manager"
     #:commit-ish "release_103"
     #:hash (base32 "1ll9kq17iaa5jkj7k0ck9r2gydw79j27jjvplbaadk31jsmh87jy"))
    #:create-tmp-directory #t
    #:precompile-assets #f))) ;; Fails as can't connect to MongoDB

(define-public specialist-publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0iwgfd6ppv40iwps1m3li1al6glh2ckinfby8vs7ckay5yr1bg7v"))
          (without '("development" "test")))
         (package-rails-app
          "specialist-publisher"
          (github-archive
           #:repository "specialist-publisher"
           #:commit-ish "release_751"
           #:hash (base32 "1l2ncdw3p1a4kygp5gc2gjzbdh2prdygaqrhvp78q5fr1pk315lv"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after
              'install 'alter-secrets.yml
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* (string-append
                              (assoc-ref outputs "out")
                              "/config/secrets.yml")
                  (("SECRET_TOKEN")
                   "SECRET_KEY_BASE")))))))))))

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0j0sms0d5dwgdlj6y9cfq7ay2r7szk5ih5cs04hrhlqplilk7q1d")))
   (package-rails-app
    "specialist-frontend"
    (github-archive
     #:repository "specialist-frontend"
     #:commit-ish "release_174"
     #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44"))
    #:precompile-assets #t)))

(define-public transition
  (package-with-bundler
   (bundle-package
    (hash (base32 "1rg9889j8mqrhz78n6hn7csph625090dvzri6kx5yp55vhwk7m8z")))
   (package-rails-app
    "transition"
    (github-archive
     #:repository "transition"
     #:commit-ish "release_786"
     #:hash (base32 "1vk2spmc7h6afq7q15qh39ahhy4k2a45zs6micdq9qilzd3kjhkk")))))

(define-public travel-advice-publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "1gjd5bf1h3xgy2j449sinzpgbmmird3a75spwfv5vhicw25ij4id")))
         (package-rails-app
          "travel-advice-publisher"
          (github-archive
           #:repository "travel-advice-publisher"
           #:commit-ish "release_238"
           #:hash (base32 "1rp6chk0d2yjb1zbl2yzksid01l9hik0vw03cw2cl5i7dkfb5zy7"))))))
       (package
        (inherit pkg)
        (arguments
         (substitute-keyword-arguments (package-arguments pkg)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'install 'replace-mongoid.yml
                ,(replace-mongoid.yml)))))))))

(define-public content-api
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0csjlacjx7r9iv8j82vl2fbki824bq76hvsjzvbxmabpwwbyn0zk")))
         (package-rails-app
          "content-api"
          (github-archive
           #:repository "govuk_content_api"
           #:commit-ish "release_397"
           #:hash (base32 "0smvrqf81c5xhcrw6bf9b28hj848d4yqibh5chlkm8hflj2d14bv"))
          #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:path "/mongoid.yml")))))))))

(define-public publisher
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "00g07lnd16cz27h46r23pjgg7wjx12y2n0vgj19ch7480aq9va85")))
         (package-rails-app
          "publisher"
          (github-archive
           #:repository "publisher"
           #:commit-ish "release_1779"
           #:hash (base32 "0qwli0c2hd3jqjr3rjlgl3xp4cn0q0g10zqjqczrnqal1nim0182"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml))
             (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
               ,(replace-gds-sso-initializer)))))))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0vkasdpgvz368f924qfk57786qmc9m2ysxfd6gjziwrdbvkq877j")))
   (package-rails-app
    "frontend"
    (github-archive
     #:repository "frontend"
     #:commit-ish "release_2120"
     #:hash (base32 "02b8k1gvsr57k2kfjb6wdr8mn07yhh55c4pdb2adfdij0lb2rskl")))))

(define-public signon
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "0vidm9aikp74yvdnh2infmf45xyx9sfiya27h4g8s6lj06dx6p2f"))
           (without '("development" "test")))
          (package-rails-app
           "signon"
           (github-archive
            #:repository "signon"
            #:commit-ish "release_896"
            #:hash (base32 "0bsg7pk5g4h2if6x1253rv48hssnm1xsqfjiyyg46w5ag5m13dl1"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
               ,(use-blank-database.yml)))))))))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "179brwqjg8hxkqc4r0ga194nc5alfajyp9ms21hbqx7ykh9vwhan")))
   (package-rails-app
    "static"
    (github-archive
     #:repository "static"
     #:commit-ish "release_2441"
     #:hash (base32 "05rbf7x2n2kzz0n1bww0190l7ly9cvvl10j3mdy9gb891sm74n2h"))
    #:precompile-assets #t)))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "10c8npzfidrs9wk29nb4b1vba2islkf83i57c410a3z0xm2hm51k")))
   (package-rails-app
    "info-frontend"
    (github-archive
     #:repository "info-frontend"
     #:commit-ish "release_71"
     #:hash (base32 "0qj5q7fv4x122x41j2ln4zg1sk017930ap51650pn5g6jdb3pcyx"))
    #:precompile-assets #t)))

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "18xi02azlxjqll2h9fqhy3rgqm41zhkkh464psli0d6h3hgw3phk")))
   (package-rails-app
    "router-api"
    (github-archive
     #:repository "router-api"
     #:commit-ish "release_106"
     #:hash (base32 "1f2gycb51mi7cfvm83lldczi56l9j6ra9c5db4b6hmm2wigwh53d"))
    #:precompile-assets #f)))

(define-public router
  (let
      ((release "release_144"))
    (package
      (name "router")
      (version release)
      (source
       (github-archive
        #:repository "router"
        #:commit-ish release
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
                     (zero? (system* "make" "build" (string-append "RELEASE_VERSION=" ,release)))
                     (mkdir-p (string-append out "/bin"))))
                  (begin
                    (copy-file "router"
                               (string-append out "/bin/router"))
                    #t))))))))
      (synopsis "")
      (description "")
      (license "")
      (home-page "https://github.com/alphagov/router"))))

(define-public rummager
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "1905s1pw9l1jv71z2paqahdy6hdbywprxq7n64929vz927zwykq9")))
          (package-rails-app
           "rummager"
           (github-archive
            #:repository "rummager"
            #:commit-ish "release_1328"
            #:hash (base32 "19a896x7sv5y468ya51cp84p7s9npbgw1i9sh6nzda37sxfdism5"))
           #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after
              'install 'replace-redis.yml
              ,(replace-redis.yml)))))))))

(define-public metadata-api
  (let
      ((release "release_70"))
    (package
      (name "metadata-api")
      (version release)
      (source
       (github-archive
        #:repository "metadata-api"
        #:commit-ish release
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
      (home-page "https://github.com/alphagov/metadata-api"))))

(define-public publishing-e2e-tests
  (package-with-bundler
   (bundle-package
    (hash
     (base32 "1bcs0j60pz620jn19n94sbkvp6drzanw60mmd361vqhgnzj8pmff")))
   (make-govuk-package
    "publishing-e2e-tests"
    (github-archive
     #:repository "publishing-e2e-tests"
     #:commit-ish "44058ef1421b81a9c4c11f5a7dba40d0404de29a"
     #:hash (base32 "1xlgs4a7k49h54nv19ax3cd4a17jv74g13zn05hbrv041css3my9")))))

(define-public maslow
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0911wcbx3ixbw3l2yqsmcjqxx8lsfqja3skbj17ah8dr22pszs5d")))
         (package-rails-app
          "maslow"
          (github-archive
           #:repository "maslow"
           #:commit-ish "release_194"
           #:hash (base32 "0npk0nj9mfw7fgs5rarhd8fig3359c6vnj6614jvmpwspqraqdxv"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:mongoid-version "3"))
             (add-after 'replace-mongoid.yml 'replace-gds-sso-initializer
               ,(replace-gds-sso-initializer)))))))))

(define-public need-api
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "0kfxvx2cw42zkc07vayymra5a6sssbqlppsnh73q7hvmr20ykyq8")))
         (package-rails-app
          "need-api"
          (github-archive
           #:repository "govuk_need_api"
           #:commit-ish "release_141"
           #:hash (base32 "1ds6mp42fflmqm7jx5aw2jfgwr33hc6r8p1krfj0jczsll7r70f9"))
          #:precompile-assets #f))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-mongoid.yml
               ,(replace-mongoid.yml #:mongoid-version "3")))))))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (github-archive
      #:repository "govuk-content-schemas"
      #:commit-ish "release_583"
      #:hash (base32 "0nwf7b7bw14v60c31800695ai5bny9ac51lr92i10dl4jkwc5rr8")))
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
  (let
      ((pkg
        (package-with-bundler
         (bundle-package
          (hash (base32 "024nibhwc61g1awj3v7mqgq80wmf7jmpa5055ryqkv9xfliijcl2")))
         (package-rails-app
          "whitehall"
          (github-archive
           #:repository "whitehall"
           #:commit-ish "release_12554"
           #:hash (base32 "15c894aaz73657c79nlws6d9243lqz5p9cngj8vqr98m1nqxbxhx"))))))
    (package
      (inherit pkg)
      (inputs
       `(("curl" ,curl)
         ("imagemagick" ,imagemagick)
         ,@(package-inputs pkg)))
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'replace-database.yml
                        ,(use-blank-database.yml))
             (add-after 'install 'set-bulk-upload-zip-file-tmp
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* (string-append
                              (assoc-ref outputs "out")
                              "/config/initializers/bulk_upload_zip_file.rb")
                  (("Rails\\.root\\.join\\('bulk-upload-zip-file-tmp'\\)")
                   "\"/tmp/whitehall/bulk-upload-zip-file\"")))))))))))

(define-public government-frontend
  (let ((pkg
         (package-with-bundler
          (bundle-package
           (hash (base32 "1nwb9qdz4iybf640rbdfg6v155gh0358c4ly77zzkds9sg79a3c4")))
          (package-rails-app
           "government-frontend"
           (github-archive
            #:repository "government-frontend"
            #:commit-ish "release_244"
            #:hash (base32 "0zlxr1340ax3xpvnry48v3jr9sv6vlr268h647svg2y86a2avm0i"))))))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'bundle-install 'replace-ruby-version
                         ,(replace-ruby-version (package-version ruby))))))))))
