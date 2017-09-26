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
    (hash (base32 "1z8n749mbma8gjndsrgqrmp3g59np4rkh6q0821mafvrihkhr2q8")))
   (package
     (name "asset-manager")
     (version "release_147")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00v4bv24c13k6cq9kgr2ccib0p9nyrmxa8j699jhc01jid0ia6xg")))
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
    (hash (base32 "00qm6xwq7qsmivy7p79wcgz5lb6ndfzfb75lcf9zn5rd888wi6x2")))
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
    (hash (base32 "1zc783hlnlzh8l9798h0bmii94kwdhmf0j9n9my24rw8lz5kdia2")))
   (package
     (name "bouncer")
     (version "release_208")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ynbayq3i6hcx78aiw59ibl0knw52yxcs5mkkx9kpbpjrda1v46v")))
     (build-system rails-build-system)
     (synopsis "Rack based redirector backed by the Transition service")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/bouncer"))))

(define-public calculators
  (package-with-bundler
   (bundle-package
    (hash (base32 "1bck6marq2k7m35ivxkz5hvr84xv1dbvv1wzvhmkr3xbiaqbzrvf")))
   (package
     (name "calculators")
     (version "release_199")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1fblxma3lj9ak3a7vqkzbjqag8v4jp5calfmv19nrbgxzxmrssaq")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "0v0jvqxp6qvk9s4an7dq8llayn68964xsvxrqbkdhpda70q95j8f")))
   (package
     (name "calendars")
     (version "release_426")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0m9bzc4fl9xvfzsv7jdhshk97lch5sdwpr82n78hlkjb7m4g73j8")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "1b7xkvx4ww5pqby908c1bg27r5x88d4s2pvymnangqc1cgyb35hv")))
   (package
     (name "collections")
     (version "release_366")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1a7rp4cygx3iipra1shfhaj1w6c2i2x0p0b92wb6i7igvwiw9sv6")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1fhbawz9jn2qhvwv2ximnplzf7dbrspk8bfk8zq4ldyzlm5dk2qr")))
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
    (hash (base32 "1xdccghf5v6v6wr9a69b7v8bg7jn6xnijbcan234rzqr34gvwdzl")))
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
    (hash (base32 "1xjfdbhd5s9zhnmh3kg8kw15m94lm29wy7l2rb7gga2j3c2k33kh")))
   (package
     (name "content-performance-manager")
     (version "release_275")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "09xdajzaslab2vy4ymj72m90q2gxrd389dqmsbrfqrmpq503vyk7")))
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
    (hash (base32 "18bfmqrx4b5lv68ql8bxi8z735mffqsqz9lahwh3lk8jdgraxmj1")))
   (package
     (name "content-store")
     (version "release_665")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0idwqwjphqg5hdx22v3amc9q8rgba2f3js1w36q8mzmwb9sk18ha")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-store"))))

(define-public content-tagger
  (package-with-bundler
   (bundle-package
    (hash (base32 "0qrmhf1y0iil2ap2pyjvn9kr3nnsgp2292fckq46fkxxgram6xqz")))
   (package
     (name "content-tagger")
     (version "release_529")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0wb5ld97vcrvisg875hbx72288rrmi4si66kdi8y5q38ifqqgxpl")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/content-tagger"))
   #:extra-inputs (list postgresql)))

(define-public design-principles
  (package-with-bundler
   (bundle-package
    (hash (base32 "18mrx0jfyns3yv1z9bg3xi5xka4p5gxvr5xjbhz4ixhkd9vd2wnd")))
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
    (hash (base32 "129wa8n9inz5a81bl5cpmrcaa2k8ywhd894fsxi67f4q42wrdf5d")))
   (package
     (name "email-alert-api")
     (version "release_198")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0shxhgc7b69328l2d3r2klxvwg09p4jc3l4qn4vj2rm1qijghk12")))
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
    (hash (base32 "1wcvkwjbrwyk1533i6mfjpafl149n1k10dbp7lxfghmrn0scfhfv")))
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
    (hash (base32 "14223q51y30rsrnind8z62f3mvr7grxwk5ijf84b9fzcxidsjnxh")))
   (package
     (name "email-alert-service")
     (version "release_85")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0jrbkia8jvqxszsg7kzgjf8hadqszrcchrjm43dkgq67j0sbyqbh")))
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
    (hash (base32 "0z8nkpq4wahvilclqr6s12pz6h1pj3ialkjy7j3la3mqawxk0zil")))
   (package
     (name "feedback")
     (version "release_310")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "14lx9m0zv8j5qn2264kckwibad46ndi5skicv612i1fya4km72bj")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1nf1lw7sc58rzcyxnf833rlqswyizpd1ls93szl77569mlsfsg09")))
   (package
     (name "finder-frontend")
     (version "release_319")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10g9chr27sij6gkzfgjb3hnlbgszywij80cyl79c5n15565v5jly")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "15d57npwnwbf2g73m531sl3vh0szwf47m7vsvrnpjbriyqvamb28")))
   (package
     (name "frontend")
     (version "release_2731")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1ha4zd0c437pdw91778qkjvj9hfkbafh8brbfhdm24ljq7bj85d7")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1c6nx8k0z2wf055x9aah6nrrkcxcl6wdk2fa4p8yr1jlgzi24h8x")))
   (package
     (name "government-frontend")
     (version "release_456")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01dk1x5g89d831kv00vxq63d5jhlm1xlylmp0z8nzxj05pamjgrj")))
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
    (version "release_628")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "1qb7pf6zg1acw6gga1bmn0z9v50znljiszsq59ay8dha137yxzds")))
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
    (hash (base32 "0clc4hys4dnsk17d3762g418ryiyzn7ym42vcm7cq2dcz7g53q2k")))
   (package
     (name "hmrc-manuals-api")
     (version "release_191")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r8s2mjzv3hn1n806hj8wvq7virymhwkc95kazxq65x2jlaxpfg5")))
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
    (hash (base32 "0lg1p2ll1lys6pnlbk90k6835113y07wg84d6w2xnym9z7nqk23w")))
   (package
     (name "imminence")
     (version "release_320")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "18425b7a73gn4w7jfak8gi2mxagia7y4w3gp0nramwi71dw9njc8")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/imminence"))))

(define-public info-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f406vh24gnzc0qxwqgnl5jn0zy9qdf9s03q2sms0gsl5rm6gcaa")))
   (package
     (name "info-frontend")
     (version "release_84")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0g4dxjps1fa74jjn5psv5bal5jy537hvfpc0nj16kpywjg76dklz")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/info-frontend"))))

(define-public licence-finder
  (package-with-bundler
   (bundle-package
    (hash (base32 "0mps287rrpmwrxhgppb8qshh6p24j5kyxljnj9b8fw7vck9iphhq")))
   (package
     (name "licence-finder")
     (version "release_291")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x4lvmcb2bbs86x17klygh7g4kg3y4gl9n6ccdjlf8qz1yx5qgsj")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "0jdgb4jw5pjxx932lgssa6jjyv85vj31jd4fslc0fwvdcdnl113z")))
   (package
     (name "local-links-manager")
     (version "release_128")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0kiwkwwkm2i45f09nysh79yk8pilpsb7q3mg97zq1lsfx3p4if8w")))
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
    (hash (base32 "1h8fc5wz0i8yzbqhdr5f043sghhdr6ki5mjhrfnak1yfwvz4j2q5")))
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
    (hash (base32 "0aj4cp5zbhjb9x3x6c8gcpccyhxbffmkdw7rjmsdrj8a4v4xnc27")))
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
    (hash (base32 "0sgwm594fd6k0l0cvmr417fwn3mjd78b1c7iil13iyfy0f1vz1ix")))
   (package
     (name "maslow")
     (version "release_202")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0zv8n3p8f4jh7mmlsai0lmgvq01ykp8miji1ng2a7s498736cbdb")))
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
    (hash (base32 "0yg4ffs433hrw7csa51m93hi04fc2gjrl1xkj2bzhhgrc5lvsv5q")))
   (package
     (name "policy-publisher")
     (version "release_179")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mxwc8s412cwmd6dcvb7xhkk27kwbdgvjq3bg1x7z19rn94al5jp")))
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
    (hash (base32 "1dhqm67a4y4i55wkdixabgcwbngivqk1hhpg4mgainwwpjrw2ki7")))
   (package
     (name "publisher")
     (version "release_1810")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1i7iivqz6jasv8ss9r0pcmvvwfcsyrzp9549nj8500pkvcibcpwq")))
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
    (hash (base32 "11gvn0szfspsx4h08hv48lzn8n53v1fzm79iznz5kwwi41qbplmj")))
   (package
     (name "publishing-api")
     (version "release_1008")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "04ijpdv9zkqimy40456zl58bkw4k9n3ngwy2497p9mjpmsg88mdb")))
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
     (base32 "079kqv4kya282hnh1dr4mn8ksn7zc0mgmk0wjbsl9g41cvg5dqxy")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "97d9bb58332a32b42053f054c45fd0c3b94b0fb5"
       #:hash (base32 "1chsq8almh1qm4026z2hcz3mk2hqrrbd6qlkvzbjnwa6j4ijqsj4")))
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
    (hash (base32 "0yzkv98df4ywpnimc02fd54qf4gsn7rhhwzvhysf5kll3pzfsyqy")))
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
    (version "release_174")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "0mmyakm684kgv23ymwnz2kqpkl5cnam1gybqghy5cwn4k1w6j50c")))
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
    (hash (base32 "08ix77pc459l08gw956dwc4n6fmxq1n5nz2g631jv7r8kx5v8fpl")))
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
    (hash (base32 "03mxlz5cwpg2cakal1bvc2f59fici3zgjrh00fp1q7pmdn4zvj0j")))
   (package
     (name "rummager")
     (version "release_1479")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1lb4451vbx8ha36xdd746r10g2r83wlp8cpy7wl7fjy9c2rdaj2g")))
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
    (hash (base32 "1jgjnhcgax0jfzwrkzyil9598krw3q2hxh9qwbv4vlzl3xr87ax5")))
   (package
     (name "search-admin")
     (version "release_98")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06man4b6y1fsjpjv7xsmibm708r495509j0ry52fmnrvkrmwj797")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/search-admin"))
   #:extra-inputs (list mariadb)))

(define-public service-manual-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "16hjzwjjy63dqgxp6n8kdp2p8gsz89vc6p0mvvh8qipqz5gnmmmx")))
   (package
     (name "service-manual-frontend")
     (version "release_104")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "028gg8w7488b2dwbsiy19v4mmp01pvy869b66jlh08000k0qpn1r")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/service-manual-frontend"))))

(define-public service-manual-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1jw8f05z8v2v0h1gxrkarw8v2hs57bsb66x5pxd31603l493fjci")))
   (package
     (name "service-manual-publisher")
     (version "release_306")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0r4q4779mlbhx12k599jxg0cjib5n3hkpm13kkbm9a3s91q7sfwx")))
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
    (hash (base32 "1l33lx8qbhw0cg7dzwhvxwfrzkflvqxr7qwlvygpnpqqad2d87hn")))
   (package
     (name "short-url-manager")
     (version "release_123")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "02vfvzlqghhsaz2qgdbf71z2bssxjdmzbpbymi0psjl4xjxi4br0")))
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
    (hash (base32 "0813xpm6bfzj7wyyr9iwlsybjrq51r9sbr33a25ipx1rzpv4z3iy"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_924")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ibaa6hfyb74dsw08893yhf1yw66h6i2d6gc7yrqn80yr2l66phl")))
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
    (hash (base32 "16razi14b3jlq4pyhdgylvq8fsyajk0k5xnyw8rrf0my0rycd67y")))
   (package
     (name "smart-answers")
     (version "release_3747")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "192fc7wp0zbn802p8qca9yqa41rn44b2rsspf5cp3f0nm53n1m5j")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1xa7v9zh5d6v0f3km0i1vqzgl4qpmy7wa03adp16xry5ggcdmsfi"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_830")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1d1pqva4kxgz21zlj8lc1c46gp6zi8l57ifx37lrhx2r6l84ypnv")))
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
    (hash (base32 "1yv32mx794cygskvb5qg9d3xg7zr4lmmirhal0nwmpg4c5qnxyqh")))
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
    (hash (base32 "060dp1pblbq9i1vf79220qar1960x165y439hxx181xl8j0hc6zc")))
   (package
     (name "static")
     (version "release_2680")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1mkmal7s2kd7glhj7f4c2lh762wffqh1blgp7ynblia1fwdfdg31")))
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
    (hash (base32 "0b9nzzzzwyanklj5v3g9idsriihzvnq6yz9ym9py63kib02df3c1")))
   (package
     (name "support")
     (version "release_596")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1m7q64wbvp8vm9birc3ihqg8vs8l27vdk8hwbmnfk6cgcxni4am2")))
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
    (hash (base32 "1jbwxnvqzc205gc3yd25sgw2xk8bifb7cvqyhfl4xxb9g2qa6a66")))
   (package
     (name "support-api")
     (version "release_131")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0z53ac843liayy593d8vfzrak3w8r12gwhh62lb1gb1i2dpxfq36")))
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
    (hash (base32 "0bfx5kyjykzr6yh90ansm0k7i97g0whng2dlfq1rqhpy4c9m0qhz")))
   (package
     (name "transition")
     (version "release_795")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08hfi4cdcaxbv7hrvnw635g3vrxkciy0x6bjfdflszslwln16c8z")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/transition"))))

(define-public travel-advice-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0r4kz45bqybnik191xakmzq5w11gxx65i4cda9nyijr74720ay4p")))
   (package
     (name "travel-advice-publisher")
     (version "release_271")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08ghym3c9kdjrhj5zjx8dsl3a947mhlr4ncvsz8d8qx56jqgiisx")))
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
    (hash (base32 "1xlxpl3m4x9h24bli5cf9f7kjwag6awlafxqjns4pg42iy4w5svq")))
   (package
     (name "whitehall")
     (version "release_13008")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0xyibx4hg05y6f5b5lzqczq5qccq9llzp658vvdbqa3xfq9mla5k")))
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
