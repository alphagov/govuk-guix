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
    (hash (base32 "0q1wcn6vmqscjd4swfrrjklnz69fr1lz5aimzj96xq9m9x0bmymw")))
   (package
     (name "asset-manager")
     (version "release_210")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0hqswnnrr7fwajvpjhyzgc3f9w6aqkl13nhxw44s551l50bsxhfa")))
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
    (hash (base32 "075hlkfxcq8f5icgkr4rxvfp2qihi2yl4ic20lsryqicxbd79c5n")))
   (package
     (name "authenticating-proxy")
     (version "release_50")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1qkp3zv4abba0ca0xjxkb6cak3258a100p6bn667kk482mvzxfbd")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
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
    (hash (base32 "14pb6532apainqy7clrq4fmvh7ga2x12l7bb54kjkj820b8hvgcr")))
   (package
     (name "calculators")
     (version "release_222")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "108yfnzyxzcbqwmzijkkmg940kn7q18fn1zrbi9x1fggwv7wf1wi")))
     (build-system rails-build-system)
     (synopsis "Calculators provides the Child benefit tax calculator")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calculators"))
   #:extra-inputs (list libffi)))


(define-public calendars
  (package-with-bundler
   (bundle-package
    (hash (base32 "1lh8v8z5ixg619hwzk5g6ix0bwpgg7mcdi32k6achfpa0n4pis11")))
   (package
     (name "calendars")
     (version "release_447")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x4pbvzrkm84ar2n4jhc0brpfl826xmk6jgvv9nij1qk6smq3pka")))
     (build-system rails-build-system)
     (synopsis "Serves calendars on GOV.UK")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/calendars"))
   #:extra-inputs (list libffi)))

(define-public collections
  (package-with-bundler
   (bundle-package
    (hash (base32 "0bv8znyvqypnj133ffnvf5agn59riprvz686pqnhzipsj8p3c65m")))
   (package
     (name "collections")
     (version "release_419")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0cybwacgvnb7lk6zsrg7s0cw0sv9f9an2hkkd6qzw9wsvzdmn18x")))
     (build-system rails-build-system)
     (synopsis "Collections serves the new GOV.UK navigation and other pages")
     (description "")
     (license license:expat)
     (home-page "https://github.com/alphagov/collections"))
   #:extra-inputs (list libffi)))

(define-public collections-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1dkwr81lgaa5s8x8nhbw8hqz3p4jayn09mzr483as28j18gda5jg")))
   (package
     (name "collections-publisher")
     (version "release_290")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "10anxr3k2104a3dn2vjp0yf0gf7xjjfyh5gsc2wsx2m4in2yacgb")))
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
    (hash (base32 "1v3jj3js06v6rwf32rvwn6l244slplvn91vysjm0f2f6qdcb8d2j")))
   (package
     (name "contacts-admin")
     (version "release_373")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0w0wr8ixqp4bx4f2rbsgb2lh04hm5whk9czml42hp6cxry24n8g4")))
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
    (hash (base32 "1mnh5lsd3s86hv9hdnbvkkg3zj9lyzwnfmsmcgbl9sd0764cz3qj")))
   (package
     (name "content-performance-manager")
     (version "release_352")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1f21hfs26g0agikhf1zkc01k52z4qv4320k568xni55p2jmm6ynw")))
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
    (hash (base32 "0cyj767ky58mfbaz593jhhwc1n3p1nrysdw5r17zg603z3bpg2zb")))
   (package
     (name "content-store")
     (version "release_694")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1njmfqjmdyb0q5bn5ssfjdf0682gmfcf5qi6kbk3nww5k3vc715b")))
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
    (hash (base32 "0qrqhfz6da2c7yjs64fsngy7lbvl120rbxy3idd4nkr2rid1zhb8")))
   (package
     (name "content-tagger")
     (version "release_640")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0yr3lxxph853rlqqfhab2jp35ldmalbx8c8sr09kpgs5qdykgvvb")))
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
    (hash (base32 "0750ph6k24r8kg4l7la5q1b0f5zlpbiy2zdbsmvwf6g1cd6prrw6")))
   (package
     (name "email-alert-api")
     (version "release_352")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0966rhjyy1mm4prlymyxc64s4iqzqds15xlqsflphlcahfz0ssil")))
     (build-system rails-build-system)
     (arguments
      `(#:precompile-rails-assets? #f
        #:phases
        (modify-phases %standard-phases
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
    (hash (base32 "19m1k2p9j7a5bldaxly31gxyna10lfx420lrs0smnq1x3bg8a1wj")))
   (package
     (name "email-alert-frontend")
     (version "release_77")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l1nmfa1s29qn69ynwddiiaybdnxbr41sbwj7wbnyfgsg3kikl2i")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/email-alert-frontend"))
   #:extra-inputs (list libffi)))

(define-public email-alert-service
  (package-with-bundler
   (bundle-package
    (hash (base32 "1z2b15ggn35x2qimc63hwfzs6a84l2fmlcj1dnfvss39ga53g5j2")))
   (package
     (name "email-alert-service")
     (version "release_102")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "03fyzz2lxxz5mzp6xfw51qkj81d29qlvj14snqmm6qdh6v9kv3vc")))
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
    (hash (base32 "17x3yb8wahbs2wv44yqk5p7csf9iz0ymfvb26rd7dq2xia859gar")))
   (package
     (name "feedback")
     (version "release_351")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0q2q6fxlsvwq79y2f6jh06v9n2w8ipdm0sw40mcmysjs9yfsdkxd")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/feedback"))
   #:extra-inputs (list libffi)))

(define-public finder-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "0ra298r1alhqcfmk6ahxxh5ps4w32sdj1p2vc3g3mvy9k27a3sjq")))
   (package
     (name "finder-frontend")
     (version "release_354")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ag1drk4j078fp1q8z9zjj6szly3xfs1qrgv7p01853mdwrjmnwh")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/finder-frontend"))
   #:extra-inputs (list libffi)))

(define-public frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1pbap3x3s7pqx26lp8nhk35q8q34nmg5j9k5n937lc1awz78fqqh")))
   (package
     (name "frontend")
     (version "release_2794")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "00r8mqr2djbxvgz57rak30zfwsf76h9h6d1hma81vvny1q4xsim2")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/frontend"))))

(define-public government-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1128jfzdaw7x7n8ywjmmhqxa1k8nx1jhml4y0q37qrfc3srcj558")))
   (package
     (name "government-frontend")
     (version "release_563")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0x006qa4bvl75v59d4s6y9mbkl4rd2rjr9nrv5klhmdh1ipkqdrl")))
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
    (version "release_671")
    (source
     (github-archive
      #:repository name
      #:commit-ish version
      #:hash (base32 "07l027p0lihswqnz6wzwpga6pm49crdvxzn0q571dbpnwz73h24x")))
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
    (hash (base32 "1s62bw8414380smfb2yklgidl4ibmjc1aa0dbh7jiiyikpknd7cz")))
   (package
     (name "hmrc-manuals-api")
     (version "release_216")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0ysgs67mr10z1g3h5civx5qz8jzsf3j86xhmwvk7n21pl65gc0hj")))
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
    (hash (base32 "0w05bwcw6ha7aiq7ka78mcblpmbavz4mdb6sap124rfcacix40ml")))
   (package
     (name "info-frontend")
     (version "release_93")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "157qa96r6qgfm626xv1j3irrfn4ybyiz8c7fw28x4m0j6lfjdn7x")))
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
    (hash (base32 "0vl21zxz2aw09mvfynk1wx5wq56v4pvvyxvcl7g5qbslghrbfcqm")))
   (package
     (name "licence-finder")
     (version "release_310")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06014yif6zmivrny5gb6iafa5kh5skqxnf9ni1zi609d2ywn7acf")))
     (build-system rails-build-system)
     (arguments `(#:ruby ,ruby-2.3))
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/licence-finder"))))

(define-public local-links-manager
  (package-with-bundler
   (bundle-package
    (hash (base32 "1zg23hq724y1c60gwy5kfkbv87xbcn8x0gv49aw31xk1y8fgdyvc")))
   (package
     (name "local-links-manager")
     (version "release_144")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1042zkkf7larqnizd84sy5jphw38srckwsy6h2y0argw71njb94y")))
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
    (hash (base32 "0dhjjnbkaa7kw4v85wnjn66408k0ylb899g5pf722n1i01zv0mja")))
   (package
     (name "manuals-frontend")
     (version "release_236")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0y6wy3q7q66vh030d40bz7lna0jjnys9jlirm0ncvikmas20g8dg")))
     (build-system rails-build-system)
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/manuals-frontend"))
   #:extra-inputs (list libffi)))

(define-public manuals-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "15c53fxiz3gsasx42ad77277izywmd7i8q3q10fr8sn7kncda7vv")))
   (package
     (name "manuals-publisher")
     (version "release_1008")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0lxhyfy20sj31i1zqy0ajycnn8s6szhpmbh730l3bg6a2xhz274s")))
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
    (hash (base32 "1zl6p4f99d5mhbma2hfhbkj30hxd99n4dlmzh72l5p1ldd64nnjs")))
   (package
     (name "maslow")
     (version "release_221")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p704ajl4m8kf76v8k69nmdqad6md0422gdbkig386v97y2lfdc5")))
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
    (hash (base32 "1q0smcaq16m4pinb0lgs649m3rihc1x0yx1nlrwcq2bskir2pmbz")))
   (package
     (name "policy-publisher")
     (version "release_207")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "06zriig6c52snq4rdymmzxabm51lhrz6zp9nh347hjypxfisplq0")))
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
    (hash (base32 "1gr8bb4fw345gqha7ygy8z3m0m8yi85cc0j0dvl420qpm1n1g10b")))
   (package
     (name "publisher")
     (version "release_1864")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1hynav04xywkh0p7si3k88g1w7llpbc3p4rdgd1bfxgy5alhrf0l")))
     (build-system rails-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
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
    (hash (base32 "0ddw555rd2419g17xqa96zhpi63dxr5qbxmpz5lff49yzrsaplg5")))
   (package
     (name "publishing-api")
     (version "release_1075")
     (source
      (github-archive
       #:repository "publishing-api"
       #:commit-ish version
       #:hash (base32 "12jf0684hcmfb20zzar4wnvvydrqyryn6qkvnyp9qq6vy75rbsb8")))
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
     (base32 "0w1air5z0dhdwbsm5mml1lnvx04k893rln4fypxj6xmvb28aw60i")))
   (package
     (name "publishing-e2e-tests")
     (version "0")
     (source
      (github-archive
       #:repository "publishing-e2e-tests"
       #:commit-ish "16c03fb65aae5cb3df5523fe51abb125375bafb3"
       #:hash (base32 "1pv6nqqc9c69nv3dnpdfjcwq9g9s9disz41sdjsrgzj38m0xclf8")))
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
    (hash (base32 "1qymvf2w9a2fqngh7jpax270y8ylry7a9h5h83a7gax1a84wvvm5")))
   (package
     (name "router-api")
     (version "release_130")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "01vsy1pdwyw8b875xd882n5k32gv4dfybvidybwh1d883flznbnm")))
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
    (hash (base32 "0gqa2jbmgzvcxnjn4zjy5hcx0hncg11ss0d8q6fxk3l38l4rvxcm")))
   (package
     (name "rummager")
     (version "release_1650")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "096xjdgd5z8v7g6ri0aw33c5rg1j77p71n2xyid6zd7yybnm1npg")))
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
    (hash (base32 "12h4yaya9f3jvdbqzlikbmwk9nsddp6rhc4iwfh070zx2f6300kc")))
   (package
     (name "search-admin")
     (version "release_119")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0a21pxxhfx2s21xk8kypshbq4rqg50ys8wdm5x5bn41avia70hff")))
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
    (hash (base32 "13nmbskywvn1b5393p5g5faw3yz694bybacm001cjmb9h0dpkg75")))
   (package
     (name "short-url-manager")
     (version "release_146")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0bi1cl1cvzs17sag2a8yzdv1jr3398grc5cgf9033hpysi52q7an")))
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
    (hash (base32 "05q7qnxrpp1y6hqlvsa4l1nad876jnkcs62as606y57vi4n4phwn"))
    (without '("development" "test")))
   (package
     (name "signon")
     (version "release_960")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1l1nyacx77236c06zlx40wm05p4h716x7qjnj51lm0s61ar8hir9")))
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
    (hash (base32 "0qhfv5dvkibz1vxx73dlh5cqfzin3akhm5a99rv8k6pl9wmk4iha")))
   (package
     (name "smart-answers")
     (version "release_3811")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "113yk8gkb0a7c2ixp17ldnj7y1vycm8x0m9d06av2zicdpy2bc85")))
     (build-system rails-build-system)
     (arguments `(#:precompile-rails-assets? #f)) ;; Asset precompilation fails
     (synopsis "")
     (description "")
     (license #f)
     (home-page "https://github.com/alphagov/smart-answers"))
   #:extra-inputs (list libffi)))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "0p4ln3a6klj0db6jkpjy5f4zrs1kjm5cb3sks9cjfbwylhckfimz"))
    (without '("development" "test")))
   (package
     (name "specialist-publisher")
     (version "release_864")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1v7hdqn32hr4h20h9zzsi2yfr4q7034z9yz3c7155mg9gvxbrg3l")))
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
    (hash (base32 "0p5l2x1dhzmjqx5898a8r7kcm110n3lc03b04zwahwdd7x686pzm")))
   (package
     (name "static")
     (version "release_2750")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "1dgn6sxisc1rkidl5jn446z8lym3fdl63jr0jq2znr5qr0jp2v4c")))
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
    (hash (base32 "0z5q3gckvhwmsl8r6mcdwq3pa0vfcvanvqnrg8ricmcakwq6pmbs")))
   (package
     (name "support")
     (version "release_616")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "08dcl3zcc8a2l4fi758rjhn7gxgy8glcgyxpvw90l6qwsmxy0alv")))
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
    (hash (base32 "1xq5lg1bq0svz2dhlk98si4l0h1wh86lnl8cs9al05v9j8d12gyv")))
   (package
     (name "support-api")
     (version "release_146")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "032x351pq78qm62c4d80zyygjzr9y8qmp61frh045xdvc49qjmm9")))
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
    (hash (base32 "1qg66y4pp0ysjyq8ahckfabf7ssxbsvdba7h36vlmva8l0bkm5ng")))
   (package
     (name "transition")
     (version "release_814")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0p1aa3mlc7llbksp97d02xaspyhrbmcpv6gpgwrysykcxrv98wl6")))
     (build-system rails-build-system)
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
    (hash (base32 "16236gnmci8g015a2blym6k4767lqfxbi122fngw9gcn05wllmv1")))
   (package
     (name "travel-advice-publisher")
     (version "release_310")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "158lj6b3rp34a32dczp12j24gcykg8d9cqs2rxl2rakinz6q94nj")))
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
    (hash (base32 "039ivrv2iz4b4472rnyfjfjb5i1n1x9vqi6ynisaljznniiwmmg5")))
   (package
     (name "whitehall")
     (version "release_13165")
     (source
      (github-archive
       #:repository name
       #:commit-ish version
       #:hash (base32 "0fr3g18vv1qvm6pxnl4qvx1kq4fggqwrwm6hx6awzq8l0gzv4zpf")))
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
