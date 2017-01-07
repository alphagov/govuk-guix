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
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages rsync)
  #:use-module (gds packages utils)
  #:use-module (gds packages utils bundler)
  #:use-module (gds packages third-party))

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
         (delete 'configure)
         (delete 'build)
         (delete 'check)
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

(define* (github-archive
          #:optional #:key
          repository
          (commit-ish (default-commit-ish))
          (user-or-org "alphagov")
          (url (if repository
                (string-append
                 "https://github.com/"
                 user-or-org "/"
                 repository "/archive/"
                 commit-ish ".tar.gz")
                #f))
          (hash-algo 'sha256)
          (hash #f))
  (if (not url)
      (error "Either repository, or the full url must be specified"))
  (origin
    (method url-fetch)
    (uri url)
    (sha256 hash)))

(define-public publishing-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "1mdcrhn96an5j9dbn2d3gli7a4b555zabihc93v4snzcjmfzin1k")))
   (make-govuk-package
    "publishing-api" ;; TODO: Pass archive details to make-govuk-package
    (github-archive
     #:repository "publishing-api"
     #:commit-ish "release_607"
     #:hash (base32 "1gz4njkbfdmv1qxb0d28wlzkmp421sxid5m7dh42vknjkp0prdqy")))))

(define-public content-store
  (package-with-bundler
   (bundle-package
    (hash (base32 "0938azjd7k7j4f7w6r8ydwd25bzjn39vxnn8k1vxnjwnn5pgcyff")))
   (make-govuk-package
    "content-store"
    (github-archive
     #:repository "content-store"
     #:commit-ish "release_585"
     #:hash (base32 "0i42za14jn0wrsninahymg9w3sddfnll8fbxmirrvcqcbxx55ydc")))))

(define-public specialist-publisher
  (package-with-bundler
   (bundle-package
    (hash (base32 "1ys7ahlw216hr6p80kbaxmwzrdyv60fh3a6qbzgsz76fvq2drakd"))
    (without '("development" "test")))
   (make-govuk-package
    "specialist-publisher"
    (github-archive
     #:repository "specialist-publisher"
     #:commit-ish "release_725"
     #:hash (base32 "0a6n4a1qhfmak089xjn3payyz6b3vyfkbljdifxzrw4b9m232yn9")))))

(define-public specialist-frontend
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f32hnh59wq9h6sf6aswsar566axqk269sp2czg3lh8wgvxjp4zl")))
   (make-govuk-package
    "specialist-frontend"
    (github-archive
     #:repository "specialist-frontend"
     #:commit-ish "release_174"
     #:hash (base32 "19dhk18as5w709rpyjncvk99ym1x12bpch25a1r6r858c71gia44")))))

(define-public signonotron2
  (package-with-bundler
   (bundle-package
    (hash (base32 "1f14g354y6xiy86vl5dwjz7yfq92cg15pwviw21c4awch04b0hm9")))
   (make-govuk-package
    "signonotron2"
    (github-archive
     #:repository "signonotron2"
     #:commit-ish "release_688"
     #:hash (base32 "1xqqcs4bncb8lkk4x0h90a4v1sfvmzr6x6b4sa24v8ap8707bbsa")))))

(define-public static
  (package-with-bundler
   (bundle-package
    (hash (base32 "01x4z3f1mls4m0wx8hn8h5cqkabii7gzcwg52ys3sd41g1rms9jg")))
   (make-govuk-package
    "static"
    (github-archive
     #:repository "static"
     #:commit-ish "release_2431"
     #:hash (base32 "14l7523zhb07vxql8i87v9wmzcw6g2328k8zaiqp2s8x8i2iwrs2")))))

(define-public router-api
  (package-with-bundler
   (bundle-package
    (hash (base32 "0a77i21cmjwvk2dqkx80fv8vkm7s2spsvaqh21nrz14c7gzrr6hz")))
   (make-govuk-package
    "router-api"
    (github-archive
     #:repository "router-api"
     #:commit-ish "release_106"
     #:hash (base32 "1f2gycb51mi7cfvm83lldczi56l9j6ra9c5db4b6hmm2wigwh53d")))))

(define-public router
  (let
      ((release "release_140"))
    (package
      (name "router")
      (version "0")
      (source
       (github-archive
        #:repository "router"
        #:commit-ish release
        #:hash (base32 "0v4r3vglqyrl6nrh24n7dxvh80vysgximgccqgyfxvmwlqrx26m7")))
      (build-system gnu-build-system)
      (native-inputs
       `(("go" ,go)
         ("bash" ,bash)
         ("rsync" ,rsync)))
      (arguments
       `(#:make-flags (list ,(string-append "RELEASE_VERSION=" release))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'install)
           (delete 'check)
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (cwd (getcwd))
                      (bash (string-append
                             (assoc-ref inputs "bash")
                             "/bin/bash")))
                 (mkdir-p "__build/src/github.com/alphagov")
                 (mkdir-p "__build/bin")
                 (setenv "GOPATH" (string-append cwd "/__build"))
                 (setenv "BINARY" (string-append cwd "/router"))
                 (system*
                  "rsync"
                  "-a"
                  "./"
                  "__build/src/github.com/alphagov/router" "--exclude=__build")
                 (and
                  (with-directory-excursion
                      "__build/src/github.com/alphagov/router"
                    (and
                     (zero? (system* "make" "build"))
                     (mkdir-p (string-append out "/bin"))))
                  (begin
                    (copy-file "router"
                               (string-append out "/bin/router"))
                    #t))))))))
      (synopsis name)
      (description name)
      (license #f)
      (home-page #f))))

(define-public publishing-e2e-tests
  (let
      ((govuk-package
        (make-govuk-package
         "publishing-e2e-tests"
         (github-archive
          #:repository "publishing-e2e-tests"
          #:commit-ish "da2c5e759fd4ff4bae62b7a41a80cee40cf136cb"
          #:hash (base32 "1arrlch7681yjs6v4qyn9wnwsxzpar1lsca0qfm8zdizl6nfi0l1")))))
    (package-with-bundler
     (bundle-package
      (hash
       (base32 "03ydsia5ipqclrs5yx8wldgi4m881hmvndwjsrb5s751fsbf8gmj")))
     (package
       (inherit govuk-package)
       (arguments
        (substitute-keyword-arguments (package-arguments govuk-package)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-before 'install 'add-bundle
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let*
                      ((out (assoc-ref outputs "out"))
                       (gemfile (string-append out "/Gemfile"))
                       (ruby
                        (string-append (assoc-ref inputs "ruby")
                                       "/bin/ruby")))
                    (define* (bundle ruby-path #:optional (port #f))
                      (format port "#!~A
ENV[\"BUNDLE_GEMFILE\"] ||= \"~A\"

load Gem.bin_path(\"bundler\", \"bundler\")" ruby-path gemfile))

                    (mkdir-p (string-append out "/bin"))
                    (call-with-output-file (string-append out "/bin/bundle")
                      (lambda (port)
                        (bundle ruby port)))
                    (chmod (string-append out "/bin/bundle") #o544)
                    #t)))))))))))

(define-public maslow
  (package-with-bundler
   (bundle-package
    (hash (base32 "0g1y50vds1rc2qgis6b8wbydmfmja9wgr5s6hw8hra1z7nm3m3hs")))
   (make-govuk-package
    "maslow"
    (github-archive
     #:repository "maslow"
     #:commit-ish "release_178"
     #:hash (base32 "1jyganvji06lkvs86fam7glvrn5gcx2jl9r6108ysbzdr2wfzs6j")))))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (github-archive
      #:repository "govuk-content-schemas"
      #:commit-ish "release_416"
      #:hash (base32 "03is44yfnq5xs08d80cw4rhw4psm6v09ki4cascx3k3rllcs8a6p")))
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
