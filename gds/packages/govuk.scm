(define-module (gds packages govuk)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download))

(define (make-govuk-package
         name
         treeish
         hash)
  (package
    (name name)
    (version "0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append
                   "https://github.com/alphagov/" name ".git"))
             (commit treeish)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256 hash)))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)))
    (propagated-inputs
     `(("ruby" ,ruby)
       ("bundler" ,bundler)
       ("gnu-make" ,gnu-make)
       ("gcc-toolchain" ,gcc-toolchain-5)
       ("linux-libre-headers" ,linux-libre-headers)
       ("nss-certs" ,nss-certs)
       ("postgresql" ,postgresql)
       ("libxml2" ,libxml2)
       ("tzdata" ,tzdata)
       ("coreutils" ,coreutils) ;; just for ls
       ("libxslt" ,libxslt)
       ("node" ,node)
       ))
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
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path
                     (list->search-path-as-string
                       (filter
                        (lambda (path)
                          (not (string-prefix? (assoc-ref %build-inputs "source")
                                              path)))
                        (search-path-as-string->list (getenv "PATH")));)
                      ":")))
               (copy-recursively "." out)
               (patch-shebang
                (string-append out "/startup.sh"))
               (substitute* (find-files (string-append out "/bin"))
                 (("/usr/bin/env") (which "env")))
               (for-each
                (lambda (script)
                  (wrap-program
                      script
                    `("PATH" = (,path))
                    `("SSL_CERT_DIR" = (,(string-append (assoc-ref %build-inputs "nss-certs")
                                                        "/etc/ssl/certs")))
                    `("C_INCLUDE_PATH" ":" prefix (,(getenv "C_INCLUDE_PATH")))
                    `("CPLUS_INCLUDE_PATH" ":" prefix (,(getenv "CPLUS_INCLUDE_PATH")))
                    `("LIBRARY_PATH" ":" prefix (,(getenv "LIBRARY_PATH")))
                    `("GEM_PATH" ":" prefix (,(getenv "GEM_PATH")))))
                (find-files (string-append out "/bin")))
               #t))))))
    (synopsis name)
    (description name)
    (license #f)
    (home-page #f)))

(define-public publishing-api
  (make-govuk-package "publishing-api"
                      "a35bd59c2020569f8318d4e7fd73c88a5af6f796"
                      (base32
                       "0vr9bih705jhxb10p2z95jv94b7yj5wmn2c8xgvdh70j1ifi3dqc")))

(define-public content-store
  (make-govuk-package "content-store"
                      "a92c2b81d43e8f03aad146f5e6532395a5209737"
                      (base32
                       "1xz2qhy9ci2796csl2zs6p7vb065n594bcawnl4i80w5fp6pps4j")))

(define-public specialist-publisher
  (make-govuk-package "specialist-publisher"
                      "464e216577d597a6195a287d233269a700b517d1"
                      (base32
                       "1pcxzp1s55rmbyry07pzwksb5h8qfrh4wadk4h7j6b7mp2wmy2s1")))

(define-public govuk-content-schemas
  (package
    (name "govuk-content-schemas")
    (version "0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alphagov/govuk-content-schemas.git")
             (commit "master")))
       (file-name (string-append "govuk-content-schemas-master-checkout"))
       (sha256
        (base32
         "0q2gdlgx4kc3pj70vrw4ajcbb8hphx5slc8mbhs5g2lyw5d2ah9r"))))
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
