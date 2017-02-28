(define-module (gds packages third-party elasticsearch)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages java)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux))

(define-public elasticsearch-1.7.2
  (package
   (name "elasticsearch")
   (version "1.7.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://download.elastic.co/elasticsearch/elasticsearch/elasticsearch-"
           version ".tar.gz"))
     (sha256
      (base32 "1lix4asvx1lbc227gzsrws3xqbcbqaal7v10w60kch0c4xg970bg"))))
   (build-system gnu-build-system)
   (inputs
    `(("jre" ,icedtea)
      ("coreutils" ,coreutils)
      ("inetutils" ,inetutils)
      ("util-linux" ,util-linux)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'check)
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each
               (lambda (dir)
                 (copy-recursively dir (string-append out "/" dir)
                                   #:log (%make-void-port "w")))
               '("bin" "config" "lib"))
              (for-each
               delete-file
               (find-files
                (string-append out "/lib")
                (lambda (name stat)
                  (or (string-contains name "freebsd")
                      (string-contains name "solaris")))))
              (wrap-program
                  (string-append out "/bin/elasticsearch")
                `("ES_CLASSPATH" = (,(string-append
                                      out "/lib/elasticsearch-1.7.2.jar")
                                    ,(string-append out "/lib/*")
                                    ,(string-append out "/lib/sigar/*")))
                `("PATH" = (,(string-append (assoc-ref inputs "util-linux")
                                            "/bin")
                            ,(string-append (assoc-ref inputs "coreutils")
                                            "/bin")
                            ,(string-append (assoc-ref inputs "inetutils")
                                            "/bin")))
                `("JAVA_HOME" = (,(assoc-ref inputs "jre"))))
              #t))))))
   (home-page "")
   (synopsis "")
   (description "")
   (license "")))

(define-public elasticsearch elasticsearch-1.7.2)
