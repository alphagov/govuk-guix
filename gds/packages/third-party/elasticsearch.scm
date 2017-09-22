(define-module (gds packages third-party elasticsearch)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages java)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux))

(define-public elasticsearch-2.4.6
  (package
   (name "elasticsearch")
   (version "2.4.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://download.elastic.co/elasticsearch/elasticsearch/elasticsearch-"
           version ".tar.gz"))
     (sha256
      (base32 "0vzw9kpyyyv3f1m5sy9zara6shc7xkgi5xm5qbzvfywijavlnzjz"))))
   (build-system gnu-build-system)
   (inputs
    `(("jre" ,icedtea)
      ("coreutils" ,coreutils)
      ("inetutils" ,inetutils)
      ("util-linux" ,util-linux)
      ("grep" ,grep)))
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
               '("bin" "config" "lib" "modules"))
              (for-each
               (lambda (dir)
                 (mkdir (string-append out "/" dir)))
               '("plugins"))
              (for-each
               delete-file
               (find-files
                (string-append out "/lib")
                (lambda (name stat)
                  (or (string-contains name "freebsd")
                      (string-contains name "solaris")))))
              (wrap-program
                  (string-append out "/bin/elasticsearch")
                `("PATH" = (,(string-append (assoc-ref inputs "util-linux")
                                            "/bin")
                            ,(string-append (assoc-ref inputs "coreutils")
                                            "/bin")
                            ,(string-append (assoc-ref inputs "inetutils")
                                            "/bin")
                            ,(string-append (assoc-ref inputs "grep")
                                            "/bin")))
                `("JAVA_HOME" = (,(assoc-ref inputs "jre"))))
              #t))))))
   (home-page "")
   (synopsis "")
   (description "")
   (license "")))

(define-public elasticsearch elasticsearch-2.4.6)
