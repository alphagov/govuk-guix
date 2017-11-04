(define-module (gds services govuk tls)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages certs)
  #:export (development-os-tls-private-key
            development-os-tls-certificate
            development-os-certificates-package-for-domains))

(define development-os-tls-private-key
  (computed-file
   "dev.gov.uk.key"
   #~(let ((certtool (string-append
                      #$gnutls "/bin/certtool")))
       (zero?
        (system*
         certtool
         "--generate-privkey"
         "--outfile" #$output)))))

(define (development-os-tls-certificate-template dns-names)
  (plain-file
   "dev.gov.uk.cert.template"
   (string-append
    "# The organization of the subject.
organization = \"govuk-guix development certificate\"

# In how many days, counting from today, this certificate will expire.
expiration_days = 365

"
   (string-join
    (map (lambda (name)
           (string-append "dns_name = " name))
         dns-names)
    "\n")
"

# Whether this certificate will be used for a TLS server
tls_www_server

# Whether this certificate will be used to encrypt data (needed
# in TLS RSA ciphersuites). Note that it is preferred to use different
# keys for encryption and signing.
encryption_key

serial 007

ca

cert_signing_key")))

(define (development-os-tls-certificate dns-names)
  (computed-file
   "dev.gov.uk.pem"
   #~(let ((certtool (string-append
                      #$gnutls "/bin/certtool")))
       (zero?
        (system*
         certtool
         "--generate-self-signed"
         "--load-privkey" #$development-os-tls-private-key
         "--template"
         #$(development-os-tls-certificate-template dns-names)
         "--outfile" #$output)))))

(define (development-os-certificates-package-for-domains dns-names)
  (package
    (name "govuk-development-certificates")
    (version "0")
    (source (development-os-tls-certificate dns-names))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (let ((dest
              (string-append
               (assoc-ref %outputs "out")
               "/etc/ssl/certs/dev.gov.uk.pem")))
         (use-modules (guix build utils))
         (mkdir-p (dirname dest))
         (copy-recursively
          (assoc-ref %build-inputs "source")
          dest)
         (setenv "PATH"
                 (string-append
                  (getenv "PATH")
                  ":"
                  (assoc-ref %build-inputs "perl") "/bin"))
         (with-directory-excursion (dirname dest)
           (or (zero?
                (system* (string-append
                          (assoc-ref %build-inputs "openssl")
                          "/bin/c_rehash")
                         "."))
               (error "couldn't rehash"))))))
    (native-inputs
     `(("perl" ,perl)
       ("openssl" ,openssl)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

