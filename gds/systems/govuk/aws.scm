(define-module (gds systems govuk aws)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gds systems govuk production)
  #:export (aws-pubkey-service-type
            govuk-aws-os))

(define aws-fetch-ssh-key-script
  (plain-file
   "aws-fetch-ssh-key"
   "#!/usr/bin/guile
!#

(use-modules
  (ice-9 receive)
  (web client)
  (ice-9 binary-ports))

(define (meta-data path)
  (let ((uri (string-append \"http://169.254.169.254/latest/\" path)))
    (receive (header body)
        (http-get uri #:decode-body? #f)
      body)))

(let* ((pubkey (meta-data \"meta-data/public-keys/0/openssh-key\")))
  (call-with-output-file \"/etc/ssh/authorized_keys.d/govuk\"
    (lambda (port)
      (put-bytevector port pubkey))))"))

(define-public aws-fetch-ssh-key
  (package
    (name "aws-fetch-ssh-key")
    (version "0.1")
    (source aws-fetch-ssh-key-script)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bin-dir  (string-append %output "/bin"))
                (bin-file (string-append bin-dir "/aws-fetch-ssh-key"))
                (guile-bin (string-append (assoc-ref %build-inputs "guile")
                                          "/bin")))
           (mkdir-p bin-dir)
           (copy-file (assoc-ref %build-inputs "source") bin-file)
           (patch-shebang bin-file (list guile-bin))
           (chmod bin-file #o555)))))
    (inputs `(("guile" ,guile-2.2)))
    (home-page #f)
    (synopsis "A simple AWS EC2 ssh key fetcher")
    (description "fetch-ssh-key.scm is a simple tool that fetches the
ssh public key from instance metadata, assuming you're running on
AWS EC2.")
    (license #f)))

(define aws-pubkey-service-type
  (shepherd-service-type
   'aws-pubkey
   (lambda (package)
     (shepherd-service
      (documentation "Initialize admin user public key.")
      (requirement '(networking))
      (provision '(aws-pubkey))
      (one-shot? #t)
      (start
       #~(lambda _
           (system* #$(file-append package "/bin/aws-fetch-ssh-key"))))
      (respawn? #f)))
   aws-fetch-ssh-key))

(define govuk-aws-os
  (operating-system
    (inherit govuk-production-os)

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/xvdf")))

    (kernel-arguments
     '("quiet"
       ;; The following argument makes the "System Log" work on the AWS
       ;; website
       "console=ttyS0"))

    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "govuk")
                  (comment "Admin user")
                  (group "users")

                  ;; Adding the account to the "wheel" group
                  ;; makes it a sudoer.
                  (supplementary-groups '("wheel"))
                  (home-directory "/home/govuk"))
                 %base-user-accounts))

    (sudoers-file (plain-file
                   "sudoers"
                   (string-append
                    "root ALL=(ALL) ALL\n"
                    "%wheel ALL=(ALL) NOPASSWD:ALL\n")))

    (services (cons* (service aws-pubkey-service-type)
                     (service openssh-service-type
                              (openssh-configuration
                               (password-authentication? #f)))
                     (operating-system-user-services govuk-production-os)))))
