(define-module (gds scripts govuk system init)
  #:use-module (srfi srfi-1)
  #:use-module (guix derivations)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix scripts build)
  #:use-module (gnu system)
  #:use-module (gnu bootloader)
  #:use-module (gnu system vm)
  #:use-module (gnu system file-systems)
  #:use-module (gds systems utils packer)
  #:use-module (gds scripts utils)
  #:use-module (gds scripts govuk system)
  #:export (init))

(define guix-system-perform-action
  (@@ (guix scripts system) perform-action))

(define (init opts)
  (let* ((target (assq-ref opts 'target))
         (os     (alter-services-for-vm
                  (opts->operating-system opts)))
         (bootloader-target
          (option-value
           opts
           'bootloader-target
           #:default
           (bootloader-configuration-target
            (operating-system-bootloader os)))))

    (display-system-information os)

    (with-store store
      (set-build-options-from-command-line store opts)

      (run-with-store store
        (mbegin %store-monad
          (set-grafting #f)
          (guix-system-perform-action
           'init os
           #:dry-run? #f
           #:derivations-only? #f
           #:skip-safety-checks? #f

           #:use-substitutes? #t
           #:file-system-type "ext4"
           #:image-size 'guess
           #:full-boot? #t
           #:install-bootloader? #t
           #:target (assq-ref opts 'target)
           #:bootloader-target bootloader-target))))
    (exit 0)))
