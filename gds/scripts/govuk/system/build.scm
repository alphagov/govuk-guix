(define-module (gds scripts govuk system build)
  #:use-module (guix derivations)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix scripts build)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system file-systems)
  #:use-module (gds scripts govuk system)
  #:export (build))

(define (vm-image-and-system os opts)
  (define vm-os
    (let ((base-os
           (virtualized-operating-system
            (alter-services-for-vm os)
            '())))
      (operating-system
        (inherit base-os)
        (file-systems
         (map (lambda (fs)
                (if (string=? (file-system-mount-point fs)
                              "/gnu/store")
                    (file-system
                      (inherit fs)
                      (device "TAG_gnu_store"))
                    fs))
              (operating-system-file-systems base-os))))))

  (with-store store
    (set-build-options-from-command-line store opts)

    (run-with-store store
      (mbegin %store-monad
        (set-grafting #f)
        (mlet* %store-monad
            ((item
              (gexp->derivation
               "vm-image-and-system"
               #~(begin
                   (mkdir #$output)
                   (chdir #$output)
                   (symlink #$((operating-system-derivation vm-os) store)
                            "system")
                   (symlink #$((system-qemu-image/shared-store
                                vm-os
                                ;; TODO: This shouldn't be necessary
                                #:disk-image-size (* 80 (expt 2 20)))
                               store)
                            "image")))))
          (mbegin %store-monad
            (built-derivations (list item))
            (return (derivation->output-path item))))))))

(define (disk-image os opts)
  (with-store store
    (set-build-options-from-command-line store opts)

    (run-with-store store
      (mbegin %store-monad
        (set-grafting #f)
        (mlet* %store-monad
            ((item (system-disk-image
                    (alter-services-for-vm os)
                    #:name "disk-image"
                    #:disk-image-size 'guess)))

          (mbegin %store-monad
            (built-derivations (list item))
            (return (derivation->output-path item))))))))

(define (build opts)
  (let* ((type (assq-ref opts 'type))
         (os   (opts->operating-system
                opts
                #:default-read-bundle-install-input-as-tar-archive?
                (assq-ref
                 '((vm-image-and-system . #t)
                   (vm-start-script . #t)
                   (container-start-script . #f)
                   (disk-image . #f))
                 type)))
         (build-function
          (assq-ref
           `((vm-image-and-system . ,vm-image-and-system)
             (vm-start-script . ,vm-start-script)
             (disk-image . ,disk-image)
             (container-start-script . ,container-start-script))
           type))
         (output (build-function os opts)))

    (display-system-information os)
    (newline)
    (display output)
    (newline)
    (exit 0)))
