(define-module (gds services govuk data-snapshot)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:export (load-data-snapshot-service-type))

(define (load-data-snapshot-activation snapshot)
  (with-imported-modules '((guix build utils))
    #~(let* ((wrap-coreutils-binary
              (lambda (binary)
                (lambda args
                  (simple-format #t "running: ~A ~A\n"
                                 binary
                                 (string-join args " "))
                  (apply invoke
                         (string-append #$coreutils "/bin/" binary)
                         args))))
             (rm (wrap-coreutils-binary "rm"))
             (cp (wrap-coreutils-binary "cp"))

             (tar (lambda args
                    (simple-format #t "running: tar ~A\n"
                                   (string-join args " "))
                    (apply invoke
                           #$(file-append tar "/bin/tar")
                           args)))

             (snapshot-var-lib (string-append
                                #$snapshot
                                "/var/lib")))

        (use-modules (ice-9 ftw)
                     (guix build utils))

        (mkdir-p "/var/lib")
        (with-directory-excursion "/var/lib"
          (for-each (lambda (archive-name)
                      (if (file-exists? archive-name)
                          (rm "-rf" archive-name))

                      (tar "--extract"
                           "--file" (string-append snapshot-var-lib
                                                   "/" archive-name)))
                    (scandir snapshot-var-lib
                             (negate
                              (lambda (f)
                                (member f '("." ".."))))))))))

(define load-data-snapshot-service-type
  (service-type
   (name 'load-data-snapshot)
   (extensions
    (list (service-extension activation-service-type
                             load-data-snapshot-activation)))))
