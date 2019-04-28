(define-module (gds scripts govuk system start)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gds scripts govuk system)
  #:export (start))

(define (start opts)
  (define (sudo-path)
    (find
     file-exists?
     '("/run/setuid-programs/sudo"
       "/usr/bin/sudo")))

  (define* (run args #:key as-root)
    (let ((command
           (if as-root
               (if (eq? (getuid) 1)
                   args
                   (cons (sudo-path) args))
               args)))
      (format #t "Running command:~%  ~a~2%" (string-join command " "))
      (status:exit-val
       (apply system* command))))

  (let* ((type (assq-ref opts 'type))
         (start-script-builder
          (or (assq-ref `((vm-start-script . ,vm-start-script)
                          (container-start-script . ,container-start-script))
                        type)
              (begin
                (simple-format #t "start is not supported for type:~A\n"
                             type)
                (exit 1))))
         (os (opts->operating-system
              opts
              #:default-read-bundle-install-input-as-tar-archive?
              (assq-ref
               '((vm-image-and-system . #t)
                 (vm-start-script . #t)
                 (container-start-script . #f))
               type)))
         (start-script (start-script-builder os opts)))

    (display-system-information os)
    (newline)
    (exit
     (run `(,start-script
            ,@(if (eq? type 'vm-start-script)
                  '("-m" "8G"
                    "-net" "user"
                    "-net" "nic,model=virtio"
                    "-enable-kvm")
                  '()))
          #:as-root (eq? type 'container-start-script)))))
