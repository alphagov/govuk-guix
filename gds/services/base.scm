(define-module (gds services base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:export (pretend-loopback-service
            set-file-ownership-service-type))

(define pretend-loopback-service
  (service
   (shepherd-service-type
    'dummy-loopback-service
    (const
     (shepherd-service
      (documentation "Pretend loopback service, just provides 'loopback")
      (provision '(loopback))
      (start #~(const #t))
      (stop #~(const #t)))))
   '()))

(define set-file-ownership-service-type
  (service-type
   (name 'set-file-ownership)
   (extensions
    (list (service-extension
           activation-service-type
           (lambda (paths-and-owners)
             #~(begin
                 (use-modules (ice-9 match))
                 (for-each
                  (match-lambda
                    ((path user group options ...)
                     (if (file-exists? path)
                         (let
                             ((command
                               `(,#$(file-append coreutils "/bin/chown")
                                 ,(simple-format #f "~A:~A" user group)
                                 ,@(if (memq #:recursive options) '("-R") '())
                                 ,path)))
                           (simple-format #t "Changing owner of ~A to ~A:~A\n"
                                          path user group)
                           (simple-format #t "Running command: ~A\n" (string-join command))
                           (apply system* command))
                         (simple-format #t "Skipping ~A as it does not exist\n" path))))
                  '#$paths-and-owners))))))
   (compose concatenate)
   (extend append)))
