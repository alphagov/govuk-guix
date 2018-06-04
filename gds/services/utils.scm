(define-module (gds services utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (guix base16)
  #:use-module (guix gcrypt)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (gds packages utils custom-sources)
  #:export (ensure-service-parameters
            update-service-parameters
            update-services-parameters
            default-shepherd-service
            random-base16-string
            correct-services-package-source
            correct-services-package-source-from-environment
            setup-app-directory))

(define (ensure-service-parameters s test-and-value-pairs)
  (service
   s
   (fold
    (lambda (test+value parameters)
      (match test+value
        ((test? . value)
         (if (list? parameters)
             (if (any test? parameters)
                 (map (lambda (x) (if (test? x) value x))
                      parameters)
                 (append parameters (list value)))
             (if (test? parameters)
                 value
                 (list parameters value))))))
    (service-parameters s)
    test-and-value-pairs)))

(define (update-service-parameters s test-and-function-pairs)
  (define (update-parameter initial-parameter)
    (fold
     (match-lambda*
       (((test . function) parameter)
        (if (test parameter)
            (function parameter)
            parameter)))
     initial-parameter
     test-and-function-pairs))

  (service
   (service-kind s)
   (let
       ((parameters (service-parameters s)))
     (if
      (list? parameters)
      (map update-parameter parameters)
      (update-parameter parameters)))))

(define (update-services-parameters services tests-and-functions)
  (map
   (lambda (service)
     (fold
      (match-lambda*
        (((type-or-test . test-and-function-pairs) service)
         (if (if (service-type? type-or-test)
                 (eq? (service-kind service) type-or-test)
                 (type-or-test service))
             (update-service-parameters
              service
              test-and-function-pairs)
             service)))
      service
      tests-and-functions))
   services))

;; Just specify #f for the fields without defaults
(define default-shepherd-service
  (shepherd-service
   (provision #f)
   (start #f)))

;;;
;;; Crypto
;;;

(define* (gen-random-bv #:optional (bv-length 50))
  (let* ((ptr (libgcrypt-func "gcry_create_nonce"))
         (proc (pointer->procedure void ptr `(* ,size_t))) ; buffer, length
         (bv (make-bytevector bv-length))
         (bv-ptr (bytevector->pointer bv)))
    (proc bv-ptr bv-length)
    bv))

(define (random-base16-string length)
  (bytevector->base16-string
   (gen-random-bv length)))

;;;
;;; Service package sources
;;;

(define (correct-service-package-source service
                                        service-revisions
                                        service-path-list
                                        service-commit-ish-list)
  (define service-name
    (symbol->string
     (service-type-name (service-kind service))))

  (update-service-parameters
   service
   (list
    (cons
     package?
     (lambda (pkg)
       (correct-source-of
        service-name
        pkg
        (assoc-ref service-path-list service-name)
        (or (assq-ref service-revisions (service-kind service))
            (assoc-ref service-commit-ish-list service-name))))))))

(define (correct-services-package-source service-revisions
                                         services)
  (let* ((service-commit-ish-list
          (get-service-package-source-config-list-from-environment
           environment-variable-commit-ish-regex))
         (service-path-list
          (get-service-package-source-config-list-from-environment
           environment-variable-path-regex)))

    (validate-custom-source-data service-revisions
                                 service-path-list
                                 service-commit-ish-list
                                 (filter-map
                                  (lambda (service)
                                    (let ((parameters
                                           (service-parameters service)))
                                      (if (list? parameters)
                                          (if (find package? parameters)
                                              (symbol->string
                                               (service-type-name
                                                (service-kind service)))
                                              #f)
                                          #f)))
                                  services))

    (log-service-package-path-list service-path-list)
    (log-service-package-commit-ish-list service-commit-ish-list)

    (map (lambda (service)
           (correct-service-package-source
            service
            service-revisions
            service-path-list
            service-commit-ish-list))
         services)))

;;;
;;; Apps
;;;

(define (setup-app-directory name package)
  (with-imported-modules (source-module-closure
                          '((guix build syscalls)
                            (gnu build file-systems)))

    #~(let* ((user (getpwnam #$name))
             (root-directory #$(string-append "/var/apps/" name))
             (bundle (string-append root-directory "/bin/bundle")))

        (define (bind-mount-package-in-store)
          (mkdir-p root-directory)
          (chown root-directory (passwd:uid user) (passwd:gid user))
          (bind-mount #$package root-directory)

          (let ((source #$(string-append "/var/log/apps/" name))
                (target (string-append root-directory "/log")))
            (if (file-exists? target)
                (begin
                  (mkdir-p source)
                  (chown source (passwd:uid user) (passwd:gid user))
                  (bind-mount source target)))))

        (define (tweak-existing-app-directory)
          (mkdir-p (string-append root-directory "/bin"))
          (mount "tmpfs" (string-append root-directory "/bin") "tmpfs")
          (copy-recursively
           (string-append #$package "/bin")
           (string-append root-directory "/bin")
           #:log (%make-void-port "w")
           #:follow-symlinks? #f)
          (substitute* (find-files (string-append root-directory "/bin")
                                   (lambda (name stat)
                                     (access? name X_OK)))
            (((string-append #$package "/bin"))
             "${BASH_SOURCE%/*}"))
          (substitute* (find-files (string-append root-directory "/bin")
                                   (lambda (name stat)
                                     (access? name X_OK)))
            (("File\\.expand_path\\([\"']\\.\\./spring[\"'], __FILE__\\)")
             "File.expand_path('../.spring-real', __FILE__)"))
          (for-each
           (lambda (path)
             (mkdir-p (string-append root-directory path))
             (chmod (string-append root-directory path) #o777))
           '("/tmp" "/log"))

          (for-each
           (cut chmod <> #o666)
           (find-files (string-append root-directory "/log")
                       #:directories? #f)))

        (use-modules (guix build utils)
                     (gnu build file-systems)
                     (guix build syscalls)
                     (ice-9 match)
                     (ice-9 ftw)
                     (srfi srfi-26))

        (if (file-exists? root-directory)
            (tweak-existing-app-directory)
            (bind-mount-package-in-store))

        (if (file-exists? (string-append root-directory "/tmp"))
            (mount "tmpfs" (string-append root-directory "/tmp") "tmpfs")))))
