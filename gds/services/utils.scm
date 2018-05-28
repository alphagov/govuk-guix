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

(define (correct-services-package-source services
                                         service-path-list
                                         service-commit-ish-list)
  (validate-custom-source-data service-path-list
                               service-commit-ish-list
                               (filter-map
                                (lambda (service)
                                  (let ((parameters (service-parameters service)))
                                    (if (list? parameters)
                                        (if (find package? parameters)
                                            (symbol->string
                                             (service-type-name
                                              (service-kind service)))
                                            #f)
                                        #f)))
                                services))
  (map
   (lambda (service)
     (update-service-parameters
      service
      (list
       (cons
        package?
        (lambda (pkg)
          (correct-source-of (symbol->string
                              (service-type-name (service-kind service)))
                             pkg
                             service-path-list
                             service-commit-ish-list))))))
   services))

(define (correct-services-package-source-from-environment services)
  (let* ((service-commit-ish-list
          (get-service-package-source-config-list-from-environment
           environment-variable-commit-ish-regex))
         (service-path-list
          (get-service-package-source-config-list-from-environment
           environment-variable-path-regex))
         (new-services
          (correct-services-package-source
           services
           service-path-list
           service-commit-ish-list)))
      (log-service-package-path-list service-path-list)
      (log-service-package-commit-ish-list service-commit-ish-list)
      new-services))

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

        (use-modules (guix build utils)
                     (gnu build file-systems)
                     (guix build syscalls)
                     (ice-9 match)
                     (ice-9 ftw)
                     (srfi srfi-26))

        (if
         (not (file-exists? root-directory))
         (begin
           (mkdir-p root-directory)
           (chown root-directory (passwd:uid user) (passwd:gid user))
           (bind-mount #$package root-directory)

           (for-each
            (lambda (file)
              (if (file-exists? file)
                  (mount "tmpfs" file "tmpfs")))
            (map
             (lambda (dir)
               (string-append root-directory "/" dir))
             '("log"))))
         (begin
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
                        #:directories? #f))))

        (if (file-exists? (string-append root-directory "/tmp"))
            (mount "tmpfs" (string-append root-directory "/tmp") "tmpfs")))))
