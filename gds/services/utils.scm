(define-module (gds services utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (guix gcrypt)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define-public (ensure-service-parameters s test-and-value-pairs)
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

(define-public (update-service-parameters s test-and-function-pairs)
  (define (update-parameter parameter)
    (fold
     (lambda (test+function parameter)
       (match test+function
         ((test . function)
          (if (test parameter)
              (function parameter)
               parameter))))
     parameter
     test-and-function-pairs))

  (service
   (service-kind s)
   (let
       ((parameters (service-parameters s)))
     (if
      (list? parameters)
      (map update-parameter parameters)
      (update-parameter parameters)))))

(define-public (update-services-parameters services tests-and-functions)
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

(define-public (random-base16-string length)
  (bytevector->base16-string
   (gen-random-bv length)))

;;;
;;; G expressions
;;;

(define-public (run-command . args)
  #~(lambda ()
      (simple-format #t "Running command: ~A\n" (string-join '#$args))
      (let
          ((exit-val
            (status:exit-val (system* #$@args))))
        (display "\n")
        (if (zero? exit-val)
            #t
            (begin
              (simple-format
               #t
               "Command failed with exit status ~A: ~A\n"
               exit-val
               (string-join
                '#$args))
              #f)))))
