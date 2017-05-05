(define-module (gds build utils)
  #:export (run-command))

(define (run-command . args)
  (simple-format #t "Running command: ~A\n" (string-join args))
  (let
      ((exit-val
        (status:exit-val (apply system* args))))
    (display "\n")
    (if (zero? exit-val)
        #t
        (begin
          (simple-format
           #t
           "Command failed with exit status ~A: ~A\n"
           exit-val
           (string-join args))
          #f))))
