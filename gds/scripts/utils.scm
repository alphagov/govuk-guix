(define-module (gds scripts utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (option-values
            option-value))

(define (option-values opts key)
  (reverse
   (filter-map (match-lambda
                 ((head . tail)
                  (and (eq? key head) tail))
                 (_ #f))
               opts)))

(define* (option-value opts key #:key default)
  (let ((values (option-values opts key)))
    (if (null? values)
        default
        (car values))))
