(define-module (gds data transformations)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:export (<data-transformation>
            data-transformation
            data-transformation?
            data-transformation-output-name
            data-transformation-output-description
            data-transformation-operation))

(define-record-type* <data-transformation>
  data-transformation make-data-transformation
  data-transformation?
  (output-name data-transformation-output-name)
  (output-description data-transformation-output-description
                      (default #f))
  (operation data-transformation-operation))

(define-gexp-compiler (data-transformation-compiler
                       (data-transformation <data-transformation>)
                       system target)
  (match data-transformation
    (($ <data-transformation> output-name output-description
                              operation)

     (gexp->derivation output-name operation
                       #:system system))))

